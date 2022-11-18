module Main where

import Control.Lens (makeLenses, (%~), (.~), (?~), (^.))
import Data.Binary.Get (getWord16le, isEmpty, runGet)
import Data.Bits
import Data.ByteString.Builder (doubleBE, toLazyByteString, word16LE)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Modular (Mod, toMod, unMod)
import Data.Text qualified as T
import Relude.Unsafe qualified as U
import System.IO (getChar, getLine, putChar)
import Prelude hiding (getLine)

newtype Register = Register Value deriving (Show)

type RegisterId = Integer `Mod` 8

type RegisterMap = Map RegisterId Register

mkRegisterMap :: RegisterMap
mkRegisterMap = M.fromList $ zip [0 .. 7] (repeat $ Register (ValNumber 0))

newtype Address = Address {unAddress :: Nat15} deriving (Enum, Num, Ord, Eq, Show)

type Memory = Map Address Value

type Nat15 = Integer `Mod` 32768

data Value
  = ValNumber Nat15
  | ValAddress Address
  | ValRegister RegisterId
  deriving (Show)

type Stack = [Value]

data Machine = Machine
  { _memory :: Memory,
    _registers :: RegisterMap,
    _stack :: Stack,
    _pc :: Address,
    _stdOut :: Maybe Char,
    _stdIn :: Maybe Char,
    _halted :: Bool
  }

makeLenses ''Machine

toNat15 :: Machine -> Value -> Nat15
toNat15 _ (ValNumber n) = n
toNat15 _ (ValAddress (Address a)) = a
toNat15 m (ValRegister i) = let (Register v) = (m ^. registers) M.! i in toNat15 m v

data OpCode
  = OpHalt
  | OpSet
  | OpPush
  | OpPop
  | OpEq
  | OpGt
  | OpJmp
  | OpJt
  | OpJf
  | OpAdd
  | OpMult
  | OpMod
  | OpAnd
  | OpOr
  | OpNot
  | OpRmem
  | OpWmem
  | OpCall
  | OpRet
  | OpOut
  | OpIn
  | OpNoop
  deriving (Eq, Ord, Show)

toOpCode :: Nat15 -> Maybe OpCode
toOpCode 0 = Just OpHalt
toOpCode 1 = Just OpSet
toOpCode 2 = Just OpPush
toOpCode 3 = Just OpPop
toOpCode 4 = Just OpEq
toOpCode 5 = Just OpGt
toOpCode 6 = Just OpJmp
toOpCode 7 = Just OpJt
toOpCode 8 = Just OpJf
toOpCode 9 = Just OpAdd
toOpCode 10 = Just OpMult
toOpCode 11 = Just OpMod
toOpCode 12 = Just OpAnd
toOpCode 13 = Just OpOr
toOpCode 14 = Just OpNot
toOpCode 15 = Just OpRmem
toOpCode 16 = Just OpWmem
toOpCode 17 = Just OpCall
toOpCode 18 = Just OpRet
toOpCode 19 = Just OpOut
toOpCode 20 = Just OpIn
toOpCode 21 = Just OpNoop
toOpCode _ = Nothing

numArgs :: OpCode -> Integer
numArgs OpHalt = 0
numArgs OpSet = 2
numArgs OpPush = 1
numArgs OpPop = 1
numArgs OpEq = 3
numArgs OpGt = 3
numArgs OpJmp = 1
numArgs OpJt = 2
numArgs OpJf = 2
numArgs OpAdd = 3
numArgs OpMult = 3
numArgs OpMod = 3
numArgs OpAnd = 3
numArgs OpOr = 3
numArgs OpNot = 2
numArgs OpRmem = 2
numArgs OpWmem = 2
numArgs OpCall = 1
numArgs OpRet = 0
numArgs OpOut = 1
numArgs OpIn = 1
numArgs OpNoop = 0

isJump :: OpCode -> Bool
isJump OpJmp = True
isJump OpJt = True
isJump OpJf = True
isJump _ = False

mkMemory :: [Word16] -> Memory
mkMemory bs = M.fromList [(i, parseWord16 b) | (i, b) <- zip [Address 0 ..] bs]

parseWord16 :: Word16 -> Value
parseWord16 b
  | bi <= 32767 = ValNumber (toMod bi)
  | bi <= 32775 = ValRegister (toMod $ bi - 32768)
  | otherwise = error $ "Invalid byte encountered: " <> show b
  where
    bi = toInteger b

mkMachine :: Memory -> Machine
mkMachine mem =
  Machine
    { _memory = mem,
      _registers = mkRegisterMap,
      _stack = [],
      _pc = Address 0,
      _stdOut = Nothing,
      _stdIn = Nothing,
      _halted = False
    }

nat15At :: Machine -> Address -> Nat15
nat15At m a = toNat15 m $ (m ^. memory) M.! a

getOpCode :: Machine -> Maybe OpCode
getOpCode m = toOpCode . nat15At m $ m ^. pc

step :: Bool -> Machine -> Machine
step dbg m = runOp dbg m opCode args
  where
    opCode = case getOpCode m of
      Nothing -> error "Invalid opcode"
      Just op -> op
    args = ((m ^. memory) M.!) <$> [m ^. pc + Address 1 .. m ^. pc + Address (toMod $ numArgs opCode)]

takeNat15Args :: Machine -> [Value] -> (Nat15, Nat15, Nat15)
takeNat15Args m = toTuple3 . fmap (toNat15 m) . take 3
  where
    toTuple3 [a, b, c] = (a, b, c)
    toTuple3 [a, b] = (a, b, 0)
    toTuple3 [a] = (a, 0, 0)
    toTuple3 _ = (0, 0, 0)

takeRegisterArg :: [Value] -> RegisterId
takeRegisterArg = deref . U.head
  where
    deref (ValRegister rId) = rId
    deref _ = error "Bad dereference"

runOp :: Bool -> Machine -> OpCode -> [Value] -> Machine
runOp dbg m opCode args =
  let outM = resetOut $ if jumped then m' else m' & pc .~ nextInstr
   in if dbg
        then
          traceShow (m ^. stack) $
            traceShow (m ^. registers) $
              traceShow (m ^. pc, opCode, args) $ outM
        else outM
  where
    (a, b, c) = takeNat15Args m args
    ra = takeRegisterArg args
    setA v = m & registers %~ M.adjust (const . Register . ValNumber $ v) ra
    push v = m & stack %~ (v :)
    nextInstr = m ^. pc + 1 + fromIntegral (length args)
    unjust (Just x) = x
    resetOut m = if opCode == OpOut then m else m & stdOut .~ Nothing
    (m', jumped) =
      case opCode of
        OpHalt -> (m & halted .~ True, False)
        OpSet -> (setA b, False)
        OpPush -> (push $ ValNumber a, False)
        OpPop -> (let (v : vs) = m ^. stack in setA (toNat15 m v) & stack .~ vs, False)
        OpEq -> (setA $ if b == c then 1 else 0, False)
        OpGt -> (setA $ if b > c then 1 else 0, False)
        OpJmp -> (m & pc .~ Address a, True)
        OpJt -> if a /= 0 then (m & pc .~ Address b, True) else (m, False)
        OpJf -> if a == 0 then (m & pc .~ Address b, True) else (m, False)
        OpAdd -> (setA $ b + c, False)
        OpMult -> (setA $ b * c, False)
        OpMod -> (setA $ toMod $ unMod b `mod` unMod c, False)
        OpAnd -> (setA $ toMod $ unMod b .&. unMod c, False)
        OpOr -> (setA $ toMod $ unMod b .|. unMod c, False)
        -- TODO: Is this a valid 15-bit complement???
        OpNot -> (setA $ toMod $ complement $ unMod b, False)
        OpRmem -> (setA $ toNat15 m $ (m ^. memory) M.! Address b, False)
        OpWmem -> (m & memory %~ M.insert (Address a) (ValNumber b), False)
        OpCall -> (push (ValAddress nextInstr) & pc .~ Address a, True)
        OpRet ->
          ( case m ^. stack of
              [] -> m & halted .~ True
              (a : vs) -> m & pc .~ Address (toNat15 m a) & stack .~ vs,
            True
          )
        OpOut -> (m & stdOut ?~ (chr . fromIntegral . unMod $ a), False)
        OpIn -> (setA (toMod . fromIntegral . ord . unjust $ m ^. stdIn), False)
        OpNoop -> (m, False)

runMachine :: Machine -> IO ()
runMachine =
  loop
    ( Just
        ( T.unpack $
            unlines
              [ "take tablet",
                "doorway",
                "north",
                "north",
                "bridge",
                "continue",
                "down",
                "east",
                "take empty lantern",
                "west",
                "west",
                "passage",
                "ladder"
              ]
        )
    )
  where
    getInput = do
      s <- getLine
      return $ s ++ "\n"
    loop lastIn m = do
      (mIn, lastIn') <- do
        if getOpCode m == Just OpIn
          then do
            (c : nextIn) <- maybe getInput return lastIn
            return $
              ( m & stdIn ?~ c,
                case nextIn of
                  [] -> Nothing
                  xs -> Just xs
              )
          else return (m, lastIn)
      let dbg = False -- TODO: Set up a debugger
      let m' = step dbg mIn
      forM_ (m' ^. stdOut) putChar
      if m' ^. halted then return () else loop lastIn' m'

readFile16 :: String -> IO [Word16]
readFile16 path = do
  input <- BL.readFile path
  let getter = do
        done <- isEmpty
        if done
          then return []
          else do
            x <- getWord16le
            xs <- getter
            return (x : xs)
  return $ runGet getter input

-- TODO: Step through assembly while going between rooms, look for Jt / Jf / Eq / Gt failures
main :: IO ()
main = runMachine . mkMachine . mkMemory =<< readFile16 "data/challenge.bin"

--main = putTextLn . unlines . fmap T.pack . prettyMemory . mkMemory =<< readFile16 "data/challenge.bin"

prettyMemory :: Memory -> [String]
prettyMemory mem = pretty . M.toList $ mem
  where
    machine = mkMachine mem
    pretty [] = []
    pretty ((i, opB) : rest) =
      let opCode = toOpCode (toNat15 machine opB)
          toC (ValNumber v) = chr . fromIntegral $ unMod v
          toC _ = '#'
       in case opCode of
            Just op ->
              let nArgs = fromIntegral $ numArgs op
                  args = snd <$> take nArgs rest
                  rest' = drop nArgs rest
               in case op of
                    OpOut -> show (toC <$> args) : pretty rest'
                    _ -> show (unAddress i, op, args) : pretty rest'
            Nothing ->
              show (unAddress i, "GLOBAL", opB) : pretty rest
