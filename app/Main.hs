module Main where

import Control.Exception
import Control.Lens (makeLenses, (%~), (.~), (?~), (^.))
import Control.Monad.Memo
import Data.Binary.Get (getWord16le, isEmpty, runGet)
import Data.Bits ( Bits(complement, (.&.), (.|.)) )
import Data.ByteString.Builder (doubleBE, toLazyByteString, word16LE)
import Data.ByteString.Lazy qualified as BL
import Data.IntMap.Lazy (insertLookupWithKey)
import Data.List hiding (unlines)
import Data.List.Extra (minimumOn)
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Modular (Mod, toMod, unMod)
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Haskell.Interpreter
import Relude.Extra (Foldable1 (minimumOn1))
import Relude.Unsafe qualified as U
import System.IO (getChar, getLine, putChar)
import System.IO.Unsafe
import Prelude hiding (getLine)

newtype Register = Register Value deriving (Show)

type RegisterId = Integer `Mod` 8

type RegisterMap = Map RegisterId Register

mkRegisterMap :: RegisterMap
mkRegisterMap =
  M.fromList $
    zip
      [0 .. 7]
      (replicate 8 (Register (ValNumber 0)))

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

data InvalidOpcodeError = InvalidOpcodeError

step :: Machine -> Either InvalidOpcodeError (Machine, OpCode, [Value])
step m =
  case getOpCode m of
    Nothing -> Left InvalidOpcodeError
    Just opCode ->
      let args = ((m ^. memory) M.!) <$> [m ^. pc + Address 1 .. m ^. pc + Address (toMod $ numArgs opCode)]
       in Right (runOp m opCode args, opCode, args)

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

runOp :: Machine -> OpCode -> [Value] -> Machine
runOp m opCode args =
  --traceShow (m ^. pc, opCode, args) $
  resetOut $ if jumped then m' else m' & pc .~ nextInstr
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

runMachine :: Nat15 -> Machine -> IO ()
runMachine reg7Override =
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
                "ladder",
                "west",
                "south",
                "north",
                "take can",
                "use can",
                "use lantern",
                "west",
                "ladder",
                "darkness",
                "continue",
                "west",
                "west",
                "west",
                "west",
                "north",
                "take red coin", -- 2
                "north",
                "east",
                "take concave coin", -- 7
                "down",
                "take corroded coin", -- 3
                "up",
                "west",
                "west",
                "take blue coin", -- 9
                "up",
                "take shiny coin", -- 5
                "down",
                "east",
                -- 9 2 5 7 3
                "use blue coin",
                "use red coin",
                "use shiny coin",
                "use concave coin",
                "use corroded coin",
                "north",
                "take teleporter",
                "use teleporter",
                "north",
                "north",
                "north",
                "north",
                "north",
                "north",
                "north",
                "north",
                "north",
                "take orb",
                "north",
                "east",
                "east",
                "north",
                "west",
                "south",
                "east",
                "east",
                "west",
                "north",
                "north",
                "east",
                "vault",
                "take mirror",
                "use mirror"
              ]
        )
    )
    False
    []
  where
    getInput = do
      s <- getLine
      return $ s ++ "\n"
    loop lastIn dbg history m = do
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
      -- let breakpoints = S.fromList [5449, 6027] -- before teleport, verification call
      let breakpoints = S.fromList []
      let dbgBreak = dbg || ((m ^. pc) `S.member` breakpoints)
      case step mIn of
        Left InvalidOpcodeError -> putStrLn $ "Failing due to invalid op code: " <> show (m ^. pc)
        Right (m', opCode, args) -> do
          let dbgLoop = do
                putStrLn $ "dbg (" <> show (m ^. pc, opCode, args) <> ")> "
                dbgLine <- getLine
                case dbgLine of
                  "s" -> return (m', True)
                  "c" -> return (m', False)
                  "st" -> print (m ^. stack) >> dbgLoop
                  "r" -> print (m ^. registers) >> dbgLoop
                  "dump" -> forM_ (prettyMemory $ m ^. memory) putStrLn >> dbgLoop
                  "block" -> forM_ (blockMemory $ m ^. memory) putStrLn >> dbgLoop
                  "set" -> return (m' & registers %~ M.insert 7 (Register (ValNumber 12345)), True)
                  "h" -> forM_ (take 10 history) print >> dbgLoop
                  "j" -> return (m' & pc .~ 5491 & registers %~ M.insert 0 (Register (ValNumber 6)) & stack %~ U.tail, True)
                  _ -> dbgLoop
          (m'', dbg') <-
            if not dbgBreak
              then return (m', False)
              else dbgLoop

          forM_ (m'' ^. stdOut) putChar

          -- Auto-teleportation
          let m''' =
                if m'' ^. pc == 5449
                  then m'' & registers %~ M.insert 7 (Register (ValNumber reg7Override))
                  else
                    if m'' ^. pc == 6027
                      then m'' & pc .~ 5491 & registers %~ M.insert 0 (Register (ValNumber 6)) & stack %~ U.tail
                      else m''

          if m''' ^. halted then return () else loop lastIn' dbg' ((m ^. pc, opCode, args) : history) m'''

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

main :: IO ()
main = runMachine 25734 . mkMachine . mkMemory =<< readFile16 "data/challenge.bin"

prettyMemory :: Memory -> [String]
prettyMemory mem = pretty . M.toList $ mem
  where
    machine = mkMachine mem
    pretty [] = []
    pretty ((i, opB) : rest) =
      let opCode = toOpCode (toNat15 machine opB)
          toC (ValNumber v)
            | v > 31 && v < 127 = show $ chr . fromIntegral $ unMod v
            | otherwise = show v
          toC _ = "#"
       in case opCode of
            Just op ->
              let nArgs = fromIntegral $ numArgs op
                  args = snd <$> take nArgs rest
                  rest' = drop nArgs rest
               in case op of
                    OpOut -> show (toC <$> args) : pretty rest'
                    _ -> show (unAddress i, op, args) : pretty rest'
            Nothing ->
              show (unAddress i, "GLOBAL", toC opB) : pretty rest

blockMemory :: Memory -> [String]
blockMemory mem = (fmap . fmap) toChar . chunksOf 64 . M.toList $ mem
  where
    machine = mkMachine mem
    toChar (_, ValNumber v)
      | v > 31 && v < 127 = chr . fromIntegral $ unMod v
      | otherwise = '.'
    toChar _ = '.'

solveMonument :: [[Integer]]
solveMonument =
  [ p
    | p@[a, b, c, d, e] <- permutations [2, 7, 3, 9, 5],
      let v = a + b * c ^ 2 + d ^ 3 - e,
      v == 399
  ]

{-
(6027,OpJt,[ValRegister 0,ValNumber 6035])
(6030,OpAdd,[ValRegister 0,ValRegister 1,ValNumber 1])
(6034,OpRet,[])
(6035,OpJt,[ValRegister 1,ValNumber 6048])
(6038,OpAdd,[ValRegister 0,ValRegister 0,ValNumber 32767])
(6042,OpSet,[ValRegister 1,ValRegister 7])
(6045,OpCall,[ValNumber 6027])
(6047,OpRet,[])
(6048,OpPush,[ValRegister 0])
(6050,OpAdd,[ValRegister 1,ValRegister 1,ValNumber 32767])
(6054,OpCall,[ValNumber 6027])
(6056,OpSet,[ValRegister 1,ValRegister 0])
(6059,OpPop,[ValRegister 0])
(6061,OpAdd,[ValRegister 0,ValRegister 0,ValNumber 32767])
(6065,OpCall,[ValNumber 6027])
(6067,OpRet,[])
-}
f :: (Nat15, Nat15, Nat15) -> Memo (Nat15, Nat15, Nat15) Nat15 Nat15
f (0, r1, _) = return $ r1 + 1
f (r0, 0, r7) = memo f (r0 - 1, r7, r7)
f (r0, r1, r7) = do
  a <- memo f (r0, r1 - 1, r7)
  memo f (r0 - 1, a, r7)

fMemo :: (Nat15, Nat15, Nat15) -> Nat15
fMemo = startEvalMemo . f

findR7 :: IO ()
findR7 = mapM_ print [(r7, v) | r7 <- [1 .. 32767], let v = fMemo (4, 1, traceShowId r7), v == 6]

{-
        DOOR
  * 8 -  1

  4 * 11 *

  + 4 -  18

ORB - 9 *
22
-}

data Cell = NumCell Int | OpCell Char

cellStr :: Cell -> String
cellStr (NumCell x) = show x
cellStr (OpCell c) = [c]

pathStr :: [Cell] -> String
pathStr (start : path) =
  let n = (length path `div` 2)
   in replicate n '(' ++ cellStr start ++ go path
  where
    go [] = ""
    go (a : b : xs) = cellStr a <> cellStr b <> ")" ++ go xs

evalPath :: [Cell] -> Int
evalPath path = unsafePerformIO $ do
  print $ pathStr path
  res <- runInterpreter $ setImports ["Prelude"] >> eval (pathStr path)
  case res of
    Left e -> error $ show e
    Right v -> putStrLn v >> return (U.read v)

shortestPath :: Maybe [Cell]
shortestPath = go (SQ.singleton ((0, 3), []))
  where
    rows =
      [ [OpCell '*', NumCell 8, OpCell '-', NumCell 1],
        [NumCell 4, OpCell '*', NumCell 11, OpCell '*'],
        [OpCell '+', NumCell 4, OpCell '-', NumCell 18],
        [NumCell 22, OpCell '-', NumCell 9, OpCell '*']
      ]
    grid = M.fromList $ zip [0 ..] [M.fromList $ zip [0 ..] row | row <- rows]
    ns (x, y) =
      catMaybes $
        [ if x > 0 then Just (x -1, y) else Nothing,
          if y > 0 then Just (x, y -1) else Nothing,
          if x < 3 then Just (x + 1, y) else Nothing,
          if y < 3 then Just (x, y + 1) else Nothing
        ]
    go SQ.Empty = Nothing
    go (((x, y), path) SQ.:<| rest)
      | (x, y) == (3, 0) && evalPath (reverse path') == 30 = Just (reverse path')
      | otherwise =
        let nextStates = [(c, path') | c <- ns (x, y), c /= (0, 3)]
         in go $ rest SQ.>< SQ.fromList nextStates
      where
        path' = grid M.! y M.! x : path

-- 22 + 4 - 11 * 4 - 18 - 11 - 1
printShortestPath :: IO ()
printShortestPath = print $ pathStr <$> shortestPath
