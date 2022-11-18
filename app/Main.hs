module Main where

import Control.Lens (makeLenses, (%~), (.~), (?~), (^.))
import Data.Binary.Get (getWord16le, isEmpty, runGet)
import Data.ByteString.Builder (toLazyByteString, word16LE)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Modular (Mod, toMod, unMod)
import Relude.Unsafe qualified as U
import System.IO (putChar)

newtype Register = Register Value

type RegisterId = Integer `Mod` 8

type RegisterMap = Map RegisterId Register

mkRegisterMap :: RegisterMap
mkRegisterMap = M.fromList $ zip [0 .. 7] (repeat $ Register (ValNumber 0))

newtype Address = Address Nat15 deriving (Enum, Num, Ord, Eq, Show)

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
toNat15 _ (ValAddress _) = error "Address arithmetic performed"
toNat15 m (ValRegister i) = let (Register v) = (m ^. registers) M.! i in toNat15 m v

data OpCode
  = OpHalt
  | OpSet
  | OpJmp
  | OpJt
  | OpJf
  | OpOut
  | OpNoop
  deriving (Eq, Ord, Show)

toOpCode :: Nat15 -> OpCode
toOpCode 0 = OpHalt
toOpCode 1 = OpSet
toOpCode 6 = OpJmp
toOpCode 7 = OpJt
toOpCode 8 = OpJf
toOpCode 19 = OpOut
toOpCode 21 = OpNoop
toOpCode i = error $ "Op not implemented: " <> show i

numArgs :: OpCode -> Integer
numArgs OpHalt = 0
numArgs OpSet = 2
numArgs OpJmp = 1
numArgs OpJt = 2
numArgs OpJf = 2
numArgs OpOut = 1
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
      _pc = (Address 0),
      _stdOut = Nothing,
      _stdIn = Nothing,
      _halted = False
    }

nat15At :: Machine -> Address -> Nat15
nat15At m a = toNat15 m $ (m ^. memory) M.! a

step :: Machine -> Machine
step m = runOp m opCode args
  where
    opCode = toOpCode . nat15At m $ m ^. pc
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

runOp :: Machine -> OpCode -> [Value] -> Machine
runOp m opCode args =
  traceShow (m ^. pc, opCode, args) $
    if jumped then m' else m' & pc %~ (+ (1 + fromIntegral (length args)))
  where
    (a, b, _) = takeNat15Args m args
    ra = takeRegisterArg args
    (m', jumped) =
      case opCode of
        OpHalt -> (m & halted .~ True, False)
        OpSet -> (m & registers %~ M.adjust (const . Register . ValNumber $ b) ra, False)
        OpJmp -> (m & pc .~ Address a, True)
        OpJt -> if a /= 0 then (m & pc .~ Address b, True) else (m, False)
        OpJf -> if a == 0 then (m & pc .~ Address b, True) else (m, False)
        OpOut -> (m & stdOut ?~ (chr . fromIntegral . unMod $ a), False)
        OpNoop -> (m, False)

runMachine :: Machine -> IO ()
runMachine m' = do
  let m = step m'
  forM_ (m ^. stdOut) putChar
  if m ^. halted then return () else runMachine m

encodeWord16 :: Word16 -> [Word8]
encodeWord16 = BL.unpack . toLazyByteString . word16LE

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
main = do
  -- binBS <- readFileBS "data/challenge.bin"
  binBS <- readFile16 "data/challenge.bin"
  --testBS = [9, 32768, 32769, 4, 19, 32768]
  --testBS = [19, 65, 21, 0]
  --machine = mkMachine (mkMemory testBS)
  let machine = mkMachine (mkMemory binBS)
  runMachine machine
