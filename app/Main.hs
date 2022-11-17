module Main where

import Control.Lens
import Data.Binary.Get
import Data.ByteString qualified as B
import Data.ByteString.Builder (doubleBE, toLazyByteString, word16LE)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Modular (Mod, toMod, unMod)
import System.IO (putChar)

newtype Register = Register (Maybe Value)

type RegisterId = Integer `Mod` 8

type RegisterMap = Map RegisterId Register

mkRegisterMap :: RegisterMap
mkRegisterMap = M.fromList $ zip [0 .. 7] (repeat $ Register Nothing)

newtype Address = Address Nat15 deriving (Enum, Num, Ord, Eq, Show)

type Memory = Map Address Value

type Nat15 = Integer `Mod` 32768

data Value
  = ValNumber Nat15
  | ValAddress Address
  | ValRegister RegisterId
  deriving (Show)

toNat15 :: RegisterMap -> Value -> Nat15
toNat15 _ (ValNumber n) = n
toNat15 _ (ValAddress _) = error "Address arithmetic performed"
toNat15 rm (ValRegister i) =
  case rm M.! i of
    (Register (Just v)) -> toNat15 rm v
    (Register (Nothing)) -> error ("Dereferencing empty register: " <> (show i))

type Stack = [Value]

data OpCode
  = OpHalt
  | OpOut
  | OpNoop
  deriving (Eq, Ord, Show)

toOpCode :: Nat15 -> OpCode
toOpCode 0 = OpHalt
toOpCode 19 = OpOut
toOpCode 21 = OpNoop
toOpCode i = error $ "Op not implemented: " <> (show i)

numArgs :: OpCode -> Integer
numArgs OpHalt = 0
numArgs OpOut = 1
numArgs OpNoop = 0

mkMemory :: [Word16] -> Memory
mkMemory bs = M.fromList [(i, parseWord16 b) | (i, b) <- zip [Address 0 ..] bs]

parseWord16 :: Word16 -> Value
parseWord16 b
  | bi <= 32767 = ValNumber (toMod bi)
  | bi <= 32775 = ValRegister (toMod $ bi - 32768)
  | otherwise = error $ "Invalid byte encountered: " <> show b
  where
    bi = toInteger b

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
nat15At m a = toNat15 (m ^. registers) $ (m ^. memory) M.! a

step :: Machine -> Machine
step m = runOp m opCode args
  where
    opCode = toOpCode . nat15At m $ m ^. pc
    args = ((m ^. memory) M.!) <$> [m ^. pc + Address 1 .. m ^. pc + Address (toMod $ numArgs opCode)]

runOp :: Machine -> OpCode -> [Value] -> Machine
runOp m opCode args =
  --traceShow (m ^. pc, m ^. memory, opCode, args) $
  m' & pc %~ (+ (1 + fromIntegral (length args)))
  where
    m' =
      case opCode of
        OpHalt -> m & halted .~ True
        OpOut -> let (v : _) = args in m & stdOut ?~ (chr . fromIntegral . unMod . toNat15 (m ^. registers) $ v)
        OpNoop -> m

runMachine :: Machine -> IO ()
runMachine m' = do
  let m = step m'
  case m ^. stdOut of
    Nothing -> return ()
    Just s -> putChar s
  if m ^. halted then return () else runMachine m

encodeWord16 :: Word16 -> [Word8]
encodeWord16 = BL.unpack . toLazyByteString . word16LE

readFile16 :: String -> IO [Word16]
readFile16 path = do
  input <- BL.readFile path
  let getter = do
        empty <- isEmpty
        if empty
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
  let testBS :: [Word16]
      --testBS = [9, 32768, 32769, 4, 19, 32768]
      testBS = [19, 65, 21, 0]
      --machine = mkMachine (mkMemory testBS)
      machine = mkMachine (mkMemory binBS)
  print testBS
  runMachine machine
