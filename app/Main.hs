module Main where

import Data.ByteString qualified as B
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Modular
import Control.Lens

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

toOpCode :: Nat15 -> OpCode
toOpCode 0 = OpHalt
toOpCode 19 = OpOut
toOpCode 21 = OpNoop
toOpCode i = error $ "Op not implemented: " <> (show i)

numArgs :: OpCode -> Int
numArgs OpHalt = 0
numArgs OpOut = 1
numArgs OpNoop = 0

mkMemory :: ByteString -> Memory
mkMemory bs = M.fromList [(i, parseByte b) | (i, b) <- zip [Address 0 ..] $ B.unpack bs]

parseByte :: Word8 -> Value
parseByte b
  | bi <= 32767 = ValNumber (toMod bi)
  | bi <= 32775 = ValRegister (toMod $ bi - 32768)
  | otherwise = error $ "Invalid byte encountered: " <> (show b)
  where
    bi = toInteger b

data Machine = Machine
  { _memory :: Memory,
    _registers :: RegisterMap,
    _stack :: Stack,
    _pc :: Address,
    _stdOut :: Maybe Text,
    _stdIn :: Maybe Text,
    _halted :: Bool
  }

makeLenses ''Machine

mkMachine :: Memory -> Machine
mkMachine memory = Machine {
  _memory=memory,
  _registers=mkRegisterMap,
  _stack=[],
  _pc=(Address 0),
  _stdOut=Nothing,
  _stdIn=Nothing,
  _halted=False
  }

-- get op, get args, get next pc (+1 or jmp), 
step :: Machine -> Machine
step m = over pc (+1) $ m
  where
    opCode = (m ^. memory) M.! (m ^. pc)
    pc' = m ^. pc + (Address 1)

runMachine :: Machine -> IO ()
runMachine m' = do
  let m = step m'
  print $ m ^. pc
  case m ^. stdOut of
    Nothing -> return ()
    Just s -> putText s
  if m ^. halted then return () else runMachine m

encodeWord16 :: Word16 -> [Word8]
encodeWord16 = BL.unpack . toLazyByteString . word16LE

main :: IO ()
main = do
  binBS <- readFileBS "data/challenge.bin"
  let testBS :: [Word16]
      testBS = [9,32768,32769,4,19,32768]
      testBSPacked = B.pack $ encodeWord16 =<< testBS
      --memory = mkMemory binBS
      memory = mkMemory testBSPacked
      machine = mkMachine memory
  runMachine machine
