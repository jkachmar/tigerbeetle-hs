{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Internal.FFI where

import Control.Exception
import Data.WideWord.Word128
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

data TbClient
data UserData
data TbPacketData

data TbStatus
  = TbStatusSuccess
  | TbStatusUnexpected
  | TbStatusOutOfMemory
  | TbStatusAddressInvalid
  | TbStatusAddressLimitExceeded
  | TbStatusSystemResources
  | TbStatusNetworkSubsystem
  deriving (Enum, Eq, Show)

data TbPacket
  = TbPacket
  { tbPacketNext :: Ptr TbPacket
  , tbPacketUserData :: Ptr UserData
  , tbPacketOperation :: CChar
  , tbPacketStatus :: CChar
  , tbPacketDataSize :: CUInt
  , tbPacketData :: Ptr TbPacketData
  , tbPacketBatchNext :: Ptr TbPacket
  , tbPacketBatchTail :: Ptr TbPacket
  , tbPacketBatchSize :: CUInt
  , tbPacketReserved :: Ptr CChar
  }

type TbOnCompletion
  = CUIntPtr
  -> TbClient
  -> Ptr TbPacket
  -> Ptr CChar
  -> CUInt
  -> IO ()

foreign import capi "tb_client_shim.h hs_tb_client_init"
  hs_tb_client_init
  :: Ptr TbClient
  -> Ptr Word128
  -> CString
  -> CUInt
  -> CUIntPtr
  -> FunPtr TbOnCompletion
  -> IO CInt

foreign import capi "tb_client_shim.h hs_tb_client_init_echo"
  hs_tb_client_init_echo
  :: Ptr TbClient
  -> Ptr Word128
  -> CString
  -> CUInt
  -> CUIntPtr
  -> FunPtr TbOnCompletion
  -> IO CInt

-- | Creates a Ptr Word128.
--
-- Caller is responsible for calling 'free' on the pointer.
unsafeMkWord128Ptr :: Word128 -> IO (Ptr Word128)
unsafeMkWord128Ptr w = do
  p <- malloc @Word128
  poke p w
  pure p

withWord128Ptr :: Word128 -> (Ptr Word128 -> IO a) -> IO a
withWord128Ptr w = bracket (unsafeMkWord128Ptr w) free

unsafeMkTbClient :: IO (Ptr TbClient)
unsafeMkTbClient = malloc @() >>= pure . castPtr

withTbClient = bracket unsafeMkTbClient free
