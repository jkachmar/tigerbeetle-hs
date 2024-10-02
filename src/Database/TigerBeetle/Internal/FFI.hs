{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CApiFFI #-}

module Database.TigerBeetle.Internal.FFI where

import Data.WideWord.Word128
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

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
  -> Ptr ()

foreign import capi "tb_client_shim.h hs_tb_client_init"
  hs_tb_client_shim
  :: Ptr TbClient
  -> Ptr Word128
  -> CString
  -> CUInt
  -> CUIntPtr
  -> FunPtr TbOnCompletion
  -> IO CInt
