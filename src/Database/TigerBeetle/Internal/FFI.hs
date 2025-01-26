{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Internal.FFI where

import Control.Exception
import Data.WideWord.Word128
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

data TbClient
data UserData
type TbPacketData = ()

data TbStatus
  = TbStatusSuccess
  | TbStatusUnexpected
  | TbStatusOutOfMemory
  | TbStatusAddressInvalid
  | TbStatusAddressLimitExceeded
  | TbStatusSystemResources
  | TbStatusNetworkSubsystem
  deriving (Enum, Eq, Show)

data TbAccount
  = TbAccount
  { tbAccountId             :: Word128
  , tbAccountDebitsPending  :: Word128
  , tbAccountDebitsPosted   :: Word128
  , tbAccountCreditsPending :: Word128
  , tbAccountCreditsPosted  :: Word128
  , tbAccountUserData128    :: Word128
  , tbAccountUserData64     :: Word64
  , tbAccountUserData32     :: Word32
  , tbAccountReserved       :: Word32
  , tbAccountLedger         :: Word32
  , tbAccountCode           :: Word16
  , tbAccountFlags          :: Word16
  , tbAccountTimestamp      :: Word64
  }

instance Storable TbAccount where
  sizeOf = undefined
  alignment = undefined
  peek = undefined
  poke = undefined


data TbOperation
  = TbOperationPulse
  | TbOperationCreateAccounts
  | TbOperationCreateTransfers
  | TbOperationLookupAccounts
  | TbOperationLookupTransfers
  | TbOperationGetAccountTransfers
  | TbOperationGetAccountBalances
  | TbOperationQueryAccounts
  | TbOperationQueryTransfers
  deriving (Eq, Show)

instance Enum TbOperation where
   fromEnum TbOperationPulse = 128
   fromEnum TbOperationCreateAccounts = 129
   fromEnum TbOperationCreateTransfers = 130
   fromEnum TbOperationLookupAccounts = 131
   fromEnum TbOperationLookupTransfers = 132
   fromEnum TbOperationGetAccountTransfers = 133
   fromEnum TbOperationGetAccountBalances = 134
   fromEnum TbOperationQueryAccounts = 135
   fromEnum TbOperationQueryTransfers = 136

   toEnum 128 = TbOperationPulse
   toEnum 129 = TbOperationCreateAccounts
   toEnum 130 = TbOperationCreateTransfers
   toEnum 131 = TbOperationLookupAccounts
   toEnum 132 = TbOperationLookupTransfers
   toEnum 133 = TbOperationGetAccountTransfers
   toEnum 134 = TbOperationGetAccountBalances
   toEnum 135 = TbOperationQueryAccounts
   toEnum 136 = TbOperationQueryTransfers
   toEnum unmatched
     = error $ "TbOperation.toEnum cannot match: " ++ show unmatched

data TbPacketStatus
  = TbPacketOk
  | TbPacketTooMuchData
  | TbPacketClientShutdown
  | TbPacketInvalidOperation
  | TbPacketInvalidDataSize
  deriving (Bounded, Enum, Eq, Show)

data TbPacket
  = TbPacket
  { tbPacketNext      :: Ptr TbPacket
  , tbPacketUserData  :: Ptr UserData
  , tbPacketOperation :: CChar
  , tbPacketStatus    :: CChar
  , tbPacketDataSize  :: CUInt
  , tbPacketData      :: Ptr TbPacketData
  , tbPacketBatchNext :: Ptr TbPacket
  , tbPacketBatchTail :: Ptr TbPacket
  , tbPacketBatchSize :: CUInt
  , tbPacketReserved  :: Ptr CChar
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

foreign import capi "tb_client.h tb_client_submit"
  hs_tb_client_submit
  :: Ptr TbClient
  -> Ptr TbPacket
  -> IO ()

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

withTbClient :: (Ptr TbClient -> IO a) -> IO a
withTbClient = bracket unsafeMkTbClient free
