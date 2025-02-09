module Database.TigerBeetle.Client.Account where

import Data.Word
import Data.WideWord.Word128

-- TODO: This should be the user API type and use a nicer,
-- higher-level interface.
data Account
  = Account
  { id_            :: Word128
  , debitsPending  :: Word128
  , debitsPosted   :: Word128
  , creditsPending :: Word128
  , creditsPosted  :: Word128
  , userData128    :: Word128
  , userData64     :: Word64
  , userData32     :: Word32
  , reserved       :: Word32
  , ledger         :: Word32
  , code           :: Word16
  , flags          :: Word16 -- TODO: Convert this to set of flags
  , timestamp      :: Word64
  }
  deriving (Eq, Show)
