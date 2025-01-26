{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Account where

import Control.Monad
import Database.TigerBeetle.Client.Account
import Database.TigerBeetle.Internal.FFI
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

createAccounts :: [Account] -> IO TbPacket
createAccounts accounts = do
  accountData <- pack accounts
  pure
    $ TbPacket
    { tbPacketNext      = nullPtr
    , tbPacketUserData  = nullPtr -- TODO: This should be a generated
                                  -- id from client state
    , tbPacketOperation = fromIntegral $ fromEnum TbOperationCreateAccounts
    , tbPacketStatus    = fromIntegral $ fromEnum TbPacketOk
    , tbPacketDataSize  = fromIntegral $ sizeOf accountData
    , tbPacketData      = castPtr @TbAccount @() accountData
    , tbPacketBatchNext = nullPtr
    , tbPacketBatchTail = nullPtr
    , tbPacketBatchSize = 0
    , tbPacketReserved  = nullPtr
  }
  where
    pack :: [Account] -> IO (Ptr TbAccount)
    pack accts = do
      tbaccounts <- malloc @TbAccount
      forM_ (zip [0..] accts) $ \(offset, acct) -> do
        tbAccount <- packAccount acct
        pokeElemOff tbaccounts offset tbAccount
      pure tbaccounts

    packAccount :: Account -> IO TbAccount
    packAccount = undefined
