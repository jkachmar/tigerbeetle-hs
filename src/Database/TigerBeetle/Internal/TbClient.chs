{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include "tb_client.h"

-- Enumerations

{#enum TB_ACCOUNT_FLAGS {underscoreToCase} deriving (Eq)#}

{#enum TB_TRANSFER_FLAGS {underscoreToCase} deriving (Eq)#}

{#enum TB_CREATE_ACCOUNT_RESULT {underscoreToCase} deriving (Eq)#}

{#enum TB_CREATE_TRANSFER_RESULT {underscoreToCase} deriving (Eq)#}

{#enum TB_ACCOUNT_FILTER_FLAGS {underscoreToCase} deriving (Eq)#}

{#enum TB_OPERATION {underscoreToCase} deriving (Eq)#}

{#enum TB_PACKET_STATUS {underscoreToCase} deriving (Eq)#}

{#enum TB_STATUS {underscoreToCase} deriving (Eq)#}

-- Pointer Types

{#pointer tb_client_t as TbClient newtype#}
