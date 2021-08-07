module Repld.Aeson where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Internal as Aeson (iparse)
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import Repld.Prelude

decode :: (Aeson.Value -> Aeson.Parser a) -> ByteString -> Either String a
decode parser bytes =
  Aeson.eitherDecodeStrictWith Aeson.json' (Aeson.iparse parser) bytes
    & mapLeft snd
