{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Response
    ( Response(Error, Ok, description, parameters)
    , ResponseParameters(ResponseParameters, migrateToChatId, retryAfter)
    ) where

import Data.Aeson
    ( (.=)
    , Options(fieldLabelModifier, omitNothingFields)
    , ToJSON(toJSON)
    , camelTo2
    , defaultOptions
    , genericToJSON
    , object
    )
import Data.Text (Text)
import GHC.Generics (Generic)

data Response a
  = Ok a
  | Error
      { description :: Text,
        parameters :: Maybe ResponseParameters
      }

instance ToJSON a => ToJSON (Response a) where
    toJSON (Ok value) = object ["ok" .= True, "result" .= value]
    toJSON Error { description, parameters } = object
        (["ok" .= False, "description" .= description] ++ parameters')
        where parameters' = maybe [] (\x -> ["parameters" .= x]) parameters

data ResponseParameters = ResponseParameters
    { migrateToChatId :: Maybe Integer
    , retryAfter :: Maybe Integer
    }
    deriving Generic

instance ToJSON ResponseParameters where
    toJSON = genericToJSON options
      where
        options = defaultOptions
            { fieldLabelModifier = camelTo2 '_'
            , omitNothingFields = True
            }
