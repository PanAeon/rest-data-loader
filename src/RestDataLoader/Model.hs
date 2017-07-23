module RestDataLoader.Model(
    Manifest(Manifest, api),
    Api(Api, host, port),
    Definition(..),
    RequestType(..),
    RequestMethod(..),
    ResponseType,
    ResponseAction(..),
    RequestHeader(..)
    ) where

import qualified Data.Aeson as Aeson

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Api = Api {
    host:: String
  , port:: Int
  -- TODO: auth
} deriving Show

data Manifest = Manifest {
  api::Api
} deriving Show

data RequestMethod = PUT | POST | GET | OPTION | HEAD | DELETE deriving Show

type RequestHeader = (String, String)

data RequestType = REST | MultipartUpload deriving Show

type ResponseType = RequestType -- FIXME what to do in this situation?

data ResponseAction = SAVE | SKIP deriving Show -- TODO: write normal response actions

data Definition = Definition {
      _type  ::RequestType
    , depends :: [String]
    , headers :: [RequestHeader]
    , multipartField :: Maybe String
    , path   ::String      -- TODO: url instead of string?
    , method ::RequestMethod
    , responseAction :: ResponseAction
    , dataDir :: Maybe FilePath
    , _data :: Maybe Aeson.Value -- FIXME: validate that either dataDir or _data is present
} deriving Show
{-

{
  "type": "multipart/form-data",
  "path": "/api/v1/images",
  "method": "PUT",
  "response-type": "json",
  "response-action": "save",
  "data-dir": "images"
}


{

  "depends": ["users"],
  "type": "json",
  "path": "/api/v1/departments",
  "method": "PUT",
  "headers": {},
  "response-action": {
    "type": "copy-fields",
    "fields": "id"
  },
  "data": {
    "1": {
      "name": "violin",
      "users": ["{{users['vasia'].id}}"]
    },
    "2": {
      "name": "guitar",
      "avatar": "{{users[zoya].id}}"
    }
  }
}


-}
