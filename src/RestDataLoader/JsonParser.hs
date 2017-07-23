{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module RestDataLoader.JsonParser
    (
    ) where -- instances are exported/imported automatically. 

import RestDataLoader.Model
import Data.Aeson
import qualified Data.Text as T



instance FromJSON Manifest where
  parseJSON = withObject "manifest" $ \m -> do
    o     <- m .: "api"
    host  <- o .: "host"
    port  <- o .: "port"
    return  $ Manifest Api{..}

-- FIXME: to json is wrong :(
instance ToJSON Manifest where
   toJSON (Manifest api) = object [
      "api" .= object [
        "name" .= host api,
        "port" .= port api
      ]
    ]

instance FromJSON RequestType where
  parseJSON (String s) | s == "REST" = return REST
                       | s == "MultipartUpload" = return MultipartUpload
                       | otherwise = fail $ "Expected REST or MultipartUpload got '" ++ (T.unpack s) ++ "'"


instance FromJSON RequestMethod where
    parseJSON = withText "request method" $ \s -> case s of
                         "GET" -> return GET -- TODO: case insensitive?
                         "POST" -> return GET
                         "PUT" -> return GET
                         "OPTION" -> return GET
                         "HEAD" -> return GET
                         "DELETE" -> return DELETE
                         otherwise -> fail $ "request method could be one of GET|POST|PUT|OPTION|HEAD|DELETE, got: '" ++ (T.unpack s) ++ "''"


instance FromJSON ResponseAction where
    parseJSON = withText "response action" $ \s -> case s of
                          "SAVE" -> return SAVE
                          "SKIP" -> return SKIP
                          otherwise -> fail $ "response action could be on of SAVE|SKIP, got: '" ++ (T.unpack s) ++ "'"

instance FromJSON Definition where
  parseJSON (Object o) =
    Definition <$>
      (o .:? "type" .!= REST) <*>
      (o .:? "depends" .!= []) <*>
      (o .:? "headers" .!= []) <*>
      (o .:? "multipart-field") <*>
      (o .:  "path") <*>
      (o .: "method") <*>
      (o .: "response-action") <*>
      (o .:? "data-dir") <*>
      (o .:? "data")

-- TODO: toJson
