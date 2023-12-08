module Network.HTTP.Headers

import Data.String
import Network.HTTP.Protocol


public export
record Header where
  constructor MkHeader
  name : String
  values : List String


public export
Show Header where
  show (MkHeader name values) =
    "MkHeader " ++ show name ++ " [" ++ (joinBy ", " (map show values)) ++ "]"


public export
Headers : Type
Headers = List Header


export
parseHeader : String -> Either ProtocolError (Maybe Header)
parseHeader l = match 0 $ strM l where
  mkHeader : Int -> StrM s -> Either ProtocolError (Maybe Header)
  mkHeader i (StrCons ' ' s) = Right $ Just $ MkHeader (strSubstr 0 i l) [s]
  mkHeader _ _ = Left $ ProtocolErrorMessage "Invalid header"
  match : Int -> StrM s -> Either ProtocolError (Maybe Header)
  match 0 StrNil = Right Nothing
  match i (StrCons ':' s) = mkHeader i $ strM s
  match i (StrCons c s) = match (i + 1) $ strM s
  match _ StrNil = Left $ ProtocolErrorMessage "Invalid header"


export
addHeader : Header -> Headers -> Headers
addHeader header [] = [header]
addHeader header (MkHeader name values :: headers) =
  if name == header.name
  then MkHeader name (header.values ++ values) :: headers
  else MkHeader name values :: addHeader header headers


export
getHeader : String -> Headers -> Maybe String
getHeader name (MkHeader name' (value :: _) :: headers) =
  if toLower name == toLower name'
  then Just value
  else getHeader name headers
getHeader _ _ = Nothing


export
hasHeader : String -> Headers -> Bool
hasHeader name [] = False
hasHeader name (MkHeader name' _ :: headers) =
  if name == name'
  then True
  else hasHeader name headers


export
empty : Headers
empty = []
