module Network.HTTP.Protocol

import Data.String


public export
data ProtocolError : Type where
  ProtocolErrorMessage : String -> ProtocolError


public export
Show ProtocolError where
  show (ProtocolErrorMessage msg) = msg


export
parseRequestLine : String -> Maybe (String, String, String)
parseRequestLine line =
  case words line of
    [method, path, version] => Just (method, path, version)
    _ => Nothing
