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


export
parseResponseLine : String -> Maybe (String, Int, String)
parseResponseLine line =
  case words line of
    [version, statusCode, statusText] => return version statusCode statusText
    _ => Nothing
  where
    return : String -> String -> String -> Maybe (String, Int, String)
    return version codeString status =
      case parseInteger codeString of
        Just code => Just (version, code, status)
        Nothing => Nothing
