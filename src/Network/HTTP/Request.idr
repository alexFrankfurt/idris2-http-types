module Network.HTTP.Request

import Data.Buffer.Indexed
import Data.ByteString
import Data.IORef
import Data.String
import Network.HTTP.Connection
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.HTTP.Protocol
import Network.HTTP.URL
import Network.Socket


public export
record Request b where
  constructor MkRequest
  body : b
  method : Method
  resource : String
  headers : Headers

public export
Show (Request a) where
  show req = "MkRequest { body = <Body>"
                        ++ ", method = " ++ show req.method
                        ++ ", resource = " ++ show req.resource
                        ++ ", headers = " ++ show req.headers ++ " }"


public export
contentLength : Request b -> Maybe Nat
contentLength request =
  case getHeader "Content-Length" request.headers of
    Just contentLengthString =>
      parsePositive contentLengthString
    Nothing =>
      Nothing


export
readRequestHeaders : Connection -> IO (Either ConnectionError (Request Connection))
readRequestHeaders connection = do
  -- Receive the request line
  Right line <- recvLine connection
  | Left err => pure $ Left $ ConnectionSocketError err

  -- Parse the request line
  Just (method, resource, "HTTP/1.1") <- pure $ parseRequestLine $ toString line
  | _ => pure $ Left $ ConnectionProtocolError $ ProtocolErrorMessage "Invalid request"

  -- Receive the headers
  Right headers <- recvHeaders connection empty
  | Left err => pure $ Left err

  -- Parse the method
  Just method' <- pure $ stringToMethod method
  | _ => pure $ Left $ ConnectionProtocolError $ ProtocolErrorMessage "Invalid method"

  -- Assemble the request
  pure $ Right $ MkRequest connection method' resource headers


public export
readRequestBody : Request Connection -> IO (Either ConnectionError ByteString)
readRequestBody request =
  case contentLength request of
    Just contentLength => do
      Right body <- Network.HTTP.Connection.recvBytes request.body contentLength
      | Left err => pure $ Left $ ConnectionSocketError err
      pure $ Right body
    Nothing => pure $ Left $ ConnectionProtocolError $
      ProtocolErrorMessage "No Content-Length header"


public export
http1RequestLine : Request b -> String
http1RequestLine request
  = methodToString request.method ++ " "
  ++ request.resource
  ++ " HTTP/1.1"


public export
http1Request : Request ByteString -> String
http1Request request
  = http1RequestLine request ++ "\r\n"
  ++ http1Headers request.headers ++ "\r\n"
  ++ toString request.body


public export
addHeader : String -> String -> Request b -> Request b
addHeader k v request =
  { headers := addHeader (MkHeader k [v]) request.headers } request


public export
withContentLength : Request ByteString -> Request ByteString
withContentLength request =
  if hasHeader "Content-Length" request.headers
    then request
    else addHeader "Content-Length" (show $ length request.body) request


public export
mkRequest : Method -> URL -> Headers -> ByteString -> Request ByteString
mkRequest method url headers body =
  withContentLength $ MkRequest body method (resource url) headers
