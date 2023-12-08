module Network.HTTP.Request

import Data.Buffer.Indexed
import Data.ByteString
import Data.IORef
import Data.String
import Network.HTTP.Connection
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.HTTP.Protocol
import Network.Socket


public export
record Request where
  constructor MkRequest
  connection : Maybe HTTPConnection
  method : Method
  resource : String
  headers : Headers

public export
Show Request where
  show req =
    "MkRequest { connection = " ++ show req.connection
               ++ ", method = " ++ show req.method
               ++ ", resource = " ++ show req.resource
               ++ ", headers = " ++ show req.headers ++ " }"


contentLength : Request -> Maybe Nat
contentLength request =
  case getHeader "Content-Length" request.headers of
    Just contentLengthString =>
      parsePositive contentLengthString
    Nothing =>
      Nothing


public export
readRequestBody : Request -> IO (Either HTTPConnectionError ByteString)
readRequestBody request =
  case (request.connection, contentLength request) of
    (Just connection, Just contentLength) => do
      Right body <- Network.HTTP.Connection.recvBytes connection contentLength
      | Left err => pure $ Left $ ConnectionSocketError err
      pure $ Right body
    (Just connection, Nothing) =>
      pure $ Left $ ConnectionProtocolError $
        ProtocolErrorMessage "No Content-Length header"
    (Nothing, _) =>
      pure $ Left $ ConnectionProtocolError $
        ProtocolErrorMessage "No connection"


export
readRequestHeaders : HTTPConnection -> IO (Either HTTPConnectionError Request)
readRequestHeaders connection = do
  -- Receive the request line
  Right line <- recvLine connection
  | Left err =>
    pure $ Left $ ConnectionSocketError err

  -- Parse the request line
  Just (method, resource, "HTTP/1.1") <- pure $ parseRequestLine $ toString line
  | _ =>
    pure $ Left $ ConnectionProtocolError $ ProtocolErrorMessage "Invalid request"

  -- Receive the headers
  Right headers <- recvHeaders connection empty
  | Left err =>
    pure $ Left err

  -- Parse the method
  Just method' <- pure $ stringToMethod method
  | Nothing =>
    pure $ Left $ ConnectionProtocolError $ ProtocolErrorMessage "Invalid method"

  -- Assemble the request
  pure $ Right $ MkRequest (Just connection) method' resource headers
