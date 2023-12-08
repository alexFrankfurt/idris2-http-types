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


data RequestBody : Type where
  RequestBodyPending : HTTPConnection -> RequestBody
  RequestBodyContent : ByteString -> RequestBody


public export
record Request where
  constructor MkRequest
  body : IORef RequestBody
  method : Method
  resource : String
  headers : Headers

public export
Show Request where
  show req = "MkRequest { body = <RequestBody>"
                        ++ ", method = " ++ show req.method
                        ++ ", resource = " ++ show req.resource
                        ++ ", headers = " ++ show req.headers ++ " }"


public export
contentLength : Request -> Maybe Nat
contentLength request =
  case getHeader "Content-Length" request.headers of
    Just contentLengthString =>
      parsePositive contentLengthString
    Nothing =>
      Nothing


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
  body <- newIORef $ RequestBodyPending connection
  pure $ Right $ MkRequest body method' resource headers


public export
readRequestBody : Request -> IO (Either HTTPConnectionError ByteString)
readRequestBody request = do
  body <- readIORef request.body
  case (body, contentLength request) of
    (RequestBodyContent content, _) =>
      pure $ Right content
    (RequestBodyPending connection, Just contentLength) => do
      Right body <- Network.HTTP.Connection.recvBytes connection contentLength
      | Left err => pure $ Left $ ConnectionSocketError err
      writeIORef request.body $ RequestBodyContent body
      pure $ Right body
    (_, Nothing) => pure $ Left $ ConnectionProtocolError $
      ProtocolErrorMessage "No Content-Length header"
