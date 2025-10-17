module Network.HTTP.Application

import Data.Buffer.Indexed
import Data.ByteString
import Network.HTTP.Connection
import Network.HTTP.Request
import Network.HTTP.Response
import Network.Socket


public export
data Responded : Type where
  SendResponseError : Response ByteString -> SocketError -> Responded
  SentResponse : Response ByteString -> Responded


public export
Respond : Type
Respond = Response ByteString -> IO Responded


public export
Application : Type
Application = Request Connection -> Respond -> IO Responded


export
mkRespond : Socket -> Respond
mkRespond sock response0 = do
  let response1 = withContentLength response0
  putStrLn "[app] preparing to send response"
  Right _ <- send sock $ http1Response response1
  | Left err => do
      putStrLn $ "[app] send failed: " ++ show err
      pure $ SendResponseError response1 err
  putStrLn "[app] response send succeeded"
  pure $ SentResponse response1
