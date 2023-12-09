module Network.HTTP.Application

import Network.HTTP.Connection
import Network.HTTP.Request
import Network.HTTP.Response
import Network.Socket


public export
data Responded : Type where
  SendResponseError : Response -> SocketError -> Responded
  SentResponse : Response -> Responded


public export
Application : Type
Application = Request Connection -> (Response -> IO Responded) -> IO Responded


export
mkRespond : Socket -> Response -> IO Responded
mkRespond sock response0 = do
  let response1 = withContentLength response0
  Right _ <- send sock $ http1Response response1
  | Left err => pure $ SendResponseError response1 err
  pure $ SentResponse response1
