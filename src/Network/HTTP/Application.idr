module Network.HTTP.Application

import Network.HTTP.Request
import Network.HTTP.Response
import Network.Socket


public export
data Responded : Type where
  SendResponseError : Response -> SocketError -> Responded
  SentResponse : Response -> Responded


public export
Application : Type
Application = Request -> (Response -> IO Responded) -> IO Responded


export
mkRespond : Socket -> Request -> Response -> IO Responded
mkRespond sock request response0 = do
  let response1 = withContentLength response0
  Right _ <- send sock $ responseString request response1
  | Left err => pure $ SendResponseError response1 err
  pure $ SentResponse response1
