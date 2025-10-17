module Network.HTTP.Connection

import Data.Buffer.Indexed
import Data.ByteString
import Data.IORef
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.HTTP.Protocol
import Network.Socket


public export
record ConnectionBuffer where
  constructor MkConnectionBuffer
  buffer : ByteString
  socket : Socket


public export
Connection : Type
Connection = IORef ConnectionBuffer

export
Show Connection where
  show _ = "<Connection>"


public export
data ConnectionError : Type where
  ConnectionSocketError : SocketError -> ConnectionError
  ConnectionProtocolError : ProtocolError -> ConnectionError


public export
Show ConnectionError where
  show (ConnectionSocketError err) = "ConnectionSocketError: " ++ show err
  show (ConnectionProtocolError err) = "ConnectionProtocolError: " ++ show err


crlfBreak : ByteString -> Maybe (ByteString, ByteString)
crlfBreak bs = match 0 False $ uncons bs where
  match : Nat -> Bool -> Maybe (Bits8, ByteString) -> Maybe (ByteString, ByteString)
  match _ _ Nothing = Nothing
  match n False (Just (13, bs')) = match (n + 1) True $ uncons bs'
  match (S n) True (Just (10, bs')) = Just (take n bs, bs')
  match n _ (Just (_, bs')) = match (n + 1) False $ uncons bs'


setBuffer : Connection -> ByteString -> IO ()
setBuffer connection content = do
  conn <- readIORef connection
  writeIORef connection $ MkConnectionBuffer content conn.socket


export
newConnection : Socket -> IO Connection
newConnection socket = newIORef $ MkConnectionBuffer empty socket


export
recvBytes : Connection -> Nat -> IO (Either SocketError ByteString)
recvBytes connection n = do
  MkConnectionBuffer buf sock <- readIORef connection
  -- Try to read from the buffer first
  Nothing <- pure $ splitAt n buf
  | Just (bs, buf') => setBuffer connection buf' >> pure (Right bs)
  -- Otherwise, read from the socket
  let requested : ByteLength = cast n
  Right bytes <- Network.Socket.recvBytes sock requested
  | Left err => pure $ Left err
  let chunk = pack bytes
  -- Update the buffer and recurse
  writeIORef connection $ MkConnectionBuffer (buf `append` chunk) sock
  recvBytes connection n


export
recvLine : Connection -> IO (Either SocketError ByteString)
recvLine connection = do
  MkConnectionBuffer buf sock <- readIORef connection
  -- Try to read from the buffer first
  Nothing <- pure $ crlfBreak buf
  | Just (line, buf') => setBuffer connection buf' >> (pure $ Right line)
  -- Otherwise, read from the socket
  let chunkSize : ByteLength = cast 4096
  Right bytes <- Network.Socket.recvBytes sock chunkSize
  | Left err => pure $ Left err
  let chunk = pack bytes
  -- Update the buffer and recurse
  writeIORef connection $ MkConnectionBuffer (buf `append` chunk) sock
  recvLine connection


export
recvHeaders : Connection -> Headers -> IO (Either ConnectionError Headers)
recvHeaders connection headers = do
  Right line <- recvLine connection
  | Left err => pure $ Left $ ConnectionSocketError err
  Right (Just header) <- pure $ parseHeader $ toString line
  | Right Nothing => pure $ Right headers
  | Left err => pure $ Left $ ConnectionProtocolError err
  recvHeaders connection (addHeader header headers)
