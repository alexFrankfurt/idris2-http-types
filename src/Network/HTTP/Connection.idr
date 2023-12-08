module Network.HTTP.Connection

import Data.Buffer.Indexed
import Data.ByteString
import Data.IORef
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.HTTP.Protocol
import Network.Socket


export
record ConnectionBuffer where
  constructor MkConnectionBuffer
  buffer : ByteString
  socket : Socket


public export
HTTPConnection : Type
HTTPConnection = IORef ConnectionBuffer

export
Show HTTPConnection where
  show _ = "<HTTPConnection>"


public export
data HTTPConnectionError : Type where
  ConnectionSocketError : SocketError -> HTTPConnectionError
  ConnectionProtocolError : ProtocolError -> HTTPConnectionError


public export
Show HTTPConnectionError where
  show (ConnectionSocketError err) = "ConnectionSocketError: " ++ show err
  show (ConnectionProtocolError err) = "ConnectionProtocolError: " ++ show err


crlfBreak : ByteString -> Maybe (ByteString, ByteString)
crlfBreak bs = match 0 False $ uncons bs where
  match : Nat -> Bool -> Maybe (Bits8, ByteString) -> Maybe (ByteString, ByteString)
  match _ _ Nothing = Nothing
  match n False (Just (13, bs')) = match (n + 1) True $ uncons bs'
  match (S n) True (Just (10, bs')) = Just (take n bs, bs')
  match n _ (Just (_, bs')) = match (n + 1) False $ uncons bs'


setBuffer : HTTPConnection -> ByteString -> IO ()
setBuffer connection content = do
  conn <- readIORef connection
  writeIORef connection $ MkConnectionBuffer content conn.socket


export
newConnection : Socket -> IO HTTPConnection
newConnection socket = newIORef $ MkConnectionBuffer empty socket


export
recvBytes : HTTPConnection -> Nat -> IO (Either SocketError ByteString)
recvBytes connection n = do
  MkConnectionBuffer buf sock <- readIORef connection
  -- Try to read from the buffer first
  Nothing <- pure $ splitAt n buf
  | Just (bs, buf') => setBuffer connection empty >> (pure $ Right bs)
  -- Otherwise, read from the socket
  Right bs <- recvByteString n sock
  | Left err => pure $ Left err
  -- Update the buffer and recurse
  writeIORef connection $ MkConnectionBuffer (buf `append` bs) sock
  recvBytes connection n


export
recvLine : HTTPConnection -> IO (Either SocketError ByteString)
recvLine connection = do
  MkConnectionBuffer buf sock <- readIORef connection
  -- Try to read from the buffer first
  Nothing <- pure $ crlfBreak buf
  | Just (line, buf') => setBuffer connection buf' >> (pure $ Right line)
  -- Otherwise, read from the socket
  Right bs <- recvByteString 4096 sock
  | Left err => pure $ Left err
  -- Update the buffer and recurse
  writeIORef connection $ MkConnectionBuffer (buf `append` bs) sock
  recvLine connection


export
recvHeaders : HTTPConnection -> Headers -> IO (Either HTTPConnectionError Headers)
recvHeaders connection headers = do
  Right line <- recvLine connection
  | Left err => pure $ Left $ ConnectionSocketError err
  Right (Just header) <- pure $ parseHeader $ toString line
  | Right Nothing => pure $ Right headers
  | Left err => pure $ Left $ ConnectionProtocolError err
  recvHeaders connection (addHeader header headers)
