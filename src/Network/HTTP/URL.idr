module Network.HTTP.URL

import Data.Fin
import Data.String


record URL where
  constructor MkURL
  scheme : String
  host : String
  port : Maybe Int
  path : String
  query : Maybe String
  fragment : Maybe String


-- | Split a URL string into a scheme and the rest.
splitScheme : String -> Maybe (String, String)
splitScheme url = match 0 0 $ strM url where
  match : Nat -> Fin 3 -> StrM _ -> Maybe (String, String)
  match i 0 (StrCons ':' s) = match i 1 $ strM s
  match i 1 (StrCons '/' s) = match i 2 $ strM s
  match i 2 (StrCons '/' s) = pure (strSubstr 0 (cast i) url, s)
  match i _ (StrCons c s) = match (i + 1) 0 $ strM s
  match i _ StrNil = Nothing


splitAddr : String -> Maybe (String, String)
splitAddr url = match 0 $ strM url where
  match : Nat -> StrM _ -> Maybe (String, String)
  match i (StrCons '/' s) = pure (strSubstr 0 (cast i) url, "/" ++ s)
  match i (StrCons c s) = match (i + 1) $ strM s
  match i StrNil = pure (url, "/")


parseAddr : String -> Maybe (String, Maybe Int)
parseAddr addr = match 0 $ strM addr where
  match : Nat -> StrM _ -> Maybe (String, Maybe Int)
  match _ StrNil = pure (addr, Nothing)
  match i (StrCons ':' s) = do
    port <- parsePositive s
    pure (strSubstr 0 (cast i) addr, Just port)
  match i (StrCons c s) = match (i + 1) $ strM s


parsePath : String -> (String, Maybe String, Maybe String)
parsePath s = matchPath 0 $ strM s where
  matchFragment :
    String -> String -> Nat -> StrM _ -> (String, Maybe String, Maybe String)
  matchFragment path s' _ StrNil = (path , Just s' , Nothing)
  matchFragment path s' i (StrCons '#' s'') =
    ( path
    , Just $ strSubstr 0 (cast i) s'
    , Just s''
    )
  matchFragment path s' i (StrCons c s'') = matchFragment path s' (i + 1) $ strM s''
  matchPath : Nat -> StrM _ -> (String, Maybe String, Maybe String)
  matchPath _ StrNil = (s, Nothing, Nothing)
  matchPath i (StrCons '?' s') = matchFragment (strSubstr 0 (cast i) s) s' 0 $ strM s'
  matchPath i (StrCons c s') = matchPath (i + 1) $ strM s'


-- | Parse a URL string into a URL record.
public export
parse : String -> Maybe URL
parse url = do
  (scheme, rest) <- splitScheme url
  (addr, rest) <- splitAddr rest
  (host, port) <- parseAddr addr
  let (path, query, fragment) = parsePath rest
  pure $ MkURL scheme host port path query fragment
