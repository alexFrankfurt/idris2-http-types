module Network.HTTP.Methods

import Data.String
import Derive.Prelude

%language ElabReflection


public export
data Method
  = GET
  | POST
  | PUT
  | DELETE
  | HEAD
  | OPTIONS
  | TRACE
  | CONNECT
  | PATCH


%runElab derive "Method" [Show, Eq]


export
methodToString : Method -> String
methodToString GET     = "GET"
methodToString POST    = "POST"
methodToString PUT     = "PUT"
methodToString DELETE  = "DELETE"
methodToString HEAD    = "HEAD"
methodToString OPTIONS = "OPTIONS"
methodToString TRACE   = "TRACE"
methodToString CONNECT = "CONNECT"
methodToString PATCH   = "PATCH"


stringToMethod' : String -> Maybe Method
stringToMethod' "GET"     = Just GET
stringToMethod' "POST"    = Just POST
stringToMethod' "PUT"     = Just PUT
stringToMethod' "DELETE"  = Just DELETE
stringToMethod' "HEAD"    = Just HEAD
stringToMethod' "OPTIONS" = Just OPTIONS
stringToMethod' "TRACE"   = Just TRACE
stringToMethod' "CONNECT" = Just CONNECT
stringToMethod' "PATCH"   = Just PATCH
stringToMethod' _         = Nothing


export
stringToMethod : String -> Maybe Method
stringToMethod s = stringToMethod' $ toUpper s


public export
Show Method where
  show = methodToString
