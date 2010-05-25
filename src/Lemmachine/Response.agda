module Lemmachine.Response where
open import Lemmachine.Response.Status public
open import Data.Product
open import Data.List hiding (_++_)
open import Data.Nat
open import Data.String

data ResponseHeader : Set where
  _,_ : String → String → ResponseHeader

{-# COMPILED_DATA ResponseHeader
    Lemmachine.FFI.ResponseHeader
    Lemmachine.FFI.ResponseHeader
#-}

ResponseHeaders = List ResponseHeader

record Response : Set where 
  field 
    status : Status
    headers : ResponseHeaders
    body : String

showStatus : Status → String
showStatus OK = "200"
showStatus Created = "201"
showStatus Accepted = "202"
showStatus NoContent = "204"
showStatus MultipleChoices = "300"
showStatus MovedPermanently = "301"
showStatus SeeOther = "303"
showStatus NotModified = "304"
showStatus MovedTemporarily = "307"
showStatus BadRequest = "400"
showStatus Unauthorized = "401"
showStatus Forbidden = "403"
showStatus NotFound = "404"
showStatus MethodNotAllowed = "405"
showStatus NotAcceptable = "406"
showStatus Conflict = "409"
showStatus Gone = "410"
showStatus PreconditionFailed = "412"
showStatus RequestEntityTooLarge = "413"
showStatus RequestURItooLong = "414"
showStatus UnsupportedMediaType = "415"
showStatus NotImplemented = "501"
showStatus ServiceUnavailable = "503"

defaultHtml : Status → String
defaultHtml x = doctype ++ html x where
  doctype = "<!DOCTYPE html>"
  head = "<head><meta charset=utf-8 /><title>Lemmachine</title></head>"
  body : Status → String
  body x = "<body>This HTTP status (" ++ showStatus x ++ ") is brought to you by Lemmachine!</body>"
  html : Status → String
  html x = "<html lang='en'>" ++ head ++ body x ++ "</html>"
