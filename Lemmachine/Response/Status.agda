module Lemmachine.Response.Status where
{-# IMPORT FFI #-}
open import Data.Nat

data Status : Set where
  -- 1xx: Informational - Request received, continuing process
  -- 2xx: Success - The action was successfully received, understood, and accepted
  OK Created Accepted NoContent : Status
  -- 3xx: Redirection - Further action must be taken in order to complete the request
  MultipleChoices MovedPermanently SeeOther NotModified MovedTemporarily : Status
  -- 4xx: Client Error - The request contains bad syntax or cannot be fulfilled
  BadRequest Unauthorized Forbidden NotFound MethodNotAllowed : Status
  NotAcceptable Conflict Gone PreconditionFailed : Status
  RequestEntityTooLarge RequestURItooLong UnsupportedMediaType : Status
  -- 5xx: Server Error - The server failed to fulfill an apparently valid request
  NotImplemented ServiceUnavailable : Status

{-# COMPILED_DATA Status FFI.Status
    FFI.OK FFI.Created FFI.Accepted FFI.NoContent
    FFI.MultipleChoices FFI.MovedPermanently FFI.SeeOther FFI.NotModified FFI.MovedTemporarily
    FFI.BadRequest FFI.Unauthorized FFI.Forbidden FFI.NotFound FFI.MethodNotAllowed 
    FFI.NotAcceptable FFI.Conflict FFI.Gone FFI.PreconditionFailed
    FFI.RequestEntityTooLarge FFI.RequestURItooLong FFI.UnsupportedMediaType
    FFI.NotImplemented FFI.ServiceUnavailable
#-}
