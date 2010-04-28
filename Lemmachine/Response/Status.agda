module Lemmachine.Response.Status where
{-# IMPORT Lemmachine.FFI #-}
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

{-# COMPILED_DATA Status Lemmachine.FFI.Status
    Lemmachine.FFI.OK Lemmachine.FFI.Created Lemmachine.FFI.Accepted Lemmachine.FFI.NoContent
    Lemmachine.FFI.MultipleChoices Lemmachine.FFI.MovedPermanently 
    Lemmachine.FFI.SeeOther Lemmachine.FFI.NotModified Lemmachine.FFI.MovedTemporarily
    Lemmachine.FFI.BadRequest Lemmachine.FFI.Unauthorized Lemmachine.FFI.Forbidden 
    Lemmachine.FFI.NotFound Lemmachine.FFI.MethodNotAllowed 
    Lemmachine.FFI.NotAcceptable Lemmachine.FFI.Conflict Lemmachine.FFI.Gone Lemmachine.FFI.PreconditionFailed
    Lemmachine.FFI.RequestEntityTooLarge Lemmachine.FFI.RequestURItooLong Lemmachine.FFI.UnsupportedMediaType
    Lemmachine.FFI.NotImplemented Lemmachine.FFI.ServiceUnavailable
#-}
