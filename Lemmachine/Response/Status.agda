module Lemmachine.Response.Status where
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

toℕ : Status → ℕ
toℕ OK = 200
toℕ Created = 201
toℕ Accepted = 202
toℕ NoContent = 204
toℕ MultipleChoices = 300
toℕ MovedPermanently = 301
toℕ SeeOther = 303
toℕ NotModified = 304
toℕ MovedTemporarily = 307
toℕ BadRequest = 400
toℕ Unauthorized = 401
toℕ Forbidden = 403
toℕ NotFound = 404
toℕ MethodNotAllowed = 405
toℕ NotAcceptable = 406
toℕ Conflict = 409
toℕ Gone = 410
toℕ PreconditionFailed = 412
toℕ RequestEntityTooLarge = 413
toℕ RequestURItooLong = 414
toℕ UnsupportedMediaType = 415
toℕ NotImplemented = 501
toℕ ServiceUnavailable = 503
