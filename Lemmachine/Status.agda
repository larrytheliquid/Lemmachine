module Lemmachine.Status where
open import Data.Nat

data Status : ℕ -> Set where
  OK : Status 200
  Created : Status 201
  Accepted : Status 202
  NoContent : Status 204
  MultipleChoices : Status 300
  MovedPermanently : Status 301
  SeeOther : Status 303
  NotModified : Status 304
  MovedTemporarily : Status 307
  BadRequest : Status 400
  Unauthorized : Status 401
  Forbidden : Status 403
  NotFound : Status 404
  MethodNotAllowed : Status 405
  NotAcceptable : Status 406
  Conflict : Status 409
  Gone : Status 410
  PreconditionFailed : Status 412
  RequestEntityTooLarge : Status 413
  RequestURItooLong : Status 414
  UnsupportedMediaType : Status 415
  NotImplemented : Status 501
  ServiceUnavailable : Status 503
