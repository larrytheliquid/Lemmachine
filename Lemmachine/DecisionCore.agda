module Lemmachine.DecisionCore where
open import Lemmachine.Resource
open import Lemmachine.Request
open import Lemmachine.Status
open import Lemmachine.Utils
open import Data.Bool
open import Data.Nat
open import Data.String
open import Relation.Binary
open import Relation.Binary.PropositionalEquality
open import Data.Maybe
open import Data.List
open import Data.Product

postulate L13 : Request → Status
postulate I7 : Request → Status

J18 : Request → Status
J18 r with Request.method r
... | HEAD = NotModified
... | GET  = NotModified
... | _    = PreconditionFailed

I12+I13+K13 : Request → Status
I12+I13+K13 r with fetch "If-None-Match" (Request.headers r)
... | nothing = L13 r
... | just clientETag with "*" == clientETag
... | true = J18 r
... | false with generateETag r
... | nothing = L13 r
... | just serverETag with clientETag == serverETag
... | true = J18 r
... | false = L13 r

H10+H11+H12 : Request → Status
H10+H11+H12 r with fetch "If-Unmodified-Since" (Request.headers r)
... | nothing = I12+I13+K13 r
... | just clientDate with isDate clientDate
... | false = I12+I13+K13 r
... | true  with lastModified r
... | nothing = I12+I13+K13 r
... | just serverDate with isModified clientDate serverDate
... | true = PreconditionFailed
... | false = I12+I13+K13 r

G8+G9+G11 : Request → Status
G8+G9+G11 r with fetch "If-Match" (Request.headers r)
... | nothing = H10+H11+H12 r
... | just clientETag with "*" == clientETag
... | true  = H10+H11+H12 r
... | false with generateETag r
... | nothing = PreconditionFailed
... | just serverETag with clientETag == serverETag
... | true = H10+H11+H12 r
... | false = PreconditionFailed

H7 : Request → Status
H7 r with fetch "If-Match" (Request.headers r)
... | just _  = PreconditionFailed
... | nothing = I7 r

G7 : Request → Status
G7 r with resourceExists r
... | true  = G8+G9+G11 r
... | false = H7 r

F6+F7 : Request → Status
F6+F7 r with fetch "Accept-Encoding" (Request.headers r)
... | nothing = G7 r
... | just encoding with fetch encoding (encodingsProvided r)
... | just _  = G7 r
... | nothing = NotAcceptable

E5+E6 : Request → Status
E5+E6 r with fetch "Accept-Charset" (Request.headers r)
... | nothing = F6+F7 r
... | just charset with fetch charset (charsetsProvided r)
... | just _  = F6+F7 r
... | nothing = NotAcceptable

D4+D5 : Request → Status
D4+D5 r with fetch "Accept-Language" (Request.headers r)
... | nothing = E5+E6 r
... | just language with languageAvailable r
... | true    = E5+E6 r
... | false   = NotAcceptable

C3+C4 : Request → Status
C3+C4 r with fetch "Accept" (Request.headers r)
... | nothing = D4+D5 r
... | just contentType with fetch contentType (contentTypesProvided r)
... | just _  = D4+D5 r
... | nothing = NotAcceptable

B3 : Request → Status
B3 r with Request.method r
... | OPTIONS = OK
... | _       = C3+C4 r

B4 : Request → Status
B4 r with validEntityLength r
... | true  = B3 r
... | false = RequestEntityTooLarge

B5 : Request → Status
B5 r with knownContentType r
... | true  = B4 r
... | false = UnsupportedMediaType

B6 : Request → Status
B6 r with validContentHeaders r
... | true  = B5 r
... | false = NotImplemented

B7 : Request → Status
B7 r with forbidden r
... | true  = Forbidden
... | false = B6 r

B8 : Request → Status
B8 r with isAuthorized r
... | true  = B7 r
... | false = Unauthorized

B9 : Request → Status
B9 r with malformedRequest r
... | true  = BadRequest
... | false = B8 r

B10 : Request → Status
B10 r with any (eqMethod (Request.method r))
               (allowedMethods r)
... | true  = B9 r
... | false = MethodNotAllowed

B11 : Request → Status
B11 r with uriTooLong r
... | true  = RequestURItooLong
... | false = B10 r

B12 : Request → Status
B12 r with any (eqMethod (Request.method r))
               (knownMethods r)
... | true  = B11 r 
... | false = NotImplemented

B13 : Request → Status
B13 r with serviceAvailable r 
... | true  = B12 r 
... | false = ServiceUnavailable

decide : Request → Status
decide r = B13 r
