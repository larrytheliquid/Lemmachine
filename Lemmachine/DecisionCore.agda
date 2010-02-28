open import Lemmachine.Resource
module Lemmachine.DecisionCore (c : Config) where
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

O18 : Request → Status
O18 r with (Config.multipleChoices c) r
... | false = OK
... | true  = MultipleChoices

O20 : Request → Status
O20 r with Request.body r
... | nothing = NoContent
... | just _ = O18 r

P11 : Request → Status
P11 r with fetch "Location" (Request.headers r)
... | nothing = O20 r
... | just _  = Created

O14 : Request → Status
O14 r with (Config.isConflict c) r
... | true  = Conflict
... | false = P11 r

O16 : Request → Status
O16 r with Request.method r
... | PUT = O14 r
... | _ = O18 r

N11 : Request → Status
N11 r with isRedirect r
... | true  = SeeOther
... | false = P11 r

N16 : Request → Status
N16 r with Request.method r
... | POST = N11 r
... | _ = O16 r

M20 : Request → Status
M20 r with (Config.deleteResource c) r
... | false = Accepted
... | true = O20 r

M16 : Request → Status
M16 r with Request.method r
... | DELETE = M20 r
... | _ = N16 r

L13+L14+L15+L17 : Request → Status
L13+L14+L15+L17 r with fetch "If-Modified-Since" (Request.headers r)
... | nothing = M16 r
... | just clientDate with isDate clientDate
... | false = M16 r
... | true with isModified now clientDate
... | true = M16 r
... | false with (Config.lastModified c) r
... | nothing = M16 r
... | just serverDate with isModified clientDate serverDate
... | true = M16 r
... | false = NotModified

J18 : Request → Status
J18 r with Request.method r
... | HEAD = NotModified
... | GET  = NotModified
... | _    = PreconditionFailed

I12+I13+K13 : Request → Status
I12+I13+K13 r with fetch "If-None-Match" (Request.headers r)
... | nothing = L13+L14+L15+L17 r
... | just clientETag with "*" == clientETag
... | true = J18 r
... | false with (Config.generateETag c) r
... | nothing = L13+L14+L15+L17 r
... | just serverETag with clientETag == serverETag
... | true = J18 r
... | false = L13+L14+L15+L17 r

H10+H11+H12 : Request → Status
H10+H11+H12 r with fetch "If-Unmodified-Since" (Request.headers r)
... | nothing = I12+I13+K13 r
... | just clientDate with isDate clientDate
... | false = I12+I13+K13 r
... | true  with (Config.lastModified c) r
... | nothing = I12+I13+K13 r
... | just serverDate with isModified clientDate serverDate
... | true = PreconditionFailed
... | false = I12+I13+K13 r

G8+G9+G11 : Request → Status
G8+G9+G11 r with fetch "If-Match" (Request.headers r)
... | nothing = H10+H11+H12 r
... | just clientETag with "*" == clientETag
... | true  = H10+H11+H12 r
... | false with (Config.generateETag c) r
... | nothing = PreconditionFailed
... | just serverETag with clientETag == serverETag
... | true = H10+H11+H12 r
... | false = PreconditionFailed

I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 : Request → Status
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r   with Request.method r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | PUT    with (Config.movedPermanently c) r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 _ | PUT  | just _  = MovedPermanently
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | PUT  | nothing   with (Config.isConflict c) r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | PUT  | nothing | false   = N11 r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 _ | PUT  | nothing | true    = Conflict
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | _      with (Config.previouslyExisted c) r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | _    | true      with (Config.movedPermanently c) r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 _ | _    | true    | just _  = MovedPermanently
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | _    | true    | nothing   with (Config.movedTemporarily c) r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 _ | _    | true    | nothing | just _  = MovedTemporarily
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | POST | true    | nothing | nothing   with (Config.allowMissingPost c) r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 _ | POST | true    | nothing | nothing | false = Gone
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | POST | true    | nothing | nothing | true  = N11 r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 _ | _    | true    | nothing | nothing = Gone
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | POST | false     with (Config.allowMissingPost c) r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 _ | POST | false   | false = NotFound
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r | POST | false   | true  = N11 r
I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 _ | _    | false   = NotFound

H7 : Request → Status
H7 r with fetch "If-Match" (Request.headers r)
... | just "*"  = PreconditionFailed
... | _ = I7+I4+P3+K7+K5+L5+M5+N5+L7+M7 r

G7 : Request → Status
G7 r with (Config.resourceExists c) r
... | true  = G8+G9+G11 r
... | false = H7 r

F6+F7 : Request → Status
F6+F7 r with fetch "Accept-Encoding" (Request.headers r)
... | nothing = G7 r
... | just encoding with fetch encoding ((Config.encodingsProvided c) r)
... | just _  = G7 r
... | nothing = NotAcceptable

E5+E6 : Request → Status
E5+E6 r with fetch "Accept-Charset" (Request.headers r)
... | nothing = F6+F7 r
... | just charset with fetch charset ((Config.charsetsProvided c) r)
... | just _  = F6+F7 r
... | nothing = NotAcceptable

D4+D5 : Request → Status
D4+D5 r with fetch "Accept-Language" (Request.headers r)
... | nothing = E5+E6 r
... | just language with (Config.languageAvailable c) r
... | true    = E5+E6 r
... | false   = NotAcceptable

C3+C4 : Request → Status
C3+C4 r with fetch "Accept" (Request.headers r)
... | nothing = D4+D5 r
... | just contentType with fetch contentType ((Config.contentTypesProvided c) r)
... | just _  = D4+D5 r
... | nothing = NotAcceptable

B3 : Request → Status
B3 r with Request.method r
... | OPTIONS = OK
... | _       = C3+C4 r

B4 : Hook Bool → Request → Status
B4 validEntityLength r with validEntityLength r
... | true  = B3 r
... | false = RequestEntityTooLarge

B5 : Hook Bool → Request → Status
B5 knownContentType r with knownContentType r
... | true  = B4 (Config.validEntityLength c) r
... | false = UnsupportedMediaType

B6 : Hook Bool → Request → Status
B6 validContentHeaders r with validContentHeaders r
... | true  = B5 (Config.knownContentType c) r
... | false = NotImplemented

B7 : Hook Bool → Request → Status
B7 forbidden r with forbidden r
... | true  = Forbidden
... | false = B6 (Config.validContentHeaders c) r

B8 : Hook Bool → Request → Status
B8 isAuthorized r with isAuthorized r
... | true  = B7 (Config.forbidden c) r
... | false = Unauthorized

B9 : Hook Bool → Request → Status
B9 malformedRequest r with malformedRequest r
... | true  = BadRequest
... | false = B8 (Config.isAuthorized c) r

B10 : Hook (List Method) → Request → Status
B10 allowedMethods r with 
  any (eqMethod (Request.method r))
      (allowedMethods r)
... | true  = B9 (Config.malformedRequest c) r
... | false = MethodNotAllowed

B11 : Hook Bool → Request → Status
B11 uriTooLong r with uriTooLong r
... | true  = RequestURItooLong
... | false = B10 (Config.allowedMethods c) r

B12 : Hook (List Method) → Request → Status
B12 knownMethods r with 
  any (eqMethod (Request.method r))
      (knownMethods r)
... | true  = B11 (Config.uriTooLong c) r 
... | false = NotImplemented

B13 : Hook Bool → Request → Status
B13 serviceAvailable r with serviceAvailable r 
... | true  = B12 (Config.knownMethods c) r 
... | false = ServiceUnavailable

resolve : Request → Status
resolve r = B13 (Config.serviceAvailable c) r
