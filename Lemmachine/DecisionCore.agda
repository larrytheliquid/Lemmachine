module Lemmachine.DecisionCore where
open import Lemmachine.Resource
open import Lemmachine.Request
open import Lemmachine.Status
open import Data.Bool

-- data Decision : Set where
--   B13 B12 B11 B10 B9 : Decision
--   B8 B7 B6 B5 B4 B3 : Decision
--   C3 C4 D4 D5 E5 E6 F6 F7 : Decision
--   G7 G8 G9 G11 : Decision
--   H7 H10 H11 H12 : Decision
--   I4 I7 I12 I13 : Decision
--   J18 K5 K7 K13 : Decision
--   L5 L7 L13 L14 L15 L17 : Decision
--   M5 M7 M16 M20 : Decision
--   N5 N11 N16 : Decision
--   O14 O16 O18 O20 : Decision
--   P3 P11 : Decision

B12 : Request → Status
B12 _ = OK

B13 : Request → Status
B13 r = if serviceAvailable r then B12 r else ServiceUnavailable

decide : Request → Status
decide r = B13 r
