module Spike where

data GET-Header : Set where
  Pragma : GET-Header
  Authorization From : GET-Header
  If-Modified-Since : GET-Header
  Referer User-Agent : GET-Header

data HEAD-Header : Set where
  Pragma : HEAD-Header
  Authorization From : HEAD-Header
  Referer User-Agent : HEAD-Header

data POST-Header : Set where
  Date Pragma : POST-Header
  Content-Encoding : POST-Header

  Content-Length : POST-Header
  Content-Type : POST-Header
  Entity-Body : POST-Header
