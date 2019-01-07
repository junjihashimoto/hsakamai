import Test.DocTest
main = doctest [
    "-isrc"
  , "-XOverloadedStrings"
  , "src/Akamai/NetStorage.hs"
  , "src/Akamai/Edgegrid.hs"
  ]
