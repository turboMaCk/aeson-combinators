import Test.DocTest
main = doctest [ "-isrc"
               , "-XOverloadedStrings"
               , "lib/Data/Aeson/Combinators/Decode.hs"
               , "lib/Data/Aeson/Combinators/Encode.hs"
               ]
