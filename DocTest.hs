import Test.DocTest
main = doctest [ "-XOverloadedStrings"
               , "-XCPP"
               , "-XLambdaCase"
               , "lib/Data/Aeson/Combinators/Compat.hs"
               , "lib/Data/Aeson/Combinators/Decode.hs"
               , "lib/Data/Aeson/Combinators/Encode.hs"
               ]
