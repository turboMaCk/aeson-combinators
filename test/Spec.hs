import Test.Hspec

import qualified JSONDecodeSpec as Decode

main :: IO ()
main = hspec $ do
  Decode.spec
