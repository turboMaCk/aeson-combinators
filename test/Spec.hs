import qualified JSONDecodeSpec as Decode
import qualified JSONEncodeSpec as Encode
import           Test.Hspec     (hspec)

main :: IO ()
main = hspec $ do
  Decode.spec
  Encode.spec
