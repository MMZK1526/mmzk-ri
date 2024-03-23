import           Int.Injection.Spec
import           List.Spec
import           Maybe.Spec
import           Read.Spec
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  intInjectionSpec
  listSpec
  maybeSpec
  readSpec
