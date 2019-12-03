import Test.Hspec

import qualified Day3Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day3" Day3Spec.spec
--   describe "Foo.Bar" Foo.BarSpec.spec
--   describe "Baz"     BazSpec.spec
