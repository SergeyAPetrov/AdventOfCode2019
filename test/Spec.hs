import Test.Hspec

import qualified Day1Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day1" Day1Spec.spec
--   describe "Foo.Bar" Foo.BarSpec.spec
--   describe "Baz"     BazSpec.spec
