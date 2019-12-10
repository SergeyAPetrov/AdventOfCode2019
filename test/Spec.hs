import Test.Hspec

import qualified Day10Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day10" Day10Spec.spec
--   describe "Foo.Bar" Foo.BarSpec.spec
--   describe "Baz"     BazSpec.spec
