import Test.Hspec

import qualified Day4Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day4" Day4Spec.spec
--   describe "Foo.Bar" Foo.BarSpec.spec
--   describe "Baz"     BazSpec.spec
