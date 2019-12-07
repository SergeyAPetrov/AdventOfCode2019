import Test.Hspec

import qualified Day6Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day6" Day6Spec.spec
--   describe "Foo.Bar" Foo.BarSpec.spec
--   describe "Baz"     BazSpec.spec
