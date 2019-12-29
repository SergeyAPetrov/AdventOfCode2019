import Test.Hspec

import qualified Day12Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day12" Day12Spec.spec
--   describe "Foo.Bar" Foo.BarSpec.spec
--   describe "Baz"     BazSpec.spec
