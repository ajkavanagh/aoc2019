import           Test.Hspec        (Spec)
import           Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import           Day01Specs        (m01Specs, m02Specs)
import           Day02Specs        (m03Specs, m04Specs)
import           Day03Specs        (m05Specs, m06Specs)
import           Day04Specs        (m07Specs, m08Specs)
import           Day07Specs        (m13Specs)
import           Day10Specs        (m19Specs, m20Specs)


main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    m01Specs
    m02Specs
    m03Specs
    m04Specs
    m05Specs
    m06Specs
    m07Specs
    m08Specs
    m13Specs
    m19Specs
    m20Specs
