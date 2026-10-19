
import Data.Text (pack)
import Numeric
import Web.Console
import Web.Random

main = do
  let g = mkStdGen 1
      (w1, g1) = genWord32 g
      (w2, g2) = genWord32 g1
      (w3, g3) = genWord32 g2
  consoleLog $ pack $
    showString "w1=0x" $ showHex w1 $
     showString ", w2=0x" $ showHex w2
      $ showString ", w3=0x" $ showHex w3 ""

