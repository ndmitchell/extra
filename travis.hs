
import Control.Monad
import System.IO.Extra
import System.Process.Extra

main = do
    let files = ["src/Extra.hs","test/TestGen.hs"]
    system_ "runhaskell -isrc Generate"
    system_ "git diff --exit-code"
    system_ "runhaskell -isrc -itest Test"
