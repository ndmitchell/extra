
import System.Process.Extra

main = do
    system_ "runhaskell -isrc Generate"
    system_ "git diff --exit-code"
    system_ "runhaskell -isrc -itest Test"
