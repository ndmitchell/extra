
import Control.Monad
import System.IO.Extra
import System.Process.Extra

main = do
    let files = ["src/Extra.hs","test/TestGen.hs"]
    before <- mapM readFile' files
    system_ "runhaskell -isrc Generate"
    after <- mapM readFile' files
    when (before /= after) $ error "Generator changed some files, that's a bug"
    system_ "runhaskell -isrc -itest Test"
