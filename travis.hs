
import Control.Monad
import System.IO.Extra
import System.Process.Extra

main = do
    let files = ["src/Extra.hs","test/TestGen.hs"]
    before <- mapM readFile' files
    mapM_ (`appendFile` " ") files -- ensure that if they don't regen, it will error
    system_ "runhaskell -isrc Generate"
    after <- mapM readFile' files
    when (before /= after) $ error "Generator changed some files, that's a bug"

    src <- systemOutput_ "git checkout" -- should have no stdout
    when (lines src /= []) $ error $ "generating changed something!" ++ src

    system_ "runhaskell -isrc -itest Test"
