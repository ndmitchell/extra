
import Neil
import System.IO.Extra

main = do
    let files = ["src/Extra.hs","src/Test.hs"]
    before <- mapM readFile' files
    cmd "runhaskell -isrc Generate"
    after <- mapM readFile' files
    print (before,after)
    when (before /= after) $ error "Generator changed some files, that's a bug"
    cmd "runhaskell -isrc Test"
