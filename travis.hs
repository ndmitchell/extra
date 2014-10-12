
import Neil
import System.IO.Extra

main = do
    let files = ["src/Extra.hs","src/Test.hs"]
    before <- mapM readFile' files
    cmd "runhaskell -isrc Generate"
    after <- mapM readFile' files
    when (before /= after) $ error "Generator changed some files, that's a bug"

    cmd "git checkout > temp.txt" -- should have no stdout
    src <- readFile "temp.txt"
    print src
    when (lines src /= []) $ error "generating changed something!"

    cmd "runhaskell -isrc Test"
