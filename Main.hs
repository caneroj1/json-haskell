import           Control.Monad        (liftM)
import           Data.ByteString.Lazy (hGetContents)
import           Parser
import           System.Environment   (getArgs)
import           System.IO            hiding (hGetContents)

main :: IO ()
main = do
  fp <- liftM head getArgs
  fh <- openFile fp ReadMode
  liftM (parse fp) (hGetContents fh) >>= printJson

printJson :: (Show a, Show b) => Either a b -> IO ()
printJson (Left a)  = do
  putStr "Parse Error at "
  print a
printJson (Right b) =
  print b
