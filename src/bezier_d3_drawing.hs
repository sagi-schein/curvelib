import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import D3JS

test :: Int -> IO ()
test n = T.writeFile "generated.js" $ reify (box (T.pack "#div1") (300,300) >>= bars n 300 (Data1D [100,20,80,60,120]))

drawPoints :: Data2D -> String -> IO ()
drawPoints pts fileName = T.writeFile fileName $ T.pack "test"
