module Examples where

import Control.Applicative
import Data.Machine
import Data.Machine.Group

onlyFirst :: Monad m => ProcessT m Int String
onlyFirst = construct $ do
  x <- await
  yield $ show x
  yield "stopping early"
  stop

showEnd :: Monad m => ProcessT m Int String
showEnd = repeatedly $ do
  x <- await <|> (yield "group end" >> stop)
  yield $ show x

-- | Example using 'taggedBy' and 'partitioning'
test :: Monad m => ProcessT m Int String
test = (supply [1,2,2,3,3,3] $ taggedBy (==)) ~> partitioning showEnd

-- | Example using 'groupingOn'
test2 :: Monad m => ProcessT m Int String
test2 = supply [1,2,2,3,3,3] $ groupingOn (==) onlyFirst

main :: IO ()
main = do
  runT test >>= print
  runT test2 >>= print
