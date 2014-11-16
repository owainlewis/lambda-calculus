module Main where

import           Data.Monoid
import           Language.LambdaCalculus

maybeFromEither (Left _)  = Nothing
maybeFromEither (Right x) = Just x

-- | Read a lambda calclus example from file
--
openExample :: String -> IO (Maybe Term)
openExample ex = do
   contents <- readFile $ "samples/" ++ ex ++ ".lc"
   return $ maybeFromEither $ parseTerm contents
