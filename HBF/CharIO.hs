{-# LANGUAGE TypeSynonymInstances #-} 
module HBF.CharIO (
    CharIO, 
    MockIO, 
    runMockIO, 
    putCh, 
    getCh
) where

import Control.Monad.State

class (Monad m) => CharIO m where
    getCh :: m (Maybe Char)
    putCh :: Char -> m ()
    
instance CharIO IO where
    getCh = do
        c <- getChar
        return $ Just c
    putCh = putChar
    
data MockIOData = MockIOData {
    moInput :: [Char],
    moOutput :: [Char]
    }

type MockIO = State MockIOData
    
instance CharIO MockIO where
    getCh = do
        d <- get
        case d of
            MockIOData (i:is) o -> do
                put $ MockIOData is o
                return $ Just i
            MockIOData [] _ ->
                return Nothing
    putCh c = modify $ \s -> s{moOutput = moOutput s ++ [c]}

makeMockIOData i = MockIOData i []

runMockIO :: String -> MockIO () -> String
runMockIO input mockIO =
    let mockIOData = makeMockIOData input
        MockIOData i o = execState mockIO mockIOData
    in  o
