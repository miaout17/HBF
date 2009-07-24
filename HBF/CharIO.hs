{-# LANGUAGE TypeSynonymInstances #-} 
module HBF.CharIO (
    CharIO, 
    MockIO, 
    MockIOData, 
    makeMockIOData, 
    getOutput, 
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
    input :: [Char],
    output :: [Char]
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
    putCh c = modify $ \s -> s{output = output s ++ [c]}

--getOutput :: MockIO () -> [Char]
getOutput :: MockIO a -> MockIOData -> [Char]
getOutput s d = 
    let MockIOData i o = execState s d
    in  o
    
makeMockIOData i = MockIOData i []
