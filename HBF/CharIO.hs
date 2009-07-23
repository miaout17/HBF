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
    getCh :: m Char
    putCh :: Char -> m ()
    
instance CharIO IO where
    getCh = getChar
    putCh = putChar
    
data MockIOData = MockIOData {
    input :: [Char],
    output :: [Char]
    }

type MockIO = State MockIOData
    
instance CharIO MockIO where
    getCh = do
        s@(MockIOData (i:is) o) <- get
        put $ MockIOData is o
        return i
    putCh c = modify $ \s -> s{output = output s ++ [c]}

--getOutput :: MockIO () -> [Char]
getOutput :: MockIO a -> MockIOData -> [Char]
getOutput s d = 
    let MockIOData i o = execState s d
    in  o
    
makeMockIOData i = MockIOData i []
