module HBF.TapeList(
    TapeList(), 
    leftTP, 
    rightTP, 
    
    beginTP, 
    endTP, 
    
    makeTP, 
    fwdTP, 
    backTP, 
    getTP, 
    putTP, 
    applyTP, 
    getEntireTP
) where

data TapeList a = TapeList {
    leftTP :: [a],
    rightTP :: [a]
    } deriving(Show)

makeTP :: [a] -> TapeList a
makeTP n = TapeList { leftTP = [], rightTP = n }

beginTP :: TapeList a -> Bool
beginTP = null . leftTP

endTP :: TapeList a -> Bool
endTP = null . rightTP

fwdTP :: TapeList a -> TapeList a
fwdTP (TapeList ls (r:rs)) = TapeList (r:ls) rs

backTP :: TapeList a -> TapeList a
backTP (TapeList (l:ls) rs) = TapeList ls (l:rs)

getTP :: TapeList a -> a
getTP = head . rightTP

putTP :: TapeList a -> a -> TapeList a
putTP (TapeList ls (r:rs)) newValue = TapeList ls (newValue:rs)

applyTP :: TapeList a -> (a->a) -> TapeList a
applyTP tp f = putTP tp ( f $ getTP tp )

getEntireTP:: TapeList a -> [a]
getEntireTP tp = reverse (leftTP tp) ++ rightTP tp