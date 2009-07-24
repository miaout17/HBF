module HBF.VM (
    runBF, 
    runMockBF
    ) where

import HBF.TapeList
import HBF.CharIO

import Control.Monad.State
import Data.Word(Word8)
import Data.Char(chr, ord)

data VMData = VMData {
    vmMem :: TapeList Word8, 
    vmCode :: TapeList Char, 
    isRunning :: Bool
    } deriving(Show)

newVMData :: [Char] -> VMData
newVMData code = 
    let m = makeTP $ replicate 65536 0
        c = makeTP code
    in  VMData m c True
    
type VMState = StateT VMData

getMem :: CharIO m => VMState m Word8
getMem = gets $ getTP . vmMem

setMem :: CharIO m => Word8 -> VMState m ()
setMem v = modify $ \s -> s{ vmMem=(putTP (vmMem s) v) }

getCode :: CharIO m => VMState m Char
getCode = gets $ getTP . vmCode

applyMem :: CharIO m => (Word8->Word8) -> VMState m ()
applyMem f = modify $ \s -> s{ vmMem=(applyTP (vmMem s) f) }

word8ToChar :: Word8 -> Char
word8ToChar = chr . fromInteger . toInteger 

charToWord8 :: Char -> Word8
charToWord8 = fromInteger . toInteger . ord

fwdCode, backCode :: CharIO m => VMState m ()
fwdCode = modify $ \s -> s{ vmCode=(fwdTP (vmCode s)) }
backCode = modify $ \s -> s{ vmCode=(backTP (vmCode s)) }

terminate :: CharIO m => VMState m ()
terminate = modify $ \s -> s{ isRunning=False }

inc, dec, fwdMem, backMem, inputCh, outputCh :: CharIO m => VMState m ()
inc = applyMem (+ 1)
dec = applyMem (\n -> n-1)
fwdMem = modify $ \s -> s{ vmMem=(fwdTP (vmMem s)) }
backMem = modify $ \s -> s{ vmMem=(backTP (vmMem s)) }
outputCh = getMem >>= lift . putCh . word8ToChar
inputCh = do
    d <- lift getCh 
    case d of 
        Just c -> setMem $ charToWord8 c
        Nothing -> terminate

blockCount :: Char -> Int
blockCount '[' = 1
blockCount ']' = -1
blockCount _ = 0
        
fwdBlock :: CharIO m => Int -> VMState m ()
fwdBlock n = do
    c <- getCode
    fwdCode
    n' <- return $ n + blockCount c
    if n'==0 then return () else fwdBlock n'
    
backBlock :: CharIO m => Int -> VMState m ()
backBlock n = do
    backCode
    c <- getCode
    n' <- return $ n - blockCount c
    if n'==0 then return () else backBlock n'

stepVM :: CharIO m => VMState m ()
stepVM = do
    c <- getCode
    d <- getMem
    fwdCode
    case c of 
        '+' -> inc
        '-' -> dec
        '>' -> fwdMem
        '<' -> backMem
        '[' -> if d==0 then fwdBlock 1 else return ()
        ']' -> if d==0 then return () else backBlock 0
        ',' -> inputCh 
        '.' -> outputCh
        _ -> return ()
        
checkTerminate :: CharIO m => VMState m ()
checkTerminate = do
    vmData <- get
    if endTP $ vmCode vmData 
        then terminate
        else return ()

runVM :: CharIO m => VMState m ()
runVM = do
    checkTerminate
    r <- gets isRunning 
    if not r then return () else do
        stepVM
        runVM

runBF :: CharIO m => String -> m ()
runBF code = 
    let vmData = newVMData code
    in do runStateT runVM vmData >> return ()

runMockBF :: String -> String -> String -- code, input => output
runMockBF code input =  
    runMockIO input (runBF code)
    --getOutput (runBF code) (makeMockIOData i)
