-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------
{-Trabalho 11, Questão 1-}

{-Trabalho 11, Questão 2-}
import Control.Monad

strCheck :: String -> [Maybe Char]
strCheck [] = [Nothing]
strCheck (s:ss) = if ((fromEnum s) > 64 && (fromEnum s) < 91) || ((fromEnum s) > 96 && (fromEnum s) < 123) then (Just s) : (strCheck ss)
                  else (Nothing) : (strCheck ss)

strCaps :: [Maybe Char] -> [Maybe Char]
strCaps [] = []
strCaps ((Nothing):ss) = (Nothing) : (strCaps ss)
strCaps ((Just s):ss) = if (fromEnum s) > 96 then (Just (toEnum ((fromEnum s) - 32))) : (strCaps ss)
                        else (Just s) : (strCaps ss)

strPart :: [Maybe Char] -> [String]
strPart [] = []
strPart ((Nothing):ss) = [] : (strPart ss)
strPart ((Just s):ss) = [[s]] ++ (strPart ss)
{-
main = do {
 input <- getLine; -- strRead
 check = strCheck input; -- strCheck
 caps = strCaps check; -- strCaps
 part = strPart caps; -- strPart
 putStrLn (part) -- strPrint
}-}