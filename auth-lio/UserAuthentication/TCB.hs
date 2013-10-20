{-# LANGUAGE Unsafe #-}

module UserAuthentication.TCB (
    login
  , authenticate
  , register
  , dcPutStrLnTCB
  ) where

import Data.Char
import qualified Data.Map as Map
import Numeric

import LIO
import LIO.TCB
import LIO.DCLabel
import LIO.LIORef

-- | Print to standard output
dcPutStrLnTCB :: String -> DC ()
dcPutStrLnTCB = ioTCB . putStrLn

hash password salt =
    let convert [] acc = acc
        convert (c:cs) acc = convert cs (((ord c) + (10 * acc)) `mod` 999991)
    in showHex (convert (salt ++ password) 0) ""

loginLabel = "L" %% "L"

db = newLIORef loginLabel Map.empty

makeUserRecord username password salt = do
  dcPutStrLnTCB "makeUserRecord"
  let lab = (username /\ "L") %% (username /\ "L")
  s <- labelP lab salt
  h <- label lab (hash password salt)
  return (s, h, PrivTCB username)

register username password salt = do
  dcPutStrLnTCB "register"
  usr <- makeUserRecord username password salt
  (\db -> modifyLIORef db (\m -> Map.insert username usr m)) =<< db

lookup username = do
  db <- readLIORef =<< db
  return (Map.lookup username db)

login username password = do
  username <- label ("L" %% "L") username
  password <- label ("L" %% "L") password
  authenticate username password

authenticate username password = do
  return ()
