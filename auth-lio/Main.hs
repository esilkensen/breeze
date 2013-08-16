{-# LANGUAGE Unsafe #-}

module Main (main) where
    
import LIO.DCLabel
    
import safe UserAuthentication
    
main :: IO ()
main = evalDC $ do
  register "User" "Password" "Salt"
  return ()
                                     
