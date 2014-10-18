-- file: ch05/Main.hs
module Main (main) where

import SimpleJSON

main = print (JObject [("foo", JNumber 2), ("bar", JBool False)])
