module Lib
    ( someFunc
    ) where

import Parser (parseFile)
import Text.XML.HXT.Parser.XmlParsec

import System.IO
import Control.Monad

uml_url = "https://www.omg.org/spec/UML/20161101/UML.xmi"
mof_url = "https://www.omg.org/spec/MOF/20131001/MOF.xmi"
uml_std_profile_url =  "https://www.omg.org/spec/UML/20161101/StandardProfile.xmi"
base_path = "./stds/"

--proc x = 
--someFunc :: IO 
someFunc = do
  putStrLn "someFunc"  
  --parseFile (base_path ++ "MOF_class.xmi")
  putStrLn "main"  
    --  parseFile (base_path ++ "MOF.xmi")
    --putStrLn "someFunc"
  handle <- openFile "./stds/MOF.xmi" ReadMode
  contents <- hGetContents handle
  let xdoc = xreadDoc contents
  let xmltree = head $  xdoc
  print $ xmltree
  
  --map print xmltree
