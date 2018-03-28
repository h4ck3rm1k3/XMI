module Lib
    ( someFunc
    ) where

import Parser (parseFile)
import Text.XML.HXT.Parser.XmlParsec

import System.IO
import Control.Monad
import Text.XML.HaXml.Parse     (xmlParse)
import Text.XML.HaXml.XmlContent.Parser
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Data.Maybe 
import Text.XML.HaXml.Types

import MOF

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
  ---handle <- openFile "./stds/MOF.xmi" ReadMode
  ---contents <- hGetContents handle
  -- doc = xmlParse "somefile" contents
  -- x = fromXml doc
  -- print "Hello"
  m <- fReadXml "./stds/MOF.xmi"::IO Xmi'XMI
  print m
  --let xdoc = xreadDoc contents
  --let xmltree = head $  xdoc
  --print $ xmltree
  
  --map print xmltree
