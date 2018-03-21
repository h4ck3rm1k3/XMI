module Lib
    ( someFunc
    ) where

import Parser (parseFile)


uml_url = "https://www.omg.org/spec/UML/20161101/UML.xmi"
mof_url = "https://www.omg.org/spec/MOF/20131001/MOF.xmi"
uml_std_profile_url =  "https://www.omg.org/spec/UML/20161101/StandardProfile.xmi"
base_path = "./stds/"

someFunc :: IO ()
someFunc = do
  parseFile (base_path ++ "MOF.xmi")
  putStrLn "someFunc"
