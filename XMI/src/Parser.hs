module Parser (parseString, parseFile) where

import AST(Name, Abstract, ReadOnly, UppVal, LowVal,
            Qualified(Qualified),
            Imports (ImportRef),
            Package(Package, Foo),
            Packaged(Class, Enumeration, SubPackage, Import, Fake ),
            Literal(Literal),
            Owned(Property, Operation, Generalization),
            Parameter(InOut,Return),
            Limit(LUnlimitedNatural,LInteger),
            Type(RefType,PrimitiveType),
            Value,
            )

import Prelude hiding (LT, GT, not)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language


import Data.Char
import Data.List (intersperse)

---
import Text.Parsec.Perm
import Text.ParserCombinators.Parsec.Expr

import Control.Monad
import Control.Monad.State
import Debug.Trace
import Debug.Trace(trace, traceM)


---------------------------------
data ClassTag = ClassTag { idC :: Name, nC :: Name, aC :: Abstract }
  deriving ( Eq, Show)
data EnumTag  = EnumTag  { idE :: Name, nE :: Name }
  deriving (Eq, Show)
data PropTag  = PropTag  { nP  :: Name, idP:: Name, r  :: ReadOnly, tP :: Name }
  deriving ( Eq, Show)
data Prop1Tag = Prop1Tag { upP :: UppVal, lwP :: LowVal, qP :: [Qualified] }
  deriving ( Eq, Show)
data QualTag  = QualTag  { nQ  :: Name, tQ :: Name }
  deriving ( Eq, Show)


--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellStyle
      { P.reservedNames = [
          --"uml:Package",
          "xmlns:xmi","xmlns:xmi", "xmi:idref", "xmi:id", "xmi:type", 
                           "packagedElement",

            "packageImport",
            "importedPackage",
            
                           "ownedMember", "ownedAttribute", "ownedOperation",
                           "ownedParameter", "ownedLiteral", xmiVersion
                          ]
                               , P.reservedOpNames = ["="]
                               , P.commentLine = "--"
                               , P.commentStart = "<!--"
                               , P.commentEnd = "-->"
                               , P.caseSensitive = True
                               , P.identLetter = alphaNum <|> oneOf ":/_.-#"
                               , P.identStart = alphaNum <|> oneOf "_"       }
                          )

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
integer    = P.integer lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

--------------------------------------------------------------------------------
-- Parsing functions
--------------------------------------------------------------------------------


parseString :: String -> Package
parseString s = case parse parser "" s of
                  Left err -> error $ "parse error " ++ (show err)
                  Right val -> val

parseFile :: String -> IO Package
parseFile file = do p <- parseFromFile parser file
                    case p of
                      Left err -> error $ "parse error " ++ (show err)
                      Right val -> return val
       
-------------------------------------------------------------------------------

xmlVersion = "?xml version=\"1.0\"?"
-- xmiVersion = "xmi:version=\"2.1\" xmlns:xmi=\"http://schema.omg.org/spec/XMI/2.1\" xmlns:uml=\"http://www.eclipse.org/uml2/3.0.0/UML\""
xmiVersion = "xmi:XMI xmlns:mofext=\"http://www.omg.org/spec/MOF/20131001\" xmlns:uml=\"http://www.omg.org/spec/UML/20131001\" xmlns:xmi=\"http://www.omg.org/spec/XMI/20131001\""



quotes = between ( (symbol "\'")<|>(symbol "\"") ) ( (symbol "\'")<|>(symbol "\"") )
brs    = between (symbol "<" ) (symbol ">" )
brs1   = between (symbol "<" ) (symbol "/>")
brsEnd = between (symbol "</") (symbol ">" )


--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
                 
parser :: Parser Package
parser = do whiteSpace; v <- spec; eof; return v

xmlattrs =
  xmiId
  <|> fName
  
--packageElement :: Parser PackagedUmlPackage
--packageElement  =
    
importedPackage :: Parser Imports
importedPackage  =
    brs1 ( do
            reserved "importedPackage";
            aid <- xmiRef
            traceM ("imported package: " ++ show aid) 
            return (ImportRef aid)
        )

  
packageImport :: Parser Packaged -- ImportRef
packageImport  = do
  aid <- brs ( do
                 reserved "packageImport";
                 t <- xmiTy ; guard $ (t=="uml:PackageImport");
                 aid <- xmiId
                 traceM ("package import: " ++ show aid) 
                 return aid
             );
  traceM ("before list of imported: " ++ show aid) 
--  imports <- many importedPackage;
  imports <- importedPackage;
  traceM ("before end of package import: " ++ show aid) 
  brsEnd ( reserved "packageImport" );
  traceM ("end of package import: " ++ show aid) 
  return (Import aid [imports])

umlPackage :: Parser Package
umlPackage = do
  name <- brs ( do
            reserved "packagedElement";
            t <- xmiTy ; guard $ (t=="uml:Package");
              --return $ aid name
            name <- many xmlattrs
            traceM ("package element: " ++ show name) 
            return name
        )
  --es <- many $ ( packageImport )
  traceM ("expecting element next") 
  --es <- many element
  es <- element
  traceM ("got elements " ++ show es) 
  brsEnd ( reserved "packagedElement" )
  return $ Foo
      --Package aid name es

  
spec :: Parser Package
spec = do       brs (string xmlVersion)
                brs (string xmiVersion)
                pkg <- umlPackage
                return $ pkg


--xmi :: Parser String
-- reserved "uml:Package";
--xmi = do reserved xmiVersion;
--x <- fName; return x

subPackage :: Parser Packaged
subPackage = do
  u <- umlPackage
  return (SubPackage u )


element :: Parser Packaged
element = do
  --try (subPackage) -- nested package
    try (subPackage) -- nested package
      <|> try (packageImport) -- imported thing
      <|> try (eClass)
    --traceM ("element: " ++ show r)
    --r
    --case r of
    --  Left l -> $ (Fake)
    
                --traceM ("subpackage failed: " ++ show l)
        --return Fake
      --Right val -> do
      --  traceM ("subpackage OK: " ++ show val)
      --  val

    -- try (eEnum)
    


    -- <?> "packagedElement or something"



eClass :: Parser Packaged
eClass  =
  --try (clSimple0) <|>
  --try  (clComp)  <?> "class"
  clComp  <?> "class"

hdClass  = brs  (do
                    traceM ("class start ")
                    tagPack
                    t <- xmiTy
                    traceM ("hrd 1: " ++ show t)
                    guard $ (t=="uml:Class")
                    traceM ("hrd class: " ++ show t)
                    x <- xmlCl
                    traceM ("hrd name: " ++ show x)
                    return x
                )
hdClass1 = brs1 (do tagPack; t <- xmiTy; guard $ (t=="uml:Class"); x <- xmlCl; return x )

clSimple0:: Parser Packaged
clSimple0 =  do p  <- hdClass1; return (Class (idC p) (aC p) [])

clComp   :: Parser Packaged
clComp    =  do p  <- hdClass;
                traceM ("hrd class1: " ++ show p)
                --os <- many ownedClass;
                oc1 <- ownedClass;
                traceM ("owned class1: " ++ show oc1)
                let os = [oc1]
                brsEnd (tagPack); return (Class (idC p) (aC p) os)

eEnum :: Parser Packaged
eEnum     =  do p  <- brs (do tagPack; t <- xmiTy; guard $ (t=="uml:Enumeration"); 
                              x <- xmlEnum; return x)
                ls <- many $ (try (lit)); brsEnd (tagPack)
                return (Enumeration (idE p) ls)

lit :: Parser Literal
lit       =  do l <- brs1 (do reserved "ownedLiteral"; t <- xmiTy; 
                              guard $ (t=="uml:EnumerationLiteral"); 
                              n <- fName; return $ (Literal n));  return l
                
ownedClass :: Parser Owned
ownedClass = try (property) <|> try (general) <|> try (operation) <?> "owned class"


property :: Parser Owned
property =
     do p  <- brs (do   reserved "ownedAttribute"  ; t <- xmiTy; 
                        guard $ (t=="uml:Property"); x <- xmlProp; return x)
        t1 <- optionMaybe $ (try (typ));  p1 <- xmlProp1;  
        t <- do (if (tP p) == "" then return t1 else return $ (Just (RefType (tP p))))
        brsEnd (reserved "ownedAttribute")
        return (Property (nP p) (r p) t (upP p1) (lwP p1) (qP p1))


operation :: Parser Owned
operation =
     do id <- brs (do   reserved "ownedOperation"   ; t <- xmiTy; 
                        guard $ (t=="uml:Operation"); n <- fName; return n)
        ps <- many $ (param)
        brsEnd (reserved "ownedOperation")
        return (Operation id ps)

general :: Parser Owned
general =
     do gl <- brs1 (do  string "generalization "  ; t <- xmiTy; 
                        guard $ (t=="uml:Generalization");
                        g <- xmiId
                        --g <- fGrl;
                        return g )
        return (Generalization gl)


param :: Parser Parameter
param = try (ret) <|> try (inOut) <?> "parameter"

inOut :: Parser Parameter
inOut = 
     do io <- brs1 (do reserved "ownedParameter"; t <- xmiTy; guard $ (t=="uml:Parameter"); 
                       n <- fName; ty <- fTyp; return $ (InOut n (RefType ty) )  )
        return io

ret   :: Parser Parameter
ret   =  
     do ty <- brs  (do reserved "ownedParameter"; t <- xmiTy; guard $ (t=="uml:Parameter"); 
                       ty <- fTyp; d <- fDir; guard $ (d=="return"); return ty  );
        up <- optionMaybe $ (uppVal); lw <- optionMaybe $ (lowVal); 
        brsEnd (reserved "ownedParameter");     
        return $ (Return (RefType ty) up lw)


uppVal :: Parser Limit
uppVal = do up <- brs1 (do string "upperValue "; l <- limit; return $ l); return up 
                        
lowVal :: Parser Limit
lowVal = do lw <- brs1 (do string "lowerValue "; l <- limit; return $ l); return lw

limit  :: Parser Limit
limit  = try (lInteger) <|> lUnlimited <?> "upper or lower value" 

lUnlimited ::  Parser Limit
lUnlimited = 
         do t <- xmiTy ; guard $ (t=="uml:LiteralUnlimitedNatural"); 
            v <- fValue; return (LUnlimitedNatural v) 

lInteger  ::  Parser Limit
lInteger   = 
         do t <- xmiTy ; guard $ (t=="uml:LiteralInteger"); 
            v <- fValue; return (LInteger v) 

qual :: Parser Qualified
qual  =  do q <- brs1 (do string "qualifier "; x <- xmlQual; return x) 
            return $ (Qualified (nQ q) (RefType (tQ q))) 

typ ::  Parser Type
typ =   try (refType) <|> try (primitive) <?> "type"


refType ::  Parser Type
refType = do t <- brs1 (do string "type "; r <- xmiRef; return r); return $ (RefType t) 

primitive ::  Parser Type
primitive = 
          do t <- brs1 (do string "type "; ty <- xmiTy; 
                           guard $ (ty=="uml:PrimitiveType"); 
                           r <- fHref; return r)
             return (PrimitiveType t)


---------- XMI auxiliar functions -----------

tagPack = reserved "packagedElement"

  -- <|> reserved "ownedMember"


xmlns =  do     reserved "xmlns:xmi"<|> reserved "xmlns:uml"; 
                reservedOp "="; id <- quotes identifier; return id

xmiId :: Parser Name
xmiId =  do     reserved "xmi:id"   ; reservedOp "="; id <- quotes identifier; return id

xmiTy :: Parser Name
xmiTy =  do     reserved "xmi:type" ; reservedOp "="; id <- quotes identifier; return id

xmiRef :: Parser Name
xmiRef = do     reserved "xmi:idref"; reservedOp "="; id <- quotes identifier; return id

fName :: Parser Name
fName =  do     string "name" ; reservedOp "="; n <- quotes identifier; return n

fTyp  :: Parser Name
fTyp  =   do    string "type" ; reservedOp "="; n <- quotes identifier; return n

fValue :: Parser Value
fValue = do     string "value"; reservedOp "="; n <- quotes integer; return n

fHref :: Parser Name
fHref  = do     string "href" ; reservedOp "="; n <- quotes identifier; return n

fDir  :: Parser Name
fDir   = do     string "direction" ; reservedOp "="; n <- quotes identifier; return n

fGrl  :: Parser Name
fGrl   = do     string "general" ; reservedOp "="; n <- quotes identifier; return n

fAbs :: Parser  Abstract
fAbs  =  do     string "isAbstract"; reservedOp "="; n <- quotes identifier; 
                if n=="true" then return $ True else return $ False 

fRead :: Parser  ReadOnly
fRead  = do     string "isReadOnly"; reservedOp "="; n <- quotes identifier; 
                if n=="true" then return $ True else return $ False 


                
xmlCl :: Parser ClassTag
xmlCl = permute (ClassTag 
 <$$> (try $ xmiId) 
 <||> (try $ fName) 
 <|?> (False, (try $ fAbs))
 )

xmlEnum :: Parser EnumTag
xmlEnum = permute (EnumTag 
 <$$> (try $ xmiId) 
 <||> (try $ fName) 
 )


xmlProp :: Parser PropTag
xmlProp = permute (PropTag 
 <$$> (try $ fName)
 <|?> ("", (try $ xmiId))
 <|?> (False, (try $ fRead))
 <|?> (""   , (try $ refTy))
 )

xmlProp1 :: Parser Prop1Tag
xmlProp1 = permute (Prop1Tag 
 <$$> (try $ (optionMaybe $ (uppVal)))
 <||> (try $ (optionMaybe $ (lowVal)))
 <|?> ([], (many1 $ (try qual)))
 )


xmlQual :: Parser QualTag
xmlQual = permute (QualTag 
 <$$> (try $ fName) 
 <|?> ("", (try $ refTy))
 )


refTy :: Parser Name
refTy = do id <- fTyp; return id



