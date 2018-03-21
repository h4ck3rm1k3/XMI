{-# LANGUAGE DeriveDataTypeable #-}

module AST where

import Data.Generics

type Name   = String
type IdRef  = String
type Id  = String
type HRef   = String
type Assoc  = String
type Abstract = Bool
type ReadOnly = Bool
type Value    = Integer
type MemberEnd  = IdRef
type AssocEnd = Name
type General  = Name

data Package = Package Name Name [Packaged]
  | Foo
               deriving (Typeable, Data, Eq, Show)
data Imports =
  ImportRef Name
             deriving (Typeable, Data, Eq, Show)

data Packaged = Class Name Abstract [Owned] 
              | Enumeration Name [Literal]
              | SubPackage Package
              | Import Name [Imports]
                deriving (Typeable, Data, Eq, Show)

data Literal = Literal Name
               deriving (Typeable, Data, Eq, Show)

data Owned = Property Name ReadOnly (Maybe Type) UppVal LowVal [Qualified]
           | Generalization General
           | Operation Name [Parameter] 
             deriving (Typeable, Data, Eq, Show)

data Parameter = InOut Name Type | Return Type UppVal LowVal
                deriving (Typeable, Data, Eq, Show)

data OwnedEnd = OwnedEnd AssocEnd UppVal LowVal
           deriving (Typeable, Data, Eq, Show)

data Qualified = Qualified Name Type
                deriving (Typeable, Data, Eq, Show)
    
data Type = RefType IdRef | PrimitiveType HRef
            deriving (Typeable, Data, Eq, Show)

type UppVal = Maybe Limit

type LowVal = Maybe Limit

data Limit = LUnlimitedNatural Value  | LInteger Value
             deriving (Typeable, Data, Eq, Show)


