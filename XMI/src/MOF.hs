{-# LANGUAGE MultiParamTypeClasses #-}

module MOF where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types
import Text.XML.HaXml.OneOfN

xmlns = defaultA fromAttrToStr "" "xmlns"
possible_attr x = possibleA fromAttrToStr x 
def_attr x = definiteA fromAttrToStr x

xmi_spec = defaultA fromAttrToStr "http://www.omg.org/spec/XMI/20131001" "xmlns:xmi"
tag x =  def_attr "mofext:Tag" x 
{-Type decls-}

data Xmi'XMI = Xmi'XMI Xmi'XMI_Attrs PackagedElement
                       (List1 Mofext'Tag)
             deriving (Eq,Show)
data Xmi'XMI_Attrs = Xmi'XMI_Attrs
    { xmi'XMIXmlns'xmi :: (Defaultable String)
    } deriving (Eq,Show)
data Mofext'Tag = Mofext'Tag Mofext'Tag_Attrs XMIElement
                deriving (Eq,Show)
data Mofext'Tag_Attrs = Mofext'Tag_Attrs
    { mofext'TagXmlns'mofext :: (Defaultable String)
    , mofext'TagXmlns'xmi :: (Defaultable String)
    , mofext'TagName :: String
    , mofext'TagValue :: String
    , mofext'TagXmi'id :: String
    , mofext'TagXmi'type :: String
    } deriving (Eq,Show)
data XMIElement = XMIElement
    { elementXmlns :: (Defaultable String)
    , elementXmlns'xmi :: (Defaultable String)
    , elementXmi'idref :: String
    } deriving (Eq,Show)
data PackagedElement = PackagedElement PackagedElement_Attrs
                                       [(OneOf3 PackagedElement PackageImport PackageMerge)]
                                       [MemberEnd] (Maybe NavigableOwnedEnd) (Maybe OwnedEnd)
                                       (Maybe Generalization) [OwnedAttribute] [OwnedOperation]
                     deriving (Eq,Show)
data PackagedElement_Attrs = PackagedElement_Attrs
    { packagedElementXmlns :: (Defaultable String)
    , packagedElementXmlns'xmi :: (Defaultable String)
    , packagedElementURI :: (Maybe String)
    , packagedElementIsAbstract :: (Maybe String)
    , packagedElementName :: String
    , packagedElementXmi'id :: String
    , packagedElementXmi'type :: String
    } deriving (Eq,Show)
data PackageImport = PackageImport PackageImport_Attrs
                                   ImportedPackage
                   deriving (Eq,Show)
data PackageImport_Attrs = PackageImport_Attrs
    { packageImportXmlns :: (Defaultable String)
    , packageImportXmlns'xmi :: (Defaultable String)
    , packageImportXmi'id :: String
    , packageImportXmi'type :: String
    } deriving (Eq,Show)
data PackageMerge = PackageMerge PackageMerge_Attrs MergedPackage
                  deriving (Eq,Show)
data PackageMerge_Attrs = PackageMerge_Attrs
    { packageMergeXmlns :: (Defaultable String)
    , packageMergeXmlns'xmi :: (Defaultable String)
    , packageMergeXmi'id :: String
    , packageMergeXmi'type :: String
    } deriving (Eq,Show)
data MemberEnd = MemberEnd
    { memberEndXmlns :: (Defaultable String)
    , memberEndXmlns'xmi :: (Defaultable String)
    , memberEndXmi'idref :: String
    } deriving (Eq,Show)
data NavigableOwnedEnd = NavigableOwnedEnd
    { navigableOwnedEndXmlns :: (Defaultable String)
    , navigableOwnedEndXmlns'xmi :: (Defaultable String)
    , navigableOwnedEndXmi'idref :: String
    } deriving (Eq,Show)
data OwnedEnd = OwnedEnd OwnedEnd_Attrs Type Association
                         (Maybe SubsettedProperty) (Maybe (UpperValue,LowerValue))
              deriving (Eq,Show)
data OwnedEnd_Attrs = OwnedEnd_Attrs
    { ownedEndXmlns :: (Defaultable String)
    , ownedEndXmlns'xmi :: (Defaultable String)
    , ownedEndAggregation :: (Maybe String)
    , ownedEndName :: String
    , ownedEndVisibility :: String
    , ownedEndXmi'id :: String
    , ownedEndXmi'type :: String
    } deriving (Eq,Show)
data Generalization = Generalization Generalization_Attrs General
                    deriving (Eq,Show)
data Generalization_Attrs = Generalization_Attrs
    { generalizationXmlns :: (Defaultable String)
    , generalizationXmlns'xmi :: (Defaultable String)
    , generalizationXmi'id :: String
    , generalizationXmi'type :: String
    } deriving (Eq,Show)
data OwnedAttribute = OwnedAttribute OwnedAttribute_Attrs
                                     (List1 (OneOf2 Association Type))
                                     (Maybe (OneOf2 SubsettedProperty UpperValue))
                                     (Maybe LowerValue)
                    deriving (Eq,Show)
data OwnedAttribute_Attrs = OwnedAttribute_Attrs
    { ownedAttributeXmlns :: (Defaultable String)
    , ownedAttributeXmlns'xmi :: (Defaultable String)
    , ownedAttributeIsDerived :: (Maybe String)
    , ownedAttributeName :: String
    , ownedAttributeVisibility :: String
    , ownedAttributeXmi'id :: String
    , ownedAttributeXmi'type :: String
    } deriving (Eq,Show)
data OwnedOperation = OwnedOperation OwnedOperation_Attrs
                                     [OwnedParameter]
                    deriving (Eq,Show)
data OwnedOperation_Attrs = OwnedOperation_Attrs
    { ownedOperationXmlns :: (Defaultable String)
    , ownedOperationXmlns'xmi :: (Defaultable String)
    , ownedOperationIsQuery :: (Maybe String)
    , ownedOperationName :: String
    , ownedOperationVisibility :: String
    , ownedOperationXmi'id :: String
    , ownedOperationXmi'type :: String
    } deriving (Eq,Show)
data ImportedPackage = ImportedPackage
    { importedPackageXmlns :: (Defaultable String)
    , importedPackageXmlns'xmi :: (Defaultable String)
    , importedPackageHref :: (Maybe String)
    , importedPackageXmi'idref :: (Maybe String)
    } deriving (Eq,Show)
data MergedPackage = MergedPackage
    { mergedPackageXmlns :: (Defaultable String)
    , mergedPackageXmlns'xmi :: (Defaultable String)
    , mergedPackageHref :: (Maybe String)
    , mergedPackageXmi'idref :: (Maybe String)
    } deriving (Eq,Show)
data General = General
    { generalXmlns :: (Defaultable String)
    , generalXmlns'xmi :: (Defaultable String)
    , generalHref :: (Maybe String)
    , generalXmi'idref :: (Maybe String)
    } deriving (Eq,Show)
data OwnedParameter = OwnedParameter OwnedParameter_Attrs Type
                                     (Maybe UpperValue) (Maybe LowerValue)
                    deriving (Eq,Show)
data OwnedParameter_Attrs = OwnedParameter_Attrs
    { ownedParameterXmlns :: (Defaultable String)
    , ownedParameterXmlns'xmi :: (Defaultable String)
    , ownedParameterDirection :: (Maybe String)
    , ownedParameterIsStream :: (Maybe String)
    , ownedParameterName :: String
    , ownedParameterVisibility :: String
    , ownedParameterXmi'id :: String
    , ownedParameterXmi'type :: String
    } deriving (Eq,Show)
data Type = Type
    { typeXmlns :: (Defaultable String)
    , typeXmlns'xmi :: (Defaultable String)
    , typeHref :: (Maybe String)
    , typeXmi'idref :: (Maybe String)
    } deriving (Eq,Show)
data Association = Association
    { associationXmlns :: (Defaultable String)
    , associationXmlns'xmi :: (Defaultable String)
    , associationXmi'idref :: String
    } deriving (Eq,Show)
data SubsettedProperty = SubsettedProperty
    { subsettedPropertyXmlns :: (Defaultable String)
    , subsettedPropertyHref :: String
    } deriving (Eq,Show)
data UpperValue = UpperValue
    { upperValueXmlns :: (Defaultable String)
    , upperValueXmlns'xmi :: (Defaultable String)
    , upperValueValue :: String
    , upperValueXmi'id :: String
    , upperValueXmi'type :: String
    } deriving (Eq,Show)
data LowerValue = LowerValue
    { lowerValueXmlns :: (Defaultable String)
    , lowerValueXmlns'xmi :: (Defaultable String)
    , lowerValueValue :: (Maybe String)
    , lowerValueXmi'id :: String
    , lowerValueXmi'type :: String
    } deriving (Eq,Show)

{-Instance decls-}

instance HTypeable Xmi'XMI where
    toHType x = Defined "xmi:XMI" [] []
instance XmlContent Xmi'XMI where
    toContents (Xmi'XMI as a b) =
        [CElem (Elem (N "xmi:XMI") (toAttrs as) (toContents a ++
                                                 toContents b)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["xmi:XMI"]
        ; interior e $ return (Xmi'XMI (fromAttrs as))
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <xmi:XMI>, "++)
instance XmlAttributes Xmi'XMI_Attrs where
    fromAttrs as =
        Xmi'XMI_Attrs
          { xmi'XMIXmlns'xmi = xmi_spec as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns:xmi" (xmi'XMIXmlns'xmi v)
        ]

instance HTypeable Mofext'Tag where
    toHType x = Defined "mofext:Tag" [] []
instance XmlContent Mofext'Tag where
    toContents (Mofext'Tag as a) =
        [CElem (Elem (N "mofext:Tag") (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["mofext:Tag"]
        ; interior e $ return (Mofext'Tag (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <mofext:Tag>, "++)
instance XmlAttributes Mofext'Tag_Attrs where
    fromAttrs as =
        Mofext'Tag_Attrs
          { mofext'TagXmlns'mofext = defaultA fromAttrToStr "http://www.omg.org/spec/MOF/20131001" "xmlns:mofext" as
          , mofext'TagXmlns'xmi = xmi_spec as
          , mofext'TagName = tag "name" as
          , mofext'TagValue = tag "value" as
          , mofext'TagXmi'id = tag "xmi:id" as
          , mofext'TagXmi'type = tag "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns:mofext" (mofext'TagXmlns'mofext v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (mofext'TagXmlns'xmi v)
        , toAttrFrStr "name" (mofext'TagName v)
        , toAttrFrStr "value" (mofext'TagValue v)
        , toAttrFrStr "xmi:id" (mofext'TagXmi'id v)
        , toAttrFrStr "xmi:type" (mofext'TagXmi'type v)
        ]

instance HTypeable XMIElement where
    toHType x = Defined "element" [] []
instance XmlContent XMIElement where
    toContents as =
        [CElem (Elem (N "element") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["element"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <element>, "++)
instance XmlAttributes XMIElement where
    fromAttrs as =
        XMIElement
          { elementXmlns = xmlns as
          , elementXmlns'xmi = xmi_spec as
          , elementXmi'idref = def_attr "element" "xmi:idref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (elementXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (elementXmlns'xmi v)
        , toAttrFrStr "xmi:idref" (elementXmi'idref v)
        ]

instance HTypeable PackagedElement where
    toHType x = Defined "packagedElement" [] []
instance XmlContent PackagedElement where
    toContents (PackagedElement as a b c d e f g) =
        [CElem (Elem (N "packagedElement") (toAttrs as) (concatMap toContents a
                                                         ++ concatMap toContents b ++
                                                         maybe [] toContents c ++
                                                         maybe [] toContents d ++
                                                         maybe [] toContents e ++
                                                         concatMap toContents f ++
                                                         concatMap toContents g)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["packagedElement"]
        ; interior e $ return (PackagedElement (fromAttrs as))
                       `apply` many parseContents `apply` many parseContents
                       `apply` optional parseContents `apply` optional parseContents
                       `apply` optional parseContents `apply` many parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <packagedElement>, "++)
instance XmlAttributes PackagedElement_Attrs where
    fromAttrs as =
        PackagedElement_Attrs
          { packagedElementXmlns = xmlns as
          , packagedElementXmlns'xmi = xmi_spec as
          , packagedElementURI = possible_attr "URI" as
          , packagedElementIsAbstract = possible_attr "isAbstract" as
          , packagedElementName = def_attr "packagedElement" "name" as
          , packagedElementXmi'id = def_attr "packagedElement" "xmi:id" as
          , packagedElementXmi'type = def_attr "packagedElement" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (packagedElementXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (packagedElementXmlns'xmi v)
        , maybeToAttr toAttrFrStr "URI" (packagedElementURI v)
        , maybeToAttr toAttrFrStr "isAbstract" (packagedElementIsAbstract v)
        , toAttrFrStr "name" (packagedElementName v)
        , toAttrFrStr "xmi:id" (packagedElementXmi'id v)
        , toAttrFrStr "xmi:type" (packagedElementXmi'type v)
        ]

instance HTypeable PackageImport where
    toHType x = Defined "packageImport" [] []
instance XmlContent PackageImport where
    toContents (PackageImport as a) =
        [CElem (Elem (N "packageImport") (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["packageImport"]
        ; interior e $ return (PackageImport (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <packageImport>, "++)
instance XmlAttributes PackageImport_Attrs where
    fromAttrs as =
        PackageImport_Attrs
          { packageImportXmlns = xmlns as
          , packageImportXmlns'xmi = xmi_spec as
          , packageImportXmi'id = def_attr "packageImport" "xmi:id" as
          , packageImportXmi'type = def_attr "packageImport" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (packageImportXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (packageImportXmlns'xmi v)
        , toAttrFrStr "xmi:id" (packageImportXmi'id v)
        , toAttrFrStr "xmi:type" (packageImportXmi'type v)
        ]

instance HTypeable PackageMerge where
    toHType x = Defined "packageMerge" [] []
instance XmlContent PackageMerge where
    toContents (PackageMerge as a) =
        [CElem (Elem (N "packageMerge") (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["packageMerge"]
        ; interior e $ return (PackageMerge (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <packageMerge>, "++)
instance XmlAttributes PackageMerge_Attrs where
    fromAttrs as =
        PackageMerge_Attrs
          { packageMergeXmlns = xmlns as
          , packageMergeXmlns'xmi = xmi_spec as
          , packageMergeXmi'id = def_attr "packageMerge" "xmi:id" as
          , packageMergeXmi'type = def_attr "packageMerge" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (packageMergeXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (packageMergeXmlns'xmi v)
        , toAttrFrStr "xmi:id" (packageMergeXmi'id v)
        , toAttrFrStr "xmi:type" (packageMergeXmi'type v)
        ]

instance HTypeable MemberEnd where
    toHType x = Defined "memberEnd" [] []
instance XmlContent MemberEnd where
    toContents as =
        [CElem (Elem (N "memberEnd") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["memberEnd"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <memberEnd>, "++)
instance XmlAttributes MemberEnd where
    fromAttrs as =
        MemberEnd
          { memberEndXmlns = xmlns as
          , memberEndXmlns'xmi = xmi_spec as
          , memberEndXmi'idref = def_attr "memberEnd" "xmi:idref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (memberEndXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (memberEndXmlns'xmi v)
        , toAttrFrStr "xmi:idref" (memberEndXmi'idref v)
        ]

instance HTypeable NavigableOwnedEnd where
    toHType x = Defined "navigableOwnedEnd" [] []
instance XmlContent NavigableOwnedEnd where
    toContents as =
        [CElem (Elem (N "navigableOwnedEnd") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["navigableOwnedEnd"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <navigableOwnedEnd>, "++)
instance XmlAttributes NavigableOwnedEnd where
    fromAttrs as =
        NavigableOwnedEnd
          { navigableOwnedEndXmlns = xmlns as
          , navigableOwnedEndXmlns'xmi = xmi_spec as
          , navigableOwnedEndXmi'idref = def_attr "navigableOwnedEnd" "xmi:idref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (navigableOwnedEndXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (navigableOwnedEndXmlns'xmi v)
        , toAttrFrStr "xmi:idref" (navigableOwnedEndXmi'idref v)
        ]

instance HTypeable OwnedEnd where
    toHType x = Defined "ownedEnd" [] []
instance XmlContent OwnedEnd where
    toContents (OwnedEnd as a b c d) =
        [CElem (Elem (N "ownedEnd") (toAttrs as) (toContents a ++
                                                  toContents b ++ maybe [] toContents c ++
                                                  maybe [] toContents d)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["ownedEnd"]
        ; interior e $ return (OwnedEnd (fromAttrs as))
                       `apply` parseContents `apply` parseContents
                       `apply` optional parseContents `apply` optional parseContents
        } `adjustErr` ("in <ownedEnd>, "++)
instance XmlAttributes OwnedEnd_Attrs where
    fromAttrs as =
        OwnedEnd_Attrs
          { ownedEndXmlns = xmlns as
          , ownedEndXmlns'xmi = xmi_spec as
          , ownedEndAggregation = possible_attr "aggregation" as
          , ownedEndName = def_attr "ownedEnd" "name" as
          , ownedEndVisibility = def_attr "ownedEnd" "visibility" as
          , ownedEndXmi'id = def_attr "ownedEnd" "xmi:id" as
          , ownedEndXmi'type = def_attr "ownedEnd" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (ownedEndXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (ownedEndXmlns'xmi v)
        , maybeToAttr toAttrFrStr "aggregation" (ownedEndAggregation v)
        , toAttrFrStr "name" (ownedEndName v)
        , toAttrFrStr "visibility" (ownedEndVisibility v)
        , toAttrFrStr "xmi:id" (ownedEndXmi'id v)
        , toAttrFrStr "xmi:type" (ownedEndXmi'type v)
        ]

instance HTypeable Generalization where
    toHType x = Defined "generalization" [] []
instance XmlContent Generalization where
    toContents (Generalization as a) =
        [CElem (Elem (N "generalization") (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["generalization"]
        ; interior e $ return (Generalization (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <generalization>, "++)
instance XmlAttributes Generalization_Attrs where
    fromAttrs as =
        Generalization_Attrs
          { generalizationXmlns = xmlns as
          , generalizationXmlns'xmi = xmi_spec as
          , generalizationXmi'id = def_attr "generalization" "xmi:id" as
          , generalizationXmi'type = def_attr "generalization" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (generalizationXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (generalizationXmlns'xmi v)
        , toAttrFrStr "xmi:id" (generalizationXmi'id v)
        , toAttrFrStr "xmi:type" (generalizationXmi'type v)
        ]

instance HTypeable OwnedAttribute where
    toHType x = Defined "ownedAttribute" [] []
instance XmlContent OwnedAttribute where
    toContents (OwnedAttribute as a b c) =
        [CElem (Elem (N "ownedAttribute") (toAttrs as) (toContents a ++
                                                        maybe [] toContents b ++
                                                        maybe [] toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["ownedAttribute"]
        ; interior e $ return (OwnedAttribute (fromAttrs as))
                       `apply` parseContents `apply` optional parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <ownedAttribute>, "++)
instance XmlAttributes OwnedAttribute_Attrs where
    fromAttrs as =
        OwnedAttribute_Attrs
          { ownedAttributeXmlns = xmlns as
          , ownedAttributeXmlns'xmi = xmi_spec as
          , ownedAttributeIsDerived = possible_attr "isDerived" as
          , ownedAttributeName = def_attr "ownedAttribute" "name" as
          , ownedAttributeVisibility = def_attr "ownedAttribute" "visibility" as
          , ownedAttributeXmi'id = def_attr "ownedAttribute" "xmi:id" as
          , ownedAttributeXmi'type = def_attr "ownedAttribute" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (ownedAttributeXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (ownedAttributeXmlns'xmi v)
        , maybeToAttr toAttrFrStr "isDerived" (ownedAttributeIsDerived v)
        , toAttrFrStr "name" (ownedAttributeName v)
        , toAttrFrStr "visibility" (ownedAttributeVisibility v)
        , toAttrFrStr "xmi:id" (ownedAttributeXmi'id v)
        , toAttrFrStr "xmi:type" (ownedAttributeXmi'type v)
        ]

instance HTypeable OwnedOperation where
    toHType x = Defined "ownedOperation" [] []
instance XmlContent OwnedOperation where
    toContents (OwnedOperation as a) =
        [CElem (Elem (N "ownedOperation") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["ownedOperation"]
        ; interior e $ return (OwnedOperation (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <ownedOperation>, "++)
instance XmlAttributes OwnedOperation_Attrs where
    fromAttrs as =
        OwnedOperation_Attrs
          { ownedOperationXmlns = xmlns as
          , ownedOperationXmlns'xmi = xmi_spec as
          , ownedOperationIsQuery = possible_attr "isQuery" as
          , ownedOperationName = def_attr "ownedOperation" "name" as
          , ownedOperationVisibility = def_attr "ownedOperation" "visibility" as
          , ownedOperationXmi'id = def_attr "ownedOperation" "xmi:id" as
          , ownedOperationXmi'type = def_attr "ownedOperation" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (ownedOperationXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (ownedOperationXmlns'xmi v)
        , maybeToAttr toAttrFrStr "isQuery" (ownedOperationIsQuery v)
        , toAttrFrStr "name" (ownedOperationName v)
        , toAttrFrStr "visibility" (ownedOperationVisibility v)
        , toAttrFrStr "xmi:id" (ownedOperationXmi'id v)
        , toAttrFrStr "xmi:type" (ownedOperationXmi'type v)
        ]

instance HTypeable ImportedPackage where
    toHType x = Defined "importedPackage" [] []
instance XmlContent ImportedPackage where
    toContents as =
        [CElem (Elem (N "importedPackage") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["importedPackage"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <importedPackage>, "++)
instance XmlAttributes ImportedPackage where
    fromAttrs as =
        ImportedPackage
          { importedPackageXmlns = xmlns as
          , importedPackageXmlns'xmi = xmi_spec as
          , importedPackageHref = possible_attr "href" as
          , importedPackageXmi'idref = possible_attr "xmi:idref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (importedPackageXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (importedPackageXmlns'xmi v)
        , maybeToAttr toAttrFrStr "href" (importedPackageHref v)
        , maybeToAttr toAttrFrStr "xmi:idref" (importedPackageXmi'idref v)
        ]

instance HTypeable MergedPackage where
    toHType x = Defined "mergedPackage" [] []
instance XmlContent MergedPackage where
    toContents as =
        [CElem (Elem (N "mergedPackage") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["mergedPackage"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <mergedPackage>, "++)
instance XmlAttributes MergedPackage where
    fromAttrs as =
        MergedPackage
          { mergedPackageXmlns = xmlns as
          , mergedPackageXmlns'xmi = xmi_spec as
          , mergedPackageHref = possible_attr "href" as
          , mergedPackageXmi'idref = possible_attr "xmi:idref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (mergedPackageXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (mergedPackageXmlns'xmi v)
        , maybeToAttr toAttrFrStr "href" (mergedPackageHref v)
        , maybeToAttr toAttrFrStr "xmi:idref" (mergedPackageXmi'idref v)
        ]

instance HTypeable General where
    toHType x = Defined "general" [] []
instance XmlContent General where
    toContents as =
        [CElem (Elem (N "general") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["general"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <general>, "++)
instance XmlAttributes General where
    fromAttrs as =
        General
          { generalXmlns = xmlns as
          , generalXmlns'xmi = xmi_spec as
          , generalHref = possible_attr "href" as
          , generalXmi'idref = possible_attr "xmi:idref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (generalXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (generalXmlns'xmi v)
        , maybeToAttr toAttrFrStr "href" (generalHref v)
        , maybeToAttr toAttrFrStr "xmi:idref" (generalXmi'idref v)
        ]

instance HTypeable OwnedParameter where
    toHType x = Defined "ownedParameter" [] []
instance XmlContent OwnedParameter where
    toContents (OwnedParameter as a b c) =
        [CElem (Elem (N "ownedParameter") (toAttrs as) (toContents a ++
                                                        maybe [] toContents b ++
                                                        maybe [] toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["ownedParameter"]
        ; interior e $ return (OwnedParameter (fromAttrs as))
                       `apply` parseContents `apply` optional parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <ownedParameter>, "++)
instance XmlAttributes OwnedParameter_Attrs where
    fromAttrs as =
        OwnedParameter_Attrs
          { ownedParameterXmlns = xmlns as
          , ownedParameterXmlns'xmi = xmi_spec as
          , ownedParameterDirection = possible_attr "direction" as
          , ownedParameterIsStream = possible_attr "isStream" as
          , ownedParameterName = def_attr "ownedParameter" "name" as
          , ownedParameterVisibility = def_attr "ownedParameter" "visibility" as
          , ownedParameterXmi'id = def_attr "ownedParameter" "xmi:id" as
          , ownedParameterXmi'type = def_attr "ownedParameter" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (ownedParameterXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (ownedParameterXmlns'xmi v)
        , maybeToAttr toAttrFrStr "direction" (ownedParameterDirection v)
        , maybeToAttr toAttrFrStr "isStream" (ownedParameterIsStream v)
        , toAttrFrStr "name" (ownedParameterName v)
        , toAttrFrStr "visibility" (ownedParameterVisibility v)
        , toAttrFrStr "xmi:id" (ownedParameterXmi'id v)
        , toAttrFrStr "xmi:type" (ownedParameterXmi'type v)
        ]

instance HTypeable Type where
    toHType x = Defined "type" [] []
instance XmlContent Type where
    toContents as =
        [CElem (Elem (N "type") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["type"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <type>, "++)
instance XmlAttributes Type where
    fromAttrs as =
        Type
          { typeXmlns = xmlns as
          , typeXmlns'xmi = xmi_spec as
          , typeHref = possible_attr "href" as
          , typeXmi'idref = possible_attr "xmi:idref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (typeXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (typeXmlns'xmi v)
        , maybeToAttr toAttrFrStr "href" (typeHref v)
        , maybeToAttr toAttrFrStr "xmi:idref" (typeXmi'idref v)
        ]

instance HTypeable Association where
    toHType x = Defined "association" [] []
instance XmlContent Association where
    toContents as =
        [CElem (Elem (N "association") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["association"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <association>, "++)
instance XmlAttributes Association where
    fromAttrs as =
        Association
          { associationXmlns = xmlns as
          , associationXmlns'xmi = xmi_spec as
          , associationXmi'idref = def_attr "association" "xmi:idref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (associationXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (associationXmlns'xmi v)
        , toAttrFrStr "xmi:idref" (associationXmi'idref v)
        ]

instance HTypeable SubsettedProperty where
    toHType x = Defined "subsettedProperty" [] []
instance XmlContent SubsettedProperty where
    toContents as =
        [CElem (Elem (N "subsettedProperty") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["subsettedProperty"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <subsettedProperty>, "++)
instance XmlAttributes SubsettedProperty where
    fromAttrs as =
        SubsettedProperty
          { subsettedPropertyXmlns = xmlns as
          , subsettedPropertyHref = def_attr "subsettedProperty" "href" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (subsettedPropertyXmlns v)
        , toAttrFrStr "href" (subsettedPropertyHref v)
        ]

instance HTypeable UpperValue where
    toHType x = Defined "upperValue" [] []
instance XmlContent UpperValue where
    toContents as =
        [CElem (Elem (N "upperValue") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["upperValue"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <upperValue>, "++)
instance XmlAttributes UpperValue where
    fromAttrs as =
        UpperValue
          { upperValueXmlns = xmlns as
          , upperValueXmlns'xmi = xmi_spec as
          , upperValueValue = def_attr "upperValue" "value" as
          , upperValueXmi'id = def_attr "upperValue" "xmi:id" as
          , upperValueXmi'type = def_attr "upperValue" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (upperValueXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (upperValueXmlns'xmi v)
        , toAttrFrStr "value" (upperValueValue v)
        , toAttrFrStr "xmi:id" (upperValueXmi'id v)
        , toAttrFrStr "xmi:type" (upperValueXmi'type v)
        ]

instance HTypeable LowerValue where
    toHType x = Defined "lowerValue" [] []
instance XmlContent LowerValue where
    toContents as =
        [CElem (Elem (N "lowerValue") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["lowerValue"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <lowerValue>, "++)
instance XmlAttributes LowerValue where
    fromAttrs as =
        LowerValue
          { lowerValueXmlns = xmlns as
          , lowerValueXmlns'xmi = xmi_spec as
          , lowerValueValue = possible_attr "value" as
          , lowerValueXmi'id = def_attr "lowerValue" "xmi:id" as
          , lowerValueXmi'type = def_attr "lowerValue" "xmi:type" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (lowerValueXmlns v)
        , defaultToAttr toAttrFrStr "xmlns:xmi" (lowerValueXmlns'xmi v)
        , maybeToAttr toAttrFrStr "value" (lowerValueValue v)
        , toAttrFrStr "xmi:id" (lowerValueXmi'id v)
        , toAttrFrStr "xmi:type" (lowerValueXmi'type v)
        ]



{-Done-}
