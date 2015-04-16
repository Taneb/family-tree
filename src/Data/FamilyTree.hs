{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
{-|
Maintainer  :  nvd124@gmail.com
Stability   :  unstable
Portability :  portable

This module is for Family Trees.

It uses lenses for the manipulation of people. For the usage of lenses, see
"Control.Lens"

It is reccomended to use "Data.Binary" to do saving and loading.
-}
module Data.FamilyTree
 (
 -- * Types
 -- ** Main types
 Person(..),
 name,
 attributes,
 birthdate,
 birthplace,
 deathdate,
 deathplace,
 Family(..),
 head1,
 head2,
 children,
 relationFrom,
 relationTo,
 relationship,
 FamilyTree(..),
 treeName,
 people,
 families,
 -- ** Other types
 PartialDate,
 Location(..),
 Relationship(..),
 -- * Functions
 -- ** Creation
 newTree,
 addPerson,
 addFamily,
 -- ** Manipulation
 FamilyTreePart (..),
 -- ** Destruction
 deletePerson,
 deleteFamily,
 -- * Utility functions
 partialDateFromYear,
 partialDateFromMonth,
 partialDateFromDay
) where

import Control.Applicative (Applicative(..), (<$>), Alternative((<|>)))
import Control.Lens hiding (children, (...))

import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Function (on)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid (Monoid(..), First(..))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Table
import Data.String
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (Day(..), fromGregorian, gregorianMonthLength)

import GHC.Generics (Generic)

import Numeric.Interval (Interval, (...))
import qualified Numeric.Interval as I

-- | The Location type. Either a coordinate or a placename.  
data Location = Coord Double Double | PlaceName Text deriving (Eq, Show, Generic)

instance IsString Location where
  fromString = PlaceName . fromString
  
-- | The Relationship type. Marriage is the default for similarity to GEDCOM.
data Relationship = Marriage | Other Text deriving (Eq, Show, Generic)

type PartialDate = Interval Day

partialDateFromYear :: Integer -> PartialDate
partialDateFromYear n = fromGregorian n 1 1 ... fromGregorian n 12 31

partialDateFromMonth :: Integer -> Int -> PartialDate
partialDateFromMonth y m = (...) (fromGregorian y m 1) . 
    fromGregorian y m $ gregorianMonthLength y m

partialDateFromDay :: Integer -> Int -> Int -> PartialDate
partialDateFromDay y m d = I.singleton $ fromGregorian y m d

-- | The basic type for a person. 'Nothing' meaning unknown (or otherwise 
-- non-existent, for intance a death date for someone still alive) is a
-- convention used throughout this library.
data Person = Person
  {_personId :: Int
  ,_name :: Maybe Text
  ,_birthdate :: Maybe PartialDate
  ,_birthplace :: Maybe Location
  ,_deathdate :: Maybe PartialDate
  ,_deathplace :: Maybe Location
  ,_attributes :: HashMap Text Text
  } deriving (Eq, Show, Generic)
             
makeLenses ''Person

instance Tabular Person where
  type PKT Person = Int
  data Tab Person i = PersonTab (i Primary Int) (i SupplementalHash (Maybe Text)) (i Supplemental (Maybe PartialDate))
                      (i SupplementalHash (Maybe Location)) (i Supplemental (Maybe PartialDate)) 
                      (i SupplementalHash (Maybe Location))
  data Key k Person t where
    PersonID :: Key Primary Person Int
    Name :: Key SupplementalHash Person (Maybe Text)
    BirthDate :: Key Supplemental Person (Maybe PartialDate)
    BirthPlace :: Key SupplementalHash Person (Maybe Location)
    DeathDate :: Key Supplemental Person (Maybe PartialDate)
    DeathPlace :: Key SupplementalHash Person (Maybe Location)
  fetch PersonID = _personId
  fetch Name = _name
  fetch BirthDate = _birthdate
  fetch BirthPlace = _birthplace
  fetch DeathDate = _deathdate
  fetch DeathPlace = _deathplace
  primary = PersonID
  primarily PersonID r = r
  mkTab f = PersonTab <$> f PersonID <*> f Name <*> f BirthDate <*> f BirthPlace
            <*> f DeathDate <*> f DeathPlace  
  ixTab (PersonTab i n bd bp dd dp) k = case k of
    PersonID -> i
    Name -> n
    BirthDate -> bd
    BirthPlace -> bp
    DeathDate -> dd
    DeathPlace -> dp
  forTab (PersonTab i n bd bp dd dp) f =
    PersonTab <$> f PersonID i <*> f Name n <*> f BirthDate bd <*> f BirthPlace bp
    <*> f DeathDate dd <*> f DeathPlace dp
  autoTab = autoIncrement personId

-- | The basic type for a family. Which person is head1 and which is head2 is
-- arbitrary, but try to use a consistent rule
data Family = Family
  {_familyId :: Int
  ,_head1 :: Maybe Int
  ,_head2 :: Maybe Int
  ,_relationship :: Maybe Relationship
  ,_relationFrom :: Maybe PartialDate
  ,_relationTo :: Maybe PartialDate
  ,_children :: IntSet
  } deriving (Eq, Show, Generic)
  
makeLenses ''Family

instance Tabular Family where
  type PKT Family = Int
  data Tab Family i = FamilyTab (i Primary Int) (i Supplemental (Maybe Int)) (i Supplemental (Maybe Int))
                      (i SupplementalHash (Maybe Relationship)) (i Supplemental (Maybe PartialDate))
                      (i Supplemental (Maybe PartialDate)) (i InvertedInt (IntSet))
  data Key k Family t where
    FamilyID :: Key Primary Family Int
    Head1 :: Key Supplemental Family (Maybe Int)
    Head2 :: Key Supplemental Family (Maybe Int)
    Relationship :: Key SupplementalHash Family (Maybe Relationship)
    RelationFrom :: Key Supplemental Family (Maybe PartialDate)
    RelationTo :: Key Supplemental Family (Maybe PartialDate)
    Children :: Key InvertedInt Family IntSet
  fetch FamilyID = _familyId
  fetch Head1 = _head1
  fetch Head2 = _head2
  fetch Relationship = _relationship
  fetch RelationFrom = _relationFrom
  fetch RelationTo = _relationTo
  fetch Children = _children
  primary = FamilyID
  primarily FamilyID r = r
  mkTab f = FamilyTab <$> f FamilyID <*> f Head1 <*> f Head2 <*> f Relationship
            <*> f RelationFrom <*> f RelationTo <*> f Children
  ixTab (FamilyTab i h1 h2 r rf rt c) k = case k of
    FamilyID -> i
    Head1 -> h1
    Head2 -> h2
    Relationship -> r
    RelationFrom -> rf
    RelationTo -> rt
    Children -> c
  forTab (FamilyTab i h1 h2 r rf rt c) f =
    FamilyTab <$> f FamilyID i <*> f Head1 h1 <*> f Head2 h2 <*> f Relationship r
    <*> f RelationFrom rf <*> f RelationTo rt <*> f Children c
  autoTab = autoIncrement familyId

-- | The core structure of a family tree.
data FamilyTree = FamilyTree
  {_treeName :: Text
  ,_people :: Table Person
  ,_families :: Table Family
  } deriving (Eq, Show, Generic)
  
makeLenses ''FamilyTree

instance Monoid Person where
  mempty = Person {
    _personId = 0,
    _name = Nothing,
    _birthdate = Nothing,
    _birthplace = Nothing,
    _deathdate = Nothing,
    _deathplace = Nothing,
    _attributes = HM.empty
    }
  p1 `mappend` p2 = Person {
    _personId = _personId p1,
    _name = ((<|>) `on` _name) p1 p2,
    _birthdate = ((<|>) `on` _birthdate) p1 p2,
    _birthplace = ((<|>) `on` _birthplace) p1 p2,
    _deathdate = ((<|>) `on` _deathdate) p1 p2,
    _deathplace = ((<|>) `on` _deathplace) p1 p2,
    _attributes = (HM.union `on` _attributes) p1 p2
    }

instance Monoid Family where
  mempty = Family {
    _familyId = 0,
    _head1 = Nothing,
    _head2 = Nothing,
    _relationship = Nothing,
    _relationFrom = Nothing,
    _relationTo = Nothing,
    _children = IS.empty
    }
  f1 `mappend` f2 = Family {
    _familyId = _familyId f1,
    _head1 = getFirst $ (mappend `on` First . _head1) f1 f2,
    _head2 = getFirst $ (mappend `on` First . _head2) f1 f2,
    _relationship = getFirst $ (mappend `on` First . _relationship) f1 f2,
    _relationFrom = getFirst $ (mappend `on` First . _relationFrom) f1 f2,
    _relationTo = getFirst $ (mappend `on` First . _relationTo) f1 f2,
    _children = (IS.union `on` _children) f1 f2
    }

instance Binary Location where
  put (Coord x y) = do
    putWord8 0
    put x
    put y
  put (PlaceName t) = do
    putWord8 1
    put (encodeUtf8 t)
  get = do
    tag <- getWord8
    case tag of
      0 -> do
        x <- get
        y <- get
        return $ Coord x y
      _ -> PlaceName . decodeUtf8 <$> get

instance Hashable Location
  
instance Binary Relationship where
  put (Other t) = do
    putWord8 0
    put (encodeUtf8 t)
  put Marriage = putWord8 1
  get = do
    tag <- getWord8
    case tag of
      0 -> Other . decodeUtf8 <$> get
      _ -> return Marriage

instance Hashable Relationship

instance Binary Person where
  put person = do
    put (_personId person)
    put (encodeUtf8 <$> _name person)
    put (toModifiedJulianDay . I.inf <$> _birthdate person)
    put (toModifiedJulianDay . I.sup <$> _birthdate person)
    put (_birthplace person)
    put (toModifiedJulianDay . I.inf <$> _deathdate person)
    put (toModifiedJulianDay . I.sup <$> _deathdate person)
    put (_deathplace person)
    put (map (both %~ encodeUtf8) . HM.toList $ _attributes person)
  get = do
    i <- get
    n <- get
    bdi <- get
    bds <- get
    bp <- get
    ddi <- get
    dds <- get
    dp <- get
    a <- get
    return Person
      {_personId = i
      ,_name = fmap decodeUtf8 n
      ,_birthdate = (...) <$> fmap ModifiedJulianDay bdi <*>
        fmap ModifiedJulianDay bds
      ,_birthplace = bp
      ,_deathdate = (...) <$> fmap ModifiedJulianDay ddi <*>
        fmap ModifiedJulianDay dds
      ,_deathplace = dp
      ,_attributes = HM.fromList $ map (both %~ decodeUtf8) a
      }

instance Binary Family where
  put fam = do
    put $ _familyId fam
    put $ _head1 fam
    put $ _head2 fam
    put $ _relationship fam
    put $ toModifiedJulianDay . I.inf <$> _relationFrom fam
    put $ toModifiedJulianDay . I.sup <$> _relationFrom fam
    put $ toModifiedJulianDay . I.inf <$> _relationTo fam
    put $ toModifiedJulianDay . I.sup <$> _relationTo fam
    put $ _children fam
  get = do
    f <- get
    h1 <- get
    h2 <- get
    r <- get
    rfi <- get
    rfs <- get
    rti <- get
    rts <- get
    c <- get
    return Family
      {_familyId = f
      ,_head1 = h1
      ,_head2 = h2
      ,_relationship = r
      ,_relationFrom = (...) <$> fmap ModifiedJulianDay rfi <*> fmap ModifiedJulianDay rfs
      ,_relationTo = (...) <$> fmap ModifiedJulianDay rti <*> fmap ModifiedJulianDay rts
      ,_children = c
      }
      
instance Binary FamilyTree where
  put tree = do
    put $ encodeUtf8 $ _treeName tree
    put $ _people tree
    put $ _families tree
  get = do
    n <- get
    p <- get
    f <- get
    return FamilyTree
      {_treeName = decodeUtf8 n
      ,_people = p
      ,_families = f
      }

class FamilyTreePart part where
  -- | 'accessFT' is a 'Traversal' to the part of the family tree (either a 'Person' or a 'Family')
  accessFT :: Int -> IndexedTraversal' Int FamilyTree part

instance FamilyTreePart Person where
  accessFT n f familyTree = case familyTree ^. people . at n of
    Nothing -> pure familyTree
    Just oldPerson ->
      let newPerson_ = indexed f n oldPerson
      in alterPerson <$> newPerson_
    where
      alterPerson newPerson = people . ix n .~ newPerson $ familyTree

instance FamilyTreePart Family where
  accessFT n f familyTree = case familyTree ^. families . at n of
    Nothing -> pure familyTree
    Just oldFamily -> let newFamily_ = indexed f n oldFamily
                      in alterFamily <$> newFamily_
    where
      alterFamily newFamily =
        familyTree & families . ix n .~ newFamily

-- | Adds a person with minimal information, returning the updated family tree
-- and the ID of the new person.  
addPerson :: FamilyTree -> (Int, FamilyTree)
addPerson = people %%~ (_1 %~ _personId) . insert' mempty

-- | Adds a family with minimal information, returning the updated family tree
-- and the ID of the new family.  
addFamily :: FamilyTree -> (Int, FamilyTree)
addFamily = families %%~ (_1 %~ _familyId) . insert' mempty

-- | Deletes a person from the family tree, removing all references to them.  
deletePerson :: Int -> FamilyTree -> FamilyTree
deletePerson n familyTree =
  familyTree &
  people . at n .~ Nothing &
  families . each %~ 
    \fam -> fam &
            head1 %~ set (ix $ Just n) Nothing id & 
            head2 %~ set (ix $ Just n) Nothing id &
            children . contains n .~ False
            

-- | Deletes a family from the family tree, removing all references to it.    
deleteFamily :: Int -> FamilyTree -> FamilyTree
deleteFamily n = families . at n .~ Nothing

-- | Creates a new tree with a given name.       
newTree :: Text -> FamilyTree
newTree n = FamilyTree
  {_treeName = n
  ,_people = empty
  ,_families = empty
  }

