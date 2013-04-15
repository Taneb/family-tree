{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  -- ** ID types
 -- $ids
 PersonID(..),
 FamilyID(..),
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

import Control.Applicative (Applicative(..), (<$>), Alternative(..))
import Control.Lens hiding (children)

import Data.Binary (Binary(..), Word8, getWord8)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..), First(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
--import Data.Table
import Data.String
import Data.Text (Text)
--import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (Day(..), fromGregorian, gregorianMonthLength)

import GHC.Generics

import Numeric.Interval (Interval)
import qualified Numeric.Interval as I

-- $ids
-- The various ID types represent an identifier for a person, family, or event. 
-- While the constructors are exported, it is probably better to use the
-- various 'Traversal's for manipulation, as they echo the changes around the
-- tree automatically.
newtype PersonID = PersonID {getPersonID :: Int} deriving (Eq, Ord, Show, Read, Generic)

instance Wrapped Int Int PersonID PersonID where
  wrapped = iso PersonID getPersonID

newtype FamilyID = FamilyID {getFamilyID :: Int} deriving (Eq, Ord, Show, Read, Generic)

instance Wrapped Int Int FamilyID FamilyID where
  wrapped = iso FamilyID getFamilyID

-- | The Location type. Either a coordinate or a placename.  
data Location = Coord Double Double | PlaceName Text deriving (Eq, Show, Generic)

instance IsString Location where
  fromString = PlaceName . fromString
  
-- | The Relationship type. Marriage is the default for similarity to GEDCOM.
data Relationship = Marriage | Other Text deriving (Eq, Show, Generic)

type PartialDate = Interval Day

partialDateFromYear :: Integer -> PartialDate
partialDateFromYear n = I.I (fromGregorian n 1 1) (fromGregorian n 12 31)

partialDateFromMonth :: Integer -> Int -> PartialDate
partialDateFromMonth y m = I.I (fromGregorian y m 1) . 
    fromGregorian y m $ gregorianMonthLength y m

partialDateFromDay :: Integer -> Int -> Int -> PartialDate
partialDateFromDay y m d = I.singleton $ fromGregorian y m d

-- | The basic type for a person. 'Nothing' meaning unknown (or otherwise 
-- non-existent, for intance a death date for someone still alive) is a
-- convention used throughout this library.
data Person = Person
  {_name :: Maybe Text
  ,_birthdate :: Maybe PartialDate
  ,_birthplace :: Maybe Location
  ,_deathdate :: Maybe PartialDate
  ,_deathplace :: Maybe Location
  ,_attributes :: HashMap Text Text
  } deriving (Eq, Show, Generic)
             
makeLenses ''Person

-- | The basic type for a family. Which person is head1 and which is head2 is
-- arbitrary, but try to use a consistent rule
data Family = Family
  {_head1 :: Maybe PersonID
  ,_head2 :: Maybe PersonID
  ,_relationship :: Maybe Relationship
  ,_relationFrom :: Maybe PartialDate
  ,_relationTo :: Maybe PartialDate
  ,_children :: IntSet
  } deriving (Eq, Show, Generic)
  
makeLenses ''Family

-- | The core structure of a family tree.
data FamilyTree = FamilyTree
  {_treeName :: Text
  ,_people :: IntMap Person
  ,_families :: IntMap Family
  } deriving (Eq, Show, Generic)
  
makeLenses ''FamilyTree

instance Monoid Person where
  mempty = Person {
    _name = Nothing,
    _birthdate = Nothing,
    _birthplace = Nothing,
    _deathdate = Nothing,
    _deathplace = Nothing,
    _attributes = HM.empty
    }
  p1 `mappend` p2 = Person {
    _name = ((<|>) `on` _name) p1 p2,
    _birthdate = ((<|>) `on` _birthdate) p1 p2,
    _birthplace = ((<|>) `on` _birthplace) p1 p2,
    _deathdate = ((<|>) `on` _deathdate) p1 p2,
    _deathplace = ((<|>) `on` _deathplace) p1 p2,
    _attributes = (HM.union `on` _attributes) p1 p2
    }

instance Monoid Family where
  mempty = Family {
    _head1 = Nothing,
    _head2 = Nothing,
    _relationship = Nothing,
    _relationFrom = Nothing,
    _relationTo = Nothing,
    _children = IS.empty
    }
  f1 `mappend` f2 = Family {
    _head1 = getFirst $ (mappend `on` First . _head1) f1 f2,
    _head2 = getFirst $ (mappend `on` First . _head2) f1 f2,
    _relationship = getFirst $ (mappend `on` First . _relationship) f1 f2,
    _relationFrom = getFirst $ (mappend `on` First . _relationFrom) f1 f2,
    _relationTo = getFirst $ (mappend `on` First . _relationTo) f1 f2,
    _children = (IS.union `on` _children) f1 f2
    }


instance Binary Location where
  {-put (Coord x y) = do
    put (0 :: Word8)
    put x
    put y
  put (PlaceName t) = do
    put (1 :: Word8)
    put (encodeUtf8 t)
  get = do
    tag <- getWord8
    case tag of
      0 -> do
        x <- get
        y <- get
        return $ Coord x y
      _ -> PlaceName . decodeUtf8 <$> get-}

instance Binary Relationship where
  put (Other t) = do
    put (0 :: Word8)
    put (encodeUtf8 t)
  put Marriage = put (1 :: Word8)
  get = do
    tag <- getWord8
    case tag of
      0 -> Other . decodeUtf8 <$> get
      _ -> return Marriage

instance Binary Person where
  put person = do
    put (encodeUtf8 <$> _name person)
    put (toModifiedJulianDay . I.inf <$> _birthdate person)
    put (toModifiedJulianDay . I.sup <$> _birthdate person)
    put (_birthplace person)
    put (toModifiedJulianDay . I.inf <$> _deathdate person)
    put (toModifiedJulianDay . I.sup <$> _deathdate person)
    put (_deathplace person)
    put (map (both %~ encodeUtf8) . HM.toList $ _attributes person)
  get = do
    n <- get
    bdi <- get
    bds <- get
    bp <- get
    ddi <- get
    dds <- get
    dp <- get
    a <- get
    return Person
      {_name = fmap decodeUtf8 n
      ,_birthdate = I.I <$> fmap ModifiedJulianDay bdi <*>
        fmap ModifiedJulianDay bds
      ,_birthplace = bp
      ,_deathdate = I.I <$> fmap ModifiedJulianDay ddi <*>
        fmap ModifiedJulianDay dds
      ,_deathplace = dp
      ,_attributes = HM.fromList $ map (both %~ decodeUtf8) a
      }

instance Binary Family where
  put fam = do
    put $ getPersonID <$> _head1 fam
    put $ getPersonID <$> _head2 fam
    put $ _relationship fam
    put $ toModifiedJulianDay . I.inf <$> _relationFrom fam
    put $ toModifiedJulianDay . I.sup <$> _relationFrom fam
    put $ toModifiedJulianDay . I.inf <$> _relationTo fam
    put $ toModifiedJulianDay . I.sup <$> _relationTo fam
    put $ _children fam
  get = do
    h1 <- get
    h2 <- get
    r <- get
    rfi <- get
    rfs <- get
    rti <- get
    rts <- get
    c <- get
    return Family
      {_head1 = PersonID <$> h1
      ,_head2 = PersonID <$> h2
      ,_relationship = r
      ,_relationFrom = I.I <$> fmap ModifiedJulianDay rfi <*> fmap ModifiedJulianDay rfs
      ,_relationTo = I.I <$> fmap ModifiedJulianDay rti <*> fmap ModifiedJulianDay rts
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
  type ID part
  -- | 'accessFT' is a 'Traversal' to the part of the family tree (either a 'Person' or a 'Family') with the given 'ID'
  accessFT :: ID part -> IndexedTraversal' (ID part) FamilyTree part

instance FamilyTreePart Person where
  type ID Person = PersonID
  accessFT (PersonID n) f familyTree = case familyTree ^. people . at n of
    Nothing -> pure familyTree
    Just oldPerson ->
      let newPerson_ = indexed f (PersonID n) oldPerson
      in alterPerson <$> newPerson_
    where
      alterPerson newPerson = people . ix n .~ newPerson $ familyTree

instance FamilyTreePart Family where
  type ID Family = FamilyID
  accessFT (FamilyID n) f familyTree = case familyTree ^. families . at n of
    Nothing -> pure familyTree
    Just oldFamily -> let newFamily_ = indexed f (FamilyID n) oldFamily
                      in alterFamily <$> newFamily_
    where
      alterFamily newFamily =
        familyTree & families . ix n .~ newFamily

-- | Adds a person with minimal information, returning the updated family tree
-- and the ID of the new person.  
addPerson :: FamilyTree -> (PersonID, FamilyTree)
addPerson familyTree =
  let n = maybe 0 fst $
          listToMaybe $
          dropWhile (uncurry (==)) $
          zip [1 ..] $ IM.keys $ _people familyTree
  in (PersonID n, people . at n ?~ mempty $ familyTree)

-- | Adds a family with minimal information, returning the updated family tree
-- and the ID of the new family.  
addFamily :: FamilyTree -> (FamilyID, FamilyTree)
addFamily familyTree =
  let n = maybe 0 fst $
          listToMaybe $
          dropWhile (uncurry (==)) $
          zip [1 ..] $ IM.keys $ _families familyTree
  in (FamilyID n, families . at n ?~ mempty $ familyTree)

-- | Deletes a person from the family tree, removing all references to them.  
deletePerson :: PersonID -> FamilyTree -> FamilyTree
deletePerson (PersonID n) familyTree =
  familyTree &
  people . at n .~ Nothing &
  families %~ IM.map (
    \fam -> fam &
            head1 %~ (id & ix (Just $ PersonID n) .~ Nothing) &
            head2 %~ (id & ix (Just $ PersonID n) .~ Nothing) &
            children . contains n .~ False
            )

-- | Deletes a family from the family tree, removing all references to it.    
deleteFamily :: FamilyID -> FamilyTree -> FamilyTree
deleteFamily (FamilyID n) = families . at n .~ Nothing

-- | Creates a new tree with a given name.       
newTree :: Text -> FamilyTree
newTree n = FamilyTree
  {_treeName = n
  ,_people = IM.empty
  ,_families = IM.empty
  }

