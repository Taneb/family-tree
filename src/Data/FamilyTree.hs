{- |
Maintainer  :  nvd124@gmail.com
Stability   :  unstable
Portability :  portable

This module is for Family Trees.

It uses lenses for the manipulation of people. For the usage of lenses, see
"Data.Lens.Lazy"

It is reccomended to use "Data.Binary" to do saving and loading.


-}
module Data.FamilyTree
 (
 -- * Types
 -- ** Main types
 Person(..),
 Family(..),
 Event(..),
 FamilyTree(..),
 -- ** ID types
 -- $ids
 PersonID(..),
 FamilyID(..),
 EventID(..),
 -- ** Other types
 Location(..),
 Relationship(..),
 -- * Functions
 -- ** Creation
 newTree,
 addPerson,
 addFamily,
 addEvent,
 -- ** Manipulation
 personLens,
 familyLens,
 eventLens,
 -- ** Destruction
 deletePerson,
 deleteFamily,
 deleteEvent
) where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (join)

import Data.Binary (Binary, get, put, Word8, getWord8)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Lens.Common (Lens, lens)
import Data.Maybe (listToMaybe)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (Day(..))

-- | The basic type for a person. 'Nothing' meaning unknown (or otherwise 
-- non-existent, for intance a death date for someone still alive) is a
-- convention used throughout this library.
data Person = Person
  {name :: Maybe Text
  ,birthdate :: Maybe Day
  ,birthplace :: Maybe Location
  ,deathdate :: Maybe Day
  ,deathplace :: Maybe Location
  ,attributes :: HashMap Text Text
  ,attendedEvents :: IntSet
  } deriving (Eq, Show)

-- | The basic type for a family. Which person is head1 and which is head2 is
-- arbitrary, but try to use a consistent rule
data Family = Family
  {head1 :: Maybe Int
  ,head2 :: Maybe Int
  ,relationship :: Maybe Relationship
  ,relationFrom :: Maybe Day
  ,relationTo :: Maybe Day
  ,children :: IntSet
  } deriving (Eq, Show)
  
-- | The basic type for an event. For example:
--
-- @
--   Event {
--     eventInfo = \"Battle of Agincourt\"
--     eventDate = fromGregorianValid 1415 10 25
--     eventAttendees = IM.empty
--         }
-- @
data Event = Event 
  {eventInfo :: Text
  ,eventDate :: Maybe Day
  ,eventAttendees :: IntSet
  } deriving (Eq, Show)

-- | The Location type. Either a coordinate or a placename.  
data Location = Coord Double Double | PlaceName Text deriving (Eq, Show)

-- | The Relationship type. Marriage is the default for similarity to GEDCOM.
data Relationship = Marriage | Other Text deriving (Eq, Show)

-- | The core structure of a family tree.
data FamilyTree = FamilyTree
  {treeName :: Text
  ,people :: IntMap Person
  ,families :: IntMap Family
  ,events :: IntMap Event
  } deriving (Eq, Show)
  
instance Binary Location where
  put (Coord x y) = do
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
      _ -> PlaceName . decodeUtf8 <$> get

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
    put (encodeUtf8 <$> name person)
    put (toModifiedJulianDay <$> birthdate person)
    put (birthplace person)
    put (toModifiedJulianDay <$> deathdate person)
    put (deathplace person)
    put (map (join (***) encodeUtf8) . HM.toList $ attributes person)
    put (attendedEvents person)
  get = do
    n <- get
    bd <- get
    bp <- get
    dd <- get
    dp <- get
    a <- get
    e <- get
    return Person
      {name = fmap decodeUtf8 n
      ,birthdate = fmap ModifiedJulianDay bd
      ,birthplace = bp
      ,deathdate = fmap ModifiedJulianDay dd
      ,deathplace = dp
      ,attributes = HM.fromList $ map (join (***) decodeUtf8) a
      ,attendedEvents = e
      }

instance Binary Family where
  put fam = do
    put $ head1 fam
    put $ head2 fam
    put $ relationship fam
    put $ toModifiedJulianDay <$> relationFrom fam
    put $ toModifiedJulianDay <$> relationTo fam
    put $ children fam
  get = do
    h1 <- get
    h2 <- get
    r <- get
    rf <- get
    rt <- get
    c <- get
    return Family
      {head1 = h1
      ,head2 = h2
      ,relationship = r
      ,relationFrom = fmap ModifiedJulianDay rf
      ,relationTo = fmap ModifiedJulianDay rt
      ,children = c
      }
      
instance Binary Event where
  put evnt = do
    put . encodeUtf8 $ eventInfo evnt
    put $ toModifiedJulianDay <$> eventDate evnt
    put $ eventAttendees evnt
  get = do
    n <- get
    d <- get
    a <- get
    return Event
      {eventInfo = decodeUtf8 n
      ,eventDate = fmap ModifiedJulianDay d
      ,eventAttendees = a
      }

instance Binary FamilyTree where
  put tree = do
    put $ encodeUtf8 $ treeName tree
    put $ people tree
    put $ families tree
    put $ events tree
  get = do
    n <- get
    p <- get
    f <- get
    e <- get
    return FamilyTree
      {treeName = decodeUtf8 n
      ,people = p
      ,families = f
      ,events = e
      }

-- $ids
-- The various ID types represent an identifier for a person, family, or event. 
-- While the constructors are exported, it is probably better to use the
-- various lenses for manipulation, as they echo the changes around the tree
-- automatically.
newtype PersonID = PersonID Int

newtype FamilyID = FamilyID Int

newtype EventID = EventID Int

-- | Constructs a lens for the manipulation of a person in a family tree, from
-- that person's ID. Using an ID that does not correspond to a person is an
-- error, and it is impossible to create or destroy people using a lens created
-- by this. 
personLens :: PersonID -> Lens FamilyTree Person
personLens (PersonID n) = lens ((IM.! n) . people) $
  \person familyTree ->
    let oldPerson = people familyTree IM.! n
        newattended = (IS.difference `on` attendedEvents)
          person oldPerson
        oldattended = (IS.difference `on` attendedEvents)
          oldPerson person 
    in familyTree
      {people = IM.insert n person (people familyTree)
      ,events = IS.foldr (\i -> IM.adjust
        (\event -> event 
           {eventAttendees = IS.delete i (eventAttendees event)}) i)
        (IS.foldr (\i -> IM.adjust
          (\event -> event 
             {eventAttendees = IS.insert i (eventAttendees event)}) i)
          (events familyTree) newattended) oldattended
      }

-- | Constructs a lens for the manipulation of a family in a family tree, from
-- that family's ID. Using an ID that does not correspond to a family is an
-- error, and it is impossible to create or destroy families using a lens
-- created by this.       
familyLens :: FamilyID -> Lens FamilyTree Family
familyLens (FamilyID n) = lens ((IM.! n) . families) $
  \family' familyTree ->
    familyTree
      {families = IM.insert n family' (families familyTree)
      }

-- | Constructs a lens for the manipulation of an event in a family tree, from
-- that event's ID. Using an ID that does not correspond to an event is an
-- error, and it is impossible to create or destroy events using a lens created
-- by this.       
eventLens :: EventID -> Lens FamilyTree Event
eventLens (EventID n) = lens ((IM.! n) . events) $
  \event familyTree ->
    familyTree
      {events = IM.insert n event (events familyTree)
      ,people =
        let oldEventPeople =
              eventAttendees (events familyTree IM.! n)
              IS.\\ eventAttendees event
            newEventPeople = 
              eventAttendees event
              IS.\\ eventAttendees (events familyTree IM.! n)
        in IS.foldr
          (IM.adjust (\ person -> person
             {attendedEvents = IS.insert n (attendedEvents person)}))
          (IS.foldr
            (IM.adjust (\person -> person 
              {attendedEvents = IS.delete n (attendedEvents person)}))
            (people familyTree)
            oldEventPeople)
          newEventPeople
      }
      
blankPerson :: Person
blankPerson = Person
  {name = Nothing
  ,birthdate = Nothing
  ,birthplace = Nothing
  ,deathdate = Nothing
  ,deathplace = Nothing
  ,attributes = HM.empty
  ,attendedEvents = IS.empty
  }
  
blankFamily :: Family
blankFamily = Family
  {head1 = Nothing
  ,head2 = Nothing
  ,relationship = Nothing
  ,relationFrom = Nothing
  ,relationTo = Nothing
  ,children = IS.empty
  }
  
blankEvent :: Event
blankEvent = Event
  {eventInfo = T.empty
  ,eventDate = Nothing
  ,eventAttendees = IS.empty
  }

-- | Adds a person with minimal information, returning the updated family tree
-- and the ID of the new person.  
addPerson :: FamilyTree -> (FamilyTree, PersonID)
addPerson familyTree =
  let n = maybe 0 fst $
          listToMaybe $
          dropWhile (uncurry (==)) $
          zip [1 ..] $ IM.keys $ people familyTree
  in (familyTree
       {people = IM.insert n blankPerson $ people familyTree}, PersonID n)

-- | Adds a family with minimal information, returning the updated family tree
-- and the ID of the new family.  
addFamily :: FamilyTree -> (FamilyTree, FamilyID)
addFamily familyTree =
  let n = maybe 0 fst $
          listToMaybe $
          dropWhile (uncurry (==)) $
          zip [1 ..] $ IM.keys $ families familyTree
  in (familyTree 
       {families = IM.insert n blankFamily $ families familyTree}, FamilyID n)

-- | Adds an event with minimal information, returning the updated family tree
-- and the ID of the new event.
addEvent :: FamilyTree -> (FamilyTree, EventID)
addEvent familyTree =
  let n = maybe 0 fst $
          listToMaybe $
          dropWhile (uncurry (==)) $
          zip [1 ..] $ IM.keys $ events familyTree
  in (familyTree {events = IM.insert n blankEvent $ events familyTree}, EventID n)

-- | Deletes a person from the family tree, removing all references to them.  
deletePerson :: PersonID -> FamilyTree -> FamilyTree
deletePerson (PersonID n) familyTree =
  familyTree 
    {people = IM.delete n $ people familyTree
    ,families = IM.map
       (\fam -> fam
         {head1 = if head1 fam == Just n then Nothing else head1 fam
         ,head2 = if head2 fam == Just n then Nothing else head2 fam
         ,children = IS.delete n $ children fam
         }
         )
       (families familyTree)
    ,events = IM.map
      (\evnt -> evnt
        {eventAttendees = IS.delete n $ eventAttendees evnt}
        )
      (events familyTree)
    }

-- | Deletes a family from the family tree, removing all references to it.    
deleteFamily :: FamilyID -> FamilyTree -> FamilyTree
deleteFamily (FamilyID n) familyTree =
  familyTree
    {families = IM.delete n $ families familyTree}

-- | Deletes an event from the family tree, removing all references to it.
deleteEvent :: EventID -> FamilyTree -> FamilyTree
deleteEvent (EventID n) familyTree =
  let relevantPeople = eventAttendees (events familyTree IM.! n)
  in familyTree
       {events = IM.delete n $ events familyTree
       ,people = IS.foldr (IM.adjust
         (\p -> p {attendedEvents = IS.delete n $ attendedEvents p}))
         (people familyTree) relevantPeople
       }           

-- | Creates a new tree with a given name.       
newTree :: Text -> FamilyTree
newTree n = FamilyTree
  {treeName = n
  ,people = IM.empty
  ,families = IM.empty
  ,events = IM.empty
  }