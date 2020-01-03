module BSF where

import           Universum

import           Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

type Graph = Map Text [Text]

graph1 :: Graph
graph1 = Map.fromList
  [ ("you",    ["alice", "bob", "claire"])
  , ("bob",    ["anuj", "peggy"])
  , ("alice",  ["peggy"])
  , ("claire", ["thom", "johny"])
  , ("anuj",   [])
  , ("peggy",  [])
  , ("thom",   [])
  , ("johny",  [])
  ]

search :: Graph -> Text -> Maybe Text
search graph name = searchInQueue Set.empty $ personConnections name
  where
    personConnections :: Text -> Seq Text
    personConnections name = Seq.fromList . fromMaybe [] $ Map.lookup name graph

    searchInQueue :: Set Text -> Seq Text -> Maybe Text
    searchInQueue _ Empty = Nothing
    searchInQueue searched (person :<| persons)
      | Set.member person searched = searchInQueue searched persons
      | isSeller person = Just person
      | otherwise = searchInQueue (Set.insert person searched) $ persons >< personConnections person

isSeller :: Text -> Bool
isSeller name = Text.last name == 'm'

bsfTest :: IO ()
bsfTest = do
  putTextLn "BSF test"
  putTextLn $ show graph1
  putTextLn $ fromMaybe "not found" $ search graph1 "you"
  putTextLn $ fromMaybe "not found" $ search graph1 "bob"
