{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Hasklet.Store.Transform where

import           Hasklet.Store.Types

import           Control.Arrow
import           Data.Aeson           hiding (fromJSON)
import qualified Data.ByteString.Lazy as B
import           Data.Function
import qualified Data.HashMap.Lazy    as Map
import           Data.List
import           Data.Scientific
import qualified Data.Text            as T


fromJSON :: Value -> Either B.ByteString [(T.Text, FieldValue)]
fromJSON = go [] where
    go ks (Object o) = do vs <- traverse (\(k,v) -> case T.find ('.' ==) k of
                                                Nothing -> go (k:ks) v
                                                Just _  -> Left "JSON keys cannot contain '.'") $ Map.toList o
                          pure $ concat vs
    go _ (Array _)  = Left "Cannot store JSON arrays"
    go ks (String t) = Right [(getKey ks, TextField t)]
    go ks (Number n) = Right [(getKey ks, NumberField $ toRealFloat n)]
    go ks (Bool b)   = Right [(getKey ks, BoolField b)]
    go ks Null       = Right [(getKey ks, NullField)]
    getKey = T.intercalate "."

fromKeyValuePairs :: [(T.Text, FieldValue)] -> Maybe Value
fromKeyValuePairs = go . mapFst (\k -> if T.null k then [] else T.splitOn "." k) where
    go [([], v)] = Just $ fromFieldValue v
    go kvps
        | any (null . fst) kvps = Nothing -- This key has a value and sub-keys - this is not valid JSON
        | otherwise = (Object . Map.fromList) <$> traverse fromGroup (groupByKey (head . fst) kvps)
    fromGroup (k, kvps) = (k,) <$> go (mapFst tail kvps)
    fromFieldValue NullField       = Null
    fromFieldValue (NumberField d) = Number $ fromFloatDigits d
    fromFieldValue (TextField t)   = String t
    fromFieldValue (BoolField b)   = Bool b

fieldChanges :: Value -> Value -> Either B.ByteString [(T.Text, FieldValue)]
fieldChanges v v' = do
    fs  <- fromJSON v
    fs' <- fromJSON v'
    let fsMap = Map.fromList fs
        fsMap' = Map.fromList fs'
        updates = Map.differenceWith (\a b -> if a == b then Nothing else Just a) fsMap' fsMap
        ommitedKeys = Map.keys $ Map.difference fsMap fsMap'
    pure $ Map.toList updates ++ map (\k -> (k, NullField)) ommitedKeys

groupByKey :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupByKey get = map (\g -> (get $ head g, g)) . groupBy ((==) `on` get) . sortBy (compare `on` get)

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f = map $ first f
