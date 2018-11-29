{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}
module Enecuum.Framework.Domain.Range where

import           Enecuum.Prelude hiding (foldMap)
import           Data.Foldable (Foldable (..))
import qualified Data.Aeson                           as J

data Range a where
    Range      :: (Eq a, Enum a, Ord a) => a -> a -> Range a
    EmptyRange :: Range a

deriving instance Show a => Show (Range a)

newRange :: (Eq a, Enum a, Ord a) => a -> a -> Range a
newRange a b
    | a <= b    = Range a b
    | otherwise = EmptyRange

bottomBound :: Range a -> Maybe a
bottomBound (Range a _) = Just a
bottomBound _           = Nothing

topBound :: Range a -> Maybe a
topBound (Range _ a) = Just a
topBound _           = Nothing

instance Foldable Range where
    foldMap f (Range a b)
        | a < b     = f a `mappend` foldMap f (Range (succ a) b)
        | otherwise = f a

newEmptyRange :: Range a
newEmptyRange = EmptyRange

rangeToList :: Enum a => Range a -> [a] 
rangeToList (Range a b) = [a..b]
rangeToList EmptyRange  = []

instance (FromJSON a, Eq a, Enum a, Ord a) => FromJSON (Range a) where
    parseJSON (J.Object v) = do
        tag <- v J..: "tag"
        case tag of
            ("Range" :: Text) -> do
                minVal <- v J..: "minValue"
                maxVal <- v J..: "maxValue"
                pure $ newRange minVal maxVal
            ("EmptyRange" :: Text)    -> pure EmptyRange
            _                               -> mzero
    parseJSON _ = mzero

instance (ToJSON a, Eq a, Enum a, Ord a) => ToJSON (Range a) where
    toJSON (Range a b) = J.object
        [ "tag" J..= ("Range" :: Text)
        , "minValue" J..= a
        , "maxValue" J..= b
        ]
    toJSON EmptyRange = J.object
        [ "tag" J..= ("EmptyRange" :: Text)
        ]