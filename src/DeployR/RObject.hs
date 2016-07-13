{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module DeployR.RObject where

import Data.Aeson.Types
import GHC.Generics

import Data.Text(Text)
import qualified Data.Text as T
import Data.Map(Map)
import qualified Data.Map as M

import Data.List
import Data.Maybe

import Servant.API -- for instances

-- $encoding
-- This translates R objects from deployR to Haskell
-- The encoding of R objects in deployR is described in
-- https://deployr.revolutionanalytics.com/documents/dev/api-doc/guide/encodings.html

-- | "type" in the deployR encoding. Used to select 
data RT = RTPrim   -- "primitive"
        | RTVector -- "vector" 
        | RTDF     -- "dataframe", with named vectors of equal length
        | RTList   -- "list", with named things (anything)
        -- | RTDate   -- "date"  -- why not primitive?
        -- | RTMat   -- "matrix" -- no type info, left out
        -- | RTFac   -- "factor" -- why not primitive?
          deriving (Eq, Show, Read, Generic)
rTypes :: Map Text RT
rTypes = M.fromList [ ("primitive", RTPrim)
                    , ("vector"   , RTVector) 
                    , ("dataframe", RTDF)
                    , ("list"     , RTList)
                    ]
instance FromJSON RT where
    parseJSON (String n) =
        maybe (fail $ T.unpack n ++ ": unknown R type") return $
              M.lookup n rTypes
    parseJSON other      = typeMismatch "R type name" other

-- | "rclass" in the deployR encoding. Used in the parser
data RClass = RCh | RNm | RIn | RLg -- primitive types and vector contents
            | RDF | RLs -- contain named vectors 
            -- | RDate -- Date type in R. Left out: POSIXce
            -- | RMatrix -- nested lists, no type information. left out 
            -- | RFac | ROrd -- type "factor", left out 
              deriving (Eq, Show, Read, Generic)
rClasses :: Map Text RClass
rClasses = M.fromList [ ("character", RCh)
                      , ("numeric"  , RNm)
                      , ("integer"  , RIn)
                      , ("logical"  , RLg)
                      , ("data.frame", RDF)
                      , ("list"      , RLs)
                      ]
instance FromJSON RClass where
    parseJSON (String n) =
        maybe (fail $ T.unpack n ++ ": unknown R class") return $
              M.lookup n rClasses
    parseJSON other      = typeMismatch "R class name" other

---------------------------------------------------------------------
-- | Objects retrieved from R. We keep this simple for now, but it
-- probably needs review or restrictions to become fully useful.
-- Objects in the deployR representation also carry their name, but
-- that would not be too practical to work with (what about name collisions
-- in a list, and how about comparing objects with different name?)
data RObject = RInt  Int -- ^ R integer type
             | RDouble Double -- ^ R double type
             | RString Text   -- ^ R character type (also factor)
             | RBool Bool     -- ^ R logical type
             | RVectorI [Int]    -- ^Vector of R integers
             | RVectorD [Double] -- ^Vector of R doubles
             | RVectorS [Text]   -- ^Vector of R characters
             | RVectorB [Bool]   -- ^Vector of R characters
               -- TODO switch to Vector
             | RDataframe (Map Text RObject) -- ^ Data frame, values are
                                             -- vectors in named columns
             | RList (Map Text RObject) -- ^ List, named values are anything
    deriving (Eq, Read, Generic)

instance Show RObject where
    show (RInt i)     = show i
    show (RDouble d)  = show d
    show (RString s)  = show (T.unpack s)
    show (RBool b)    = show b
    show (RVectorD as) = show as
    show (RVectorI as) = show as
    show (RVectorS as) = show as
    show (RVectorB as) = show as
    show (RDataframe m) = showColumns "Data frame" m
    show (RList m)      = showColumns "list" m

showColumns prefix m = concat $ [ prefix, "{ "]
                                ++ intersperse "," cols 
                                ++ [" }"]
    where cols = map (\(n, v) -> T.unpack n ++ ": " ++ show v) $ M.assocs m

-- instance Eq RObject where
--     (RInt i1)    == (RInt i2)    = i1 == i2
--     (RDouble d1) == (RDouble d2) = d1 == d2
--     (RString s1) == (RString s2) = s1 == s2
--     (RBool b1)   == (RBool b2)   = b1 == b2
--     (RVectorD as1)==(RVectorD as2) = as1 == as2
--     (RVectorI as1)==(RVectorI as2) = as1 == as2
--     (RVectorS as1)==(RVectorS as2) = as1 == as2
--     (RDataframe m)==(RDataframe n) = m == n
--     some1        == some2        =
--         fromR some1 == (fromR some2 :: Either String Text)
--     -- brutally compare as text.
--     -- TODO This is inconsistent if the result is not used as string
--     -- later (consider RVector [1,2,3] == RString "[1,2,3]" but the
--     -- right side crashes on sum . fromR). A silent string cast (using
--     -- Read) would fix this, but how far do we want to take auto-casts?
--     --   Alternative: intensional equality, all else unequal.

instance FromJSON RObject 
    where
      parseJSON (Object o) = do
        ty <- o .: "type" -- vector or scalar
        cl <- o .: "rclass"
        case (ty, cl) of
          (RTPrim   , RIn) -> RInt    <$> o .: "value"
          (RTPrim   , RNm) -> RDouble <$> o .: "value"
          (RTPrim   , RCh) -> RString <$> o .: "value"
          (RTPrim   , RLg) -> RBool   <$> o .: "value"
          (RTVector , RIn) -> RVectorI <$> o .: "value"
          (RTVector , RNm) -> RVectorD <$> o .: "value"
          (RTVector , RCh) -> RVectorS <$> o .: "value"
          (RTVector , RLg) -> RVectorB <$> o .: "value"
          (RTDF     , RDF) -> RDataframe <$> (o .: "value" >>= parseObjects)
          (RTList   , RLs) -> RList <$>  (o .: "value" >>= parseObjects)
          other       -> error $ show other ++ ": implement me"

-- | interface function: generate a map of named RObjects from a list
-- of JSON Objects. Recursively used in data frame and list code
parseObjects :: [Value] -> Parser (Map Text RObject)
parseObjects objs = do cols  <- mapM parseJSON objs
                       names <- mapM (withObject "R object" (.:"name")) objs
                       return $ M.fromList $ zip names cols

------------------------------------------------------------
-- | translate RObjects to Haskell (casting values when OK).
-- All types that can be returned from R have conversions.
class RType a where
    fromR :: RObject -> Either String a  -- ^ read a value from R into Hs
    -- we do not plan to push Hs values to R - at least not yet.

-- TODO check out a type family approach:
-- data RN; data RC; data RVec a; data RDF a
-- type family RData a
-- type instance RData RN = Double
-- type instance RData RC = Text
-- type instance RData RI = Int

instance RType Int where
    fromR (RInt i) = Right i
    fromR other    = Left $ "type mismatch: " ++ show other ++ " not int"

instance RType Double where
    fromR (RDouble d) = Right d
    fromR (RInt i)    = Right $ fromIntegral i
    fromR other    = Left $ "type mismatch: " ++ show other ++ " not double"

instance RType Text where
    fromR (RInt i)     = Right $ T.pack (show i)
    fromR (RDouble d)  = Right $ T.pack (show d)
    fromR (RString s)  = Right $ s
    fromR (RVectorD as) = Right $ T.pack (show as)
    fromR (RVectorI as) = Right $ T.pack (show as)
    fromR (RVectorS as) = Right $ T.intercalate "," as

instance RType [Double] where
    fromR (RVectorD as) = Right as
    fromR (RVectorI is) = Right $ map fromIntegral is
    fromR other = Left $ "type mismatch: " ++ show other ++ " not double Vector"

instance RType [Int] where
    fromR (RVectorI is) = Right is
    fromR other = Left $ "type mismatch: " ++ show other ++ " not int Vector"
