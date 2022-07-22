{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.API.TH
  ( DerivingOptions(..)
  , SumEncoding(..)
  , defaultDerivingOptions
  , derivingOptionsToJsonOptions
  , derivingOptionsToSchemaOptions
  , deriveApiInstances
#ifndef ghcjs_HOST_OS
  , deriveToParamSchema
  , deriveToSchema
#endif
  , deriveApiFromJSON
  , deriveApiToJSON
  , generateParseJSON
  ) where

import           Control.Monad
import           Data.Aeson.TH as A
import           Data.Aeson.Types
#if MIN_VERSION_th_abstraction(0,3,0)
import           Data.Functor
#endif
import qualified Data.List as List
import           Data.Maybe
#ifndef ghcjs_HOST_OS
import           Data.OpenApi hiding (prefix)
#endif
import           Data.Typeable
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Syntax


-- | derive JSON and openapi3 (ToSchema) instances
deriveApiInstances :: DerivingOptions -> Name -> Q [Dec]
#ifndef ghcjs_HOST_OS
deriveApiInstances opts tname = liftM2 (<>)
  (deriveJSON (derivingOptionsToJsonOptions opts) tname)
  (deriveToSchema opts tname)
#else
deriveApiInstances opts tname =
  deriveJSON (derivingOptionsToJsonOptions opts) tname
#endif

-- We can't directly 'lift' Options into TH, so have to use this
data DerivingOptions = DerivingOptions
  { prefix :: Maybe String
  -- ^ Optional prefix to strip from constructors and field labels
  , snake :: Bool
  -- ^ Whether to snake constructors and field labels
  --   Applied after stripping prefix
  , unwrapUnaryRecords :: Bool
  , sumEncoding :: SumEncoding
  } deriving (Lift)

deriving instance Lift SumEncoding

defaultDerivingOptions :: DerivingOptions
defaultDerivingOptions = DerivingOptions
  { prefix = Nothing
  , snake = True
  , unwrapUnaryRecords = True
  , sumEncoding = defaultTaggedObject }

derivingOptionsToJsonOptions :: DerivingOptions -> Options
derivingOptionsToJsonOptions DerivingOptions{..} = defaultOptions
  { fieldLabelModifier = snaked . stripped
  , constructorTagModifier = snaked . stripped
  , sumEncoding
  , unwrapUnaryRecords }
  where
    snaked = if snake then camelTo2 '_' else id
    stripped = maybe id stripPrefix prefix

derivingOptionsToSchemaOptions :: DerivingOptions -> SchemaOptions
derivingOptionsToSchemaOptions = fromAesonOptions . derivingOptionsToJsonOptions

#ifndef ghcjs_HOST_OS
deriveToParamSchema :: DerivingOptions -> Name -> Q [Dec]
deriveToParamSchema opts tname = do
  let
    body = AppE (VarE 'genericToParamSchema) <$>
      appE (pure $ VarE 'derivingOptionsToSchemaOptions) (lift opts)
  pure <$> instanceD
    (pure [])
    (pure $ ConT ''ToParamSchema `AppT` ConT tname)
    [ funD 'toParamSchema $ pure $ clause [] (normalB body) [] ]

deriveToSchema :: DerivingOptions -> Name -> Q [Dec]
deriveToSchema opts tname = do
  tinfo <- reifyDatatype tname
  let
    tvars = datatypeVars tinfo
#if MIN_VERSION_th_abstraction(0,3,0)
      <&> VarT . tvName
#endif
    context = do
      var <- tvars
      [ConT ''Typeable `AppT` var, ConT ''ToSchema `AppT` var]
    body = AppE (VarE 'genericDeclareNamedSchema) <$>
      appE (pure $ VarE 'derivingOptionsToSchemaOptions) (lift opts)
  pure <$> instanceD
    (pure context)
    (pure $ ConT ''ToSchema `AppT` datatypeType tinfo)
    [ funD 'declareNamedSchema $ pure $ clause [] (normalB body) [] ]
#endif

deriveApiFromJSON :: DerivingOptions -> Name -> Q [Dec]
deriveApiFromJSON = deriveFromJSON . derivingOptionsToJsonOptions

deriveApiToJSON :: DerivingOptions -> Name -> Q [Dec]
deriveApiToJSON = deriveToJSON . derivingOptionsToJsonOptions

generateParseJSON :: DerivingOptions -> Name -> Q Exp
generateParseJSON = mkParseJSON . derivingOptionsToJsonOptions

stripPrefix :: String -> String -> String
stripPrefix pfx = fromMaybe <*> List.stripPrefix pfx
