{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Derive.API.TH
  ( module A
  , DerivingOptions(..)
  , SumEncoding(..)
  , defaultDerivingOptions
  , derivingOptionsToJsonOptions
  , deriveApiInstances
  , deriveApiAndArbitraryInstances
#ifndef ghcjs_HOST_OS
  , derivingOptionsToSchemaOptions
  , deriveToParamSchema
  , deriveToSchema
  , deriveArbitrary
#endif
  , deriveApiFromJSON
  , deriveApiToJSON
  , generateParseJSON
  ) where

import Control.Monad
import Data.Aeson.TH as A
import Data.Aeson.Types
import Data.List
import Data.Map as M
import Data.Maybe
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

#ifndef ghcjs_HOST_OS
import Data.OpenApi hiding (prefix)
#endif

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

-- | derive JSON and openapi3 (ToSchema) and Arbitrary instances
deriveApiAndArbitraryInstances :: DerivingOptions -> Name -> Q [Dec]
#ifndef ghcjs_HOST_OS
deriveApiAndArbitraryInstances opts tname = liftM2 (<>)
  (deriveApiInstances opts tname)
  (deriveArbitrary tname)
#else
deriveApiAndArbitraryInstances opts tname =
  deriveApiInstances opts tname
#endif

#if MIN_VERSION_containers(0,6,6)
#elif MIN_VERSION_template_haskell(2,16,0)
instance (Lift k, Lift a) => Lift (Map k a) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift m = [|M.fromList (M.toList m)|]
#else
instance (Lift k, Lift a) => Lift (Map k a) where
  lift m = [|M.fromList (M.toList m)|]
#endif
-- We can't directly 'lift' Options into TH (there is no instance for function),
-- so we use this type.
data DerivingOptions = DerivingOptions
  { prefix :: Maybe String
  -- ^ Optional prefix to strip from constructors and field labels  
  , snake :: Bool
  -- ^ Whether to snake constructors and field labels
  --   Applied after stripping prefix
  , convertMap :: M.Map String String
  -- ^ convert names (constructors and fields) before snake and strip prefix
  , unwrapUnaryRecords :: Bool
  , omitNothingFields :: Bool
  , sumEncoding :: SumEncoding
  } deriving (Lift)

deriving instance Lift SumEncoding

defaultDerivingOptions :: DerivingOptions
defaultDerivingOptions = DerivingOptions
  { prefix = Nothing
  , snake = True
  , convertMap = mempty
  , unwrapUnaryRecords = False
  , omitNothingFields = False
  , sumEncoding = defaultTaggedObject }

derivingOptionsToJsonOptions :: DerivingOptions -> Options
derivingOptionsToJsonOptions DerivingOptions{..} = defaultOptions
  { fieldLabelModifier = snaked . stripped . convert
  , constructorTagModifier = snaked . stripped . convert
  , sumEncoding
  , omitNothingFields
  , unwrapUnaryRecords }
  where
    convert s = fromMaybe s $ M.lookup s convertMap
    snaked = if snake then camelTo2 '_' else id
    stripped = maybe id stripPrefix' prefix

#ifndef ghcjs_HOST_OS
datatypeVarsTypes :: DatatypeInfo -> [Type]
#if MIN_VERSION_th_abstraction(0,3,0)
datatypeVarsTypes = fmap (VarT . tvName) . datatypeVars 
#else
datatypeVarsTypes = datatypeVars 
#endif

derivingOptionsToSchemaOptions :: DerivingOptions -> SchemaOptions
derivingOptionsToSchemaOptions = fromAesonOptions . derivingOptionsToJsonOptions

deriveToParamSchema :: DerivingOptions -> Name -> Q [Dec]
deriveToParamSchema opts tname = do
  body <- AppE (VarE 'genericToParamSchema) 
    . AppE (VarE 'derivingOptionsToSchemaOptions) <$> lift opts
  pure [InstanceD Nothing []
    (ConT ''ToParamSchema `AppT` ConT tname)
    [FunD 'toParamSchema [Clause [] (NormalB body) []] ]]

deriveToSchema :: DerivingOptions -> Name -> Q [Dec]
deriveToSchema opts tname = do
  tinfo <- reifyDatatype tname
  let
    context = do
      var <- datatypeVarsTypes tinfo
      [ConT ''Typeable `AppT` var, ConT ''ToSchema `AppT` var]
  body <- AppE (VarE 'genericDeclareNamedSchema)
      . AppE (VarE 'derivingOptionsToSchemaOptions) <$> lift opts
  pure [InstanceD Nothing context
    (ConT ''ToSchema `AppT` datatypeType tinfo)
    [ FunD 'declareNamedSchema [Clause [] (NormalB body) []] ]]

deriveArbitrary :: Name -> Q [Dec]
deriveArbitrary t = do 
  tinfo <- reifyDatatype t
  let 
    typ = datatypeType tinfo
    ctx = datatypeVarsTypes tinfo >>= \tv -> 
      [ ConT ''Arbitrary `AppT` tv
      , ConT ''Arg `AppT` typ `AppT` tv  ]
  pure [InstanceD Nothing ctx (ConT ''Arbitrary `AppT` typ)
    [ FunD 'arbitrary [Clause [] (NormalB $ VarE 'genericArbitrary) []]
    , FunD 'shrink [Clause [] (NormalB $ VarE 'genericShrink) []] ] ]
#endif

deriveApiFromJSON :: DerivingOptions -> Name -> Q [Dec]
deriveApiFromJSON = deriveFromJSON . derivingOptionsToJsonOptions

deriveApiToJSON :: DerivingOptions -> Name -> Q [Dec]
deriveApiToJSON = deriveToJSON . derivingOptionsToJsonOptions

generateParseJSON :: DerivingOptions -> Name -> Q Exp
generateParseJSON = mkParseJSON . derivingOptionsToJsonOptions

stripPrefix' :: String -> String -> String
stripPrefix' pfx = fromMaybe <*> stripPrefix pfx

