-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Contracts.SeedPhraseManager (referenceSerialized) where

-- ( SeedPhraseParam (..),
--   SeedPhraseDatum (..),
--   requestValidator,
--   validatorCode,
-- )

-- import           Data.Maybe                (fromJust)

-- import           Plutus.V1.Ledger.Interval (contains)
-- import           Plutus.V1.Ledger.Address  ( scriptHashAddress, toValidatorHash )
-- import           Plutus.V1.Ledger.Value    (AssetClass, flattenValue)
-- import qualified Plutus.V1.Ledger.Scripts as Scripts

-- import Jambhala.Utils

-- import Utilities (wrap)

-- chuck added

-- i added
-- import qualified Plutus.V2.Ledger.Api as Api

import Cardano.Api
import Codec.Serialise (serialise)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Jambhala.Plutus
import Jambhala.Utils
import Ledger (unValidatorScript)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as ScriptsV1
import Plutus.Script.Utils.V2.Scripts qualified as V2UtilsScripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as Scripts
import Plutus.V2.Ledger.Api
  ( BuiltinByteString,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo (),
    UnsafeFromData (unsafeFromBuiltinData),
    Validator,
    fromCompiledCode,
    mkValidatorScript,
    unValidatorScript,
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx (CompiledCode, applyCode, compile, liftCode, makeLift, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude hiding (Semigroup (..))
import Utilities.PlutusTx (wrap)
import Prelude qualified as Haskell

-- import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts

-- Parameter is hash for personal information
-- String for datum should be encrypted with hash for the 23 words + an index of the 24th word --(1 word not encrypted.(USer keeps this for themselves.))
-- PubKeyHash as Datum as well.

data SeedPhraseRedeem = Unit ()

unstableMakeIsData ''SeedPhraseRedeem

data SeedPhraseDatum = SeedPhraseDatum
  { encryptedWordsWithIndex :: BuiltinByteString,
    ownerPKH :: PubKeyHash
  }

unstableMakeIsData ''SeedPhraseDatum

dt1 :: SeedPhraseDatum
dt1 =
  SeedPhraseDatum
    { encryptedWordsWithIndex = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d",
      ownerPKH = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"
    }

data SeedPhraseParam = SeedPhraseParam
  { pInfoHash :: BuiltinByteString
  }
  deriving (Haskell.Show)

sp1 :: SeedPhraseParam
sp1 = SeedPhraseParam {pInfoHash = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"}

unstableMakeIsData ''SeedPhraseParam -- This is to instantiate the IsData class
makeLift ''SeedPhraseParam

mkRequestValidator :: SeedPhraseParam -> SeedPhraseDatum -> () -> ScriptContext -> Bool
mkRequestValidator sParam dat _ ctx =
  traceIfFalse "signedByOwner: Not signed by ownerPKH" signedByOwner
  where
    txinfo :: TxInfo
    txinfo = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy txinfo $ ownerPKH dat
{-# INLINEABLE mkRequestValidator #-}

---------------------------------------------------------------------------------------------------
------------------------------------ COMPILE VALIDATOR --------------------------------------------

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: SeedPhraseParam -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrap . mkRequestValidator

-- mkWrappedValidator = mkUntypedValidator . mkRequestValidator

requestValidator :: SeedPhraseParam -> Validator
requestValidator cp =
  mkValidatorScript $
    $$(PlutusTx.compile [||mkWrappedValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode cp

-- Begin my additions from ParamVesting.hs

untypedLambda :: SeedPhraseParam -> UntypedValidator -- BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedLambda sparam = mkUntypedValidator $ mkRequestValidator sparam
-- untypedLambda sparam = mkUntypedValidator $ mkRequestValidator sparam
{-# INLINEABLE untypedLambda #-}

-- | Declare contract synonym with unique symbolic identifier.
type SeedPhraseManager = ValidatorContract "seedphrasemanager"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compileScript :: SeedPhraseParam -> SeedPhraseManager
compileScript spmParam =
  mkValidatorContract
    ( $$(compile [||untypedLambda||]) -- compile the untyped parameterized validator
        `applyCode` liftCode spmParam -- apply the result to the "lifted" argument
    )

-- https://github.com/spacebudz/wormhole/blob/main/src/onchain.hs
--    the referenceInstance and referenceSerialized for parameterized .plutus was taken from wormhole onchain code
-- referenceInstance :: Scripts.Validator
referenceInstance :: Validator
-- referenceInstance = Api.Validator $ Api.fromCompiledCode $$(PlutusTx.compile [||wrap||])
referenceInstance = Validator $ fromCompiledCode $$(PlutusTx.compile [||wrap||])
  where
    -- wrap l1 = Scripts.mkUntypedValidator $ mkRequestValidator (PlutusTx.unsafeFromBuiltinData l1)
    wrap l1 = mkUntypedValidator $ mkRequestValidator (PlutusTx.unsafeFromBuiltinData l1)

referenceSerialized :: Haskell.String
referenceSerialized =
  C.unpack $
    B16.encode $
      serialiseToCBOR
        -- ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unValidatorScript referenceInstance) :: PlutusScript PlutusScriptV2)
        ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ unValidatorScript referenceInstance) :: PlutusScript PlutusScriptV2)

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `jamb` CLI.
-- exports :: JambExports
-- exports =
--   export
--     -- (defExports compileScript)
--     (defExports referenceInstance)

-- (defExports appliedScript)
-- where
-- { -- 1. With cardano-node running, use the `calc-time` script from cardano-cli-guru to get a POSIX time value
--   --    (add the `--plus MINUTES` option, replacing MINUTES with a number of minutes to add).
--   -- 2. Replace the placeholder POSIXTime value below with your POSIX time value.
--   -- 3. Note the NEW SLOT value for later use in transaction construction.
--   dataExports = [(
--               SeedPhraseDatum {
-- encryptedWordsWithIndex = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d",
-- pInfoHash = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"} :: SeedPhraseDatum) `toJSONfile` "maturity"]
--   -- emulatorTest = test
-- }

-- The parameterized validator must be applied to a `PubKeyHash` argument before it can be exported.
-- appliedScript =
-- 4. Use the `key-hash` script from cardano-cli-guru to get the pubkey hash for a beneficiary address.
-- 5. Replace the placeholder hex string below with the beneficiary address pubkey hash.
-- compileScript sp1
-- compileScript
--   (SeedPhraseParam {pInfoHash = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"})

-- End of my additions

-- requestValHash :: SeedPhraseParam -> ValidatorHash
-- requestValHash = validatorHash requestValidator

-- address :: SeedPhraseParam -> Address
-- address = scriptHashAddress . requestValHash

{-# INLINEABLE mkWrappedValidatorLucid #-}
--                           PInfoHash    SeedPhraseDatum   redeemer       context
mkWrappedValidatorLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid pIHash = wrap $ mkRequestValidator cp
  where
    -- mkWrappedValidatorLucid pIHash = mkUntypedValidator $ mkRequestValidator cp

    cp =
      SeedPhraseParam
        { pInfoHash = unsafeFromBuiltinData pIHash
        }

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [||mkWrappedValidatorLucid||])

-- chuck added
-- validator :: V2UtilsScripts.Validator
-- validator = Scripts.validatorScript validatorCode

-- saveLucidCode :: IO ()
-- saveLucidCode = writeCodeToFile "assets/plutus-scripts/lucid-nft.plutus" nftCode

-- script :: SeedPhraseParam -> Plutus.Script
-- script = Plutus.unValidatorScript . validatorHash

-- scriptAsCbor :: SeedPhraseParam -> LBS.ByteString
-- scriptAsCbor = serialise . validatorHash

-- request :: SeedPhraseParam -> PlutusScript PlutusScriptV1
-- request = PlutusScriptSerialised . requestShortBs

-- requestShortBs :: SeedPhraseParam -> SBS.ShortByteString
-- requestShortBs = SBS.toShort . LBS.toStrict . scriptAsCbor

data DecentralSeed

instance Scripts.ValidatorTypes DecentralSeed where
  type RedeemerType DecentralSeed = SeedPhraseRedeem
  type DatumType DecentralSeed = SeedPhraseDatum

-- decentralSeedCompile :: SeedPhraseParam -> Scripts.TypedValidator DecentralSeed
-- decentralSeedCompile decentralSeedp =
--   Scripts.mkTypedValidator @DecentralSeed
--     ($$(PlutusTx.compile [||mkRequestValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode decentralSeedp)
--     $$(PlutusTx.compile [||wrap||])
--   where
--     wrap = ScriptsV1.mkUntypedValidator @SeedPhraseDatum @SeedPhraseRedeem

--------------------------------
-- validator :: SeedPhraseParam -> V2UtilsScripts.Validator
-- validator = Scripts.validatorScript . decentralSeedCompile
