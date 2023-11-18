{-# LANGUAGE NumericUnderscores #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Deploy where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..), fromPlutusData)
import Codec.Serialise (serialise)
import qualified Contracts.SeedPhraseManager as OnChain
import qualified Data.Aeson as DataAeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.String as S
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import PlutusTx.Builtins as Builtins
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Utilities.Serialise (writeCodeToFile, writeValidatorToFile)

main :: IO ()
main = do
  writeInitDatum
  writeContractDatum

  _ <- writeValidatorScript
  _ <- writeLucidValidatorScript

  return ()

basePath :: FilePath
basePath = "./assets/"

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs) = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs) = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n) = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs) = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeJSONFromPlutus :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSONFromPlutus file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . LedgerApiV2.toData

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeMintingValidator :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unMintingPolicyScript

writeInitDatum :: IO ()
writeInitDatum = writeJSON (basePath ++ "datum/unit.json") ()

writeContractDatum :: IO ()
writeContractDatum =
  let contributor = seedPhraseDatum
      d = PlutusTx.toBuiltinData contributor
   in writeJSON (basePath ++ "datum/seed-phrase-datum.json") d

readJSON :: FilePath -> IO LBS.ByteString
readJSON = LBS.readFile

ownerPubkeyhash :: LedgerApiV2.PubKeyHash
ownerPubkeyhash = LedgerApiV2.PubKeyHash $ LedgerApiV2.getLedgerBytes $ S.fromString "56370e4a6639f921c3beafd192d6e168aeee8bcf62b3af8689d819d0"

encryptedWordsWithIndex :: Builtins.BuiltinByteString
encryptedWordsWithIndex = stringToBuiltinByteString "56370e4a6639f921c3beafd192d6e168aeee8bcf62b3af8689d819d0"

personalInfoHash :: Builtins.BuiltinByteString
personalInfoHash = stringToBuiltinByteString "56370e4a6639f921c3beafd192d6e168aeee8bcf62b3af8689d819d0"

seedPhraseDatum :: OnChain.SeedPhraseDatum
seedPhraseDatum =
  OnChain.SeedPhraseDatum
    { OnChain.encryptedWordsWithIndex = encryptedWordsWithIndex,
      OnChain.ownerPKH = ownerPubkeyhash
    }

seedPhraseParam :: OnChain.SeedPhraseParam
seedPhraseParam =
  OnChain.SeedPhraseParam
    { OnChain.pInfoHash = personalInfoHash
    }

writeValidatorScript :: IO ()
writeValidatorScript = writeValidatorToFile (basePath ++ "plutus-scripts/seed-phrase.plutus") $ OnChain.requestValidator seedPhraseParam

writeLucidValidatorScript :: IO ()
-- writeLucidValidatorScript = writeCodeToFile (basePath ++ "plutus-scripts/lucid-seed-phrase.plutus") OnChain.validatorCode
-- i added below
writeLucidValidatorScript = writeCodeToFile (basePath ++ "plutus-scripts/lucid-seed-phrase.plutus") OnChain.compileScript