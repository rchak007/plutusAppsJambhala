# Decentralized Seed Phrase Manager



![image-20231222150647167](Images/image-20231222150647167.png)



This Repo has the code for developing the Cardano Plutus OnChain part for the Decentralized seed phrase manager project. For details refer to the below repo:

https://github.com/rchak007/decentralSeedRecover



Jambhala framework was used to develop the plutus code.

https://github.com/iburzynski/jambhala was used as template to start.





## OnChain Code

https://github.com/rchak007/plutusAppsJambhala/blob/main/src/Contracts/SeedPhraseManager.hs



## Datum

```haskell
data SeedPhraseDatum = SeedPhraseDatum
  { encryptedWordsWithIndex :: BuiltinByteString,
    ownerPKH :: PubKeyHash
  }

unstableMakeIsData ''SeedPhraseDatum

```

## Parameterized 

```haskell
data SeedPhraseParam = SeedPhraseParam
  { pInfoHash :: BuiltinByteString
  }
  deriving (Haskell.Show)
```



## Redeemer

```haskell
data SeedPhraseRedeem = Unit ()
```



## Validator

```haskell

mkRequestValidator :: SeedPhraseParam -> SeedPhraseDatum -> () -> ScriptContext -> Bool
mkRequestValidator sParam dat _ ctx =
  traceIfFalse "signedByOwner: Not signed by ownerPKH" signedByOwner
  where
    txinfo :: TxInfo
    txinfo = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy txinfo $ ownerPKH dat
{-# INLINEABLE mkRequestValidator #-}
```





## Deploy



https://github.com/rchak007/plutusAppsJambhala/blob/main/src/Deploy.hs















