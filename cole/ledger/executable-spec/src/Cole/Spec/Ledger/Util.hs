{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cole.Spec.Ledger.Util
  ( mkShepardGens,
  )
where

import Language.Haskell.TH
import System.FilePath ((<.>), (</>))
import Test.Shepard (loadShepardDataFromFilePath)

loadGD :: String -> Q Exp
loadGD pfName =
  loadShepardDataFromFilePath relPath
  where
    -- data files we get at compile time are always relative to the project
    relPath = "src" </> "shepard_genomes" </> pfName <.> "genome"

-- | Take a name (e.g. "DELEG") and a list of `PredicateFailure`s in `renderPF`
-- form (see repo shepards-sts-breeder; STSExtra typeclass) (e.g.
-- ["UTXOW_InsufficientWitnesses"]) and returns `n+1` declarations, where n
-- is the length of the `pfNames` list. There will be 1 decl per element of
-- `pfNames`, which wraps a shepard mutation around the valid `SignalGenerator`.
-- The final decl is a toplevel list of the previously defined `SignalGenerator`s.
mkShepardGens :: String -> [String] -> Q [Dec]
mkShepardGens stsNameStr pfNames = do
  b <- isExtEnabled TypeApplications
  if b
    then body
    else error "TypeApplications required"
  where
    body :: Q [Dec]
    body = do
      pairs <- sequence (map (mkShepardDecls stsNameStr) pfNames)
      let shepardGenNames = map fst pairs
          shepardGenDecs = concatMap snd pairs
      let listName = mkName ("shepardGens" <> stsNameStr)
          listSigDec = SigD listName (AppT ListT sigGenTy)
          listDataDec =
            ValD
              (VarP listName)
              (NormalB (ListE (map VarE shepardGenNames)))
              []
      pure (shepardGenDecs <> [listSigDec, listDataDec])

    stsName = mkName stsNameStr

    sigGenTy :: Type
    sigGenTy =
      AppT
        (ConT (mkName "SignalGenerator"))
        (ConT stsName)

-- | Makes a top-level `Dec` for the ShepardData (loaded at TH evaluation time
-- from a file) and another top-level `Dec` for an invalid generator using the
-- ShepardData declaration.
mkShepardDecls :: String -> String -> Q (Name, [Dec])
mkShepardDecls stsNameStr pfNameStr = do
  decs <- mconcat <$> sequence [shepardDataDecs, shepardGenDecs]
  pure (ggName, decs)
  where
    genomeName = stsNameStr <> "_" <> pfNameStr
    gdName = mkName ("shepardData_" <> genomeName)
    ggName = mkName ("shepardGen_" <> genomeName)
    stsName = mkName stsNameStr

    shepardDataDecs = do
      body <- loadGD genomeName
      let sigDec =
            SigD
              gdName
              ( AppT
                  (ConT (mkName "ShepardData"))
                  (ConT (mkName "Bool"))
              )
      let dataDec = ValD (VarP gdName) (NormalB body) []
      pure [sigDec, dataDec]

    shepardGenDecs = do
      body <-
        [|
          tinkerWithSigGen @Bool @ $(pure (ConT stsName))
            $(pure (VarE gdName))
          |]
      let sigDec =
            SigD
              ggName
              ( AppT
                  (ConT (mkName "SignalGenerator"))
                  (ConT stsName)
              )
      let dataDec = ValD (VarP ggName) (NormalB body) []
      pure [sigDec, dataDec]
