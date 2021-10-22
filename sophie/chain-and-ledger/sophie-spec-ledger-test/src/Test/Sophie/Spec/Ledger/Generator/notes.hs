
sudo cp $(find $HOME/bcc/bcc-node/dist-newstyle/build -type f -name "bcc-cli") /usr/local/bin/bcc-cli

sudo cp $(find $HOME/bcc/bcc-node/dist-newstyle/build -type f -name "bcc-node") /usr/local/bin/bcc-node

-- | Select one random verification staking key from list of pairs of KeyPair.
pickStakeKey :: KeyPairs crypto -> Gen (VKey 'Staking crypto)
pickStakeKey keys = vKey . snd <$> QC.elements keys


mkOCert ::
  forall crypto r.
  (CC.Crypto crypto, Signable (DSIGN crypto) (OCertSignable crypto)) =>
  AllIssuerKeys crypto r ->
  Word64 ->
  KESPeriod ->
  OCert crypto
mkOCert pkeys n c0 =
  let (_, (_, vKeyHot)) = head $ hot pkeys
      KeyPair _vKeyCold sKeyCold = cold pkeys
   in OCert
        vKeyHot
        n
        c0
        (signedDSIGN @crypto sKeyCold (OCertSignable vKeyHot n c0))

vestedCoins ::
  (Era era) =>
  Ledger.TxId (Crypto era) ->
  [Core.TxOut era] ->
  UTxO era
vestedCoins vestedTxId outs =
  UTxO $
    Map.fromList [(TxIn genesisTxId idx, out) | (idx, out) <- zip [0 ..] outs]

-- | check for instantaneous rewards from Utxow.hs
  let vestedDelegates =
        Set.fromList $
          asWitness . vestedDelegKeyHash
            <$> Map.elems vestedMapping
      (WitHashes khAsSet) = witsKeyHashes
      genSig = eval (vestedDelegates ∩ khAsSet)
      mirCerts =
        StrictSeq.forceToStrict
          . Seq.filter isInstantaneousRewards
          . StrictSeq.fromStrict
          $ getField @"certs" txbody
      VestedDelegs vestedMapping = vestedDelegs
