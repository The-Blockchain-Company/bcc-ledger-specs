# bcc-crypto-wrapper

The cryptographic primitives used in Bcc

* Cryptographic hashing, using the [cryptonite] library.

* Secure generation of cryptographically random numbers and `ByteString`s.

* Hierarchical derivation functionality for Hierarchical Deterministic key
  creation, for the wallet.

* Cryptographic signing and signature checking.

* `To/FromCBOR` (see the `bcc-binary` package) instances for the
  cryptographic data types.

[cryptonite]: https://hackage.haskell.org/package/cryptonite
