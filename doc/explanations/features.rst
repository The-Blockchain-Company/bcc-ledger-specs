What is a native token and how does it compare to bcc and ERC20?
==================================================================

Terminology
###########

+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Term             | Meaning                                               | Examples                                                  |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Token            | The basic accounting unit for a single Asset type.    | 1 entropic                                                |
|                 |                                                       |                                                           |
|                 |                                                       | 1 USDT                                                    |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Value            | The type which is used by the ledger to represent     | 1 entropic and 3 USDT                                     |
|                 |                                                       |                                                           |
|                 | tokens that are tracked. Pre-Jen, Value is just a    | 1000 entropic and 100 USDT                                |
|                 |                                                       |                                                           |
|                 | quantity of Bcc; post-Jen, Value is a bundle of      | Just 10 entropic.                                         |
|                 |                                                       |                                                           |
|                 | tokens with associated quantities.                    |                                                           |
|                 |                                                       |                                                           |
|                 | The ledger never talks about bare “tokens”, rather it |                                                           |
|                 |                                                       |                                                           |
|                 | always uses Value.                                    |                                                           |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Token bundle     | Another word for Value.                               |                                                           |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Script           | A piece of code which runs on the Bcc blockchain. |                                                           |
|                 |                                                       |                                                           |
|                 | Used to support “smart contracts”.                    |                                                           |
|                 |                                                       |                                                           |
|                 | Bcc will support several scripting languages: in  |                                                           |
|                 |                                                       |                                                           |
|                 | Jen this will just be the multisig language, in      |                                                           |
|                 |                                                       |                                                           |
|                 | subsequent eras, it will include Zerepoch Core as well. |                                                           |
|                 |                                                       |                                                           |
|                 | A multisig script stating that the transaction is     |                                                           |
|                 |                                                       |                                                           |
|                 | authorized if it is signed by both Alice and Bob.     |                                                           |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Minting policy   | A script which determines whether a transaction is    | A multisig script stating the minting (burning) is        |
|                 |                                                       |                                                           |
|script           | allowed to mint or burn a particular currency.        | authorized if the transaction is signed by Alice.         |
|                 |                                                       |                                                           |
|                 |                                                       | Alice therefore has complete freedom to mint and burn     |
|                 |                                                       |                                                           |
|                 |                                                       | tokens as she desires.                                    |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Policy ID        | An identifier that uniquely identifies a currency.    |                                                           |
|                 |                                                       |                                                           |
|                 | The Policy ID is the hash of the minting policy       |                                                           |
|                 |                                                       |                                                           |
|                 | script for the currency.                              |                                                           |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Asset            | The type of a collection of equivalent token values   | bcc                                                       |
|                 |                                                       |                                                           |
|                 | in a single currency.  An asset is uniquely           | Sword                                                     |
|                 |                                                       |                                                           |
|                 | identified by an Asset ID.                            | Voting Shares                                             |
|                 |                                                       |                                                           |
|                 |                                                       | Non Voting Shares                                         |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Asset name       | A name for a particular asset type inside a currency. | ``"bcc"``                                                 |
|                 |                                                       |                                                           |
|                 | Asset names must be unique within a currency, but do  | ``"Sword"``                                               |
|                 |                                                       |                                                           |
|                 | not need to be unique across different currencies.    | ``“Voting shares”``                                       |
|                 |                                                       |                                                           |
|                 |                                                       | ``“Non-voting shares”``                                   |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+
|Asset ID         | A pair of a policy ID and and asset name. Uniquely    | ``(<hash of a script>, “Sword”)``                         |
|                 |                                                       |                                                           |
|                 | identifies an asset.                                  |                                                           |
+-----------------+-------------------------------------------------------+-----------------------------------------------------------+


How do native tokens compare to Bcc?
####################################

Native tokens behave the same as bcc in most ways.
However, Bcc is the “principal” currency of Bcc, and is the only one which
can be used for some special purposes, such as paying fees.


+-----------------------------------+-------+-----------------+-------------------------------------------------+
|                                   | Bcc   | Native tokens   | Comment                                         |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be sent in transactions?       | Y     | Y               |                                                 |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be kept in UTXO outputs        | Y     | Y               |                                                 |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be locked with script outputs  | Y     | Y               |                                                 |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be sent to an exchange address | Y     | Y               |                                                 |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be minted/burned               | N     | Y               | Bcc cannot be created or destroyed,             |
|                                   |       |                 |                                                 |
|                                   |       |                 | its policy ID does not correspond to a script   |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be used to pay fees, receive   | Y     | N               | Bcc is the only currency which can be used for  |
|                                   |       |                 |                                                 |
|rewards, etc.                      |       |                 | fees and rewards.                               |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be used to cover the minimum   | Y     | N               | Bcc is the only currency which can be used      |
|                                   |       |                 |                                                 |
|UTXO value                         |       |                 | for deposits.                                   |
+-----------------------------------+-------+-----------------+-------------------------------------------------+

How do native tokens compare to ERC-20 tokens?
###############################################

ERC-20 is a token standard on Ethereum, and the most popular way to issue tokens on a blockchain today.



+------------------------------------+-----------------------------------------+--------------------------------------------+
|                                    | ERC-20                                  |Native tokens                               |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Backing blockchain                  | Ethereum                                |Bcc                                     |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Relationship to the blockchain      | A contract standard, users copy-paste   |Not a standard. Most functionality          |
|                                    |                                         |                                            |
|                                    | the standard code and modify it.        |is built into the ledger itself.            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Controlled by                       | A Solidity smart contract               |A minting policy script in any scripting    |
|                                    |                                         |                                            |
|                                    |                                         |language supported by Bcc               |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Requires a smart contract           | Y                                       |Y                                           |
|                                    |                                         |                                            |
|to mint/burn?                       |                                         |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Minting logic can be customized?    | Y                                       |Y                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Requires a smart contract           | Y                                       |N                                           |
|                                    |                                         |                                            |
|to transfer?                        |                                         |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Can be used by other smart          |                                         |                                            |
|                                    |                                         |                                            |
|contracts without special support?  | N                                       |Y                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Can be transferred alongside other  | N                                       |Y                                           |
|                                    |                                         |                                            |
|tokens?                             |                                         |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Transfer logic provided by?         | Copy-pasting from the ERC-20 template   |The Bcc ledger itself                   |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Transfer logic can be customized?   | Y                                       |N                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Requires special fees to transfer?  | Y                                       |N                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Requires additional event-handling  | Y                                       |N                                           |
|                                    |                                         |                                            |
|logic to track transfers?           |                                         |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Supports non-fungible tokens?       | N                                       |Y                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Human readable metadata             | Provided by the operating smart         |Provided by the off-chain metadata server   |
|                                    |                                         |                                            |
|                                    | contract                                |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+


Security
####################

ERC-20 tokens have proven vulnerable to a wide range of security issues, most of which are not present for Native Tokens.

+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|                                        |ERC-20   |Native tokens  |Comment                                                               |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|User errors in copying standard code    |Y        |N              |All shared functionality is provided by the ledger                    |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|Over-/under-flow vulnerabilities        |Y        |N              |Bcc’s scripting languages don’t have fixed-size integers          |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|Unprotected functions                   |Y        |N              |User code is called only in very specific cases, to validate minting. |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|Denial of service via gas price attacks |Y        |N              |Denial of service attacks on the entire system are still possible.    |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
