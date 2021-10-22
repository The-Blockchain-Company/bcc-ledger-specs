module Main where

import Bcc.Ledger.Aurum.TxInfo (debugZerepoch)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = getArgs >>= (print . debugZerepoch . head)
