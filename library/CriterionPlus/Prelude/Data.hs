module CriterionPlus.Prelude.Data
(
  module Exports,
  putStrLnLT,
  putStrLnST,
  putStrLT,
  putStrST,
)
where

-- string-conversions
-------------------------
import Data.String.Conversions as Exports hiding (LT, ST)

-- system-filepath
-------------------------
import Filesystem.Path as Exports (FilePath)

-- th-printf
-------------------------
import Text.Printf.TH as Exports


-- Custom
-------------------------
import CriterionPlus.Prelude.Basic
import qualified Data.Text.IO
import qualified Data.Text.Lazy.IO

putStrLnLT :: LazyText -> IO ()
putStrLnLT = Data.Text.Lazy.IO.putStrLn

putStrLT :: LazyText -> IO ()
putStrLT = Data.Text.Lazy.IO.putStr

putStrLnST :: StrictText -> IO ()
putStrLnST = Data.Text.IO.putStrLn

putStrST :: StrictText -> IO ()
putStrST = Data.Text.IO.putStr
