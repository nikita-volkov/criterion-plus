{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.QuickCheck.Monadic
import CriterionPlus.Prelude.Basic
import CriterionPlus.Prelude.Transformers
import qualified CriterionPlus.Monads as M
import qualified Criterion.Environment as C
import qualified Data.Vector.Unboxed as V

main = htfMain $ htf_thisModulesTests


-- * Tests
-------------------------

test_aMoreComplexPureBenchmarkTakesLonger = do
  replicateM_ 10 $ do
    [a, b] <- runStandoff 100 $ do
      M.subject "shorter" $ do
        M.nf pureCalc 1
      M.subject "longer" $ do
        M.nf pureCalc 4
    assertEqual LT $ compareResults a b

test_aMoreComplexImpureBenchmarkTakesLonger = do
  replicateM_ 10 $ do
    [a, b] <- runStandoff 100 $ do
      M.subject "shorter" $ do
        M.nfIO $ return $ pureCalc 1
      M.subject "longer" $ do
        M.nfIO $ return $ pureCalc 4
    assertEqual LT $ compareResults a b

test_timerControlsDontAffectTheResults = unitTestPending ""


-- * Utils
-------------------------

runStandoff :: Int -> M.Standoff a -> IO [M.SubjectReport]
runStandoff samples = M.runStandoff 1 "test-standoff" env settings where
  -- env = unsafePerformIO $ M.runBenchmark settings $ M.Benchmark $ lift $ ask
  env = C.Environment 0 0
  settings = M.Settings "dist/test" samples

compareResults :: M.SubjectReport -> M.SubjectReport -> Ordering
compareResults = on compare $ \(_, v, _, _) -> V.sum v

pureCalc :: Int -> [Int]
pureCalc a = map (+a) $ enumFromTo 0 $ 10^3*a

apxEqual :: Double -> Double -> Bool
apxEqual a b = a > b * 0.95 && a < b * 1.05
