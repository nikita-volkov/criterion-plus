-- |
-- Builder monads.
module CriterionPlus.Monads where

import CriterionPlus.Prelude.Basic
import CriterionPlus.Prelude.Data
import CriterionPlus.Prelude.Transformers
import qualified Criterion.Config as C
import qualified Criterion.Report as C
import qualified Criterion.Types as C
import qualified Criterion.Environment as C
import qualified Criterion.Measurement as C
import qualified Criterion.IO.Printf as C
import qualified Criterion.Analysis as C
import qualified Criterion.Monad as C
import qualified Statistics.Resampling.Bootstrap as S
import qualified Statistics.Types as S
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import qualified Options.Applicative as O
import qualified CriterionPlus.CSI as CSI
import qualified System.IO


-- * Shared Types
-------------------------

type Name = StrictText

data Settings = 
  Settings {
    reportsDir :: FilePath,
    samplesAmount :: Int
  }

type Environment = C.Environment


-- * IO
-------------------------

-- |
-- Parse the command line options and run the benchmark.
-- 
benchmark :: Benchmark () -> IO ()
benchmark b = do
  settings <- O.execParser $ O.info (O.helper <*> parser) $ O.fullDesc
  runBenchmark settings b
  where
    parser = Settings <$> reportsDir <*> samplesAmount where
      reportsDir = 
        O.nullOption $ 
          O.eitherReader readValue <>
          O.long "reportsDir" <>
          O.short 'd' <>
          O.value "." <>
          O.showDefault <>
          O.help "A path to directory to save all the reports in"
        where
          readValue s = let
            p = FS.decodeString s
            in if unsafePerformIO (FS.isDirectory p)
              then Right p
              else Left $ "The path does not exist or is not a directory: " <> s
      samplesAmount = 
        O.option $ 
          O.long "samplesAmount" <>
          O.short 's' <>
          O.value 100 <>
          O.eitherReader (validateValue . read) <>
          O.showDefault <>
          O.help "How many times to sample the benchmarks"
        where
          validateValue a = if a < 3 
            then Left "A value is lower than the minimum of 3"
            else Right a


-- * Benchmark
-------------------------

-- |
-- A root of the \"criterion-plus\" monad stack.
-- 
-- Use this monad to declare 'standoff's.
-- You can also lift a shared initialization into it using 'liftIO'.
newtype Benchmark a =
  Benchmark (StateT Int (ReaderT Environment (ReaderT Settings IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runBenchmark :: Settings -> Benchmark a -> IO a
runBenchmark settings (Benchmark m) = do
  env <- runCriterion settings $ C.measureEnvironment
  flip runReaderT settings $ flip runReaderT env $ flip evalStateT 1 $ m

-- |
-- Declare a named comparison of multiple subjects.
-- This will generate a separate report file.
standoff :: Name -> Standoff () -> Benchmark ()
standoff name s = Benchmark $ do
  i <- state $ id &&& succ
  env <- lift $ ask
  settings <- lift $ lift $ ask
  liftIO $ void $ runStandoff i name env settings s


-- * Standoff
-------------------------

-- |
-- A monad for declaration of independent comparison,
-- which will produce a dedicated report file.
-- 
-- Use this monad to 'group' and declare 'subject's.
-- You can also lift a shared initialization into it using 'liftIO'.
newtype Standoff a =
  Standoff (StateT [SubjectReport] (ReaderT Group (ReaderT Environment (ReaderT Settings IO))) a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | A reverse list of names of nested group declarations.
type Group = [Name]

runStandoff :: Int -> Name -> Environment -> Settings -> Standoff a -> IO [SubjectReport]
runStandoff i name env settings (Standoff m) = do
  reports <- 
    fmap reverse $
    flip runReaderT settings $ flip runReaderT env $ flip runReaderT [] $ flip execStateT [] $ m
  renderHTML settings name reports file
  return reports
  where
    file = (reportsDir settings) <> (FS.decodeString $ uniqueName <> ".html")
    uniqueName = "standoff-" <> show i

-- |
-- Put the wrapped computations into a named group.
-- Can be nested.
group :: Name -> Standoff () -> Standoff ()
group name (Standoff m) = Standoff $ local (name :) m

-- |
-- Execute a named subject.
subject :: Name -> Subject a -> Standoff ()
subject name subj = Standoff $ do
  group <- lift $ ask
  env <- lift $ lift $ ask
  settings <- lift $ lift $ lift $ ask
  report <- liftIO $ runSubject name group env settings subj
  modify (report :)


-- * Subject
-------------------------

-- |
-- A monad, which wraps the benchmarking subject and controls its measurement.
newtype Subject a = 
  Subject (StateT SampleStartTime (StateT SampleTotalTime IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

instance MonadBaseControl IO Subject where
  newtype StM Subject a = SubjectStM (StM (StateT SampleStartTime (StateT SampleTotalTime IO)) a)
  liftBaseWith run =
    Subject $ liftBaseWith $ \runStateInBase -> 
    run $ \(Subject s) -> liftM SubjectStM $ runStateInBase s
  restoreM (SubjectStM s) = Subject $ restoreM s


type SubjectReport = (Name, S.Sample, C.SampleAnalysis, C.Outliers)

type SampleStartTime = Maybe Double
type SampleTotalTime = Double

runSubject :: Name -> Group -> Environment -> Settings -> Subject a -> IO SubjectReport
runSubject name group env settings subj = do
  putStrLnLT $ [lt|\nRunning a subject "%s"|] compositeName

  samplesVec <- collectSamplesVec

  analysis@C.SampleAnalysis{..} <- C.analyseSample 0.95 samplesVec (100 * 1000)

  reportEstimate "mean" anMean
  reportEstimate "std dev" anStdDev
  
  let outliers = C.classifyOutliers samplesVec
  reportOutliers outliers
  reportOutliersVariance anOutlierVar 

  return $ (compositeName, samplesVec, analysis, outliers)
  where
    reportEstimate header S.Estimate{..} = do
      putStrLnLT $
        [lt|%s: %s, lower bound: %s, upper bound: %s, confidence: %.3f|]
          (header :: LazyText)
          (C.secs estPoint)
          (C.secs estLowerBound)
          (C.secs estUpperBound)
          (estConfidenceLevel)
    reportOutliersVariance C.OutlierVariance{..} = do
      putStrLnLT $ [lt|variance introduced by outliers: %.3f%%|] (ovFraction * 100)
      putStrLnLT $ [lt|variance is %s by outliers|] $
        case ovEffect of
          C.Unaffected -> "unaffected" :: LazyText
          C.Slight -> "slightly inflated"
          C.Moderate -> "moderately inflated"
          C.Severe -> "severely inflated"
    reportOutliers = runCriterion settings . C.noteOutliers
    collectSamplesVec = do
      let amount = samplesAmount settings
      firstSample <- runSample
      let useFirstSample = firstSample > 0.2
      vec <- VM.new amount
      when useFirstSample $ VM.write vec 0 firstSample
      forM_ (enumFromTo (if useFirstSample then 1 else 0) (amount - 1)) $ \i -> do
        printTimeLeft firstSample (amount - i)
        sample <- runSample
        VM.write vec i sample
      V.unsafeFreeze vec
      where
        printTimeLeft sample amount = do
          putStrST $ 
            [st|Collecting %d more samples in %.1f s.|] 
              (amount) 
              (sample * fromIntegral amount)
          System.IO.hFlush System.IO.stdout
          putStr (CSI.eraseLineToBeginning <> CSI.cursorHorizontalAbsolute 0)
        runSample = do
          performGC
          (_, time) <- 
            (continue >> subj >> pause) |> 
            \(Subject m) -> flip runStateT 0 $ flip runStateT Nothing $ m
          return $ time - C.envClockCost env
    compositeName = T.intercalate "/" $ reverse $ name : group

-- | 
-- Continue the timer.
-- 
-- By default it is already running, 
-- so if you need to eclude something from the beginning of the subject
-- use 'pause'. E.g.:
-- 
-- > subject "MySQL" $ do
-- >   pause
-- >   connection <- liftIO $ openConnection
-- >   continue
-- >   liftIO $ workWithConnection connection
-- >   pause
-- >   liftIO $ closeConnection connection
-- 
continue :: Subject ()
continue = Subject $ do
  !time <- liftIO $ C.getTime
  put $ Just time

-- | Pause the timer.
pause :: Subject ()
pause = Subject $ do
  !time <- liftIO $ C.getTime
  get >>= \case
    Just startTime -> do
      lift $ modify (+ (time - startTime))
      put $ Nothing
    Nothing -> return ()

-- |
-- An adaptation of @Criterion.Types.'C.whnf'@.
whnf :: (a -> b) -> a -> Subject ()
whnf f x = liftIO $ void $ evaluate (f x)
{-# NOINLINE whnf #-}

-- |
-- An adaptation of @Criterion.Types.'C.nf'@.
nf :: NFData b => (a -> b) -> a -> Subject ()
nf f x = liftIO $ evaluate (rnf (f x))
{-# NOINLINE nf #-}

-- |
-- An adaptation of @Criterion.Types.'C.nfIO'@.
nfIO :: (NFData a) => IO a -> Subject ()
nfIO = Subject . liftIO . C.nfIO
{-# NOINLINE nfIO #-}

-- |
-- An adaptation of @Criterion.Types.'C.whnfIO'@.
whnfIO :: IO a -> Subject ()
whnfIO = Subject . liftIO . C.whnfIO
{-# NOINLINE whnfIO #-}


-- * Criterion Helpers
-------------------------

runCriterion :: Settings -> C.Criterion a -> IO a
runCriterion settings = 
  C.withConfig $ C.defaultConfig {
    C.cfgSamples = Last $ Just $ (samplesAmount settings)
  }

renderHTML :: Settings -> Name -> [SubjectReport] -> FilePath -> IO ()
renderHTML settings name reports file = do
  C.withConfig config $ C.report reports'
  where
    reports' = [C.Report i (cs n) s a o | i <- [0..] | (n, s, a, o) <- reports]
    config = 
      C.defaultConfig {
        C.cfgBanner = Last $ Just $ cs name,
        C.cfgReport = Last $ Just $ FS.encodeString file,
        C.cfgSamples = Last $ Just $ samplesAmount settings
      } 
