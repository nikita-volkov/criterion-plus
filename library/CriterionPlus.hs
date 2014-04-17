-- |
-- Using this library you can create executables with benchmarks.
-- It automatically implants a command-line options parser and info generator.
-- 
-- Here is an example of how this library is supposed to be used:
-- 
-- > import CriterionPlus
-- > import qualified SomeMySQLLib as MySQL
-- > import qualified SomePostgreSQLLib as PostgreSQL
-- > 
-- > main = 
-- >   benchmark $ do
-- >     standoff "Inserting rows" $ do
-- >       subject "MySQL" $ do
-- >         -- Exclude the "setup" phase from measurement:
-- >         pause
-- >         connection <- liftIO $ MySQL.openConnection
-- >         -- Measure what we want:
-- >         continue
-- >         liftIO $ MySQL.insertAThousandRows connection
-- >         -- Exclude the "teardown" phase from measurement:
-- >         pause
-- >         liftIO $ MySQL.closeConnection connection
-- >       subject "PostgreSQL" $ do
-- >         -- This is how we can exclude the "setup" phase from monad transformers:
-- >         pause
-- >         PostgreSQL.runSession $ do
-- >           lift $ continue
-- >           PostgreSQL.insertAThousandRows
-- >           -- Exclude "teardown":
-- >           lift $ pause
-- >     -- Each standoff generates an independent report file:
-- >     standoff "Querying" $ do
-- >       subject "MySQL" $ error "So on..."
-- >       subject "PostgreSQL" $ error "So on..."
-- 
module CriterionPlus
(
  -- * IO
  benchmark,
  -- * Benchmark
  Benchmark,
  standoff,
  -- * Standoff
  Standoff,
  group,
  subject,
  -- * Subject
  Subject,
  continue,
  pause,
  -- ** Evaluation
  whnf,
  nf,
  nfIO,
  whnfIO,
  -- * Shared Types
  Name,
)
where

import CriterionPlus.Monads




