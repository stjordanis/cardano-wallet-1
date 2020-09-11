{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Dummy implementation of the database-layer, using 'MVar'. This may be good
-- for testing to compare with an implementation on a real data store, or to use
-- when compiling the wallet for targets which don't have SQLite.

module Cardano.Pool.DB.MVar
    ( newDBLayer
    ) where

import Prelude

import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists (..) )
import Cardano.Pool.DB.Model
    ( ModelOp
    , PoolDatabase
    , PoolErr (..)
    , emptyPoolDatabase
    , mCleanDatabase
    , mListPoolLifeCycleData
    , mListRegisteredPools
    , mListRetiredPools
    , mPutFetchAttempt
    , mPutPoolMetadata
    , mPutPoolProduction
    , mPutPoolRegistration
    , mPutPoolRetirement
    , mPutStakeDistribution
    , mReadCursor
    , mReadPoolLifeCycleStatus
    , mReadPoolMetadata
    , mReadPoolProduction
    , mReadPoolRegistration
    , mReadPoolRetirement
    , mReadStakeDistribution
    , mReadSystemSeed
    , mReadTotalProduction
    , mRemovePools
    , mRemoveRetiredPools
    , mRollbackTo
    , mUnfetchedPoolMetadataRefs
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, newMVar )
import Control.DeepSeq
    ( deepseq )
import Control.Exception
    ( Exception, throwIO )
import Control.Monad
    ( void )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Either
    ( fromRight )
import Data.Functor.Identity
    ( Identity )
import Data.Tuple
    ( swap )

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
newDBLayer :: TimeInterpreter Identity -> IO (DBLayer IO)
newDBLayer timeInterpreter = do
    db <- newMVar emptyPoolDatabase
    pure $ mkDBLayer db
  where
    mkDBLayer db = DBLayer {..}
      where
        readPoolRegistration =
            readPoolDB db . mReadPoolRegistration

        readPoolRetirement =
            readPoolDB db . mReadPoolRetirement

        putPoolProduction sl pool = ExceptT $
            pool `deepseq`
                alterPoolDB errPointAlreadyExists db (mPutPoolProduction sl pool)

        readPoolProduction =
            readPoolDB db . mReadPoolProduction timeInterpreter

        readTotalProduction =
            readPoolDB db mReadTotalProduction

        putStakeDistribution a0 a1 =
            void $ alterPoolDB (const Nothing) db (mPutStakeDistribution a0 a1)

        readStakeDistribution =
            readPoolDB db . mReadStakeDistribution

        readPoolProductionCursor =
            readPoolDB db . mReadCursor

        putPoolRegistration cpt cert = void
              $ alterPoolDB (const Nothing) db
              $ mPutPoolRegistration cpt cert

        readPoolLifeCycleStatus =
            readPoolDB db . mReadPoolLifeCycleStatus

        putPoolRetirement cpt cert = void
            $ alterPoolDB (const Nothing) db
            $ mPutPoolRetirement cpt cert

        unfetchedPoolMetadataRefs =
            readPoolDB db . mUnfetchedPoolMetadataRefs

        putFetchAttempt =
            void . alterPoolDB (const Nothing) db . mPutFetchAttempt

        listRegisteredPools =
            readPoolDB db mListRegisteredPools

        listRetiredPools =
            readPoolDB db . mListRetiredPools

        listPoolLifeCycleData =
            readPoolDB db . mListPoolLifeCycleData

        putPoolMetadata a0 a1 =
            void $ alterPoolDB (const Nothing) db (mPutPoolMetadata a0 a1)

        readSystemSeed =
            modifyMVar db (fmap swap . mReadSystemSeed)

        rollbackTo =
            void . alterPoolDB (const Nothing) db . mRollbackTo timeInterpreter

        removePools =
            void . alterPoolDB (const Nothing) db . mRemovePools

        removeRetiredPools =
            fmap (fromRight [])
                . alterPoolDB (const Nothing) db
                . mRemoveRetiredPools

        cleanDB =
            void $ alterPoolDB (const Nothing) db mCleanDatabase

        readPoolMetadata = readPoolDB db mReadPoolMetadata

        atomically = id

alterPoolDB
    :: (PoolErr -> Maybe err)
    -- ^ Error type converter
    -> MVar PoolDatabase
    -- ^ The database variable
    -> ModelOp a
    -- ^ Operation to run on the database
    -> IO (Either err a)
alterPoolDB convertErr db op = modifyMVar db (bubble . op)
  where
    bubble (Left e, db') = case convertErr e of
        Just e' -> pure (db', Left e')
        Nothing -> throwIO $ MVarPoolDBError e
    bubble (Right a, db') = pure (db', Right a)

readPoolDB
    :: MVar PoolDatabase
    -- ^ The database variable
    -> ModelOp a
    -- ^ Operation to run on the database
    -> IO a
readPoolDB db op =
    alterPoolDB Just db op >>= either (throwIO . MVarPoolDBError) pure

errPointAlreadyExists
    :: PoolErr
    -> Maybe ErrPointAlreadyExists
errPointAlreadyExists (PointAlreadyExists slotid) =
    Just (ErrPointAlreadyExists slotid)

newtype MVarPoolDBError = MVarPoolDBError PoolErr
    deriving (Show)

instance Exception MVarPoolDBError
