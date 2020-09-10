{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- State machine model tests for the pool database.
--
module Cardano.Pool.DB.StateMachine
    ( Command (..)
    , Success (..)
    , Response (..)
    ) where

import Prelude

import Cardano.Pool.DB.Model
    ( PoolDatabase (..), PoolErr, mCleanDatabase )
import Cardano.Wallet.Primitive.Types
    ( CertificatePublicationTime
    , EpochNo
    , PoolId
    , PoolRegistrationCertificate
    , PoolRetirementCertificate
    )
import Data.Bifunctor
    ( first )

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

data Command
    = CleanDatabase
    | ListPoolLifeCycleData
        EpochNo
    | ListRegisteredPools
    | ListRetiredPools
        EpochNo
    | PutPoolRegistration
        CertificatePublicationTime PoolRegistrationCertificate
    | PutPoolRetirement
        CertificatePublicationTime PoolRetirementCertificate
    | ReadPoolRegistration
        PoolId
    | ReadPoolRetirement
        PoolId
    | RemovePools
        [PoolId]
    | RemoveRetiredPools
        EpochNo
    deriving (Eq, Show)

data Success
    = Unit
        ()
    | PoolIds
        [PoolId]
    | RegistrationCertificates
        [PoolRegistrationCertificate]
    | RegistrationCertificatePublication
        (Maybe (CertificatePublicationTime, PoolRegistrationCertificate))
    | RetirementCertificates
        [PoolRetirementCertificate]
    | RetirementCertificatePublication
        (Maybe (CertificatePublicationTime, PoolRetirementCertificate))
    deriving (Eq, Show)

newtype Response = Response (Either PoolErr Success)

--------------------------------------------------------------------------------
-- Interpreter: mock implementation
--------------------------------------------------------------------------------

runMock :: Command -> PoolDatabase -> (Response, PoolDatabase)
runMock = \case
    CleanDatabase ->
        first (Response . fmap Unit) . mCleanDatabase


