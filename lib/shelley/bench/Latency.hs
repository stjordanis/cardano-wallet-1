{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude

import Cardano.BM.Data.LogItem
    ( LogObject )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( contramap, nullTracer )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.CLI
    ( Port (..) )
import Cardano.Startup
    ( withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiFee
    , ApiNetworkInformation
    , ApiStakePool
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.LatencyBenchShared
    ( LogCaptureFunc, fmtResult, fmtTitle, measureApiLogs, withLatencyLogging )
import Cardano.Wallet.Logging
    ( stdoutTextTracer, trMessage )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..) )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , Tracers
    , Tracers' (..)
    , nullTracers
    , serveWallet
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley )
import Cardano.Wallet.Shelley.Faucet
    ( initFaucet )
import Cardano.Wallet.Shelley.Launch
    ( RunningNode (..)
    , sendFaucetFundsTo
    , withCluster
    , withSystemTempDir
    , withTempDir
    )
import Control.Arrow
    ( first )
import Control.Concurrent.Async
    ( race_ )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM.TVar
    ( TVar )
import Control.Monad
    ( mapM_, replicateM, replicateM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Network.Wai.Middleware.Logging
    ( ApiLog (..) )
import Numeric.Natural
    ( Natural )
import System.Directory
    ( createDirectory )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( shouldBe )
import Test.Integration.Faucet
    ( shelleyIntegrationTestFunds )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , expectField
    , expectResponseCode
    , expectSuccess
    , expectWalletUTxO
    , faucetAmt
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , json
    , minUTxOValue
    , request
    , runResourceT
    , unsafeRequest
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

main :: forall t n. (t ~ Shelley, n ~ 'Mainnet) => IO ()
main = withUtf8Encoding $
    withLatencyLogging setupTracers $ \tracers capture ->
        withShelleyServer tracers $ \ctx -> do
            walletApiBench @t @n capture ctx
  where
    setupTracers :: TVar [LogObject ApiLog] -> Tracers IO
    setupTracers tvar = nullTracers
        { apiServerTracer = trMessage $ contramap snd (traceInTVarIO tvar) }

walletApiBench
    :: forall t (n :: NetworkDiscriminant). (t ~ Shelley, n ~ 'Mainnet)
    => LogCaptureFunc ApiLog ()
    -> Context t
    -> IO ()
walletApiBench capture ctx = do
    fmtTitle "Non-cached run"
    runWarmUpScenario

    fmtTitle "Latencies for 2 fixture wallets scenario"
    runScenario (nFixtureWallet 2)

    fmtTitle "Latencies for 10 fixture wallets scenario"
    runScenario (nFixtureWallet 10)

    {-- PENDING: We currently have a limited amount of available fixture
       wallets, so we can't just run a benchmark with 100 wallets in parallel.
    fmtTitle "Latencies for 100 fixture wallets scenario"
    runScenario (nFixtureWallet 100)
    --}

    fmtTitle "Latencies for 2 fixture wallets with 10 txs scenario"
    runScenario (nFixtureWalletWithTxs 2 10)

    fmtTitle "Latencies for 2 fixture wallets with 20 txs scenario"
    runScenario (nFixtureWalletWithTxs 2 20)

    fmtTitle "Latencies for 2 fixture wallets with 100 txs scenario"
    runScenario (nFixtureWalletWithTxs 2 100)

    fmtTitle "Latencies for 10 fixture wallets with 10 txs scenario"
    runScenario (nFixtureWalletWithTxs 10 10)

    fmtTitle "Latencies for 10 fixture wallets with 20 txs scenario"
    runScenario (nFixtureWalletWithTxs 10 20)

    fmtTitle "Latencies for 10 fixture wallets with 100 txs scenario"
    runScenario (nFixtureWalletWithTxs 10 100)

    fmtTitle "Latencies for 2 fixture wallets with 100 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 100)

    fmtTitle "Latencies for 2 fixture wallets with 200 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 200)

    fmtTitle "Latencies for 2 fixture wallets with 500 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 500)

    {-- PENDING: Fee estimation is taking way longer than it should and this
       scenario is not resolving in a timely manner.

    To be re-enabled once #2006 & #2051 are fixed.

    fmtTitle "Latencies for 2 fixture wallets with 1000 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 1000)
    --}
    fmtTitle "Latencies for 2 fixture wallets with 1000 utxos scenario"
    fmtTitle "CURRENTLY DISABLED. SEE #2006 & #2051"
  where

    -- Creates n fixture wallets and return two of them
    nFixtureWallet n = do
        wal1 : wal2 : _ <- replicateM n (fixtureWallet ctx)
        pure (wal1, wal2)

    -- Creates n fixture wallets and send 1-ada transactions to one of them
    -- (m times). The money is sent in batches (see batchSize below) from
    -- additionally created source fixture wallet. Then we wait for the money
    -- to be accommodated in recipient wallet. After that the source fixture
    -- wallet is removed.
    nFixtureWalletWithTxs n m = do
        (wal1, wal2) <- nFixtureWallet n

        let amt = minUTxOValue
        let batchSize = 10
        let whole10Rounds = div m batchSize
        let lastBit = mod m batchSize
        let amtExp val = ((amt * fromIntegral val) + faucetAmt) :: Natural
        let expInflows =
                if whole10Rounds > 0 then
                    [x*batchSize | x<-[1..whole10Rounds]] ++ [lastBit]
                else
                    [lastBit]
        let expInflows' = filter (/=0) expInflows

        mapM_ (repeatPostTx wal1 amt batchSize . amtExp) expInflows'
        pure (wal1, wal2)

    nFixtureWalletWithUTxOs n utxoNumber = do
        let utxoExp = replicate utxoNumber minUTxOValue
        wal1 <- fixtureWalletWith @n ctx utxoExp
        (_, wal2) <- nFixtureWallet n

        eventually "Wallet balance is as expected" $ do
            rWal1 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wal1) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                        (#balance . #getApiT . #available . #getQuantity)
                        (`shouldBe` (minUTxOValue * (fromIntegral utxoNumber)))
                ]

        rStat <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley wal1) Default Empty
        expectResponseCode HTTP.status200 rStat
        expectWalletUTxO (fromIntegral <$> utxoExp) (snd rStat)
        pure (wal1, wal2)

    repeatPostTx wDest amtToSend batchSize amtExp = do
        wSrc <- fixtureWallet ctx
        replicateM_ batchSize
            (postTx (wSrc, Link.createTransaction @'Shelley, fixturePassphrase) wDest amtToSend)
        eventually "repeatPostTx: wallet balance is as expected" $ do
            rWal1 <- request @ApiWallet  ctx (Link.getWallet @'Shelley wDest) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                    (#balance . #getApiT . #available . #getQuantity)
                    (`shouldBe` amtExp)
                ]
        rDel <- request @ApiWallet  ctx (Link.deleteWallet @'Shelley wSrc) Default Empty
        expectResponseCode HTTP.status204 rDel
        pure ()

    postTx (wSrc, postTxEndp, pass) wDest amt = do
        (_, addrs) <- unsafeRequest @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley wDest) Empty
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{pass}
            }|]
        r <- request @(ApiTransaction n) ctx (postTxEndp wSrc) Default payload
        expectResponseCode HTTP.status202 r
        return r

    runScenario scenario = runResourceT $ scenario >>= \(wal1, wal2) -> liftIO $ do
        t1 <- measureApiLogs capture
            (request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty)
        fmtResult "listWallets        " t1

        t2 <- measureApiLogs capture
            (request @ApiWallet  ctx (Link.getWallet @'Shelley wal1) Default Empty)
        fmtResult "getWallet          " t2

        t3 <- measureApiLogs capture
            (request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley wal1) Default Empty)
        fmtResult "getUTxOsStatistics " t3

        t4 <- measureApiLogs capture
            (request @[ApiAddress n] ctx (Link.listAddresses @'Shelley wal1) Default Empty)
        fmtResult "listAddresses      " t4

        t5 <- measureApiLogs capture
            (request @[ApiTransaction n] ctx (Link.listTransactions @'Shelley wal1) Default Empty)
        fmtResult "listTransactions   " t5

        (_, addrs) <- unsafeRequest @[ApiAddress n] ctx (Link.listAddresses @'Shelley wal2) Empty
        let amt = minUTxOValue
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]
        t6 <- measureApiLogs capture $ request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wal1) Default payload
        fmtResult "postTransactionFee " t6

        t7 <- measureApiLogs capture $ request @[ApiStakePool] ctx
            (Link.listStakePools arbitraryStake) Default Empty

        fmtResult "listStakePools     " t7

        t8 <- measureApiLogs capture $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        fmtResult "getNetworkInfo     " t8

        pure ()
     where
        arbitraryStake :: Maybe Coin
        arbitraryStake = Just $ ada 10000
            where ada = Coin . (1000*1000*)

    runWarmUpScenario = do
        -- this one is to have comparable results from first to last measurement
        -- in runScenario
        t <- measureApiLogs capture $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        fmtResult "getNetworkInfo     " t
        pure ()

withShelleyServer
    :: Tracers IO
    -> (Context Shelley -> IO ())
    -> IO ()
withShelleyServer tracers action = do
    ctx <- newEmptyMVar
    let setupContext np wAddr = do
            let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
            let sixtySeconds = 60*1000*1000 -- 60s in microseconds
            manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                { managerResponseTimeout =
                    responseTimeoutMicro sixtySeconds
                })
            faucet <- initFaucet
            putMVar ctx $ Context
                { _cleanup = pure ()
                , _manager = manager
                , _walletPort = Port . fromIntegral $ unsafePortNumber wAddr
                , _faucet = faucet
                , _feeEstimator = \_ -> error "feeEstimator not available"
                , _networkParameters = np
                , _target = Proxy
                , _poolGarbageCollectionEvents =
                    error "poolGarbageCollectionEvents not available"
                }
    race_ (takeMVar ctx >>= action) (withServer setupContext)

  where
    withServer act = withSystemTempDir nullTracer "latency" $ \dir -> do
            let db = dir </> "wallets"
            createDirectory db
            withCluster
                nullTracer
                Error
                []
                dir
                Nothing
                onByron
                (afterFork dir)
                (onClusterStart act dir)
    onByron _ = pure ()
    afterFork dir _ = do
        let encodeAddr = T.unpack . encodeAddress @'Mainnet
        let addresses = map (first encodeAddr) shelleyIntegrationTestFunds
        sendFaucetFundsTo stdoutTextTracer dir addresses

    onClusterStart act dir (RunningNode socketPath block0 (gp, vData)) = do
        -- NOTE: We may want to keep a wallet running across the fork, but
        -- having three callbacks like this might not work well for that.
        withTempDir nullTracer dir "wallets" $ \db -> do
            serveWallet @(IO Shelley)
                (SomeNetworkDiscriminant $ Proxy @'Mainnet)
                tracers
                (SyncTolerance 10)
                (Just db)
                Nothing
                "127.0.0.1"
                ListenOnRandomPort
                Nothing
                Nothing
                socketPath
                block0
                (gp, vData)
                (act gp)
