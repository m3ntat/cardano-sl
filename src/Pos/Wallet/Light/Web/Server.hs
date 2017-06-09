{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API

module Pos.Wallet.Light.Web.Server
       ( walletServeWebLite
       , walletServerOuts
       ) where

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Data.Tagged                   (Tagged (..))
import           Ether                         (ask, local)
import qualified Ether
import           Mockable                      (Production, runProduction)
import           Network.Wai                   (Application)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import qualified STMContainers.Map             as SM
import           System.Wlog                   (usingLoggerName)
import           Universum

import           Pos.Block.BListener           (runBListenerStub)
import           Pos.Communication             (NodeId)
import           Pos.Communication.PeerState   (PeerStateTag, runPeerStateRedirect)
import           Pos.Communication.Protocol    (SendActions, hoistSendActions)
import           Pos.Context                   (BlkSemaphore, NodeContext, NodeContextTag)
import           Pos.DB                        (NodeDBs, runDBPureRedirect)
import           Pos.DB.Block                  (runBlockDBRedirect)
import           Pos.Discovery                 (getPeers, runDiscoveryConstT)
import           Pos.Reporting.MemState        (ReportingContext, emptyReportingContext)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Util.TimeWarp             (runWithoutJsonLogT)
import           Pos.Wallet.KeyStorage         (KeyData)
import           Pos.Wallet.Light.Mode         (LightWalletMode (..))
import           Pos.Wallet.Light.Redirect     (runBalancesWalletRedirect,
                                                runBlockchainInfoNotImplemented,
                                                runTxHistoryWalletRedirect,
                                                runUpdatesNotImplemented)
import           Pos.Wallet.Light.State        (getWalletState)
import qualified Pos.Wallet.Light.State        as WS
import           Pos.Wallet.Light.State.Core   (runGStateCoreWalletRedirect)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler, bracketWalletWS,
                                                bracketWalletWebDB, walletApplication,
                                                walletServeImpl, walletServer,
                                                walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, WalletWebSockets,
                                                getWalletWebSockets, runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, WalletWebDB,
                                                getWalletWebState, runWalletWebDB)
import           Pos.Wallet.Web.Tracking       (MonadWalletTracking (..))

type WebHandler = WalletWebSockets (WalletWebDB LightWalletMode)

type MainWalletState = WS.WalletState

-- Kostil
instance Monad m => MonadWalletTracking (WalletWebSockets m) where
    syncWSetsAtStart = const pass
    syncOnImport = const pass
    txMempoolToModifier = const (pure mempty)

walletServeWebLite
    :: SscHelpersClass WalletSscType
    => Proxy WalletSscType
    -> SendActions LightWalletMode
    -> FilePath
    -> Bool
    -> Word16
    -> LightWalletMode ()
walletServeWebLite _ sendActions dbPath dbRebuild port =
    bracketWalletWebDB dbPath dbRebuild $ \db ->
        bracketWalletWS $ \conn -> do
            let runner = runWalletWebDB db . runWalletWS conn
            let hoistedSA :: SendActions (WalletWebHandler LightWalletMode)
                hoistedSA = hoistSendActions (lift . lift) runner sendActions
            let action :: WalletWebHandler LightWalletMode Application
                action = walletApplication $ walletServer hoistedSA nat
            runner $ walletServeImpl action port

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    wsConn <- getWalletWebSockets
    ws     <- getWalletWebState
    kd     <- Ether.ask'
    mws    <- getWalletState
    peers  <- getPeers
    pure $ NT (convertHandler mws kd ws wsConn peers)

convertHandler
    :: MainWalletState
    -> KeyData
    -> WalletState
    -> ConnectionsVar
    -> Set NodeId
    -> WebHandler a
    -> Handler a
convertHandler mws kd ws wsConn peers handler = do
    stateM <- liftIO SM.newIO
    liftIO ( runProduction
           . runWithoutJsonLogT
           . usingLoggerName "wallet-lite-api"
           . flip Ether.runReadersT
                ( Tagged @PeerStateTag stateM
                , Tagged @KeyData kd
                , Tagged @MainWalletState mws
                , Tagged @ReportingContext emptyReportingContext )
           . runDBPureRedirect
           . runBlockDBRedirect
           . runTxHistoryWalletRedirect
           . runBalancesWalletRedirect
           . runGStateCoreWalletRedirect
           . runPeerStateRedirect
           . runUpdatesNotImplemented
           . runBlockchainInfoNotImplemented
           . runBListenerStub
           . runDiscoveryConstT peers
           . (\(LightWalletMode m) -> m)
           . runWalletWebDB ws
           . runWalletWS wsConn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
{-# NOINLINE convertHandler #-}

-- Stub implementations for lite wallet.
instance Ether.MonadReader NodeDBs NodeDBs Production where
    ask = error "Stub implementation for Lite Wallet"
    local = error "Stub implementation for Lite Wallet"

instance Ether.MonadReader
             BlkSemaphore
             BlkSemaphore
             Production where
    ask = error "Stub implementation for Lite Wallet"
    local = error "Stub implementation for Lite Wallet"

instance Ether.MonadReader
             NodeContextTag
             (NodeContext WalletSscType)
             Production where
    ask = error "Stub implementation for Lite Wallet"
    local = error "Stub implementation for Lite Wallet"