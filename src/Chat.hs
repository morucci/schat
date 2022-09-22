-- |
-- Module      : Chat
-- Description : A minimal WEB chat through WebSocket
-- Copyright   : (c) Fabien Boucher, 2022
-- License     : MIT
-- Maintainer  : fabien.dot.boucher@gmail.com
--
-- This module implements a simple WEB chat based
-- on HTMX.
module Chat where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Control.Exception.Safe (tryAny)
import Control.Lens ((^?))
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (String))
import Data.Aeson.Lens (key)
import Data.Maybe (isJust)
import Data.String.Interpolate (i, iii)
import Data.Text (Text, unpack)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Lucid (Attribute, Html, ToHtml (toHtml), renderBS)
import Lucid.Base (makeAttribute)
import Lucid.Html5
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid (HTML)
import Servant.XStatic
import qualified XStatic
import qualified XStatic.Htmx as XStatic
import qualified XStatic.Tailwind as XStatic
import Prelude

-- | Some HTMX attribute definition for Lucid
hxWS, hxSwapOOB :: Text -> Attribute
hxWS = makeAttribute "hx-ws"
hxSwapOOB = makeAttribute "hx-swap-oob"

-- | The sChat Servant API definition
type SChatAPIv1 =
  Get '[HTML] (Html ())
    :<|> "xstatic" :> Raw
    :<|> "ws" :> WebSocket

-- |
--   Here is the definition of the Servant Handlers. sChatHTMLHandler is the
--   entry point on "\/"". xstaticServant is bound to "\/xstatic" to serve static JS assets.
--   wsChatHandler is bound to "\/ws" to serve the WebSocket.
sChatServer :: SChatS -> Server SChatAPIv1
sChatServer sChatS =
  pure sChatHTMLHandler
    :<|> xstaticServant xStaticFiles
    :<|> wsChatHandler sChatS

-- | We need static assets HTMX and Tailwindcss
xStaticFiles :: [XStatic.XStaticFile]
xStaticFiles = [XStatic.htmx, XStatic.tailwind]

data EMNotice
  = -- | Member enter event
    EMEnter UTCTime Text
  | -- | Member exit event
    EMExit UTCTime Text

data Event
  = -- | Message event
    EMessage Message
  | -- | Refresh members list event
    EMembersRefresh [Text]
  | -- |  Notice event
    EMemberNotice EMNotice

data Message = Message
  { -- | Message date
    date :: UTCTime,
    -- | Message author
    mLogin :: Text,
    -- | Message content
    content :: Text
  }
  deriving (Show)

data Client = Client
  { -- | Client login
    cLogin :: Text,
    -- | Queue of `Event` to send back to the client
    inputQ :: TBQueue Event
  }

data SChatS = SChatS
  { -- | The Chat State
    clients :: TVar [Client]
  }

-- | Create the Chat state
newSChatS :: STM SChatS
newSChatS = do
  clients <- newTVar []
  pure $ SChatS clients

-- | Add new client to the state
addClient :: Text -> SChatS -> STM Client
addClient name state = do
  q <- newTBQueue 10
  let newClient = Client name q
  modifyTVar (clients state) $ \cls -> do
    cls <> [newClient]
  pure newClient

-- | Remove a client from the state
removeClient :: Text -> SChatS -> STM ()
removeClient cLogin' state = do
  modifyTVar (clients state) $ \cls -> do
    filter (\Client {cLogin} -> not $ cLogin' == cLogin) cls

-- | Check if a client exists in the state
isClientExists :: Text -> SChatS -> STM Bool
isClientExists cLogin' state = do
  cls <- readTVar $ clients state
  pure $ Prelude.any (\Client {cLogin} -> cLogin == cLogin') cls

-- | Get names/logins of connected clients
getClientsNames :: SChatS -> STM [Text]
getClientsNames state = do
  cls <- readTVar state.clients
  pure $ map cLogin cls

-- | The WebSocket connection handler
--
-- Called when a connection is initialized by the client's User Agent (browser)
--
-- This function handles:
--
--     * the rendering of the chat WEB UI
--
--     * the client login (which is just waiting for the client name)
--
--     * client messages input (from HTMX payload)
--
--     * dispatch events to connected clients
--
--     * client output HTMX payloads to update the chat WEB UI
--
--     * client disconnection
--
-- This function loops until the client disconnect
wsChatHandler :: SChatS -> WS.Connection -> Handler ()
wsChatHandler state conn = do
  liftIO $ WS.withPingThread conn 5 (pure ()) $ do
    putStrLn [i|New connection ...|]
    -- Send the rest of WEB UI to the client
    WS.sendTextData conn $ renderBS renderSChat
    -- Wait for login
    handleWaitForLogin
  where
    -- Loop until a client send an available login name, then process I/O
    handleWaitForLogin = do
      ncE <- tryAny waitForLogin
      case ncE of
        Right (Just client) -> do
          -- Replace the input login box with the input message box
          WS.sendTextData conn $ renderInputChat client.cLogin
          -- Start handling the acknowledged client
          handleConnection client
        Right Nothing -> do
          -- This login is used so send a notice
          WS.sendTextData conn . renderBS $ do
            div_ [id_ "chatroom-notices", hxSwapOOB "afterbegin"] $ do
              div_ "This login is already used. Please choose another one."
          -- Reset the input field
          WS.sendTextData conn . renderBS $ chatInput Nothing
          -- Loop to wait for a valid login
          handleWaitForLogin
        Left e -> do
          -- If any synchronuous exception happen the we close the connection
          putStrLn [i|Terminating connection due to #{show e}|]
          closeConnection
      where
        waitForLogin = do
          login <- waitForLoginPayload
          putStrLn [i|Receiving login name: #{login}|]
          -- In the STM transaction, if the login is free then
          -- add the client to the state
          atomically $ do
            exists <- isClientExists login state
            case exists of
              False -> Just <$> addClient login state
              True -> pure Nothing
        waitForLoginPayload = do
          -- Wait until the an input name
          wsD <- WS.receiveDataMessage conn
          case extractMessage wsD "chatInputName" of
            Just login -> pure login
            Nothing -> waitForLoginPayload
    handleConnection (Client myLogin myInputQ) = do
      -- Dispatch member refresh event to all clients
      dispatchMembersRefresh
      date <- getCurrentTime
      -- Dispatch member enter to all clients
      dispatchToAll $ EMemberNotice (EMEnter date myLogin)
      -- Spawn to thread to handle I/O channels
      concurrently_ handleR handleS
      where
        handleR = do
          -- loop on handleR'
          hE <- tryAny $ forever handleR'
          case hE of
            Right _ -> pure ()
            -- If a synchronuous exception raise then handle client disconnection
            Left e -> do
              putStrLn [i|Terminating connection for #{myLogin} due to #{show e}|]
              -- Remove the client from the state
              atomically $ removeClient myLogin state
              -- Dispatch member refresh event to all clients
              dispatchMembersRefresh
              -- Dispatch member exited to all clients
              date <- getCurrentTime
              dispatchToAll $ EMemberNotice (EMExit date myLogin)
              -- Properly close the WebSocket
              closeConnection
          where
            handleR' = do
              -- Wait for an input data message on the WS
              wsD <- WS.receiveDataMessage conn
              -- Send a fresh input chat on the client
              WS.sendTextData conn $ renderInputChat myLogin
              -- If the data message if from the input chat field then
              -- we dispatch the message event
              case extractMessage wsD "chatInputMessage" of
                Just inputMsg -> do
                  now <- getCurrentTime
                  dispatchToAll $ EMessage (Message now myLogin inputMsg)
                Nothing -> pure ()
        handleS = forever handleS'
          where
            -- loop on handleS'

            handleS' = do
              -- Wait for an event in the input Queue
              event <- atomically $ readTBQueue myInputQ
              -- Based on the event type, send the right HTML payload to the client
              hE <- tryAny $ WS.sendTextData conn $ renderBS $ case event of
                EMessage msg -> renderMessage msg
                EMembersRefresh logins -> renderMembersRefresh logins
                EMemberNotice e -> renderMemberNotice e
              case hE of
                Right _ -> pure ()
                -- In case of exception, only log on output in case of sending failure
                -- But keep the loop up
                Left e -> putStrLn [i|"Unable to send a payload to client #{myLogin} due to #{show e}"|]
            -- Render the HTML payload for HTMX to display the message
            renderMessage :: Message -> Html ()
            renderMessage msg = do
              -- The id and hx-swap-oob tell HTMX which elements to update in the DOM
              div_ [id_ "chatroom-content", hxSwapOOB "afterbegin"] $ do
                div_ [id_ "chatroom-message"] $ do
                  span_ [id_ "chatroom-message-date", class_ "pr-2"] . toHtml $ formatDate (msg.date)
                  span_ [id_ "chatroom-message-login", class_ "pr-2"] . toHtml $ unpack (msg.mLogin)
                  span_ [id_ "chatroom-message-content"] . toHtml $ unpack (msg.content)
            -- Render the HTML payload for HTMX to refresh the members box
            renderMembersRefresh :: [Text] -> Html ()
            renderMembersRefresh logins = do
              -- The id and hx-swap-oob tell HTMX which elements to update in the DOM
              div_ [id_ "chatroom-members", hxSwapOOB "innerHTML"] $ do
                mapM_ (\mlogin -> div_ [] $ toHtml mlogin) logins
            -- Render the HTML payload for HTMX to display the notice
            renderMemberNotice :: EMNotice -> Html ()
            renderMemberNotice emn = do
              -- The id and hx-swap-oob tell HTMX which elements to update in the DOM
              div_ [id_ "chatroom-notices", hxSwapOOB "afterbegin"] $ do
                case emn of
                  EMEnter date uLogin ->
                    div_ $ [i|#{formatDate date} - #{uLogin} entered the channel|]
                  EMExit date uLogin ->
                    div_ $ [i|#{formatDate date} - #{uLogin} exited the channel|]

        -- Helper on top of dispatchAll
        dispatchMembersRefresh = do
          cls <- atomically $ getClientsNames state
          dispatchToAll $ EMembersRefresh cls

        -- Dispatch an `Event` to all client's input queue `inputQ`
        dispatchToAll :: Event -> IO ()
        dispatchToAll event = atomically $ do
          cls <- readTVar state.clients
          forM_ cls $ \c -> do
            writeTBQueue c.inputQ event

    -- Helper to format an UTCTime (a date)
    formatDate = formatTime defaultTimeLocale "%T"

    -- Check for the data under a given key in
    -- the HTMX payload
    extractMessage dataMessage keyName =
      case dataMessage of
        WS.Text bs _ -> do
          case bs ^? key keyName of
            Just (String m) -> Just m
            _ -> Nothing
        _other -> Nothing

    -- Properly close the WS
    closeConnection = do
      WS.sendClose conn ("Bye" :: Text)
      void $ WS.receiveDataMessage conn

    -- Helper to render the chat's input field
    renderInputChat login = renderBS . chatInput $ Just login

    -- The WEB app UI
    -- It is important to notice that some HTML tags own an id attribute mainly
    -- for HTMX to be able to swap the content based on the payload the backend
    -- send back to the browser.
    -- Also we add some Tailwindcss class to prettify the UI
    renderSChat :: Html ()
    renderSChat = do
      div_ [id_ "schat", class_ "h-auto"] $ do
        div_ [class_ "bg-purple-100 border-4 border-purple-300 w-full h-full"] $ do
          title
          chatInput Nothing
          chatDisplay
          chatNotices
      where
        title = p_ [class_ "mb-2 pb-1 bg-purple-300 text-xl"] "Simple WebSocket Chat"
        chatDisplay = do
          div_ [id_ "chatroom", class_ "flex flex-row space-x-2 mx-2 my-2 h-96"] $ do
            roomChat
            roomMembers
          where
            roomChat = do
              div_ [id_ "chatroom-chat", class_ "flex-auto w-3/4 h-full"] $ do
                div_
                  [ id_ "chatroom-content",
                    class_ "overflow-auto border-2 border-purple-200 h-full max-h-full"
                  ]
                  ""
            roomMembers = do
              div_
                [ id_ "chatroom-members",
                  class_ "overflow-auto border-2 border-purple-200 flex-auto w-1/4 h-full max-h-full"
                ]
                ""
        chatNotices = do
          div_
            [ id_ "chatroom-notices",
              class_ "overflow-auto mb-2 mx-2 border-2 border-purple-200 h-16 max-h-full"
            ]
            $ ""

    --  The chat's input field
    chatInput :: Maybe Text -> Html ()
    chatInput loginM = do
      let inputFieldName = if isJust loginM then "chatInputMessage" else "chatInputName"
      let inputFieldPlaceholder = if isJust loginM then "Enter a message" else "Enter your name"
      -- hx-ws attribute tells HTMX to send a payload on the WebSocket when the form is submitted
      form_ [hxWS "send:submit", id_ "chatroom-input", class_ "mx-2 bg-purple-200 rounded-lg"] $ do
        span_ $ do
          maybe (span_ [] "") (\login -> span_ [class_ "pl-1 pr-2"] $ toHtml login) loginM
          input_
            [ type_ "text",
              class_ "text-sm rounded-lg bg-purple-50 border border-purple-300 focus:border-purple-400",
              -- The payload sent by HTMX can be identified via the name attribute
              name_ inputFieldName,
              id_ "chatroom-input-field",
              placeholder_ inputFieldPlaceholder
            ]
        -- Ensure the field got the focus
        script_ "htmx.find('#chatroom-input-field').focus()"

-- | The root handler called when a client connect to the chat app
-- The function renders a minimal WEB page, with metadata, needed scripts
-- (such as HTMX and Tailwindcss). The HTMX library detects "hx-ws" attribute
-- and initializes a WebSocket connection to the /ws backend endpoint.
sChatHTMLHandler :: Html ()
sChatHTMLHandler = do
  doctypehtml_ $ do
    head_ $ do
      title_ "Simple WebSocket Chat "
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      xstaticScripts xStaticFiles
      script_ [iii||]
    body_ $ do
      div_ [class_ "container mx-auto", hxWS "connect:/ws"] $
        div_ [id_ "schat"] ""

-- | Create the web application
sChatApp :: SChatS -> Wai.Application
sChatApp sChatS = serve (Proxy @SChatAPIv1) $ sChatServer sChatS

-- | Start the Warp WEB server to serve the application
runServer :: IO ()
runServer = do
  sChatS <- atomically newSChatS
  Warp.run 8091 $ sChatApp sChatS
