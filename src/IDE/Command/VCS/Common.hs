-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.Common
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
module IDE.Command.VCS.Common (
    createActionFromContext
    ,setupRepoAction
    ,noOpenWorkspace
) where

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI
import qualified Graphics.UI.Gtk as Gtk

import IDE.Core.Types
import IDE.Core.State
import IDE.Command.VCS.Common.Workspaces
import IDE.Workspaces(workspaceSetVCSConfig)
import IDE.Utils.GUIUtils



import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
import qualified Control.Exception as Exc
import Data.Maybe



-- | displays a window for setting up a vcs, thereafter adding menu items and persisting the created configuration
setupRepoAction :: IDEAction
setupRepoAction = do
    ide <- ask

    mbWorkspace <- readIDE workspace
    case mbWorkspace of

        Just workspace -> do
                             let config = vcsConfig workspace
                             liftIO $ VCSGUI.showSetupConfigGUI config (callback ide)

        Nothing -> noOpenWorkspace
    where
        callback :: IDERef -> Maybe (VCS.VCSType, VCS.Config, Maybe VCSGUI.MergeTool) -> IO()
        callback ideRef mbConfig = do
                -- set config in workspace
                runReaderT (workspaceSetVCSConfig mbConfig) ideRef
                -- add menu items
                case mbConfig of
                    Nothing -> return ()
                    Just config -> do
                                    runReaderT onWorkspaceClose ideRef
                                    runReaderT (onWorkspaceOpen config) ideRef

-- | retrieves VCS configuration from the workspace and executes given computation using it
createActionFromContext :: VCS.Ctx()    -- ^ computation to execute, i.e. showCommit
                        -> IDEAction
createActionFromContext vcsAction = do
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Just workspace -> do
             let mbConfig = vcsConfig workspace
             case mbConfig of
                Nothing -> liftIO $ VCSGUI.showErrorGUI "No active repository!"
                Just (_,config, _) -> liftIO $ VCSGUI.defaultVCSExceptionHandler $ VCS.runVcs config $ vcsAction
        Nothing -> noOpenWorkspace


noOpenWorkspace = do
                    liftIO $ showDialog "No open workspace. You must have an open workspace to be able to set a repository." Gtk.MessageError



