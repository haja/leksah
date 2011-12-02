{-# LANGUAGE CPP, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Workspace
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | Represents a workspace, a work unit, which can be composed of multiple packages
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module IDE.Workspaces (
    workspaceNew
,   workspaceOpen
,   workspaceTry
,   workspaceTry_
,   workspaceOpenThis
,   workspaceClose
,   workspaceClean
,   workspaceMake
,   workspaceActivatePackage
,   workspaceAddPackage
,   workspaceAddPackage'
,   workspaceRemovePackage
,   workspacePackageNew
,   packageTry
,   packageTry_

,   backgroundMake
,   makePackage
) where

import IDE.Core.State
import Control.Monad.Trans (liftIO)
import Graphics.UI.Editor.Parameters
    (Parameter(..), (<<<-), paraName, emptyParams)
import Control.Monad (unless, when, liftM)
import Control.Monad.Reader (lift)
import Data.Maybe (isJust,fromJust )
import IDE.Utils.GUIUtils
    (chooseFile, chooseSaveFile)
import System.FilePath
       (takeFileName, (</>), isAbsolute, dropFileName, makeRelative,
        dropExtension, takeBaseName, addExtension, takeExtension,
        takeDirectory)
import Text.PrinterParser
    (readFields,
     writeFields,
     readParser,
     stringParser,
     intParser,
     mkFieldS,
     FieldDescriptionS(..))
import qualified Text.PrettyPrint as  PP (text)
import Graphics.UI.Gtk
       (dialogSetDefaultResponse, windowWindowPosition, widgetDestroy,
        dialogRun, messageDialogNew, dialogAddButton, Window(..),
        widgetHide, DialogFlags(..))
import IDE.Pane.PackageEditor (packageNew', choosePackageFile, standardSetup)
import Data.List (delete)
import IDE.Package
       (getModuleTemplate, getPackageDescriptionAndPath, activatePackage,
        deactivatePackage, idePackageFromPath)
import System.Directory
       (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.Time (getClockTime)
import Graphics.UI.Gtk.Windows.MessageDialog
    (ButtonsType(..), MessageType(..))
#if MIN_VERSION_gtk(0,10,5)
import Graphics.UI.Gtk.Windows.Dialog (ResponseId(..))
#else
import Graphics.UI.Gtk.General.Structs (ResponseId(..))
#endif
import qualified Control.Exception as Exc (SomeException(..), throw, Exception)
import Data.Typeable (Typeable)
import Control.Monad.Reader.Class (ask)
import qualified Data.Map as  Map (empty, insert, lookup)
import IDE.Pane.SourceBuffer (fileOpenThis, fileCheckAll, belongsToPackage)
import qualified System.IO.UTF8 as UTF8 (writeFile)
import System.Glib.Attributes (AttrOp(..), set)
import Graphics.UI.Gtk.General.Enums (WindowPosition(..))
import Control.Applicative ((<$>))
import IDE.Build
import IDE.Utils.FileUtils(myCanonicalizePath)
import IDE.Command.VCS.Common.Workspaces as VCSWS
import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI
import qualified IDE.Workspaces.Writer as Writer



-- | Constructs a new workspace and makes it the current workspace
workspaceNew :: IDEAction
workspaceNew = do
    window <- getMainWindow
    mbFile <- liftIO $ do
        chooseSaveFile window "New file for workspace" Nothing
    case mbFile of
        Nothing -> return ()
        Just filePath -> workspaceNewHere filePath

workspaceNewHere :: FilePath -> IDEAction
workspaceNewHere filePath =
    let realPath = if takeExtension filePath == leksahWorkspaceFileExtension
                            then filePath
                            else addExtension filePath leksahWorkspaceFileExtension
    in do
        dir <- liftIO $ myCanonicalizePath $ dropFileName realPath
        let cPath = dir </> takeFileName realPath
            newWorkspace = emptyWorkspace {
                            wsName = takeBaseName cPath,
                            wsFile = cPath}
        liftIO $ writeFields cPath newWorkspace Writer.workspaceDescr
        workspaceOpenThis False (Just cPath)
        return ()

workspaceOpen :: IDEAction
workspaceOpen = do
    window     <- getMainWindow
    mbFilePath <- liftIO $ chooseWorkspaceFile window
    workspaceOpenThis True mbFilePath
    return ()

workspaceTryQuiet :: WorkspaceM a -> IDEM (Maybe a)
workspaceTryQuiet f = do
    maybeWorkspace <- readIDE workspace
    case maybeWorkspace of
        Just ws -> liftM Just $ runWorkspace f ws
        Nothing -> do
            ideMessage Normal "No workspace open"
            return Nothing

workspaceTryQuiet_ :: WorkspaceM a -> IDEAction
workspaceTryQuiet_ f = workspaceTryQuiet f >> return ()

workspaceTry :: WorkspaceM a -> IDEM (Maybe a)
workspaceTry f = do
    maybeWorkspace <- readIDE workspace
    case maybeWorkspace of
        Just ws -> liftM Just $ runWorkspace f ws
        Nothing -> do
            mainWindow <- getMainWindow
            defaultWorkspace <- liftIO $ (</> "leksah.lkshw") <$> getHomeDirectory
            resp <- liftIO $ do
                defaultExists <- doesFileExist defaultWorkspace
                md <- messageDialogNew (Just mainWindow) [DialogModal] MessageQuestion ButtonsCancel (
                        "You need to have a workspace open for this to work. "
                     ++ "Choose ~/leksah.lkshw to "
                     ++ (if defaultExists then "open workspace " else "create a workspace ")
                     ++ defaultWorkspace)
                dialogAddButton md "_New Workspace" (ResponseUser 1)
                dialogAddButton md "_Open Workspace" (ResponseUser 2)
                dialogAddButton md "~/leksah.lkshw" (ResponseUser 3)
                dialogSetDefaultResponse md (ResponseUser 3)
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetHide md
                return resp
            case resp of
                ResponseUser 1 -> do
                    workspaceNew
                    workspaceTryQuiet f
                ResponseUser 2 -> do
                    workspaceOpen
                    workspaceTryQuiet f
                ResponseUser 3 -> do
                    defaultExists <- liftIO $ doesFileExist defaultWorkspace
                    if defaultExists
                        then workspaceOpenThis True (Just defaultWorkspace)
                        else workspaceNewHere defaultWorkspace
                    workspaceTryQuiet f
                _  -> return Nothing

workspaceTry_ :: WorkspaceM a -> IDEAction
workspaceTry_ f = workspaceTry f >> return ()

chooseWorkspaceFile :: Window -> IO (Maybe FilePath)
chooseWorkspaceFile window = chooseFile window "Select leksah workspace file (.lkshw)" Nothing

workspaceOpenThis :: Bool -> Maybe FilePath -> IDEAction
workspaceOpenThis askForSession mbFilePath =
    case mbFilePath of
        Nothing -> return ()
        Just filePath -> do
            let spath =  dropExtension filePath ++ leksahSessionFileExtension
            workspaceClose
            exists <- liftIO $ doesFileExist spath
            wantToLoadSession <-
                if exists && askForSession
                    then do
                        window <- getMainWindow
                        liftIO $ do
                            md  <- messageDialogNew (Just window) [] MessageQuestion ButtonsNone
                                    $ "There are session settings stored with this workspace."
                            dialogAddButton md "_Ignore Session" ResponseCancel
                            dialogAddButton md "_Load Session" ResponseYes
                            dialogSetDefaultResponse md ResponseYes
                            set md [ windowWindowPosition := WinPosCenterOnParent ]
                            rid <- dialogRun md
                            widgetDestroy md
                            case rid of
                                ResponseYes ->  return True
                                otherwise   ->  return False
                    else return False
            if wantToLoadSession
                then triggerEventIDE (LoadSession spath) >> return ()
                else do
                    ideR <- ask
                    catchIDE (do
                        workspace <- readWorkspace filePath
                        Writer.setWorkspace (Just workspace {wsFile = filePath})
                        VCSWS.onWorkspaceOpen workspace)
                           (\ (e :: Exc.SomeException) -> reflectIDE
                                (ideMessage Normal ("Can't load workspace file " ++ filePath ++ "\n" ++ show e)) ideR)


-- | Closes a workspace
workspaceClose :: IDEAction
workspaceClose = do
    oldWorkspace <- readIDE workspace
    case oldWorkspace of
        Nothing -> return ()
        Just ws -> do
            VCSWS.onWorkspaceClose
            let oldActivePackFile = wsActivePackFile ws
            triggerEventIDE (SaveSession ((dropExtension (wsFile ws))
                                ++  leksahSessionFileExtension))
            addRecentlyUsedWorkspace (wsFile ws)
            Writer.setWorkspace Nothing
            when (isJust oldActivePackFile) $ do
                triggerEventIDE (Sensitivity [(SensitivityProjectActive, False),
                    (SensitivityWorkspaceOpen, False)])
                return ()
            return ()
    return ()

workspacePackageNew :: WorkspaceAction
workspacePackageNew = do
    ws <- ask
    let path = dropFileName (wsFile ws)
    lift $ packageNew' (Just path) (\isNew fp -> do
        window     <-  getMainWindow
        workspaceTry_ $ workspaceAddPackage' fp >> return ()
        when isNew $ do
            mbPack <- idePackageFromPath fp
            constructAndOpenMainModule mbPack
        triggerEventIDE UpdateWorkspaceInfo >> return ())

constructAndOpenMainModule :: Maybe IDEPackage -> IDEAction
constructAndOpenMainModule Nothing = return ()
constructAndOpenMainModule (Just idePackage) =
    case (ipdMain idePackage, ipdSrcDirs idePackage) of
        ([target],[path]) -> do
            mbPD <- getPackageDescriptionAndPath
            case mbPD of
                Just (pd,_) -> do
                    liftIO $ createDirectoryIfMissing True path
                    alreadyExists <- liftIO $ doesFileExist (path </> target)
                    if alreadyExists
                        then do
                            ideMessage Normal "Main file already exists"
                            fileOpenThis (path </> target)
                        else do
                            template <- liftIO $ getModuleTemplate pd (dropExtension target)
                                "    main" "main = putStrLn \"Hello World!\""
                            liftIO $ UTF8.writeFile (path </> target) template
                            fileOpenThis (path </> target)
                Nothing     -> ideMessage Normal "No package description"
        _ -> return ()

workspaceAddPackage :: WorkspaceAction
workspaceAddPackage = do
    ws <- ask
    let path = dropFileName (wsFile ws)
    window <-  lift getMainWindow
    mbFilePath <- liftIO $ choosePackageFile window (Just path)
    case mbFilePath of
        Nothing -> return ()
        Just fp -> do
            workspaceAddPackage' fp >> return ()
            lift $ triggerEventIDE UpdateWorkspaceInfo >> return ()

workspaceAddPackage' :: FilePath -> WorkspaceM (Maybe IDEPackage)
workspaceAddPackage' fp = do
    ws <- ask
    cfp <- liftIO $ myCanonicalizePath fp
    mbPack <- lift $ idePackageFromPath cfp
    case mbPack of
        Just pack -> do
            let dir = takeDirectory cfp
            b1 <- liftIO $ doesFileExist (dir </> "Setup.hs")
            b2 <- liftIO $ doesFileExist (dir </> "Setup.lhs")
            unless (b1 || b2) $ liftIO $ do
                sysMessage Normal "Setup.(l)hs does not exist. Writing Standard"
                writeFile (dir </> "Setup.lhs") standardSetup
            unless (elem cfp (map ipdCabalFile (wsPackages ws))) $ lift $
                Writer.writeWorkspace $ ws {wsPackages =  pack : wsPackages ws,
                                     wsActivePackFile =  Just (ipdCabalFile pack)}
            return (Just pack)
        Nothing -> return Nothing

packageTryQuiet :: PackageM a -> IDEM (Maybe a)
packageTryQuiet f = do
    maybePackage <- readIDE activePack
    case maybePackage of
        Just p  -> liftM Just $ runPackage f p
        Nothing -> do
            ideMessage Normal "No active package"
            return Nothing

packageTryQuiet_ :: PackageM a -> IDEAction
packageTryQuiet_ f = packageTryQuiet f >> return ()

packageTry :: PackageM a -> IDEM (Maybe a)
packageTry f = do
    liftM (>>= id) $ workspaceTry $ do
        maybePackage <- lift $ readIDE activePack
        case maybePackage of
            Just p  -> liftM Just $ lift $ runPackage f p
            Nothing -> do
                window <- lift $ getMainWindow
                resp <- liftIO $ do
                    md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                            "You need to have an active package for this to work."
                    dialogAddButton md "_New Package" (ResponseUser 1)
                    dialogAddButton md "_Add Package" (ResponseUser 2)
                    dialogSetDefaultResponse md (ResponseUser 2)
                    set md [ windowWindowPosition := WinPosCenterOnParent ]
                    resp <- dialogRun md
                    widgetHide md
                    return resp
                case resp of
                    ResponseUser 1 -> do
                        workspacePackageNew
                        lift $ packageTryQuiet f
                    ResponseUser 2 -> do
                        workspaceAddPackage
                        lift $ packageTryQuiet f
                    _  -> return Nothing

packageTry_ :: PackageM a -> IDEAction
packageTry_ f = packageTry f >> return ()

workspaceRemovePackage :: IDEPackage -> WorkspaceAction
workspaceRemovePackage pack = do
    ws <- ask
    when (elem pack (wsPackages ws)) $ lift $
        Writer.writeWorkspace ws {wsPackages =  delete pack (wsPackages ws)}
    return ()

workspaceActivatePackage :: IDEPackage -> WorkspaceAction
workspaceActivatePackage pack = do
    ws <- ask
    lift $ activatePackage (Just pack)
    when (elem pack (wsPackages ws)) $ lift $ do
        Writer.writeWorkspace ws {wsActivePackFile =  Just (ipdCabalFile pack)}
        return ()
    return ()



readWorkspace :: FilePath -> IDEM Workspace
readWorkspace fp = do
    ws <- liftIO $ readFields fp Writer.workspaceDescr emptyWorkspace
    ws' <- liftIO $ makePathesAbsolute ws fp
    packages <- mapM idePackageFromPath (wsPackagesFiles ws')
    --TODO set package vcs here
    return ws'{ wsPackages = map fromJust $ filter isJust $ packages}




makePathesAbsolute :: Workspace -> FilePath -> IO Workspace
makePathesAbsolute ws bp = do
    wsFile'                     <-  myCanonicalizePath bp
    wsActivePackFile'           <-  case wsActivePackFile ws of
                                        Nothing -> return Nothing
                                        Just fp -> do
                                            fp' <- makeAbsolute (dropFileName wsFile') fp
                                            return (Just fp')
    wsPackagesFiles'            <-  mapM (makeAbsolute (dropFileName wsFile')) (wsPackagesFiles ws)
    return ws {wsActivePackFile = wsActivePackFile', wsFile = wsFile', wsPackagesFiles = wsPackagesFiles'}
    where
        makeAbsolute basePath relativePath  =
            if isAbsolute relativePath
                then myCanonicalizePath relativePath
                else myCanonicalizePath (basePath </> relativePath)

emptyWorkspace =  Workspace {
    wsVersion       =   Writer.workspaceVersion
,   wsSaveTime      =   ""
,   wsName          =   ""
,   wsFile          =   ""
,   wsPackages      =   []
,   wsPackagesFiles =   []
,   wsActivePackFile =   Nothing
,   wsNobuildPack   =   []
,   packageVcsConf  =   Map.empty
}



addRecentlyUsedWorkspace :: FilePath -> IDEAction
addRecentlyUsedWorkspace fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentWorkspaces' <- readIDE recentWorkspaces
        unless (elem fp recentWorkspaces') $
            modifyIDE_ (\ide -> ide{recentWorkspaces = take 12 (fp : recentWorkspaces')})
        triggerEventIDE UpdateRecent
        return ()

removeRecentlyUsedWorkspace :: FilePath -> IDEAction
removeRecentlyUsedWorkspace fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentWorkspaces' <- readIDE recentWorkspaces
        when (elem fp recentWorkspaces') $
            modifyIDE_ (\ide -> ide{recentWorkspaces = filter (\e -> e /= fp) recentWorkspaces'})
        triggerEventIDE UpdateRecent
        return ()

------------------------
-- Workspace make

workspaceClean :: WorkspaceAction
workspaceClean = do
    ws <- ask
    settings <- lift $ do
        prefs' <- readIDE prefs
        return (defaultMakeSettings prefs')
    makePackages settings (wsPackages ws) MoClean MoClean moNoOp

workspaceMake :: WorkspaceAction
workspaceMake = do
    ws <- ask
    settings <- lift $ do
        prefs' <- readIDE prefs
        return ((defaultMakeSettings prefs'){
                    msMakeMode           = True,
                    msBackgroundBuild    = False})
    makePackages settings (wsPackages ws) (MoComposed [MoConfigure,MoBuild,MoTest,MoCopy,MoRegister])
        (MoComposed [MoConfigure,MoBuild,MoTest,MoCopy,MoRegister]) MoMetaInfo

backgroundMake :: IDEAction
backgroundMake = catchIDE (do
    ideR        <- ask
    prefs       <- readIDE prefs
    mbPackage   <- readIDE activePack
    case mbPackage of
        Nothing         -> return ()
        Just package    -> do
            modifiedPacks <- if saveAllBeforeBuild prefs
                                then fileCheckAll belongsToPackage
                                else return []
            let isModified = not (null modifiedPacks)
            when isModified $ do
                let settings = defaultMakeSettings prefs
                if msSingleBuildWithoutLinking settings &&  not (msMakeMode settings)
                    then workspaceTryQuiet_ $
                        makePackages settings modifiedPacks MoBuild (MoComposed []) moNoOp
                    else workspaceTryQuiet_ $
                        makePackages settings modifiedPacks (MoComposed [MoBuild,MoTest,MoCopy,MoRegister])
                                        (MoComposed [MoConfigure,MoBuild,MoTest,MoCopy,MoRegister]) MoMetaInfo
    )
    (\(e :: Exc.SomeException) -> sysMessage Normal (show e))

makePackage ::  PackageAction
makePackage = do
    p <- ask
    (mbWs,settings) <- lift $ do
        prefs' <- readIDE prefs
        ws     <- readIDE workspace
        let settings = (defaultMakeSettings prefs'){msBackgroundBuild = False}
        return (ws,settings)
    case mbWs of
        Nothing -> sysMessage Normal "No workspace for build."
        Just ws -> lift $
            if msSingleBuildWithoutLinking settings &&  not (msMakeMode settings)
                then runWorkspace
                        (makePackages settings [p] MoBuild (MoComposed []) moNoOp) ws
                else runWorkspace
                        (makePackages settings [p]
                        (MoComposed [MoBuild,MoTest,MoCopy,MoRegister])
                        (MoComposed [MoConfigure,MoBuild,MoTest,MoCopy,MoRegister])
                        MoMetaInfo) ws
