{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Package
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | The packages methods of ide.
--
---------------------------------------------------------------------------------

module IDE.Package (
    packageConfig
,   packageConfig'
,   buildPackage

,   packageDoc
,   packageClean
,   packageClean'
,   packageCopy
,   packageCopy'
,   packageRun
,   activatePackage
,   deactivatePackage

,   packageInstallDependencies
,   packageRegister
,   packageRegister'
,   packageTest
,   packageTest'
,   packageSdist
,   packageOpenDoc

,   getPackageDescriptionAndPath
,   getEmptyModuleTemplate
,   getModuleTemplate
,   addModuleToPackageDescr
,   delModuleFromPackageDescr

,   backgroundBuildToggled
,   makeModeToggled

,   debugStart
,   printBindResultFlag
,   breakOnErrorFlag
,   breakOnExceptionFlag

,   printEvldWithShowFlag
,   tryDebug
,   tryDebug_
,   executeDebugCommand

,   choosePackageFile

,   idePackageFromPath
) where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Distribution.Package hiding (depends,packageId)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity
import System.FilePath
import Control.Concurrent
import System.Directory (setCurrentDirectory, doesFileExist)
import Prelude hiding (catch)
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Exception (SomeException(..), catch)
import Paths_leksah

import IDE.Core.State
import IDE.Utils.GUIUtils
import IDE.Pane.Log
import IDE.Pane.PackageEditor
import IDE.Pane.SourceBuffer
import IDE.Pane.PackageFlags (readFlags)
import Distribution.Text (display)
import IDE.Utils.FileUtils(getConfigFilePathForLoad)
import IDE.LogRef
import MyMissing (replace)
import Distribution.ModuleName (ModuleName(..))
import Data.List (isInfixOf, nub, foldl', delete)
import qualified System.IO.UTF8 as UTF8  (readFile)
import IDE.Utils.Tool (ToolOutput(..), runTool, newGhci, ToolState(..))
import qualified Data.Set as  Set (fromList)
import qualified Data.Map as  Map (empty)
import System.Exit (ExitCode(..))
import Control.Applicative ((<$>))
#ifdef MIN_VERSION_process_leksah
import IDE.System.Process (getProcessExitCode, interruptProcessGroup, ProcessHandle(..))
#else
import System.Process (getProcessExitCode, interruptProcessGroupOf, ProcessHandle(..))
#endif
import IDE.Utils.Tool (executeGhciCommand)
import qualified Data.Enumerator as E (run_, Iteratee(..), last)
import qualified Data.Enumerator.List as EL (foldM, zip3, zip)
import Data.Enumerator (($$))

#if MIN_VERSION_Cabal(1,8,0)
myLibModules pd = case library pd of
                    Nothing -> []
                    Just l -> libModules l
myExeModules pd = concatMap exeModules (executables pd)
#else
myLibModules pd = libModules pd
myExeModules pd = exeModules pd
#endif


packageOpen :: IDEAction
packageOpen = packageOpenThis Nothing

packageOpenThis :: Maybe FilePath -> IDEAction
packageOpenThis mbFilePath = do
    active <- readIDE activePack
    case active of
        Just p -> deactivatePackage
        Nothing -> return ()
    selectActivePackage mbFilePath
    return ()

selectActivePackage :: Maybe FilePath -> IDEM (Maybe IDEPackage)
selectActivePackage mbFilePath' = do
    window     <- getMainWindow
    mbFilePath <- case mbFilePath' of
                    Nothing -> liftIO $ choosePackageFile  window Nothing
                    Just fp -> return (Just fp)
    case mbFilePath of
        Nothing -> return Nothing
        Just filePath -> idePackageFromPath filePath >>= (\ p -> activatePackage p >> return p)

activatePackage :: Maybe IDEPackage -> IDEM ()
activatePackage mbPack@(Just pack) = do
        modifyIDE_ (\ide -> ide{activePack = mbPack})
        liftIO $ setCurrentDirectory (dropFileName (ipdCabalFile pack))
        triggerEventIDE (Sensitivity [(SensitivityProjectActive,True)])
        mbWs <- readIDE workspace
        let wsStr = case mbWs of
                Nothing -> ""
                Just ws -> wsName ws
        let txt = wsStr ++ " > " ++ packageIdentifierToString (ipdPackageId pack)
        triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
        return ()
activatePackage Nothing = return ()

deactivatePackage :: IDEAction
deactivatePackage = do
    oldActivePack <- readIDE activePack
    modifyIDE_ (\ide -> ide{activePack = Nothing})
    when (isJust oldActivePack) $ do
        triggerEventIDE (Sensitivity [(SensitivityProjectActive,False)])
        return ()
    mbWs <- readIDE workspace
    let wsStr = case mbWs of
                    Nothing -> ""
                    Just ws -> wsName ws
    let txt = wsStr ++ ":"
    triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
    return ()

packageConfig :: PackageAction
packageConfig = do
    package <- ask
    lift $ packageConfig' package (\ _ -> return ())

packageConfig'  :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageConfig' package continuation = do
    let dir = dropFileName (ipdCabalFile package)
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    runExternalTool'        "Configuring"
                            "cabal"
                            (["configure"] ++ (ipdConfigFlags package))
                            (Just dir) $ do
        (mbLastOutput, _) <- EL.zip E.last (logOutput logLaunch)
        lift $ do
            mbPack <- idePackageFromPath (ipdCabalFile package)
            case mbPack of
                Just pack -> do
                    changePackage pack
                    triggerEventIDE (WorkspaceChanged False True)
                    continuation (mbLastOutput == Just (ToolExit ExitSuccess))
                    return ()
                Nothing -> do
                    ideMessage Normal "Can't read package file"
                    continuation False
                    return()

runCabalBuild :: Bool -> Bool -> Bool -> IDEPackage -> Bool -> (Bool -> IDEAction) -> IDEAction
runCabalBuild backgroundBuild jumpToWarnings withoutLinking package shallConfigure continuation = do
    prefs   <- readIDE prefs
    let dir =  dropFileName (ipdCabalFile package)
    let args = (["build"] ++
                if backgroundBuild && withoutLinking
                    then ["--with-ld=false"]
                    else []
                        ++ ipdBuildFlags package)
    runExternalTool' "Building" "cabal" args (Just dir) $ do
        (mbLastOutput, isConfigErr, _) <- EL.zip3 E.last isConfigError $
            logOutputForBuild package backgroundBuild jumpToWarnings
        lift $ do
            errs <- readIDE errorRefs
            if shallConfigure && isConfigErr
                then
                    packageConfig' package (\ b ->
                        when b $ runCabalBuild backgroundBuild jumpToWarnings withoutLinking package False continuation)
                else do
                    continuation (mbLastOutput == Just (ToolExit ExitSuccess))
                    return ()

isConfigError :: Monad m => E.Iteratee ToolOutput m Bool
isConfigError = EL.foldM (\a b -> return $ a || isCErr b) False
    where
    isCErr (ToolError str) = str1 `isInfixOf` str || str2 `isInfixOf` str
    isCErr _ = False
    str1 = "Run the 'configure' command first"
    str2 = "please re-configure"

buildPackage :: Bool -> Bool -> Bool -> IDEPackage -> (Bool -> IDEAction) -> IDEAction
buildPackage backgroundBuild jumpToWarnings withoutLinking package continuation = catchIDE (do
    ideR      <- ask
    prefs     <- readIDE prefs
    maybeDebug <- readIDE debugState
    case maybeDebug of
        Nothing -> do
            alreadyRunning <- isRunning
            if alreadyRunning
                then do
                    interruptBuild
                    when (not backgroundBuild) $ liftIO $ do
                        timeoutAddFull (do
                            reflectIDE (do
                                buildPackage backgroundBuild jumpToWarnings withoutLinking
                                                package continuation
                                return False) ideR
                            return False) priorityDefaultIdle 1000
                        return ()
                else runCabalBuild backgroundBuild jumpToWarnings withoutLinking package True continuation
        Just debug@(_, ghci) -> do
            -- TODO check debug package matches active package
            ready <- liftIO $ isEmptyMVar (currentToolCommand ghci)
            when ready $ do
                let dir = dropFileName (ipdCabalFile package)
                when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToWorkspace; return ())
                runDebug (executeDebugCommand ":reload" (logOutputForBuild package backgroundBuild jumpToWarnings)) debug
    )
    (\(e :: SomeException) -> sysMessage Normal (show e))

packageDoc :: PackageAction
packageDoc = do
    package <- ask
    logLaunch <- lift $ getDefaultLogLaunch
    lift $ showDefaultLogLaunch'

    lift $ catchIDE (do
        let dir = dropFileName (ipdCabalFile package)
        runExternalTool' "Documenting"
                        "cabal"
                        (["haddock"] ++ (ipdHaddockFlags package))
                        (Just dir)
                        $ logOutput logLaunch)
        (\(e :: SomeException) -> putStrLn (show e))

packageClean :: PackageAction
packageClean = do
    package <- ask
    lift $ packageClean' package (\ _ -> return ())

packageClean' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageClean' package continuation = do
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    let dir = dropFileName (ipdCabalFile package)
    runExternalTool' "Cleaning"
                    "cabal"
                    ["clean"]
                    (Just dir) $ do
        (mbLastOutput, _) <- EL.zip E.last (logOutput logLaunch)
        lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))

packageCopy :: PackageAction
packageCopy = do
    package <- ask
    logLaunch <- lift $ getDefaultLogLaunch
    lift $ showDefaultLogLaunch'

    lift $ catchIDE (do
        window      <- getMainWindow
        mbDir       <- liftIO $ chooseDir window "Select the target directory" Nothing
        case mbDir of
            Nothing -> return ()
            Just fp -> do
                let dir = dropFileName (ipdCabalFile package)
                runExternalTool' "Copying"
                                "cabal"
                                (["copy"] ++ ["--destdir=" ++ fp])
                                (Just dir)
                                (logOutput logLaunch))
        (\(e :: SomeException) -> putStrLn (show e))

packageInstallDependencies :: PackageAction
packageInstallDependencies = do
    package <- ask
    logLaunch <- lift $ getDefaultLogLaunch
    lift $ showDefaultLogLaunch'

    lift $ catchIDE (do
        let dir = dropFileName (ipdCabalFile package)
        runExternalTool' "Installing" "cabal" (["install","--only-dependencies"]
            ++ (ipdInstallFlags package)) (Just dir) (logOutput logLaunch))
        (\(e :: SomeException) -> putStrLn (show e))

packageCopy' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageCopy' package continuation = do
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    catchIDE (do
        let dir = dropFileName (ipdCabalFile package)
        runExternalTool' "Copying" "cabal" (["copy"]
            ++ (ipdInstallFlags package)) (Just dir) $ do
                (mbLastOutput, _) <- EL.zip E.last (logOutput logLaunch)
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
        (\(e :: SomeException) -> putStrLn (show e))

packageRun :: PackageAction
packageRun = do
    package <- ask
    lift $ catchIDE (do
        ideR        <- ask
        maybeDebug   <- readIDE debugState
        pd <- liftIO $ readPackageDescription normal (ipdCabalFile package) >>= return . flattenPackageDescription
        case maybeDebug of
            Nothing -> do
                case executables pd of
                    (Executable name _ _):_ -> do
                        (logLaunch,logName) <- buildLogLaunchByName name
                        let path = "dist/build" </> name </> name
                        let dir = dropFileName (ipdCabalFile package)
                        IDE.Package.runPackage (addLogLaunchData logName logLaunch)
                                               ("Running " ++ name)
                                               path
                                               (ipdExeFlags package)
                                               (Just dir)
                                               (logOutput logLaunch)


                    otherwise -> do
                        sysMessage Normal "no executable in selected package"
                        return ()
            Just debug -> do
                -- TODO check debug package matches active package
                case executables pd of
                    (Executable name mainFilePath _):_ -> do
                        (logLaunch,logName) <- buildLogLaunchByName name
                        runDebug (do
                                    executeDebugCommand (":module *" ++ (map (\c -> if c == '/' then '.' else c) (takeWhile (/= '.') mainFilePath))) (logOutput logLaunch)
                                    executeDebugCommand (":main " ++ (unwords (ipdExeFlags package))) (logOutput logLaunch))
                                 debug
                    otherwise -> do
                        sysMessage Normal "no executable in selected package"
                        return ())
        (\(e :: SomeException) -> putStrLn (show e))

packageRegister :: PackageAction
packageRegister = do
    package <- ask
    lift $ packageRegister' package (\ _ -> return ())

packageRegister' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageRegister' package continuation =
    if ipdHasLibs package
        then do
          logLaunch <- getDefaultLogLaunch
          showDefaultLogLaunch'
          catchIDE (do
            let dir = dropFileName (ipdCabalFile package)
            runExternalTool' "Registering" "cabal" (["register"]
                ++ (ipdRegisterFlags package)) (Just dir) $ do
                    (mbLastOutput, _) <- EL.zip E.last (logOutput logLaunch)
                    lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
            (\(e :: SomeException) -> putStrLn (show e))
        else continuation True

packageTest :: PackageAction
packageTest = do
    package <- ask
    lift $ packageTest' package (\ _ -> return ())

packageTest' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageTest' package continuation =
    if not . null $ ipdTests package
        then do
          logLaunch <- getDefaultLogLaunch
          showDefaultLogLaunch'
          catchIDE (do
            let dir = dropFileName (ipdCabalFile package)
            runExternalTool' "Testing" "cabal" (["test"]
                ++ (ipdTestFlags package)) (Just dir) $ do
                    (mbLastOutput, _) <- EL.zip E.last (logOutput logLaunch)
                    lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
            (\(e :: SomeException) -> putStrLn (show e))
        else continuation True

packageSdist :: PackageAction
packageSdist = do
    package <- ask
    logLaunch <- lift $ getDefaultLogLaunch
    lift $ showDefaultLogLaunch'

    lift $ catchIDE (do
        let dir = dropFileName (ipdCabalFile package)
        runExternalTool' "Source Dist" "cabal" (["sdist"]
                        ++ (ipdSdistFlags package)) (Just dir) (logOutput logLaunch))
        (\(e :: SomeException) -> putStrLn (show e))


packageOpenDoc :: PackageAction
packageOpenDoc = do
    package <- ask
    logLaunch <- lift $ getDefaultLogLaunch
    lift $ showDefaultLogLaunch'

    lift $ catchIDE (do
        prefs   <- readIDE prefs
        let path = dropFileName (ipdCabalFile package)
                        </> "dist/doc/html"
                        </> display (pkgName (ipdPackageId package))
                        </> display (pkgName (ipdPackageId package))
                        </> "index.html"
            dir = dropFileName (ipdCabalFile package)
        runExternalTool' "Opening Documentation" (browser prefs) [path] (Just dir) (logOutput logLaunch))
        (\(e :: SomeException) -> putStrLn (show e))

runExternalTool' :: String
                -> FilePath
                -> [String]
                -> Maybe FilePath
                -> E.Iteratee ToolOutput IDEM ()
                -> IDEAction
runExternalTool' description executable args mbDir handleOutput = do
        runExternalTool (do
                            run <- isRunning
                            return (not run))
                        (\_ -> return ())
                        description
                        executable
                        args
                        mbDir
                        handleOutput
        return()

runExternalTool :: IDEM Bool
                -> (ProcessHandle -> IDEAction)
                -> String
                -> FilePath
                -> [String]
                -> Maybe FilePath
                -> E.Iteratee ToolOutput IDEM ()
                -> IDEAction
runExternalTool runGuard pidHandler description executable args mbDir handleOutput  = do
        prefs <- readIDE prefs
        run <- runGuard
        liftIO $ putStrLn $ "description" ++ description++ ", executable" ++executable
        when run $ do
            when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToWorkspace; return ())
            triggerEventIDE (StatusbarChanged [CompartmentState description, CompartmentBuild True])
            reifyIDE $ \ideR -> forkIO $ do
                (output, pid) <- runTool executable args mbDir
                reflectIDE (do
                    pidHandler pid
                    modifyIDE_ (\ide -> ide{runningTool = Just pid})
                    E.run_ $ output $$ handleOutput) ideR
            return ()


runPackage ::  (ProcessHandle -> IDEAction)
            -> String
            -> FilePath
            -> [String]
            -> Maybe FilePath
            -> E.Iteratee ToolOutput IDEM ()
            -> IDEAction
runPackage = runExternalTool (return True) -- TODO here one could check if package to be run is building/configuring/etc atm


-- ---------------------------------------------------------------------
-- | Handling of Compiler errors
--
isRunning :: IDEM Bool
isRunning = do
    maybeProcess <- readIDE runningTool
    liftIO $ do
        case maybeProcess of
            Just process -> do
                isNothing <$> getProcessExitCode process
            Nothing -> return False

interruptBuild :: IDEAction
interruptBuild = do
    maybeProcess <- readIDE runningTool
    liftIO $ case maybeProcess of
#ifdef MIN_VERSION_process_leksah
        Just h -> interruptProcessGroup h
#else
        Just h -> interruptProcessGroupOf h
#endif
        _ -> return ()

-- ---------------------------------------------------------------------
-- | * Utility functions/procedures, that have to do with packages
--

getPackageDescriptionAndPath :: IDEM (Maybe (PackageDescription,FilePath))
getPackageDescriptionAndPath = do
    active <- readIDE activePack
    case active of
        Nothing -> do
            ideMessage Normal "No active package"
            return Nothing
        Just p  -> do
            ideR <- ask
            reifyIDE (\ideR -> catch (do
                pd <- readPackageDescription normal (ipdCabalFile p)
                return (Just (flattenPackageDescription pd,ipdCabalFile p)))
                    (\(e :: SomeException) -> do
                        reflectIDE (ideMessage Normal ("Can't load package " ++(show e))) ideR
                        return Nothing))

getEmptyModuleTemplate :: PackageDescription -> String -> IO String
getEmptyModuleTemplate pd modName = getModuleTemplate pd modName "" ""

getModuleTemplate :: PackageDescription -> String -> String -> String -> IO String
getModuleTemplate pd modName exports body = catch (do
    dataDir  <- getDataDir
    filePath <- getConfigFilePathForLoad standardModuleTemplateFilename Nothing dataDir
    template <- UTF8.readFile filePath
    return (foldl' (\ a (from, to) -> replace from to a) template
        [   ("@License@"      , (show . license) pd)
        ,   ("@Maintainer@"   , maintainer pd)
        ,   ("@Stability@"    , stability pd)
        ,   ("@Portability@"  , "")
        ,   ("@Copyright@"    , copyright pd)
        ,   ("@ModuleName@"   , modName)
        ,   ("@ModuleExports@", exports)
        ,   ("@ModuleBody@"   , body)]))
                    (\ (e :: SomeException) -> sysMessage Normal ("Couldn't read template file: " ++ show e) >> return "")

addModuleToPackageDescr :: ModuleName -> Bool -> PackageAction
addModuleToPackageDescr moduleName isExposed = do
    p    <- ask
    lift $ reifyIDE (\ideR -> catch (do
        gpd <- readPackageDescription normal (ipdCabalFile p)
        if hasConfigs gpd
            then do
                reflectIDE (ideMessage High
                    "Cabal file with configurations can't be automatically updated with the current version of Leksah") ideR
            else
                let pd = flattenPackageDescription gpd
                    npd = if isExposed && isJust (library pd)
                            then pd{library = Just ((fromJust (library pd)){exposedModules =
                                                            moduleName : exposedModules (fromJust $ library pd)})}
                            else let npd1 = case library pd of
                                               Nothing -> pd
                                               Just lib -> pd{library = Just (lib{libBuildInfo =
                                                        addModToBuildInfo (libBuildInfo lib) moduleName})}
                               in npd1{executables = map
                                        (\exe -> exe{buildInfo = addModToBuildInfo (buildInfo exe) moduleName})
                                            (executables npd1)}
                in writePackageDescription (ipdCabalFile p) npd)
                   (\(e :: SomeException) -> do
                    reflectIDE (ideMessage Normal ("Can't upade package " ++ show e)) ideR
                    return ()))
    where
    addModToBuildInfo :: BuildInfo -> ModuleName -> BuildInfo
    addModToBuildInfo bi mn = bi {otherModules = mn : otherModules bi}

--------------------------------------------------------------------------
delModuleFromPackageDescr :: ModuleName -> PackageAction
delModuleFromPackageDescr moduleName = do
    p    <- ask
    lift $ reifyIDE (\ideR -> catch (do
        gpd <- readPackageDescription normal (ipdCabalFile p)
        if hasConfigs gpd
            then do
                reflectIDE (ideMessage High
                    "Cabal file with configurations can't be automatically updated with the current version of Leksah") ideR
            else
                let pd = flattenPackageDescription gpd
                    isExposedAndJust = isExposedModule pd moduleName
                    npd = if isExposedAndJust
                            then pd{library = Just ((fromJust (library pd)){exposedModules =
                                                             delete moduleName (exposedModules (fromJust $ library pd))})}
                            else let npd1 = case library pd of
                                               Nothing -> pd
                                               Just lib -> pd{library = Just (lib{libBuildInfo =
                                                        delModFromBuildInfo (libBuildInfo lib) moduleName})}
                               in npd1{executables = map
                                        (\exe -> exe{buildInfo = delModFromBuildInfo (buildInfo exe) moduleName})
                                            (executables npd1)}
                in writePackageDescription (ipdCabalFile p) npd)
                   (\(e :: SomeException) -> do
                    reflectIDE (ideMessage Normal ("Can't update package " ++ show e)) ideR
                    return ()))
    where
    delModFromBuildInfo :: BuildInfo -> ModuleName -> BuildInfo
    delModFromBuildInfo bi mn = bi {otherModules = delete mn (otherModules bi)}


isExposedModule :: PackageDescription -> ModuleName -> Bool
isExposedModule pd mn = do
    if isJust (library pd)
        then elem mn (exposedModules (fromJust $ library pd))
        else False

--------------------------------------------------------------------------


backgroundBuildToggled :: IDEAction
backgroundBuildToggled = do
    toggled <- getBackgroundBuildToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){backgroundBuild= toggled}})

makeModeToggled :: IDEAction
makeModeToggled = do
    toggled <- getMakeModeToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){makeMode= toggled}})

-- ---------------------------------------------------------------------
-- | * Debug code that needs to use the package
--

interactiveFlag :: String -> Bool -> String
interactiveFlag name f = (if f then "-f" else "-fno-") ++ name

printEvldWithShowFlag :: Bool -> String
printEvldWithShowFlag = interactiveFlag "print-evld-with-show"

breakOnExceptionFlag :: Bool -> String
breakOnExceptionFlag = interactiveFlag "break-on-exception"

breakOnErrorFlag :: Bool -> String
breakOnErrorFlag = interactiveFlag "break-on-error"

printBindResultFlag :: Bool -> String
printBindResultFlag = interactiveFlag "print-bind-result"

interactiveFlags :: Prefs -> [String]
interactiveFlags prefs =
    (printEvldWithShowFlag $ printEvldWithShow prefs)
    : (breakOnExceptionFlag $ breakOnException prefs)
    : (breakOnErrorFlag $ breakOnError prefs)
    : [printBindResultFlag $ printBindResult prefs]

debugStart :: PackageAction
debugStart = do
    package   <- ask
    lift $ catchIDE (do
        ideRef     <- ask
        prefs'     <- readIDE prefs
        maybeDebug <- readIDE debugState
        case maybeDebug of
            Nothing -> do
                ghci <- reifyIDE $ \ideR -> newGhci (ipdBuildFlags package) (interactiveFlags prefs')
                    $ reflectIDEI (logOutputForBuild package True False) ideR
                modifyIDE_ (\ide -> ide {debugState = Just (package, ghci)})
                triggerEventIDE (Sensitivity [(SensitivityInterpreting, True)])
                setDebugToggled True
                -- Fork a thread to wait for the output from the process to close
                liftIO $ forkIO $ do
                    readMVar (outputClosed ghci)
                    reflectIDE (do
                        setDebugToggled False
                        modifyIDE_ (\ide -> ide {debugState = Nothing})
                        triggerEventIDE (Sensitivity [(SensitivityInterpreting, False)])
                        -- Kick of a build if one is not already due
                        modifiedPacks <- fileCheckAll belongsToPackage
                        let modified = not (null modifiedPacks)
                        prefs <- readIDE prefs
                        when ((not modified) && (backgroundBuild prefs)) $ do
                            -- So although none of the pakages are modified,
                            -- they may have been modified in ghci mode.
                            -- Lets build to make sure the binaries are up to date
                            mbPackage   <- readIDE activePack
                            case mbPackage of
                                Just package -> runCabalBuild True False True package True (\ _ -> return ())
                                Nothing -> return ()) ideRef
                return ()
            _ -> do
                sysMessage Normal "Debugger already running"
                return ())
            (\(e :: SomeException) -> putStrLn (show e))

tryDebug :: DebugM a -> PackageM (Maybe a)
tryDebug f = do
    maybeDebug <- lift $ readIDE debugState
    case maybeDebug of
        Just debug -> do
            -- TODO check debug package matches active package
            liftM Just $ lift $ runDebug f debug
        _ -> do
            window <- lift $ getMainWindow
            resp <- liftIO $ do
                md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                        "GHCi debugger is not running."
                dialogAddButton md "_Start GHCi" (ResponseUser 1)
                dialogSetDefaultResponse md (ResponseUser 1)
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetDestroy md
                return resp
            case resp of
                ResponseUser 1 -> do
                    debugStart
                    maybeDebug <- lift $ readIDE debugState
                    case maybeDebug of
                        Just debug -> liftM Just $ lift $ runDebug f debug
                        _ -> return Nothing
                _  -> return Nothing

tryDebug_ :: DebugM a -> PackageAction
tryDebug_ f = tryDebug f >> return ()

executeDebugCommand :: String -> (E.Iteratee ToolOutput IDEM ()) -> DebugAction
executeDebugCommand command handler = do
    (_, ghci) <- ask
    lift $ do
        triggerEventIDE (StatusbarChanged [CompartmentState command, CompartmentBuild True])
        reifyIDE $ \ideR -> do
            executeGhciCommand ghci command $ do
                reflectIDEI handler ideR
                liftIO $ reflectIDE (triggerEventIDE (StatusbarChanged [CompartmentState "", CompartmentBuild False])) ideR
                return ()

allBuildInfo' :: PackageDescription -> [BuildInfo]
#if MIN_VERSION_Cabal(1,10,0)
allBuildInfo' pkg_descr = [ libBuildInfo lib  | Just lib <- [library pkg_descr] ]
                       ++ [ buildInfo exe     | exe <- executables pkg_descr ]
                       ++ [ testBuildInfo tst | tst <- testSuites pkg_descr ]
testMainPath (TestSuiteExeV10 _ f) = [f]
testMainPath _ = []
#else
allBuildInfo' = allBuildInfo
#endif

idePackageFromPath :: FilePath -> IDEM (Maybe IDEPackage)
idePackageFromPath filePath = do
    mbPackageD <- reifyIDE (\ideR -> catch (do
        pd <- readPackageDescription normal filePath
        return (Just (flattenPackageDescription pd)))
            (\ (e  :: SomeException) -> do
                reflectIDE (ideMessage Normal ("Can't activate package " ++(show e))) ideR
                return Nothing))
    case mbPackageD of
        Nothing       -> return Nothing
        Just packageD -> do
            let modules    = Set.fromList $ myLibModules packageD ++ myExeModules packageD
            let mainFiles  = map modulePath (executables packageD)
#if MIN_VERSION_Cabal(1,10,0)
                               ++ concatMap (testMainPath . testInterface) (testSuites packageD)
#endif
            let files      = Set.fromList $ extraSrcFiles packageD ++ mainFiles
            let srcDirs = case (nub $ concatMap hsSourceDirs (allBuildInfo' packageD)) of
                                [] -> [".","src"]
                                l -> l
#if MIN_VERSION_Cabal(1,10,0)
            let exts       = nub $ concatMap oldExtensions (allBuildInfo' packageD)
            let tests      = [ testName t | t <- testSuites packageD
                                          -- , testEnabled t
                                          , buildable (testBuildInfo t) ]
#else
            let exts       = nub $ concatMap extensions (allBuildInfo' packageD)
            let tests      = []
#endif

            let packp      = IDEPackage {
                ipdPackageId = package packageD,
                ipdCabalFile = filePath,
                ipdDepends = buildDepends packageD,
                ipdModules = modules,
                ipdHasLibs = hasLibs packageD,
                ipdTests   = tests,
                ipdMain    = mainFiles,
                ipdExtraSrcs =  files,
                ipdSrcDirs = srcDirs,
                ipdExtensions =  exts,
                ipdConfigFlags = ["--user"],
                ipdBuildFlags = [],
                ipdTestFlags = [],
                ipdHaddockFlags = [],
                ipdExeFlags = [],
                ipdInstallFlags = [],
                ipdRegisterFlags = [],
                ipdUnregisterFlags = [],
                ipdSdistFlags = []}
            let pfile      = dropExtension filePath
            pack <- (do
                flagFileExists <- liftIO $ doesFileExist (pfile ++ leksahFlagFileExtension)
                if flagFileExists
                    then liftIO $ readFlags (pfile ++ leksahFlagFileExtension) packp
                    else return packp)
            return (Just pack)

