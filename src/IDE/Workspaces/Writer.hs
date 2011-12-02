-----------------------------------------------------------------------------
--
-- Module      :  IDE.Workspaces.Writer
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

module IDE.Workspaces.Writer (
    writeWorkspace
    ,setWorkspace
    ,workspaceDescr
    ,workspaceVersion
) where

import IDE.Core.Types
import IDE.Core.State
import IDE.Package
       (getModuleTemplate, getPackageDescriptionAndPath, activatePackage,
        deactivatePackage, idePackageFromPath)
import IDE.Utils.FileUtils(myCanonicalizePath)

import Data.Maybe
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import System.Time (getClockTime)
import Text.PrinterParser
    (readFields,
     writeFields,
     readParser,
     stringParser,
     intParser,
     mkFieldS,
     FieldDescriptionS(..))
import System.FilePath
       (takeFileName, (</>), isAbsolute, dropFileName, makeRelative,
        dropExtension, takeBaseName, addExtension, takeExtension,
        takeDirectory)
import Graphics.UI.Editor.Parameters
    (Parameter(..), (<<<-), paraName, emptyParams)
import qualified Text.PrettyPrint as  PP (text)

writeWorkspace :: Workspace -> IDEAction
writeWorkspace ws = do
    timeNow      <- liftIO getClockTime
    let newWs    =  ws {wsSaveTime = show timeNow,
                         wsVersion = workspaceVersion,
                         wsPackagesFiles = map ipdCabalFile (wsPackages ws)}
    setWorkspace $ Just newWs
    newWs' <- liftIO $ makePathesRelative newWs
    liftIO $ writeFields (wsFile newWs') (newWs' {wsFile = ""}) workspaceDescr

getPackage :: FilePath -> [IDEPackage] -> Maybe IDEPackage
getPackage fp packages =
    case filter (\ p -> ipdCabalFile p == fp) packages of
        [p] -> Just p
        l   -> Nothing

-- ---------------------------------------------------------------------
-- This needs to be incremented, when the workspace format changes
--
workspaceVersion :: Int
workspaceVersion = 1

setWorkspace :: Maybe Workspace -> IDEAction
setWorkspace mbWs = do
    mbOldWs <- readIDE workspace
    modifyIDE_ (\ide -> ide{workspace = mbWs})
    let packFile =  case mbWs of
                    Nothing -> Nothing
                    Just ws -> wsActivePackFile ws
    let oldPackFile = case mbOldWs of
                    Nothing -> Nothing
                    Just ws -> wsActivePackFile ws
    let mbPackages =  case mbWs of
                        Nothing -> Nothing
                        Just ws -> Just (wsPackages ws)
    when (packFile /= oldPackFile) $
            case packFile of
                Nothing -> deactivatePackage
                Just p  -> activatePackage (getPackage p (fromJust mbPackages)) >> return ()
    mbPack <- readIDE activePack
    let wsStr = case mbWs of
                    Nothing -> ""
                    Just ws -> wsName ws
    let txt = wsStr ++ " > " ++
                    (case mbPack of
                            Nothing -> ""
                            Just p  -> packageIdentifierToString (ipdPackageId p))
    triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
    triggerEventIDE (WorkspaceChanged True True)
    triggerEventIDE UpdateWorkspaceInfo
    return ()

makePathesRelative :: Workspace -> IO Workspace
makePathesRelative ws = do
    wsFile' <- myCanonicalizePath (wsFile ws)
    wsActivePackFile'           <-  case wsActivePackFile ws of
                                        Nothing -> return Nothing
                                        Just fp -> do
                                            nfp <- liftIO $ myCanonicalizePath fp
                                            return (Just (makeRelative (dropFileName wsFile') nfp))
    wsPackagesFiles'            <-  mapM myCanonicalizePath (wsPackagesFiles ws)
    let relativePathes          =   map (\p -> makeRelative (dropFileName wsFile') p) wsPackagesFiles'
    return ws {wsActivePackFile = wsActivePackFile', wsFile = wsFile', wsPackagesFiles = relativePathes}

workspaceDescr :: [FieldDescriptionS Workspace]
workspaceDescr = [
        mkFieldS
            (paraName <<<- ParaName "Version of workspace file format" $ emptyParams)
            (PP.text . show)
            intParser
            wsVersion
            (\ b a -> a{wsVersion = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Time of storage" $ emptyParams)
            (PP.text . show)
            stringParser
            wsSaveTime
            (\ b a -> a{wsSaveTime = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Name of the workspace" $ emptyParams)
            (PP.text . show)
            stringParser
            wsName
            (\ b a -> a{wsName = b})
    ,   mkFieldS
            (paraName <<<- ParaName "File paths of contained packages" $ emptyParams)
            (PP.text . show)
            readParser
            wsPackagesFiles
            (\b a -> a{wsPackagesFiles = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Maybe file path of an active package" $ emptyParams)
            (PP.text . show)
            readParser
            wsActivePackFile
            (\fp a -> a{wsActivePackFile = fp})
    ,   mkFieldS
            (paraName <<<- ParaName "Version Control System configurations for packages" $ emptyParams)
            (PP.text . show)
            readParser
            packageVcsConf
            (\filePath a -> a{packageVcsConf = filePath})]
