-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.Mercurial
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

module IDE.Command.VCS.Mercurial (
    mkMercurialActions
) where

import IDE.Core.Types
import IDE.Core.State

import qualified IDE.Command.VCS.Common.Helper as Helper
import qualified IDE.Command.VCS.Types as Types

import qualified VCSGui.Mercurial as MercurialGUI
import qualified VCSWrapper.Mercurial as Mercurial

commitAction :: Types.VCSAction ()
commitAction = Helper.createActionFromContext MercurialGUI.showCommitGUI

viewLogAction :: Types.VCSAction ()
viewLogAction = Helper.createActionFromContext MercurialGUI.showLogGUI

pushAction :: Types.VCSAction ()
pushAction = Helper.createActionFromContext $ Mercurial.push

pullAction :: Types.VCSAction ()
pullAction = Helper.createActionFromContext $ Mercurial.pull

mkMercurialActions :: [(String, Types.VCSAction ())]
mkMercurialActions = [
                ("_Commit", commitAction)
                ,("_View Log", viewLogAction)
                ,("_Push", pushAction)
                ,("_Pull", pullAction)
                ]
