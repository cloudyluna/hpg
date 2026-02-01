module ProjectGenerator (boot) where

import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as EFS
import Effectful.FileSystem.IO.ByteString qualified as EFS
import GHC.Stack (HasCallStack)
import System.FilePath

boot :: IO ()
boot = runEff . EFS.runFileSystem $ do
    generateLib
    generateTest
    generateApp

generateLib :: (HasCallStack, IOE :> es, FileSystem :> es) => Eff es ()
generateLib = do
    let dir = "srcs"
    EFS.createDirectoryIfMissing True dir
    generateLibFile "ewo" dir

libFileContent projectName =
    let name = BL.toStrict . B.toLazyByteString $ B.stringUtf8 projectName
     in "module "
            <> name
            <> " (boot) where\n\n"
            <> """
               import Effectful

               boot :: IO ()
               boot = pure ()

               """
            <> "\n"

generateLibFile :: (HasCallStack, IOE :> es, FileSystem :> es) => String -> FilePath -> Eff es ()
generateLibFile projectName dirName = do
    let filename = dirName </> projectName <.> "hs"

    EFS.writeFile filename $ libFileContent projectName

generateTest :: (HasCallStack, FileSystem :> es) => Eff es ()
generateTest = do
    EFS.createDirectoryIfMissing True "tests"

generateApp :: (HasCallStack, FileSystem :> es) => Eff es ()
generateApp = do
    EFS.createDirectoryIfMissing True "apps"
