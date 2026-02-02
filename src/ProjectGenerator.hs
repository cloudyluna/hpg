module ProjectGenerator (boot) where

import Data.ByteString (StrictByteString)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as EE
import Effectful.Exception (Exception)
import Effectful.Exception qualified as ES
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as EFS
import Effectful.FileSystem.IO.ByteString qualified as EFS
import System.FilePath (FilePath, (<.>), (</>))


boot :: IO ()
boot = runEff . EFS.runFileSystem $ do
    generateLib
    generateTest
    generateApp


generateLib :: (FileSystem :> es, IOE :> es) => Eff es ()
generateLib = do
    let targetDir = "srcs"

    EFS.createDirectoryIfMissing True targetDir
    generateLibFile "ewo" targetDir


libFileContent :: String -> StrictByteString
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


generateLibFile :: (FileSystem :> es) => String -> FilePath -> Eff es ()
generateLibFile projectName dirName = do
    let filename = dirName </> projectName <.> "hs"

    EFS.writeFile filename $ libFileContent projectName


generateTest :: (FileSystem :> es) => Eff es ()
generateTest = do
    EFS.createDirectoryIfMissing True "tests"


generateApp :: (FileSystem :> es) => Eff es ()
generateApp = do
    EFS.createDirectoryIfMissing True "apps"
