module ProjectGenerator (boot) where

import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as EFS
import Effectful.FileSystem.IO.ByteString qualified as EFS
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as R
import System.FilePath ((<.>), (</>))
import Text.Mustache qualified as M
import Prelude hiding (getLine)


boot :: IO ()
boot = runEff . EFS.runFileSystem . Console.runConsole $ do
    project <- promptUserProject
    dataDir <- getDataDirPath

    let targetDir = T.unpack project.name

    R.runReader @AppEnv (mkAppEnv dataDir targetDir project) $ do
        generateLib
        generateTest
        generateProjectCabal
        generateLicense

        EFS.copyFile (dataDir </> "cabal.project") (targetDir </> "cabal.project")
        EFS.copyFile (dataDir </> "fourmolu.yaml") (targetDir </> "fourmolu.yaml")
        EFS.copyFile (dataDir </> "hie.yaml") (targetDir </> "hie.yaml")
        EFS.copyFile (dataDir </> ".gitignore") (targetDir </> ".gitignore")
        EFS.copyFile (dataDir </> "flake.nix") (targetDir </> "flake.nix")
        EFS.copyFile (dataDir </> ".envrc") (targetDir </> ".envrc")


getDataDirPath :: (FileSystem :> es) => Eff es FilePath
getDataDirPath = do
    exists <- EFS.doesDirectoryExist "data"

    if exists
        then
            pure "data"
        else
            EFS.getXdgDirectory EFS.XdgData $ "hpg" </> "data"


type DataDir = FilePath
type TargetDir = FilePath


data AppEnv = AppEnv
    { dataDir :: DataDir
    , targetDir :: TargetDir
    , project :: Project
    }
    deriving stock (Eq, Show)


mkAppEnv :: DataDir -> TargetDir -> Project -> AppEnv
mkAppEnv = AppEnv


data Project = Project
    { name :: Text
    , synopsis :: Text
    , author :: Text
    , authorEmail :: Text
    }
    deriving stock (Eq, Show)


mkProject :: Text -> Text -> Text -> Text -> Project
mkProject name synopsis = Project (T.toTitle name) (T.toTitle synopsis)


mkStacheObject :: Project -> Value
mkStacheObject project =
    Aeson.object
        [ "name" .= project.name
        , "synopsis" .= project.synopsis
        , "author" .= project.author
        , "authorEmail" .= project.authorEmail
        ]


promptUserProject :: (Console :> es) => Eff es Project
promptUserProject = do
    let getLine = T.decodeLatin1 <$> Console.getLine

    Console.putStr "Project Name: "
    name <- getLine
    Console.putStr "Synopsis: "
    synopsis <- getLine
    Console.putStr "Author Name: "
    author <- getLine
    Console.putStr "Author Email: "
    authorEmail <- getLine

    pure $ mkProject name synopsis author authorEmail


parseTemplate :: (IOE :> es, Reader AppEnv :> es) => FilePath -> Eff es StrictByteString
parseTemplate templateFile = do
    env <- R.ask @AppEnv

    template <- liftIO $ M.compileMustacheFile templateFile
    let text = M.renderMustache template $ mkStacheObject env.project

    pure . BL.toStrict $ TL.encodeUtf8 text


generateTemplate ::
    (FileSystem :> es, IOE :> es, Reader AppEnv :> es) => FilePath -> FilePath -> Eff es ()
generateTemplate sourceFile targetFile = do
    env <- R.ask @AppEnv

    parseTemplate (env.dataDir </> sourceFile) >>= EFS.writeFile targetFile


generateLib :: (FileSystem :> es, IOE :> es, Reader AppEnv :> es) => Eff es ()
generateLib = do
    env <- R.ask @AppEnv

    let targetDir = env.targetDir </> "src"
        targetFilename = T.unpack env.project.name <.> "hs"

    EFS.createDirectoryIfMissing True targetDir
    generateTemplate "lib.mustache" $ targetDir </> targetFilename


generateTest :: (FileSystem :> es, IOE :> es, Reader AppEnv :> es) => Eff es ()
generateTest = do
    env <- R.ask @AppEnv

    let targetDir = env.targetDir </> "test"
        targetFilename = "Main.hs"

    EFS.createDirectoryIfMissing True targetDir
    generateTemplate "test.mustache" $ targetDir </> targetFilename


generateProjectCabal :: (FileSystem :> es, IOE :> es, Reader AppEnv :> es) => Eff es ()
generateProjectCabal = do
    env <- R.ask @AppEnv

    let targetDir = env.targetDir
        targetFilename = T.unpack env.project.name <.> "cabal"

    EFS.createDirectoryIfMissing True targetDir
    generateTemplate "project.cabal.mustache" $ targetDir </> targetFilename


generateLicense :: (FileSystem :> es, IOE :> es, Reader AppEnv :> es) => Eff es ()
generateLicense = do
    env <- R.ask @AppEnv

    let targetDir = env.targetDir
        targetFilename = "LICENSE"

    EFS.createDirectoryIfMissing True targetDir
    generateTemplate "license.mustache" $ targetDir </> targetFilename
