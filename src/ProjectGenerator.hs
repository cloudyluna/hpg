module ProjectGenerator (boot) where

import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Default (Default (def))
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
import System.FilePath ((<.>), (</>))
import Text.Mustache qualified as M
import Prelude hiding (getLine)


boot :: IO ()
boot = runEff . EFS.runFileSystem . Console.runConsole $ do
    project <- promptUserProject

    let dataDir = "data"
        targetDir = T.unpack project.name

    EFS.createDirectoryIfMissing True targetDir

    generateLib project
    generateTest project
    generateProjectCabal project
    generateLicense project

    EFS.copyFile (dataDir </> "cabal.project") (targetDir </> "cabal.project")
    EFS.copyFile (dataDir </> "fourmolu.yaml") (targetDir </> "fourmolu.yaml")
    EFS.copyFile (dataDir </> "hie.yaml") (targetDir </> "hie.yaml")
    EFS.copyFile (dataDir </> ".gitignore") (targetDir </> ".gitignore")
    EFS.copyFile (dataDir </> "flake.nix") (targetDir </> "flake.nix")
    EFS.copyFile (dataDir </> ".envrc") (targetDir </> ".envrc")


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


instance Default Project where
    def =
        Project
            { name = "Project"
            , synopsis = "My fun Haskell project"
            , author = "Momo"
            , authorEmail = "luna.cloudberry@gmail.com"
            }


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


parseTemplate :: (IOE :> es) => FilePath -> Project -> Eff es StrictByteString
parseTemplate templateFile project = do
    template <- liftIO $ M.compileMustacheFile templateFile
    let text = M.renderMustache template $ mkStacheObject project

    pure . BL.toStrict $ TL.encodeUtf8 text


type Parser es = FilePath -> Project -> Eff es StrictByteString
generateTemplate :: (FileSystem :> es) => FilePath -> Project -> Parser es -> Eff es ()
generateTemplate targetFile project f = f "data" project >>= EFS.writeFile targetFile


generateLib :: (FileSystem :> es, IOE :> es) => Project -> Eff es ()
generateLib project = do
    let targetDir = T.unpack project.name </> "src"

    EFS.createDirectoryIfMissing True targetDir
    generateLibFile targetDir project


generateLibFile :: (FileSystem :> es, IOE :> es) => FilePath -> Project -> Eff es ()
generateLibFile dirName project = do
    let filename = dirName </> T.unpack project.name <.> "hs"
    generateTemplate filename project parser
    where
        parser dataDir = parseTemplate $ dataDir </> "lib.mustache"


generateTest :: (FileSystem :> es, IOE :> es) => Project -> Eff es ()
generateTest project = do
    let targetDir = T.unpack project.name </> "test"

    EFS.createDirectoryIfMissing True targetDir
    generateTestFile targetDir project


generateTestFile :: (FileSystem :> es, IOE :> es) => FilePath -> Project -> Eff es ()
generateTestFile dirName project = do
    let filename = dirName </> "Main.hs"
    generateTemplate filename project parser
    where
        parser dataDir = parseTemplate $ dataDir </> "test.mustache"


generateProjectCabal :: (FileSystem :> es, IOE :> es) => Project -> Eff es ()
generateProjectCabal project = do
    let targetDir = T.unpack project.name

    EFS.createDirectoryIfMissing True targetDir
    generateProjectCabalFile targetDir project


generateProjectCabalFile :: (FileSystem :> es, IOE :> es) => FilePath -> Project -> Eff es ()
generateProjectCabalFile dirName project = do
    let filename = dirName </> T.unpack project.name <.> "cabal"
    generateTemplate filename project parser
    where
        parser dataDir = parseTemplate $ dataDir </> "project.cabal.mustache"


generateLicense :: (FileSystem :> es, IOE :> es) => Project -> Eff es ()
generateLicense project = do
    let targetDir = T.unpack project.name

    EFS.createDirectoryIfMissing True targetDir
    generateLicenseFile targetDir project


generateLicenseFile :: (FileSystem :> es, IOE :> es) => FilePath -> Project -> Eff es ()
generateLicenseFile dirName project = do
    let filename = dirName </> "LICENSE"

    generateTemplate filename project parser
    where
        parser dataDir = parseTemplate $ dataDir </> "license.mustache"
