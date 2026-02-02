module ProjectGenerator (boot) where

import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (StrictByteString)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Default (Default (def))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as EFS
import Effectful.FileSystem.IO.ByteString qualified as EFS
import System.FilePath ((<.>), (</>))
import Text.Mustache qualified as M


boot :: IO ()
boot = runEff . EFS.runFileSystem $ do
    let project =
            mkProject
                "ewoos2"
                "This is pretty fun"
                "Momo"
                "luna.cloudberry@gmail.com"
                "BSD-3-Clause"
        dataDir = "data"
        targetDir = T.unpack project.name

    generateLib project
    generateTest project
    generateProjectCabal project

    
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
    , license :: Text
    }
    deriving stock (Eq, Show)


mkProject :: Text -> Text -> Text -> Text -> Text -> Project
mkProject name synopsis = Project (T.toTitle name) (T.toTitle synopsis)


mkStacheObject :: Project -> Value
mkStacheObject project =
    Aeson.object
        [ "name" .= project.name
        , "synopsis" .= project.synopsis
        , "author" .= project.author
        , "authorEmail" .= project.authorEmail
        , "license" .= project.license
        ]


instance Default Project where
    def =
        Project
            { name = "Project"
            , synopsis = "My fun Haskell project"
            , author = "Momo"
            , authorEmail = "luna.cloudberry@gmail.com"
            , license = "BSD-3-Clause"
            }


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
