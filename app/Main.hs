{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Aeson ( ToJSON )
import Data.Foldable ( traverse_ )
import Data.Maybe ( fromMaybe )
import Data.Text ( pack, unpack )
import GHC.Generics ( Generic )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop, setPort, runSettings, defaultSettings )
import Servant
    ( Proxy(..),
      serve,
      serveDirectoryWebApp,
      type (:<|>)(..),
      FormUrlEncoded,
      JSON,
      QueryParam,
      Raw,
      ReqBody,
      type (:>),
      Get,
      Post,
      Server,
      Handler )
import Servant.API.ContentTypes ()
import Servant.Multipart
    ( lookupFile,
      lookupInput,
      FileData(fdFileName, fdPayload),
      FromMultipart(..),
      MultipartForm,
      Tmp )
import System.Environment ( getArgs )
import System.Directory
    ( createDirectory,
      doesDirectoryExist,
      listDirectory,
      removeDirectoryRecursive,
      removeFile,
      renamePath )
import System.IO ( stderr, hPutStrLn )
import Web.FormUrlEncoded ( FromForm )

main :: IO ()
main = do
    root <- head <$> getArgs
    let port = 3000
        settings =
            setPort port $
            setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
            defaultSettings
    runSettings settings =<< mkApp root

mkApp :: String -> IO Application
mkApp root = return $ serve itemApi (server root)

type ItemApi
       = "ls" :> QueryParam "dir" String :> Get '[JSON] [File]
    :<|> "newDir" :> ReqBody '[FormUrlEncoded] NewDirRequest :> Post '[JSON] ()
    :<|> "rename" :> ReqBody '[FormUrlEncoded] RenameRequest :> Post '[JSON] ()
    :<|> "move" :> ReqBody '[FormUrlEncoded] MoveRequest :> Post '[JSON] ()
    :<|> "delete" :> ReqBody '[FormUrlEncoded] DeleteRequest :> Post '[JSON] ()
    :<|> "upload" :> MultipartForm Tmp UploadRequest :> Post '[JSON] ()

itemApi :: Proxy ItemApi
itemApi = Proxy

server :: String -> Server ItemApi
server root
       = ls root
    :<|> newDir root
    :<|> rename root
    :<|> move root
    :<|> delete root
    :<|> upload root

-- ls

data File = File
    { name :: String
    , isDir :: Bool
    } deriving Generic

instance ToJSON File

ls :: String -> Maybe String -> Handler [File]
ls root maybeDir = liftIO $ do
    let dir = root ++ fromMaybe "" maybeDir
    fileNames <- listDirectory dir
    traverse (\name -> File name <$> doesDirectoryExist (dir ++ name)) fileNames

-- newDir

data NewDirRequest = NewDirRequest
    { dir :: String
    , name :: String
    } deriving Generic

instance FromForm NewDirRequest

newDir :: String -> NewDirRequest -> Handler ()
newDir root NewDirRequest {..} = liftIO $ createDirectory (root ++ dir ++ name)

-- rename

data RenameRequest = RenameRequest
    { dir :: String
    , oldName :: String
    , newName :: String
    } deriving Generic

instance FromForm RenameRequest

rename :: String -> RenameRequest -> Handler ()
rename root RenameRequest {..} = liftIO $ renamePath (absDir ++ oldName) (absDir ++ newName)
    where
        absDir = root ++ dir

-- move

data MoveRequest = MoveRequest
    { srcDir :: String
    , files :: [String]
    , dstDir :: String
    } deriving Generic

instance FromForm MoveRequest

move :: String -> MoveRequest -> Handler ()
move root MoveRequest {..} = liftIO $ traverse_ move files
    where
        move file = renamePath (root ++ srcDir ++ file) (root ++ dstDir ++ file)

-- delete

data DeleteRequest = DeleteRequest
    { dir :: String
    , files :: [String]
    } deriving Generic

instance FromForm DeleteRequest

delete :: String -> DeleteRequest -> Handler ()
delete root DeleteRequest {..} = liftIO $ traverse_ (deletePath root dir) files

deletePath :: String -> String -> String -> IO ()
deletePath root dir file = do
    let file' = root ++ dir ++ file
    isDir <- doesDirectoryExist file'
    if isDir
        then removeDirectoryRecursive file'
        else removeFile file'

-- upload

data UploadRequest = UploadRequest
    { dir :: String
    , file :: String
    , tmp :: FilePath
    }

instance FromMultipart Tmp UploadRequest where
    fromMultipart multipartData = do
        dir <- lookupInput (pack "dir") multipartData
        fileData <- lookupFile (pack "file") multipartData
        return $ UploadRequest (unpack dir) (unpack $ fdFileName fileData) (fdPayload fileData)

upload :: String -> UploadRequest -> Handler ()
upload root UploadRequest {..} = liftIO $ renamePath tmp (root ++ dir ++ file)
