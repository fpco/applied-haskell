#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
import RIO
import RIO.Process
import RIO.FilePath (takeExtension, replaceExtension, (</>), (<.>))
import RIO.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import Conduit
import Text.Sundown
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Text.Sundown.Html.ByteString
import qualified Text.HTML.DOM as DOM
import Text.XML (Document (..), Element (..), Node (..))
import Control.Monad.Writer.Strict (execWriter, tell)
import qualified RIO.HashSet as HS
import qualified RIO.Map as Map
import qualified "cryptonite" Crypto.Hash

data App = App
  { appProcessContext :: !ProcessContext
  , appLogFunc :: !LogFunc
  }
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

main :: IO ()
main = do
  pc <- mkDefaultProcessContext
  lo <- logOptionsHandle stderr True
  withLogFunc lo $ \lf -> runRIO App
    { appProcessContext = pc
    , appLogFunc = lf
    } run

splitFiles :: LByteString -> RIO App [FilePath]
splitFiles lbs
  | BL.null lbs = pure []
  | otherwise = do
      let (next, rest) = BL.break (== 0) lbs
      fp <- either throwIO pure $ decodeUtf8' $ BL.toStrict next
      rest' <- splitFiles $ BL.drop 1 rest
      pure $ T.unpack fp : rest'

data Content = Content
  { _urls :: !(HashSet Text)
  , _snippets :: !(HashSet Text)
  }
  deriving Show
instance Semigroup Content where
  Content a b <> Content x y = Content (a <> x) (b <> y)
instance Monoid Content where
  mempty = Content mempty mempty
  mappend = (<>)

run :: RIO App ()
run = do
  rawFiles <- proc "git" ["ls-files", "-z"] readProcessStdout_
  allFiles <- splitFiles rawFiles
  let markdowns = filter (\fp -> takeExtension fp == ".md") allFiles
  Content urls snippets <- runConduit $ yieldMany markdowns .| foldMapMC parseContent
  writeFileUtf8 ("etc" </> "urls.txt") $ T.unlines $ HS.toList urls
  createDirectoryIfMissing True ("etc" </> "snippets")
  snippetFPs <- runConduit $ yieldMany snippets .| foldMapMC (\text -> do
    let digest :: Crypto.Hash.Digest Crypto.Hash.SHA256
        digest = Crypto.Hash.hash $ encodeUtf8 text
        fp = "etc" </> "snippets" </> show digest <.> ".hs"
    exists <- doesFileExist fp
    unless exists $ do
      logDebug $ "Writing new file: " <> fromString fp
      writeFileUtf8 fp text
    pure $ HS.singleton fp
    )
  runConduitRes $ sourceDirectory ("etc" </> "snippets") .| mapM_C (\fp ->
    if
      | takeExtension fp /= ".hs" -> pure ()
      | fp `HS.member` snippetFPs -> do
        compiled <- doesFileExist $ replaceExtension fp ".o"
        unless compiled $
          proc "stack" ["--resolver", "lts-12.21", "ghc", "--", fp, "-fdefer-typed-holes"] runProcess_
      | otherwise -> do
        logDebug $ "Removing file: " <> fromString fp
        removeFile fp)

parseContent :: FilePath -> RIO App Content
parseContent fp = do
  md <- readFileBinary fp
  let html = renderHtml allExtensions noHtmlModes True Nothing md
      Document _ root _ = DOM.parseBSChunks [html]
  pure $ goElem root
  where
    goElem (Element "a" attrs children)
      | Just href <- Map.lookup "href" attrs =
          Content (HS.singleton href) mempty <> foldMap goNode children
    goElem (Element "code" _ [NodeContent text])
      | "#!/usr/bin/env stack" `T.isPrefixOf` text =
          Content mempty (HS.singleton $ stripWerror text)
    goElem (Element _ _ children) = foldMap goNode children

    goNode (NodeElement e) = goElem e
    goNode _ = mempty

stripWerror :: Text -> Text
stripWerror = T.unlines . filter (not . isWerror) . T.lines
  where
    isWerror = ("-Werror #-}" `T.isSuffixOf`)
