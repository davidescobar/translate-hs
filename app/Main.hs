{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (mzero)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Maybe
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Time.Clock
import System.Console.ANSI
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment
import System.FilePath ((</>), takeDirectory, takeFileName)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import qualified Data.Text.ICU as RE
import qualified Data.Text.ICU.Replace as RE
import qualified Network.HTTP.Conduit as HTTP


data GoogleTranslation = GoogleTranslation {
  translation :: T.Text
} deriving Show

instance FromJSON GoogleTranslation where
  parseJSON (Object v) = do
    translations <- (v .: "data") >>= (.: "translations")
    translatedTexts <- mapM (.: "translatedText") translations
    let translatedText = if null translatedTexts then "" else head translatedTexts
    return $ GoogleTranslation { translation = translatedText }

  parseJSON _ = mzero


main :: IO ()
main = do
  startTime <- getCurrentTime >>= (return . utctDayTime)
  progName <- getProgName >>= (return . T.pack)
  args <- getArgs >>= (return . map T.pack)
  case args of
    [ i18nFolderPath, fromLocale, apiKey ] -> do
      i18nFiles <- getSourceYAMLPaths i18nFolderPath fromLocale
      if null i18nFiles
        then
          T.putStrLn $ "No " <> fromLocale <> " YAML files found!"
        else do
          let localeAbbreviations = Map.keys getIntoLocales
          errors <- translateFiles apiKey "en" localeAbbreviations i18nFiles >>= return . concat
          doneTime <- getCurrentTime >>= (return . utctDayTime)
          let secondsElapsed = doneTime - startTime
          if all isNothing errors
            then do
              setSGR [ SetColor Foreground Vivid Cyan ]
              T.putStr "\nAll i18n files translated successfully.\n"
              setSGR [ Reset ]
            else do
              T.putStr "\nThere were errors:\n"
              setSGR [ SetColor Foreground Vivid Red ]
              mapM_ T.putStrLn [ fromJust err | err <- errors, isJust err ]
              T.putStrLn "\n"
              setSGR [ Reset ]
          T.putStr $ "Task completed in " <> (T.pack $ show $ roundN 2 secondsElapsed) <> " seconds.\n"
          T.putStrLn "\n"
    _ ->
      T.putStrLn $ "Usage: " <> progName <> " [i18n-yaml-folder-path] [from-locale] [fasdfafdafa]"
  where
    roundN :: Int -> DiffTime -> Float
    roundN places num =
      fromIntegral (round $ num * 10^places) / 10^places


getIntoLocales :: Map.Map T.Text T.Text
getIntoLocales = Map.fromList [ ("es", "Spanish"), ("de", "German"), ("hi", "Hindi"),
                                ("zh-cn", "Chinese (simplified)"), ("pt-br", "Portuguese (Brazil)") ]


getSourceYAMLPaths :: T.Text -> T.Text -> IO [T.Text]
getSourceYAMLPaths i18nFolderPath forLocale = do
  let pathAsString = T.unpack i18nFolderPath
  isFolder <- doesDirectoryExist pathAsString
  if isFolder
    then do
      files <- getDirectoryContents pathAsString >>= return . map (T.pack . (pathAsString </>))
      return [ file | file <- files, file `notElem` [".", ".."],
                                     ".yml" `T.isSuffixOf` (T.toLower file),
                                     (forLocale <> ".") `T.isInfixOf` file ]
    else
      return []


translateFiles :: T.Text -> T.Text -> [T.Text] -> [T.Text] -> IO [[Maybe T.Text]]
translateFiles apiKey fromLocale intoLocales i18nFiles = do
  let localesAndFiles = [ (locale, file) | locale <- intoLocales, file <- i18nFiles ]
  mapConcurrently (\(intoLocale, i18nFile) -> do
                      cachedTranslations <- newTVarIO (Map.fromList [] :: Map.Map T.Text T.Text)
                      translateFile apiKey fromLocale intoLocale cachedTranslations i18nFile)
                  localesAndFiles


translateFile :: T.Text -> T.Text -> T.Text -> TVar (Map.Map T.Text T.Text) -> T.Text -> IO [Maybe T.Text]
translateFile apiKey fromLocale intoLocale cachedTranslations i18nFilePath = do
  i18nFileExists <- doesFileExist $ T.unpack i18nFilePath
  if i18nFileExists
    then do
      setSGR [ SetColor Foreground Vivid Blue ]
      let intoLang = fromMaybe intoLocale $ Map.lookup intoLocale getIntoLocales
      T.putStr $ "Translating " <> (i18nFilePath & T.unpack & takeFileName & T.pack) <> " into " <> intoLang <> "...\n"
      tlsManager <- HTTP.newManager HTTP.tlsManagerSettings
      fileLines <- (T.readFile $ T.unpack i18nFilePath) >>= return . T.lines
      translatedLines <- mapConcurrently (translateLines 1 10 apiKey fromLocale intoLocale cachedTranslations tlsManager)
                                         (List.chunksOf 25 $ zip [1..] fileLines) >>= return . concat
      let i18nFileName = i18nFilePath & T.unpack & takeFileName & T.pack
          translatedFileNameRegex = (RE.regex [] $ fromLocale <> ".")
          translatedFileName = RE.replace translatedFileNameRegex (RE.rtext $ intoLocale <> ".") i18nFileName
          translatedFilePath = (takeDirectory $ T.unpack i18nFilePath) </> (T.unpack translatedFileName)
          outputStr = T.unlines translatedLines
      outputAttempt <- try $ T.writeFile translatedFilePath outputStr :: IO (Either SomeException ())
      case outputAttempt of
        Left err -> do
          let errorMessage = i18nFileName <> ": " <> (T.pack $ displayException err)
          return [ Just errorMessage ]
        Right _ -> do
          setSGR [ SetColor Foreground Vivid Cyan ]
          T.putStr $ "Created new file: " <> (T.pack translatedFilePath) <> "\n"
          setSGR [ Reset ]
          return [ Nothing ]
    else
      return [ Just $ "File '" <> i18nFilePath <> "' does not exist." ]


translateLines :: Int -> Int -> T.Text -> T.Text -> T.Text -> TVar (Map.Map T.Text T.Text) ->
                  HTTP.Manager -> [(Int, T.Text)] -> IO [T.Text]
translateLines attempt maxAttempts apiKey fromLocale intoLocale cachedTranslations tlsManager fileLines =
  mapM (translateLine attempt maxAttempts apiKey fromLocale intoLocale cachedTranslations tlsManager) fileLines


translateLine :: Int -> Int -> T.Text -> T.Text -> T.Text -> TVar (Map.Map T.Text T.Text) ->
                 HTTP.Manager -> (Int, T.Text) -> IO T.Text
translateLine attempt maxAttempts apiKey fromLocale intoLocale cachedTranslations tlsManager (lineNum, line) = do
  if attempt <= maxAttempts
    then do
      if isFromLocaleLine fromLocale line
        then return $ (T.strip intoLocale) <> ":"
        else
          if isCommentLine line
            then return line
            else
              case extractYAMLValue $ removeCommentsFromLine line of
                Just value
                  | T.strip value == "" -> return line
                  | otherwise -> do
                      cacheMap <- readTVarIO cachedTranslations
                      case Map.lookup value cacheMap of
                        Just cachedValue -> do
                          return $ T.replace value (addSurroundingQuotes cachedValue) line
                        Nothing -> do
                          let lineParams = getYAMLParams line
                          let valueToTranslate = substituteForYAMLParams "**" (removeSurroundingQuotes value)
                          translationAttempt <- try $ getGoogleAPITranslation apiKey fromLocale intoLocale
                                                                              tlsManager valueToTranslate :: IO (Either SomeException T.Text)
                          case translationAttempt of
                            Left _err -> do
                              threadDelay $ (delayFromLineNumber lineNum) * 1000 -- Convert to microseconds
                              translateLine (attempt + 1) maxAttempts apiKey fromLocale intoLocale
                                            cachedTranslations tlsManager (lineNum, line)

                            Right translationJSONText -> do
                              case decode (LBS.fromStrict $ TE.encodeUtf8 translationJSONText) :: Maybe GoogleTranslation of
                                Just googleTranslation -> do
                                  let googleTranslationWithParams = substituteYAMLParamsForPlaceholder lineParams (translation googleTranslation)
                                  if (googleTranslationWithParams == value)
                                    then do
                                      threadDelay $ (delayFromLineNumber lineNum) * 1000 -- Convert to microseconds
                                      translateLine (attempt + 1) maxAttempts apiKey fromLocale intoLocale
                                                    cachedTranslations tlsManager (lineNum, line)
                                    else do
                                      atomically $ writeTVar cachedTranslations $ Map.insert value (translation googleTranslation) cacheMap
                                      return $ T.replace value (addSurroundingQuotes googleTranslationWithParams) line
                                Nothing -> do
                                  threadDelay $ (delayFromLineNumber lineNum) * 1000 -- Convert to microseconds
                                  translateLine (attempt + 1) maxAttempts apiKey fromLocale intoLocale
                                                cachedTranslations tlsManager (lineNum, line)
                Nothing -> do
                  return line
    else
      return $ line <> " # Not translated - all attempts failed or timed out."


addSurroundingQuotes :: T.Text -> T.Text
addSurroundingQuotes text =
  let trimmedText = T.strip text
  in
    if (not $ T.null trimmedText) &&
       (T.head trimmedText `notElem` ['\'', '"']) &&
       (T.last trimmedText `notElem` ['\'', '"'])
      then "\"" <> trimmedText <> "\""
      else trimmedText


removeSurroundingQuotes :: T.Text -> T.Text
removeSurroundingQuotes = T.dropAround (`elem` ['\'', '"']) . T.strip


getGoogleAPITranslation :: T.Text -> T.Text -> T.Text -> HTTP.Manager -> T.Text -> IO T.Text
getGoogleAPITranslation apiKey fromLocale intoLocale tlsManager term = do
  initialRequest <- HTTP.parseUrl "https://www.googleapis.com/language/translate/v2"
  let translateRequest = initialRequest {
                           HTTP.method = "GET",
                           HTTP.responseTimeout = Just 30000000,
                           HTTP.secure = True
                         }
  let queryParams = [ ("key", Just $ TE.encodeUtf8 apiKey),
                      ("source", Just $ TE.encodeUtf8 fromLocale),
                      ("target", Just $ TE.encodeUtf8 intoLocale),
                      ("q", Just $ TE.encodeUtf8 term) ]
  let trRequestWithParams = HTTP.setQueryString queryParams translateRequest
  runResourceT $ do
    response <- HTTP.httpLbs trRequestWithParams tlsManager
    let responseBody = TE.decodeUtf8 $ LBS.toStrict $ HTTP.responseBody response
    return responseBody


removeCommentsFromLine :: T.Text -> T.Text
removeCommentsFromLine line =
  let commentAtEndRegex = RE.regex [] ".+#[^'\"]*$"
  in
    case RE.find commentAtEndRegex $ T.strip line of
      Just _ -> RE.replace (RE.regex [] "\\s*#[^'\"]*$") (RE.rtext "") line
      Nothing -> line


isCommentLine :: T.Text -> Bool
isCommentLine = ("#" `T.isPrefixOf`) . T.stripStart


isFromLocaleLine :: T.Text -> T.Text -> Bool
isFromLocaleLine fromLocale line =
  ((T.strip fromLocale) <> ":") == (T.stripEnd line)


extractYAMLKey :: T.Text -> Maybe T.Text
extractYAMLKey line =
  let maybeMatch = RE.find "\\s*([^#][\\w\\-'\"]+):\\s*.+" line
  in
    case maybeMatch of
      Just match ->
        case RE.group 1 match of
          Just matchText -> Just matchText
          Nothing -> Nothing
      Nothing -> Nothing


extractYAMLValue :: T.Text -> Maybe T.Text
extractYAMLValue line =
  let maybeMatch = RE.find "\\s*[^#][\\w\\-'\"]+:\\s*(.+)" line
  in
    case maybeMatch of
      Just match ->
        case RE.group 1 match of
          Just matchText
            | matchText & T.strip & removeSurroundingQuotes & T.strip & T.null -> Nothing
            | otherwise -> Just matchText
          Nothing -> Nothing
      Nothing -> Nothing


delayFromLineNumber :: Int -> Int
delayFromLineNumber lineNumber =
  let numDigits = length $ show lineNumber
    in
      case numDigits of
        1 -> 50
        2 -> 100
        3 | lineNumber < 250 -> 750
          | (lineNumber >= 250) && (lineNumber < 750) -> 500
          | lineNumber >= 750 -> 250
        4 | lineNumber < 2500 -> 750
          | (lineNumber >= 2500) && (lineNumber < 7500) -> 500
          | lineNumber >= 7500 -> 250
        _ -> 1000


getYAMLParams :: T.Text -> [T.Text]
getYAMLParams line =
  let matches = mapM (RE.group 0) $ RE.findAll "%\\{[^\\}]*\\}" line
  in fromMaybe [] matches


substituteForYAMLParams :: T.Text -> T.Text -> T.Text
substituteForYAMLParams replacement line =
  let params = getYAMLParams line
  in
    if null params
      then line
      else List.foldl' (\newLine param -> T.replace param replacement newLine) line params


substituteYAMLParamsForPlaceholder :: [T.Text] -> T.Text -> T.Text
substituteYAMLParamsForPlaceholder params line =
  if null params
    then line
    else List.foldl' (\newLine param -> RE.replace (RE.regex [] "\\*\\*") (RE.rtext param) newLine) line params
