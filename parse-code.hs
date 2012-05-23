import Text.Regex.PCRE ((=~))
--import Text.Regex.Posix
import qualified Data.ByteString.Char8 as BS
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.String.Utils (replace)
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Data.Maybe (isJust)

parseURLs :: BS.ByteString -> [String]
parseURLs page = map (\xs -> BS.unpack (xs !! 1)) matches
    where re = "<a\\s+href=\"([^\"/]*\\.html)\""
          matches = page =~ re :: [[BS.ByteString]]

downloadURL :: String -> IO BS.ByteString
downloadURL url = do
    rsp <- simpleHTTP (getRequest url)
    fmap BS.pack (getResponseBody rsp)

main :: IO ()
main = do
    mainPage <- downloadURL "http://book.realworldhaskell.org/read/"
    let urls = parseURLs mainPage
        pages = map (downloadURL . ("http://book.realworldhaskell.org/read/"++)) urls
    pages' <- sequence pages
    mapM_ processPage pages'
    return ()

processPage :: BS.ByteString -> IO ()
processPage content = mapM_  (uncurry writeFile') files
    where files = parsePage content

unJust :: Maybe a -> a
unJust (Just a) = a

writeFile' :: FilePath -> BS.ByteString -> IO ()
writeFile' f c = do
    when (isJust dir) $ createDirectoryIfMissing True (unJust dir)
    BS.writeFile f c
    where dir = parseDir f

parseDir :: FilePath -> Maybe String
parseDir f
    | null matches = Nothing
    | otherwise    = Just (head $ map (!! 1) matches)
    where re = "(\\w+)/"
          matches = f =~ re :: [[String]]

parsePage :: BS.ByteString -> [(String, BS.ByteString)]
parsePage page = Map.foldrWithKey (\f c xs -> clear f c:xs) [] files
    where codes = filter hasFileName (parseCode page)
          fnames = map parseFileName codes
          addToMap :: (BS.ByteString,BS.ByteString) -> Map.Map BS.ByteString BS.ByteString -> Map.Map BS.ByteString BS.ByteString
          append' e n = e `BS.append` (BS.pack "\n\n") `BS.append` n
          addToMap (f,c) = Map.insertWith append' f c
          emptyMap = Map.empty :: Map.Map BS.ByteString BS.ByteString
          files = foldr addToMap emptyMap (zip fnames codes)
          clear f c = (BS.unpack f, unescape c `BS.append` BS.pack "\n")

parseCode :: BS.ByteString -> [BS.ByteString]
parseCode html = map (!! 1) matches
    -- “ - is the problem, use ByteString instead of String
    where re = "<pre\\s+id\\s*=\\s*\".+?\"\\s+class\\s*=\\s*\"programlisting\"\\s*>([\\w\\W]*?)</pre>"
          matches = html =~ re :: [[BS.ByteString]]

hasFileName :: BS.ByteString -> Bool
hasFileName code = code =~ re
    where re = "^\\s*--\\s+file:\\s+\\w[\\w/\\.]*"

parseFileName :: BS.ByteString -> BS.ByteString
parseFileName code = head matches !! 1
    where re = "^\\s*--\\s+file:\\s+(\\w[\\w-/\\.]*)"
          matches = code =~ re :: [[BS.ByteString]]

unescape :: BS.ByteString -> BS.ByteString
unescape bs = BS.pack bs2
    where bs' = BS.unpack bs
          bs0 = replace "&lt;" "<" bs'
          bs1 = replace "&gt;" ">" bs0
          bs2 = replace "&amp;" "&" bs1
