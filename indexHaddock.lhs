
A relatively simple script to generate an index.html file for all of
the haddock index.html files inside your .cabal directory.  The first
thing you should do to use this, is to find out where your local
haddock files live.  They should be somewhere under your ~/.cabal
directory.  Mine are at "/home/henry/.cabal/store/ghc-9.8.2" since I'm
currently using ghc 9.8.2.  Once you've found your ghc-x.y.z
directory, symlink to this directory and name the symlink haddock.
Then run this script with cabal run, creating an index.html file in
this directory with the appropriate links.

import Data.Function ( on )
import Data.List ( sortBy, groupBy, isSuffixOf, partition, sortOn )
import Data.List.Split ( splitOn )
import Prelude (
  map, ($), Eq((==), (/=)), Monad(return), Ord(compare),
  Foldable(null, elem), Monoid(mconcat), Bool, String, Char,
  Ordering, IO, FilePath, unlines, dropWhile, (.), flip, error,
  (!!), drop, reverse, putStrLn, writeFile )
import Shelly ( shelly, silently, findWhen, liftIO, Sh )

> import Data.Function
> import Data.List 
> import Data.List.Split 
> import Prelude
> import Shelly
> import qualified Data.Text as T

This is the name of the directory that should be symlinked to
ghc-x.y.z directory explained above

> dir :: String
> dir = "haddock"

A filter looking for files whose names are "index.html"

> hasIndex :: FilePath -> Sh Bool
> hasIndex = return . isSuffixOf "/index.html" 

Creates an a link whose href points to the actual file and whose
innerHtml is the name of the package

> addLink :: (String, String) -> String
> addLink (x,y) = mconcat
>     ["<br/><a href=\"", y, "\">",x,"</a>"]

A kinda complicated manipulation that turns something like this:
  
(A) haddock/shelly-1.12.1-e26fd9eb9e074999bcc96c/share/doc/html/index.html

into something like this

(B) (shelly, 1.12.1, haddock/shelly-1.12.1-e26fd9eb9e074999bcc96c/share/doc/html/index.html)

We take element 1 (the second) element of the list split by slash in A
then reverse it, and drop all the characters up to the next hyphen '-'
the drop the hyphen and reverse the text again. That gives us the name
and version combined.  We partition that into two strings, since versions
only have digits and the "-" and "." characters in their body.  We then
remove the leading dash in the version.

That turns (A) into (B)

> getNameAndVersion :: String -> (String,String,String)
> getNameAndVersion s = (justTheName, justTheVersion,s)
>   where
>     nameAndVersion :: String
>     nameAndVersion =
>       drop 1 .
>       dropWhile (/= '-') .
>       reverse .
>       flip (!!) 1 .
>       splitOn "/" $ s
>     (theVersion,theName) = partition isVersion nameAndVersion
>     justTheName = reverse theName
>     justTheVersion = drop 1 . reverse $ theVersion
>     isVersion :: Char -> Bool
>     isVersion = flip elem "0123456789.-"


> firstOfThree :: (a,a1,a2) -> a
> firstOfThree (a,_,_) = a

> firstAndThird :: (a,b,c) -> (a,c)
> firstAndThird (a,_,b) = (a,b)

A version of head that does not exclaim a warning

> headBecauseIKnowTheListIsNotEmpty :: [a] -> a
> headBecauseIKnowTheListIsNotEmpty (x:_) = x
> headBecauseIKnowTheListIsNotEmpty _ = error "headBecauseIKnowTheListIsNotEmpty has an empty list"

There may be many versions, and we only want to keep the latest one. 

> largestUniqueTriples :: [(String, String,String)] -> [(String,String,String)]
> largestUniqueTriples = map headBecauseIKnowTheListIsNotEmpty
>                    . map (sortBy secondIsGreater)
>                    . groupBy ((==) `on` firstOfThree)
>                    . sortOn firstOfThree
> 
> secondIsGreater :: Ord b => (a1, b,a3) -> (a2, b,a3) -> Ordering
> secondIsGreater (_,x,_) (_,y,_) = compare y x

The is at the top of the html file generated.

> headString :: String  
> headString = "<!DOCTYPE html>\
> \<html>\n\
> \<head>\n\
> \<META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">\n\
> \<link rel=\"icon\" href=\"favicon.ico\" type=\"image/x-icon\">\n\
> \<title>Haddock</title>\n\
> \</head>\n\
> \<body>\n\
> \  <h2>Haddock Index</h2>\n"

This is the whole program.  files contains the full path  of the "index.html" file as String.
link converts a text file name into an html <a href="...">name</a> element
links maps link across all of the file paths
result combine the headString above, with the links, and finally the "</body>" tag at the end
The result is written to the index.html file in this directory


> main :: IO ()
> main = shelly $ silently $ do
>   haveHaddock <-  test_d dir
>   if haveHaddock then do
>     fileList    <- findWhen hasIndex dir
>     if null fileList
>     then liftIO $ putStrLn  "No index files were found"
>     else liftIO $ runList fileList
>   else do
>       eFileList <- runSanityChecks
>       liftIO $ either putStrLn runList eFileList
>   where
>     runList files = do
>       putStrLn $ "Found " <> (show . length $ files) <> " files."
>       writeFile "index.html" . toHtml $ files 

> toHtml :: [String] -> String
> toHtml files = result
>   where
>     triples :: [(String,String,String)]
>     triples = map getNameAndVersion files
>     filteredTriples :: [(String,String,String)]
>     filteredTriples = largestUniqueTriples triples
>     filteredPairs :: [(String,String)]
>     filteredPairs  = map firstAndThird filteredTriples
>     links :: [String]
>     links = map addLink filteredPairs
>     result = mconcat [headString,unlines links,"</body>"]

Before we start, let's make sure this has a chance of working.  We need a home directory which we should find in
the environment.  If it isns't there, complain.  Next we look for potential haddock directories, which in my
system reside in ~/.cabal and ~/.ghcup.  If yours are somewhere else, change the value of whereHaddocksLive
below.  After that, we get the current ghc version, and see if such a subdirectory exists under the
potential haddock directories.

> whereHaddocksLive :: [String]
> whereHaddocksLive = [".cabal", ".ghcup", ".stack"]

> runSanityChecks :: Sh (Either String [FilePath])
> runSanityChecks = shelly $ do
>   mbHome <- get_env "HOME"
>   case mbHome of
>     Nothing -> return (Left "There is no Home Directory in your environment")
>     Just h  -> do
>       let dirs = addHaddockDirs h
>       exists <- mapM test_d dirs
>       let existing = map snd . filter fst $ zip exists dirs
>       inspect existing
>       let
>         problem1 = null existing
>         noDirs1  = unwords ["None of the directories", unwords whereHaddocksLive, "exist"]
>       if problem1 then return (Left noDirs1) else do
>         ghcVersion <- (<>) "ghc-" .T.unpack . last . T.words <$> run "ghc" ["--version"]
>         inspect ghcVersion
>         indexes <- mconcat <$> mapM (findWhen (wanted ghcVersion)) existing
>         let
>           problem2 = null indexes
>           noIndexes =  "There aren't any index.html files in any of the directories I searched:\n" <>
>                        show existing <> " that match ghc version " <> ghcVersion
>         if problem2 then return( Left noIndexes) else do
>           return (Right indexes)  
>   where
>     wanted ::  String -> String -> Sh Bool
>     wanted ghcVersion fName  = return ((ghcVersion `isInfixOf` fName) &&
>                                      ("index.html" `isSuffixOf` fName))
>     addHaddockDirs h = map ((</>) h)  whereHaddocksLive 
>

