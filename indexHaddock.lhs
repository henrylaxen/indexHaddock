
A relatively simple script to generate an index.html file for all of
the haddock index.html files inside your .cabal directory.  The first
thing you should do to use this, is to find out where your local
haddock files live.  They should be somewhere under your ~/.cabal
directory.  Mine are at "/home/henry/.cabal/store/ghc-9.8.2" since I'm
currently using ghc 9.8.2.  Once you've found your ghc-x.y.z
directory, symlink to this directory and name the symlink haddock.
Then run this script with cabal run, creating an index.html file in
this directory with the appropriate links.

> import Data.Function ( on )
> import Data.List ( sortBy, groupBy, isSuffixOf, partition, sortOn )
> import Data.List.Split ( splitOn )
> import Prelude (
>   map, ($), Eq((==), (/=)), Monad(return), Ord(compare),
>   Foldable(null, elem), Monoid(mconcat), Bool, String, Char,
>   Ordering, IO, FilePath, unlines, dropWhile, (.), flip, error,
>   (!!), drop, reverse, putStrLn, writeFile )
> import Shelly ( shelly, silently, findWhen, liftIO, Sh )


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
>   fileList    <- findWhen hasIndex dir
>   if null fileList
>   then liftIO $ putStrLn  "No index files were found"
>   else liftIO $ writeFile "index.html" $ go (fileList)


> go :: [String] -> String
> go files = result
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

> {-
> a1,a2,a3,b1,b2,b3 :: String
> a1 = "haddock/parallel-3.2.2.0-a6b3f3f1a27bc0a89445a101e19b2a53e32dd5869767317ce817c68a5fb2626b/share/doc/html/index.html"
> a2 = "haddock/parallel-3.2.2.2-a6b3f3f1a27bc0a89445a101e19b2a53e32dd5869767317ce817c68a5fb2626b/share/doc/html/index.html"
> a3 = "haddock/parallel-3.2.2.1-a6b3f3f1a27bc0a89445a101e19b2a53e32dd5869767317ce817c68a5fb2626b/share/doc/html/index.html"
> b1 = "haddock/shelly-3.2.2.0-a6b3f3f1a27bc0a89445a101e19b2a53e32dd5869767317ce817c68a5fb2626b/share/doc/html/index.html"
> b2 = "haddock/shelly-3.2.2.2-a6b3f3f1a27bc0a89445a101e19b2a53e32dd5869767317ce817c68a5fb2626b/share/doc/html/index.html"
> b3 = "haddock/shelly-3.2.2.1-a6b3f3f1a27bc0a89445a101e19b2a53e32dd5869767317ce817c68a5fb2626b/share/doc/html/index.html"
> c :: [String]
> c = [a1,b1,b2,b3,a2,a3]
> d :: [(String, String, String)]
> d = map getNameAndVersion c
> e :: [(String, String, String)]
> e = largestUniqueTriples d
> f = putStrLn $ go c
> -}
