A relatively simple script to generate an index.html file for all of
the haddock index.html files inside your .cabal directory.  The first
thing you should do to use this, is to find out where your local
haddock files live.  They should be somewhere under your ~/.cabal
directory.  Mine are at "/home/henry/.cabal/store/ghc-9.8.2" since I'm
currently using ghc 9.8.2.  Once you've found your ghc-x.y.z
directory, symlink to this directory and name the symlink haddock.
Then run this script with cabal run, creating an index.html file in
this directory with the appropriate links.

> import Shelly
> import qualified Data.Text as T
> import Data.Text ( Text )
> default (Text)

This is the name of the directory that should be symlinked to
ghc-x.y.z directory explained above

> dir :: Text
> dir = "haddock"

A filter looking for files whose names are "index.html"

> hasIndex :: FilePath -> Sh Bool
> hasIndex = return . T.isSuffixOf "/index.html" . toTextIgnore

Creates an a link whose href points to the actual file and whose
innerHtml is the name of the package, including the version number

> toLink :: Text -> Text -> Text
> toLink x y = mconcat
>     ["<br/><a href=\"", x, "\">",y,"</a>"]

A kinda complicated manipulation that turns something like this:
  
(A) haddock/shelly-1.12.1-e26fd9eb9e074999bcc96c/share/doc/html/index.html

into something like this

(B) shelly-1.12.1

We take element 1 (the second) element of the list split by slash in A
then reverse it, and drop all the characters up to the next hyphen '-'
the drop the hyphen and reverse the text again.

That turns (A) into (B)

> getName :: Text -> Text
> getName =
>   T.reverse .
>   T.drop 1 .
>   T.dropWhile (/= '-') .
>   T.reverse .
>   flip (!!) 1 .
>   T.splitOn "/"

The is at the top of the html file generated.

> headText :: Text  
> headText = "<!DOCTYPE html>\
> \<html>\n\
> \<head>\n\
> \<META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">\n\
> \<link rel=\"icon\" href=\"favicon.ico\" type=\"image/x-icon\">\n\
> \<title>Haddock</title>\n\
> \</head>\n\
> \<body>\n\
> \  <h2>Haddock Index</h2>\n"

This is the whole program.  files contains the full path  of the "index.html" file as Text.
link converts a text file name into an html <a href="...">name</a> element
links maps link across all of the file paths
result combine the headText above, with the links, and finally the "</body>" tag at the end
The result is written to the index.html file in this directory

> main :: IO ()
> main = shelly $ silently $ do
>   files    <- fmap toTextIgnore <$> findWhen hasIndex (fromText dir)
>   -- mapM_ (echo . toTextIgnore) files\
>   let
>     link :: Text -> Text  
>     link x = toLink x (getName x)
>     links :: [Text]
>     links = map link files
>     --links = map (link . toTextIgnore) files
>     result :: Text
>     result = mconcat [headText, T.unlines links, "</body>"]
>   writefile "index.html" result
>   -- mapM_ echo links
>   return ()


