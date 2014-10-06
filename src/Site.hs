--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import           Data.Monoid (mconcat, (<>))
import           Data.Char (toUpper, toLower)
import           Control.Monad (liftM)
import           Data.Aeson
import           GHC.Generics
import           qualified Data.Text as T
import           qualified Data.ByteString.Lazy.Char8 as BS
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith siteConfig $ do
    match imagesPattern $ do
        route  idRoute
        compile copyFileCompiler

    match cssPattern $ do
        route   idRoute
        compile compressCssCompiler

    match jsPattern $ do
        route idRoute
        compile copyFileCompiler

    match googleAuthPattern $ do
        route idRoute
        compile copyFileCompiler

    match bingAuthPattern $ do
        route idRoute
        compile copyFileCompiler

    tags <- buildTags allPostsPattern (fromCapture tagsCapturePattern)

    match allPostsPattern $ do
        route $ setExtension htmlExtension
        let precompiler = liftM (fmap demoteHeaders) (pandocCompiler >>= saveSnapshot contentSnapshot)
        compile $ compilerGlue precompiler [postTemplate, defaultTemplate] (postCtx tags)

    match allPapersPattern $ do
        compile $ pandocCompiler >>= saveSnapshot paperSnapshot

    tagsRules tags $ \tag pattern -> do

        route idRoute
        compile $ do
            (posts, count) <- partitionPosts (withLength $ id) . recentFirst =<< loadAll pattern
            let ctx = postListCtx posts tags <> tagNameCtx tag count <> commonCtx
            compilerGlue emptyCompiler [postsTemplate, defaultTemplate] ctx

    match indexPagePattern $ do
        route idRoute
        compile $ do
            (posts, count) <- partitionPosts (withLength $ take numPostsOnHomePage) . recentFirst =<< loadAll allPostsPattern
            let ctx = postListCtx posts tags <> homepageCtx count <> commonCtxWithDescription indexPageDescription
            compilerGlue (getResourceBody >>= applyAsTemplate ctx) [defaultTemplate] ctx

    match templatesPattern $ compile templateCompiler

    match aboutPagePattern $ do
        route $ setExtension htmlExtension
        compile $ compilerGlue pandocCompiler [aboutTemplate, defaultTemplate] commonCtx

    create [papersPage] $ do
        route idRoute
        compile $ do
            (papers, count) <- partitionPosts (withLength id) . recentFirst =<< loadAllSnapshots allPapersPattern paperSnapshot
            let ctx = paperListCtx papers count <> commonCtxWithDescription papersPageDescription
            compilerGlue emptyCompiler [papersTemplate, defaultTemplate] ctx

    create [searchDataPage] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll allPostsPattern
            getSearchablePosts posts

    create [archivePage] $ do
        route idRoute
        compile $ do
            (posts, count) <- partitionPosts (withLength id) . recentFirst =<< loadAll allPostsPattern
            let ctx = postListCtx posts tags <> archiveCtx count <> commonCtxWithDescription archivePageDescription
            compilerGlue emptyCompiler [archiveTemplate, defaultTemplate] ctx

    create [rssFeedPage] $ do
        route idRoute
        compile $
            loadAllSnapshots allPostsPattern contentSnapshot
                >>= fmap (take numPostsInRssFeed) . recentFirst
                >>= renderRss feedConfig feedCtx

    create [sitemapPage] $ do
        route idRoute
        compile $ do
             posts <- recentFirst =<< loadAllSnapshots allPostsPattern contentSnapshot
             let ctx = postListCtx posts tags <> commonCtx
             compilerGlue emptyCompiler [sitemapTemplate] ctx

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [modificationTimeField "updated" "%Y-%m-%d",
     dateField "date" "%B %e, %Y",
     tagsField "tags" tags,
     constField "root" (feedRoot feedConfig),
     commonCtx,
     defaultContext
    ]

data SearchablePost = SearchablePost { title :: !T.Text, tags :: !T.Text, url :: !T.Text } deriving (Show, Generic)

instance ToJSON SearchablePost

-- get the value of a metadata key
getField :: String -> Item String -> Compiler String
getField key item = do
        f <- getMetadataField (itemIdentifier item) key
        case f of
            Just v  -> return v
            Nothing -> return ("could not find value for " ++ key)

createSearchablePost :: Item String -> Compiler SearchablePost
createSearchablePost item =  do
        _title <- getField "title" item
        _tags <- getField "tags" item
        fp <-  getRoute (itemIdentifier item)
        let _url = maybe "/" toUrl fp
        return $ SearchablePost (T.pack _title) (T.pack _tags) (T.pack _url)

getSearchablePosts :: [Item String] -> Compiler (Item String)
getSearchablePosts items = (fmap (BS.unpack . encode) $ sequence $ map createSearchablePost items)  >>= makeItem

dumpItem :: Item String -> Compiler String
dumpItem item = do
        _title <- getField "title" item
        _tags <- getField "tags" item
        _url <- getField "tags" item
        return (BS.unpack $ encode $ SearchablePost (T.pack _title) (T.pack _tags) (T.pack _url) )

createField :: String -> Context String
createField key = field key $ dumpItem

createFieldFromOther :: String -> String -> Context String
createFieldFromOther keyn keyo = field keyn $ getField keyo

postListCtx :: [Item String] -> Tags -> Context String
postListCtx posts tags = listField "posts" (postCtx tags) (return posts)

paperCtx :: Context String
paperCtx = mconcat [modificationTimeField "mtime" "%U",
                    dateField "date" "%B %e, %Y",
                    commonCtx]

paperListCtx :: [Item String] -> Int -> Context String
paperListCtx papers paperCount = mconcat [listField "papers" paperCtx (return papers), postCountCtx  paperCount]

commonCtxWithDescription :: String -> Context String
commonCtxWithDescription description = constField "description" description <> commonCtx

commonCtx :: Context String
commonCtx = mconcat [blogTitleCtx, emailAddyCtx, siteOwnerCtx, sitDesciptionCtx, siteSEOCtx, defaultContext]

tagNameCtx :: String -> Int -> Context String
tagNameCtx tn tagCount = mconcat [tagTitleCtx tn, postCountCtx tagCount]

tagTitleCtx :: String -> Context String
tagTitleCtx tn = mconcat [constField "postTitle" (titleCase tn ++ " Posts"),
                          constField "description" ("Posts tagged as " ++ tn)]

archiveCtx :: Int -> Context String
archiveCtx postCount = mconcat [archiveTitleCtx, postCountCtx postCount]

blogTitleCtx :: Context String
blogTitleCtx =  constField "blogTitle" "BabylonCandle"

homepageCtx :: Int -> Context String
homepageCtx postCount = mconcat [homepageTitleCtx, postCountCtx postCount]

homepageTitleCtx :: Context String
homepageTitleCtx = mconcat [constField "title" "Home", constField "postTitle" "Posts"]

archiveTitleCtx :: Context String
archiveTitleCtx = constField "postTitle" "Archive"

postCountCtx :: Int -> Context String
postCountCtx = (constField "postCount") . show

emailAddyCtx :: Context String
emailAddyCtx =  constField "email" "sanjsmailbox@gmail.com"

siteOwnerCtx :: Context String
siteOwnerCtx = constField "siteOwner" "sanjiv sahayam"

sitDesciptionCtx :: Context String
sitDesciptionCtx = constField "sitDesciption" "Things I would otherwise forget."

siteSEOCtx :: Context String
siteSEOCtx = constField "siteSEO" "The personal blog of sanjiv sahayam."

feedCtx :: Context String
feedCtx = mconcat [ bodyField "description", defaultContext]
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------
allPostsPattern :: Pattern
allPostsPattern = "posts/*"

allPapersPattern :: Pattern
allPapersPattern = "papers/*"

imagesPattern :: Pattern
imagesPattern = "images/*"

cssPattern :: Pattern
cssPattern = "css/*"

jsPattern :: Pattern
jsPattern = "js/*"

templatesPattern :: Pattern
templatesPattern = "templates/*"

tagsCapturePattern :: Pattern
tagsCapturePattern = "tags/*.html"

indexPagePattern :: Pattern
indexPagePattern = "index.html"

googleAuthPattern :: Pattern
googleAuthPattern = "googleac0741b9661397ea.html"

bingAuthPattern :: Pattern
bingAuthPattern = "BingSiteAuth.xml"

aboutPagePattern :: Pattern
aboutPagePattern = "about.markdown"
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Snapshots
--------------------------------------------------------------------------------
contentSnapshot :: Snapshot
contentSnapshot = "content"

paperSnapshot :: Snapshot
paperSnapshot = "paperContent"
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Pages
--------------------------------------------------------------------------------
archivePage :: Identifier
archivePage = "archive.html"

rssFeedPage :: Identifier
rssFeedPage = "feed.xml"

sitemapPage :: Identifier
sitemapPage = "sitemap.xml"

papersPage :: Identifier
papersPage = "papers.html"

searchDataPage :: Identifier
searchDataPage = "data/pages.json"
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Extensions
--------------------------------------------------------------------------------
htmlExtension :: String
htmlExtension = "html"
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Site configuration
--------------------------------------------------------------------------------
siteConfig :: Configuration
siteConfig = defaultConfiguration {
                previewHost = "192.168.1.8",
                previewPort = 9999,
                destinationDirectory = "dist/_site",
                storeDirectory       = "dist/_cache",
                tmpDirectory         = "dist/_cache/tmp",
                deployCommand = "rsync -av --checksum --delete --progress " ++
                                 "--exclude-from 'excludes.txt' " ++
                                 "dist/_site/* $BLOG_DIR"
             }
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
--- RSS Config
--------------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig =  FeedConfiguration {
                feedTitle = "BabylonCandle",
                feedDescription = "The blog of Sanjiv Sahayam",
                feedAuthorName = "sanjiv sahayam",
                feedAuthorEmail = "sanjsmailbox@gmail.com",
                feedRoot =  "http://blog.ssanj.net"
             }
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Template definitions and helpers
--------------------------------------------------------------------------------
defaultTemplate :: String
defaultTemplate = "default.html"

archiveTemplate :: String
archiveTemplate = "archive.html"

searchDataTemplate :: String
searchDataTemplate = "pages_template.json"

aboutTemplate :: String
aboutTemplate = "about.html"

postTemplate :: String
postTemplate = "post.html"

postsTemplate :: String
postsTemplate = "posts.html"

papersTemplate :: String
papersTemplate = "papers.html"

sitemapItemTemplate :: String
sitemapItemTemplate = "sitemap-item.xml"

sitemapTemplate :: String
sitemapTemplate = "sitemap.xml"

templatesFolder :: String -> Identifier
templatesFolder file = fromFilePath ("templates/" ++ file)
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------
numPostsOnHomePage :: Int
numPostsOnHomePage = 5

numPostsInRssFeed :: Int
numPostsInRssFeed = 10
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Compilers and helpers
--------------------------------------------------------------------------------
compilerGlue :: Compiler (Item String) -> [String] -> Context String -> Compiler (Item String)
compilerGlue cmplr tmpls ctx =
                let paths = map templatesFolder tmpls in
                foldl (\c t -> c >>= loadAndApplyTemplate t ctx) cmplr paths >>=
                    relativizeUrls

emptyCompiler :: Compiler (Item String)
emptyCompiler = makeItem ""
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
partitionPosts :: (Functor m) =>  ([Item a] -> b) -> m [Item a] -> m b
partitionPosts = fmap

withLength :: ([a] -> [a]) -> [a] -> ([a], Int)
withLength f xs = let r = f xs in (r, length r)

titleCase :: String -> String
titleCase [] = []
titleCase (x:xs) = toUpper x : map toLower xs

-- | Copies @$updated$@ from @$published$@ if it is not already set.
makeItemContext :: String -> Context a -> Context a
makeItemContext fmt context = mconcat
    [dateField "published" fmt, context, dateField "updated" fmt]

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- SEO
--------------------------------------------------------------------------------
archivePageDescription :: String
archivePageDescription = "A collection of all the posts across this site in order from newest to oldest."

indexPageDescription :: String
indexPageDescription = "Welcome to Sanj's blog. Have a read of my latest posts."

papersPageDescription :: String
papersPageDescription = "A collection technical papers I have read or want to read."