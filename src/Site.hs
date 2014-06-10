--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mconcat, (<>))
import           Data.Char (toUpper, toLower)
import           Control.Monad (liftM)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith siteConfig $ do
    match imagesPattern $ do
        route   idRoute
        compile copyFileCompiler

    match cssPattern $ do
        route   idRoute
        compile compressCssCompiler

    match jsPattern $ do
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
            let ctx = postListCtx posts tags <> homepageCtx count <> commonCtx
            compilerGlue (getResourceBody >>= applyAsTemplate ctx) [defaultTemplate] ctx

    match templatesPattern $ compile templateCompiler

    match aboutPagePattern $ do
        route $ setExtension htmlExtension
        compile $ compilerGlue pandocCompiler [aboutTemplate, defaultTemplate] commonCtx

    create [papersPage] $ do
        route idRoute
        compile $ do
            (papers, count) <- partitionPosts (withLength id) . recentFirst =<< loadAllSnapshots allPapersPattern paperSnapshot
            let ctx = paperListCtx papers count <> commonCtx
            compilerGlue emptyCompiler [papersTemplate, defaultTemplate] ctx

    create [archivePage] $ do
        route idRoute
        compile $ do
            (posts, count) <- partitionPosts (withLength id) . recentFirst =<< loadAll allPostsPattern
            let ctx = postListCtx posts tags <> archiveCtx count <> commonCtx
            compilerGlue emptyCompiler [archiveTemplate, defaultTemplate] ctx

    create [rssFeedPage] $ do
        route idRoute
        compile $
            loadAllSnapshots allPostsPattern contentSnapshot
                >>= fmap (take numPostsInRssFeed) . recentFirst
                >>= renderRss feedConfig feedCtx        

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [modificationTimeField "mtime" "%U",
     dateField "date" "%B %e, %Y",
     tagsField "tags" tags,
     commonCtx,
     defaultContext
    ]

postListCtx :: [Item String] -> Tags -> Context String
postListCtx posts tags = listField "posts" (postCtx tags) (return posts)

paperCtx :: Context String
paperCtx = mconcat [modificationTimeField "mtime" "%U", 
                    dateField "date" "%B %e, %Y", 
                    commonCtx]

paperListCtx :: [Item String] -> Int -> Context String
paperListCtx papers paperCount = mconcat [listField "papers" paperCtx (return papers), postCountCtx  paperCount]

commonCtx :: Context String
commonCtx = mconcat [blogTitleCtx, emailAddyCtx, siteOwnerCtx, sitDesciptionCtx, siteSEOCtx, defaultContext]

tagNameCtx :: String -> Int -> Context String
tagNameCtx tn tagCount = mconcat [tagTitleCtx tn, postCountCtx tagCount]

tagTitleCtx :: String -> Context String
tagTitleCtx tn = constField "postTitle" (titleCase tn ++ " Posts")

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
jsPattern = "scripts/*"

templatesPattern :: Pattern
templatesPattern = "templates/*"

tagsCapturePattern :: Pattern
tagsCapturePattern = "tags/*.html"

indexPagePattern :: Pattern
indexPagePattern = "index.html"

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

papersPage :: Identifier
papersPage = "papers.html"
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
                feedRoot =  "http://sanjivsahayam.com"
             }
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Template definitions and helpers
--------------------------------------------------------------------------------
defaultTemplate :: String
defaultTemplate = "default.html"

archiveTemplate :: String
archiveTemplate = "archive.html"

aboutTemplate :: String
aboutTemplate = "about.html"

postTemplate :: String
postTemplate = "post.html"

postsTemplate :: String
postsTemplate = "posts.html"

papersTemplate :: String
papersTemplate = "papers.html"

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
--------------------------------------------------------------------------------


