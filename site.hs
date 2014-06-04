--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat, (<>))
import           Data.Char (toUpper, toLower)
import           Text.Printf
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith (defaultConfiguration { previewPort = 9999 }) $ do
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
        let precompiler = (pandocCompiler >>= saveSnapshot contentSnapshot >>= return . fmap demoteHeaders)
        compile $ compilerGlue precompiler [postTemplate, defaultTemplate] (postCtx tags)

    tagsRules tags $ \tag pattern -> do

        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = postListCtx posts tags <> (tagNameCtx tag) <> commonCtx
            compilerGlue (makeItem "") [postsTemplate, defaultTemplate] ctx

    match indexPagePattern $ do
        route idRoute
        compile $ do
            posts <- topPosts numPostsOnHomePage . recentFirst =<< loadAll allPostsPattern
            let ctx = postListCtx posts tags <> homepageCtx <> commonCtx
            compilerGlue (getResourceBody >>= applyAsTemplate ctx) [defaultTemplate] ctx

    match templatesPattern $ compile templateCompiler

    match aboutPagePattern $ do
        route $ setExtension htmlExtension
        compile $ compilerGlue pandocCompiler [aboutTemplate, defaultTemplate] commonCtx

    create [archivePagePattern] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll allPostsPattern
            let ctx =postListCtx posts tags <> archiveCtx <> commonCtx
            compilerGlue (makeItem "") [archiveTemplate, defaultTemplate] ctx

    create [rssFeedPattern] $ do
        route idRoute
        compile $ do
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

commonCtx :: Context String
commonCtx = mconcat [blogTitleCtx, emailAddyCtx, siteOwnerCtx, sitDesciptionCtx, defaultContext]

tagNameCtx :: String -> Context String
tagNameCtx tn = constField "postTitle" (titleCase tn ++ " Posts")

archiveCtx :: Context String
archiveCtx = constField "postTitle" "Archive" 

blogTitleCtx :: Context String
blogTitleCtx =  constField "blogTitle" "BabylonCandle"

homepageCtx :: Context String
homepageCtx = constField "title" "Home"

emailAddyCtx :: Context String
emailAddyCtx =  constField "email" "sanjsmailbox@gmail.com"  

siteOwnerCtx :: Context String
siteOwnerCtx = constField "siteOwner" "sanjiv sahayam"

sitDesciptionCtx :: Context String
sitDesciptionCtx = constField "sitDesciption" "Things I would otherwise forget."

feedCtx :: Context String
feedCtx = mconcat [ bodyField "description", defaultContext]
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------
allPostsPattern    = "posts/*"
contentSnapshot    = "content"
imagesPattern      = "images/*"
cssPattern         = "css/*"
jsPattern          = "scripts/*"
templatesPattern   = "templates/*"
tagsCapturePattern = "tags/*.html"
indexPagePattern   = "index.html"
aboutPagePattern   = "about.markdown"
archivePagePattern = "archive.html"
rssFeedPattern     = "feed.xml"
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Extensions
--------------------------------------------------------------------------------
htmlExtension = "html"
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
--- RSS Config
--------------------------------------------------------------------------------
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
defaultTemplate = "default.html"
archiveTemplate = "archive.html"
aboutTemplate = "about.html"
postTemplate = "post.html"
postsTemplate = "posts.html"

templatesFolder :: String -> Identifier
templatesFolder file = fromFilePath ("templates/" ++ file)                                                              
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------
numPostsOnHomePage = 5
numPostsInRssFeed = 10
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Compilers and helpers
--------------------------------------------------------------------------------
compilerGlue :: Compiler (Item String) -> [String] -> Context String -> Compiler (Item String)
compilerGlue cmplr tmpls ctx = 
                let paths = map (templatesFolder) tmpls in
                foldl (\c t -> c >>= loadAndApplyTemplate t ctx) cmplr paths >>=
                    relativizeUrls
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
topPosts :: (Functor m) => Int -> m [Item a] -> m [Item a]
topPosts num = fmap (take num)

titleCase :: String -> String
titleCase [] = []
titleCase (x:xs) = toUpper x : (map toLower xs)
--------------------------------------------------------------------------------


