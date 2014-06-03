--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat, (<>))
import           Data.Char (toUpper, toLower)
import           Text.Printf
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith (defaultConfiguration { previewPort = 9999 }) $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "scripts/*" $ do
        route idRoute
        compile copyFileCompiler           

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        let precompiler = (pandocCompiler >>= saveSnapshot "content" >>= return . fmap demoteHeaders)
        compile $ compilerGlue precompiler [postTemplate, defaultTemplate] (postCtx tags)

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        commonCtx <>
                        (tagNameCtx tag) <>
                        defaultContext
            compilerGlue (makeItem "") [postsTemplate, defaultTemplate] ctx

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx =
                    listField "posts" (postCtx tags) (return posts) <>
                    archiveCtx <>
                    commonCtx <>
                    defaultContext

            compilerGlue (makeItem "") [archiveTemplate, defaultTemplate] ctx


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- topPosts 5 . recentFirst =<< loadAll "posts/*"
            let ctx =
                    listField "posts" (postCtx tags) (return posts) <>
                    homepageCtx <>
                    commonCtx <>
                    defaultContext

            compilerGlue (getResourceBody >>= applyAsTemplate ctx) [defaultTemplate] ctx

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss feedConfig feedCtx

    match "templates/*" $ compile templateCompiler

    match "about.markdown" $ do
        route $ setExtension "html"
        compile $ do
            let ctx = commonCtx <> defaultContext
            compilerGlue pandocCompiler [aboutTemplate, defaultTemplate] ctx

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

commonCtx :: Context String
commonCtx = mconcat [blogTitleCtx, emailAddyCtx, siteOwnerCtx, sitDesciptionCtx]

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
defaultTemplate = "default2.html"
archiveTemplate = "archive2.html"
aboutTemplate = "about.html"
postTemplate = "post2.html"
postsTemplate = "posts2.html"

templatesFolder :: String -> Identifier
templatesFolder file = fromFilePath ("templates/" ++ file)                                                              
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


