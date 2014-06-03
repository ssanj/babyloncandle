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
        compile $ pandocCompiler          
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/post2.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/default2.html" (postCtx tags)
            >>= relativizeUrls    

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
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts2.html" ctx
                >>= loadAndApplyTemplate "templates/default2.html" ctx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx =
                    listField "posts" (postCtx tags) (return posts) <>
                    archiveCtx <>
                    commonCtx <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive2.html" ctx
                >>= loadAndApplyTemplate "templates/default2.html" ctx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- topPosts 5 . recentFirst =<< loadAll "posts/*"
            let ctx =
                    listField "posts" (postCtx tags) (return posts) <>
                    homepageCtx <>
                    commonCtx <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default2.html" ctx
                >>= relativizeUrls

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
            pandocCompiler                                
                >>= loadAndApplyTemplate "templates/about.html" ctx
                >>= loadAndApplyTemplate "templates/default2.html" ctx
                >>= relativizeUrls


--------------------------------------------------------------------------------
topPosts :: (Functor m) => Int -> m [Item a] -> m [Item a]
topPosts num = fmap (take num)

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

titleCase :: String -> String
titleCase [] = []
titleCase (x:xs) = toUpper x : (map toLower xs)

feedCtx :: Context String
feedCtx = mconcat [ bodyField "description", defaultContext]

feedConfig :: FeedConfiguration
feedConfig =  FeedConfiguration { 
                feedTitle = "BabylonCandle",
                feedDescription = "The blog of Sanjiv Sahayam",
                feedAuthorName = "sanjiv sahayam", 
                feedAuthorEmail = "sanjsmailbox@gmail.com",
                feedRoot =  "http://sanjivsahayam.com"
             }








