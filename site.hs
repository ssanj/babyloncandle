--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat, (<>))
import           Data.Char (toUpper, toLower)
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
                        (tagName tag) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts2.html" ctx
                >>= loadAndApplyTemplate "templates/default2.html" ctx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    archive <>
                    commonCtx <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive2.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default2.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- topPosts 5 . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    constField "title" "Home" <>
                    commonCtx <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default2.html" indexCtx
                >>= relativizeUrls

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss feedConfig feedCtx

    match "templates/*" $ compile templateCompiler


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
commonCtx = mconcat [blogTitle, emailAddy, siteOwnerCtx]

tagName :: String -> Context String
tagName tn = constField "postTitle" (titleCase tn ++ " Posts")

archive :: Context String
archive = constField "postTitle" "Archive" 

blogTitle :: Context String
blogTitle =  constField "blogTitle" "BabylonCandle"

emailAddy :: Context String
emailAddy =  constField "email" "sanjsmailbox@gmail.com"  

siteOwnerCtx :: Context String
siteOwnerCtx = constField "siteOwner" "sanjiv sahayam"

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