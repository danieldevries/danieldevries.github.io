{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.List (isSuffixOf)
import           System.FilePath.Posix (takeBaseName,takeDirectory,(</>))

main :: IO ()
main = hakyllWith configuration $ do
    match "posts/*" $ do
        route   $ cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postContext
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/layout.html" postContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    match "pages/*" $ do
        route   $ gsubRoute "^pages/" (const "") `composeRoutes` cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/layout.html" postContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postContext (return posts) `mappend`
                    constField "title" "Home"                    `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/layout.html" indexCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route $ idRoute
        compile $ feedCompiler renderAtom

    create ["rss.xml"] $ do
        route $ idRoute
        compile $ feedCompiler renderRss

    match "assets/stylesheets/main.scss" $ do
        route   $ constRoute "assets/stylesheets/main.css"
        compile $ scssCompiler

    match "static/*" $ do
        route   $ gsubRoute "^static/" (const "")
        compile $ copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
    where
        createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
        pattern = "/index.html"
        replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
    where idx = "index"

--------------------------------------------------------------------------------

pageContext :: Context String
pageContext = mconcat
    [ standardContext
    ]

postContext :: Context String
postContext = mconcat
    [ dateField "date" "%e %B %Y"
    , dateField "datetime" "%Y-%m-%d"
    , standardContext
    ]

feedContext :: Context String
feedContext = mconcat
    [ bodyField "description"
    , postContext
    ]

standardContext :: Context String
standardContext = mconcat
    [ constField "type" "default"
    , modificationTimeField "lastmod" "%Y-%m-%d"
    , defaultContext
    ]

--------------------------------------------------------------------------------

scssCompiler :: Compiler (Item String)
scssCompiler = do
    fmap (fmap compressCss) $
        getResourceString
        >>= withItemBody (unixFilter "sass" [ "-s"
                                            , "--scss"
                                            , "--style", "compressed"
                                            , "--load-path", "source/assets/stylesheets/main.scss"
                                            ])

type FeedRenderer =
    FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
    renderer feedConfiguration feedContext
        =<< fmap (take 10) . recentFirst
        =<< loadAllSnapshots "posts/*.md" "content"

--------------------------------------------------------------------------------

configuration :: Configuration
configuration = defaultConfiguration
    { providerDirectory = "source/"
    , destinationDirectory = "generated/out"
    , storeDirectory = "generated/cache"
    , tmpDirectory = "generated/cache/tmp"
    , deployCommand = "echo 'not yet'"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Daniël de Vries"
    , feedDescription = "Posts about programming etc."
    , feedAuthorName = "Daniël de Vries"
    , feedAuthorEmail = "contact@danieldevries.eu"
    , feedRoot = "https://danieldevries.eu"
    }
