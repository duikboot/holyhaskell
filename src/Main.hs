{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.ANSI
import System.IO (hFlush, stdout)
import System.Directory
import System.FilePath.Posix (takeDirectory, (</>))
import Data.List (intercalate)
import Data.List.Split (splitOneOf)
import Data.Char (toLower, toUpper, isNumber, isLetter)
import Data.Data
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LZ
import Paths_holyhaskell

data Project = Project {
    projectName   :: String
    , moduleName    :: String
    , author        :: String
    , mail          :: String
    , ghaccount     :: String
    , synopsis      :: String
    , year          :: String
    } deriving (Data, Typeable)

main :: IO ()
main = do
    pkgFilePath <- getDataFileName "scaffold/LICENSE"
    templateContent <- readFile pkgFilePath
    intro
    project <- ask "project name"
    ioassert (checkProjectName project)
        "Use only letters, numbers, spaces and dashes please"
    let projectname = projectNameFromString project
        modulename = capitalize project
    in_author <- ask "name"
    in_email <- ask "email"
    in_ghaccount <- ask "github account"
    in_synopsis <- ask "project in less than a dozen words"
    current_year <- getCurrentYear
    createProject $ Project projectname modulename in_author in_email
                        in_ghaccount in_synopsis current_year
    end


createProject :: Project -> IO ()
createProject p = do
    let context = mkGenericContext p
    createDirectory (projectName p)
    setCurrentDirectory (projectName p)
    genFile context "gitignore"      $ ".gitignore"
    genFile context "project.cabal"  $ (projectName p) ++ ".cabal"
    genFile context "src/Main.hs"   $ "src" </> "Main.hs"


genFile :: MuContext IO -> FilePath -> FilePath -> IO ()
genFile context filename outputFileName = do
    pkgfileName <- getDataFileName ("scaffold/"++filename)
    template <- BS.readFile pkgfileName
    transformedFile <- hastacheStr defaultConfig template context
    createDirectoryIfMissing True (takeDirectory outputFileName)
    LZ.writeFile outputFileName transformedFile


ioassert :: Bool -> String -> IO ()
ioassert True _ = return ()
ioassert False str = error str


checkProjectName :: String -> Bool
checkProjectName [] = False
checkProjectName str =
        all (\c -> isLetter c || isNumber c || c=='-' || c== ' ') str


colorPutStr :: Color -> String -> IO ()
colorPutStr color str = do
    setSGR  [ SetColor Foreground Dull color
            , SetConsoleIntensity NormalIntensity
            ]
    putStr str
    setSGR []


projectNameFromString :: String -> String
projectNameFromString str = intercalate "-" (splitOneOf " -" (map toLower str))


bk :: String -> IO ()
bk str = colorPutStr Green ("Bridgekeeper: " ++ str ++ "\n")
bkn :: String -> IO ()
bkn str = colorPutStr Green ("Bridgekeeper: " ++ str)
you :: String -> IO ()
you str = colorPutStr Yellow ("Bridgekeeper: " ++ str ++ "\n")


capitalize :: String -> String
capitalize str = concatMap capitalizeWord (splitOneOf " -" str)
    where
        capitalizeWord :: String -> String
        capitalizeWord (x:xs) = toUpper x:map toLower xs
        capitalizeWord _      = []


ask :: String -> IO String
ask info = do
    bk $ "What is your " ++ info ++ "?"
    putStr "> "
    hFlush stdout
    getLine


intro :: IO ()
intro = do
    bk "Stop!"
    bk "Who would cross the Bridge of Death"
    bk "must answer me these questions three,"
    bk "ere the other side he see."
    you "Ask me the questions, bridgekeeper, I am not afraid.\n"


end :: IO ()
end = do
    putStrLn "\n\n"
    bk "What... is the air-speed velocity of an unladen swallow?"
    you "What do you mean? An African or European swallow?"
    bk "Huh? I... I don't know that."
    putStrLn "[the bridgekeeper is thrown over]"
    bk "Auuuuuuuuuuuugh"
    putStrLn "Sir Bedevere: How do you know so much about swallows?"
    you "Well, you have to know these things when you're a king, you know."
