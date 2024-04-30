{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
-- import yesod
import System.IO
-- import System.IO (stdout, withFile, IOMode(WriteMode))

import Data.Text (Text, strip)
import Text.HTML.Scalpel
    ( scrapeURL,
      chroot,
      chroots,
      text,
      texts,
      (@:),
      hasClass,
      Scraper )
import qualified Data.Text as T
import Data.List (isInfixOf)

-- Define a data type to represent table data
data Data = Data
  { column1   :: Text
  , column2   :: Text
  , column3   :: Text
  , column4   :: Text
  } deriving (Show, Eq)

-- Function to scrape paragraphs from a URL
scrapeParagraphs :: String -> IO (Maybe [Text])
scrapeParagraphs url = scrapeURL url paragraphs
  where
    -- Scraper for paragraphs
    paragraphs :: Scraper Text [Text]
    paragraphs = texts "p"

-- Function to scrape the heading from a URL
scraper :: String -> IO (Maybe Text)
scraper url = scrapeURL url heading
  where
    -- Scraper for heading
    heading :: Scraper Text Text
    heading = text "h1"

-- Function to scrape table data from a URL
scrapeCities :: String -> String -> IO (Maybe [Data])
scrapeCities url myclass = scrapeURL url citiesTable
  where
    -- Scraper for the entire table
    citiesTable :: Scraper Text [Data]
    citiesTable = chroot ("table" @: [hasClass myclass]) cities
    -- Scraper for individual rows in the table
    cities :: Scraper Text [Data]
    cities = chroots "tr" city
    -- Scraper for a single row (city)
    city :: Scraper Text Data
    city = do
      rowData <- texts "td"
      let column1Data = getColumn1 rowData
      let column2Data = getCountry rowData
      let column3Data = getPopulation rowData
      let column4Data = getColumn4 rowData
      return $ Data (strip column1Data) column2Data column3Data column4Data

    -- Helper functions to extract data from table cells
    getColumn1 :: [Text] -> Text
    getColumn1 (x:_) = strip x
    getColumn1 _     = "Not available"
    
    getCountry :: [Text] -> Text
    getCountry (_:x:_) = strip x
    getCountry _       = "Not available"

    getPopulation :: [Text] -> Text
    getPopulation (_:_:y:_) = strip y
    getPopulation _         = "Not available"

    getColumn4 :: [Text] -> Text
    getColumn4 (_:_:_:y:_) = strip y
    getColumn4 _           = "Not available"

-- Main function

main :: IO ()
main = do
    fileContents <- readFile "test1.txt"
    let linesList = lines fileContents
    args <- getArgs
    case args of
        -- If entity and keyword are provided
        [entity, keyword] -> do
            withFile "output.txt" WriteMode $ \outputFile -> do
                mapM_ (processUrl outputFile) linesList
          where
            processUrl outputFile url = do
                result2 <- scraper url
                case result2 of
                    Just x -> hPrint outputFile x
                    Nothing -> hPutStrLn outputFile "Didn't find the necessary items."

                -- Scraper for table data
                result <- scrapeCities url entity
                case result of
                    Just cities -> do
                        let filteredCities = filter (\d -> keyword `isInfixOf` T.unpack (column1 d) ||
                                                           keyword `isInfixOf` T.unpack (column2 d) ||
                                                           keyword `isInfixOf` T.unpack (column3 d) ||
                                                           keyword `isInfixOf` T.unpack (column4 d)) cities
                        mapM_ (hPrint outputFile) filteredCities
                    Nothing -> hPutStrLn outputFile "Failed to scrape the website."

                -- Scraper for paragraphs
                pResults <- scrapeParagraphs url
                case pResults of
                    Just paragraphs -> do
                        hPutStrLn outputFile "Paragraphs found:"
                        let filteredParagraphs = filter (\p -> T.isInfixOf (T.pack keyword) p) paragraphs
                        mapM_ (hPutStrLn outputFile . T.unpack) filteredParagraphs
                    Nothing -> hPutStrLn outputFile "Failed to scrape paragraphs."

        -- Invalid usage
        _ -> putStrLn "Usage: stack run [entity] [keyword]"
