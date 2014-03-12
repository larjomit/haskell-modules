----------------------------------------------------------------------------
-- |
-- Module      :  DirSystem
-- Copyright   :  (c) Timo Larjo 2013
-- License     :  LGPLv3
--
-- Maintainer  :  timo.larjo@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Data types representing a directory and function for reading 
-- from file system + some other nice functions.
--
----------------------------------------------------------------------------

module DirSystem ( FileName
                 , DirSystem(..)
                 , File(..)
                 , FileAndResult
                 , FileAndResultE
                 , isDir
                 , isFile
                 , fileNameFrontPart
                 , fileSyntaxErrs
                 , filesAndResults
                 , results
                 , fileHasSuffix
                 , filesWitSuffix
                 , ppFiles
                 , readDirSystem
                 , readFileWithStrParser
                 , readFileListWithStrParser
                 , flatten
                 ) where


import Control.Monad (forM)
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import Control.Applicative
import Test.QuickCheck



-- |Contains Files of a directory.
data DirSystem = DirSystem { dirSysFiles :: [File]  -- ^ files in the directory system
                           } deriving (Show, Eq)

-- |Represents a File or a directory.                           
data File = File { fileName :: FileName  -- ^ name of a file
                 , filePath :: FilePath  -- ^ FilePath to the file
                 }
          | Dir  { dirName  :: FileName  -- ^ name of a directory
                 , dirPath  :: FilePath  -- ^ FilePath to the directory
                 , dirFiles :: [File]    -- ^ Files in the directory
                 } deriving (Show, Eq)

-- |File and a result of type a. 
-- Result is a result of reading a file with some function. 


type FileAndResultE a = (File, Either String a)
type FileAndResult a  = (File, a) 

type FileName = String                                      

-- -------------------------------------------------------------------------
-- ----------              instance declarations            ----------------
-- -------------------------------------------------------------------------

instance Arbitrary File where
    arbitrary = sized fileTree

fileTree :: Int -> Gen File
fileTree 0 = File <$> fName <*> fPath
fileTree n = do
   x <- choose (1,2) :: Gen Int
   case x of
        1 -> File <$> fName <*> fPath
        2 -> do (Positive m) <- arbitrary
                let n' = n `div` (m + 1)
                fn <- fName
                fp <- fPath
                fs <- mapM fileTree [0..n']
                return $ Dir fn fp fs

fName, fPath :: Gen String
fName = suchThat arbitrary (\s -> length s < 12)
fPath = suchThat arbitrary (\s -> length s < 24)



instance Arbitrary DirSystem where
   arbitrary = do 
       fs <- listOf arbitrary
       return (DirSystem fs)


-- =========================================================================
-- ==                  Reading DirSys from file system                    ==
-- =========================================================================

-- |Reads a directory from given path.
-- Fails if given file path is not a directory.
-- Uses function System.Directory.getDirectoryContents
readDirSystem :: FilePath -> IO DirSystem
readDirSystem fp = do
   fs    <- readDirSystemFiles fp
   return $ DirSystem fs


readDirSystemFiles :: FilePath -> IO [File]
readDirSystemFiles topDir = do
    cs    <- getDirectoryContents topDir
    let ns = filter (`notElem` [".", ".."]) cs
    forM ns $ \n -> do
       let path = topDir </> n
       isD <- doesDirectoryExist path
       if isD
          then do
            subDir <- readDirSystemFiles path
            return $ Dir n path subDir
          else
            return $ File n path

-- =========================================================================
-- ==                Other functions on files and dirs                    ==
-- =========================================================================
results :: [FileAndResult a] -> [a]
results = map snd

-- |Returns True if a File is a directory.
isDir :: File -> Bool
isDir (Dir {}) = True
isDir _        = False

isFile :: File -> Bool
isFile = not . isDir

-- |Reurns the file name part before suffix.
-- For example "foo.txt" -> "foo"
fileNameFrontPart :: FileName -> String
fileNameFrontPart = fst . span (/= '.')


-- |Checks if is a file and file name has a given suffix.
fileHasSuffix :: String ->  -- ^ suffix for example ".txt"
                 File   ->  -- ^ file to check
                 Bool
fileHasSuffix s (File n _) = isSuffixOf s n
fileHasSuffix _ _          = False

-- |Takes a suffix (for examle: ".txt") and a file list.
-- Returns all files having the given suffix.
filesWitSuffix :: String ->  -- ^ suffix
                  [File] ->  -- ^ file list to filter
                  [File]     -- ^ filtered list
filesWitSuffix suf = filter (fileHasSuffix suf)

-- |Pretty printer for file list.
ppFiles :: [File] -> String
ppFiles = concatMap ppFile

ppFile :: File -> String
ppFile = p 0
  where
    p :: Int -> File -> String
    p lvl (File fn fp)    = "\n" ++ (concat $ replicate lvl "\t") ++ fn ++
                            " (" ++ fp ++ ")"
    p lvl (Dir  dn fp fs) = "\n" ++ (concat $ replicate lvl "\t") ++ "DIR " ++
                            dn   ++ "  (" ++ fp ++ ")"  ++ concatMap (p (lvl+1)) fs



-- |Takes a String parser and a file and reads
-- the file with given String parser function.
readFileWithStrParser :: (String -> Either String a) ->  -- ^ String parser
                         File                        ->  -- ^ File
                         IO (FileAndResultE a)   -- ^ File with parse result
readFileWithStrParser func file = do
    pmCont <- readFile $ filePath file
    return (file, func pmCont)

-- |Takes a String parser and a file list and reads
-- the files with given String parser function and
-- returns IO [FileAndResult a]
readFileListWithStrParser :: (String -> Either String a) ->  -- ^ String parser
                             [File]                      ->  -- ^ File list
                             IO [FileAndResultE a]  -- ^ File list with parse result
readFileListWithStrParser func = mapM (readFileWithStrParser func)


-- |Flattens all DirSystem files from the sub directories to same level.
-- There will be no directories in the result but just a flat file projection.
flatten :: DirSystem -> DirSystem
flatten DirSystem {dirSysFiles = files} = DirSystem $ go files
   where
     go :: [File] -> [File]
     go []     = []
     go (f:fs)
        | isDir f   = go $ dirFiles f ++ go fs
        | otherwise = f : go fs

-- QuickChecks for flatten
prop_idempotent_flatten ds = flatten (flatten ds) == flatten ds
prop_isFlat_flatten ds     = all (isFile) (dirSysFiles $ flatten ds)


-- |Removes the Either monad from the results.
-- Caution! Use only if fileSyntaxErrs returns a null string.
-- Fails if given list contains any Left values as a result.
filesAndResults :: [FileAndResultE a] -> [FileAndResult a]
filesAndResults = map h
  where
    h :: FileAndResultE a -> FileAndResult a
    h (file, Right re) = (file, re)
    h _                = error "Programming error!!! #¤#¤"


-- |Takes a list of FileAndResults and returns messages from Left values
-- if there are such values as result.
fileSyntaxErrs :: [FileAndResultE a] -> String
fileSyntaxErrs = concatMap getErrMsg . filter h
  where
    h (File {} , Left {})  = True
    h _                    = False
    getErrMsg ((File n fp, Left msg)) =
         msg ++ ", when reading file: " ++ n   ++ " (" ++ fp ++ ")\n"
    getErrMsg _                       = ""
