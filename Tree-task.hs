{- | This is the third assignment for IB016, semester spring 2016.

Name: Maros Seleng

UID: 422624


== Tree

This time your task is to implement a simplified version of unix utility @tree@,
which can print a directory tree on the terminal. For more details about @tree@,
you can refer to @man tree@ or to its online version, for example
<http://linux.die.net/man/1/tree>.

Your version should support at least the following options:

*   @--help@ -- will print help for your version of @tree@, including a short
    description and available options.

    *   The format of the output can be arbitrary, however, it must contain
        short information about all options.
    *   Help is printed on the standard output.

*   @--version@ -- prints the version and optionally the licence and authors of
    the program.

    *   The format of the output can be arbitrary.

*   @-a@ -- All files are printed.

    *   By default, @tree@ does not print hidden files (those beginning with a dot
        @.@). In @tree@ should never print the file system constructs @.@
        (current directory) and @..@ (previous directory).

*   @-d@ -- List directories only.

    *   By default, @tree@ prints both files and directories.
    *   In combination with @-a@ it prints hidden directories (but no files).

*   @-L level@ --  Max display depth of the directory tree.

    *   The level must be greater then 0.
    *   If more than one @-L@ options are given, the latest is used.

*   @--noreport@ --  Omits printing of the file and directory report at the end
    of the tree listing.

    *   If this option is not given @tree@ will print short summary at the end,
        in the form "X directories, Y files" where X and Y are the numbers of
        printed directories and files.
    *   Symbolic links can be counted as files, or as the GNU tree does, counted
        as files if they refer to files (or are invalid) and as directories if
        they link to directories.

*   @-U@ -- Do not sort.

    *   Disables @--dirsfirst@
    *   Without this option, the content of each directory is sorted
        lexicographically by the name.

*   @--dirsfirst@ -- List directories before files.

    *   This option is disabled when @-U@ is used.
    *   Without this option, sorting is applied regardless of the entry type.

*   If both @--dirsfirst@ and @-U@ are used, the effect is given by the last
one.

If the program is run with wrong arguments, it prints help (as with @--help@) to
the standard error output and exits with nonzero exit code.

If the program finds a directory it cannot access, it will print the name of the
directory (and not descent into it) and it will print a relevant error message
on the standard error output. It will also exit with a nonzero exit code in this
case.

The implementation must be fully in Haskell, it must not call any external
utilities. You are allowed to use any libraries from
<https://downloads.haskell.org/~ghc/latest/docs/html/libraries/>. You can also
use a library for command line parsing (even if it is not listed in the above
libraries, for example @System.Console.GetOpt@). However, it would be nice if
you used monoid-based command line parsing from the lectures.

The format of the output should be same as for unix utility @tree@, small
deviations might be tolerated.

== Example

@
test
├── file01.txt
├── file02.txt
├── sub_01
│   ├── file01_01.txt
│   ├── file01_02.txt
│   └── sub_01_01
│       ├── file01_01_01.txt
│       └── file01_01_02.txt
├── sub_02
│   ├── file02_01.txt
│   ├── file02_02.txt
│   ├── sub_02_01
│   │   ├── file02_02_01.txt
│   │   └── file02_02_02.txt
│   └── sub_02_02
├── sub_03
└── sub_04

7 directories, 10 files
@

For more examples use @tree@ for example on @aisa@.

Tip: design of the appropriate data structures will help you a lot.
└──├──
-}

module Main ( main ) where
import           Control.Monad      (forM)
import           Data.List          (intercalate)
import           System.Directory
import           System.Environment (getArgs)
import           System.FilePath

data Config = Config
    { showHelp      :: Bool
    , showVersion   :: Bool
    , noreport      :: Bool
    , includeHidden :: Bool
    , onlyDirs      :: Bool
    , level         :: Flag Int
    , dirsfirst     :: Flag Bool
    } deriving (Eq, Show)

data Flag a = NotSet | Set a deriving (Eq, Show)

instance Monoid (Flag a) where
  mempty = NotSet
  _ `mappend` Set x  = Set x
  x `mappend` NotSet = x

instance Monoid Config where
  mempty = defaultConfig
  c1 `mappend` c2 =
    Config (noreport c1 || noreport c2)
           (showHelp c1 || showHelp c2)
           (showVersion c1 || showVersion c2)
           (includeHidden c1 || includeHidden c2)
           (onlyDirs c1 || onlyDirs c2)
           (level c1 `mappend` level c2)
           (dirsfirst c1 `mappend` dirsfirst c2)

defaultConfig :: Config
defaultConfig = Config False False False False False NotSet NotSet

processArg :: String -> Config
processArg arg = let opt = takeWhile (/= ' ') arg in
    case lookup opt configTable of
    Nothing -> defaultConfig { showHelp = True}
    Just f -> f arg

configTable :: [ (String, String -> Config) ]
configTable = [ ("-a", \_ -> defaultConfig { includeHidden = True })
              , ("-d", \_ -> defaultConfig { onlyDirs = False })
              , ("-L", \x -> defaultConfig { level = Set (read (tail $ dropWhile (/= ' ') x) :: Int) })
              , ("--noreport", \_ -> defaultConfig { noreport = True })
              , ("-U", \_ -> defaultConfig { dirsfirst = Set False })
              , ("--dirsfirst", \_ -> defaultConfig { dirsfirst = Set True })
              , ("--help", \_ -> defaultConfig { showHelp = True })
              , ("--version", \_ -> defaultConfig { showVersion = True })
              ]

getLevel :: Flag Int -> Int
getLevel NotSet  = -1
getLevel (Set x) = x

-- | The entry of your program.
main :: IO ()
main = do
  args <- getArgs
  let config = mconcat . map processArg $ joinLevelOption args
  pwd <- getCurrentDirectory
  printed <- printDir pwd "" config
  putStrLn $ filterDoubleSpaces printed

printDir :: FilePath -> String -> Config -> IO String
printDir _ _ (Config _ True _ _ _ _ _)     = displayHelp
printDir _ _ (Config _ _ True _ _ _ _)     = displayVersion
printDir f p c@(Config _ _ _ _ _ NotSet _) = printDir f p c { level = Set (-1) }
printDir _ _ (Config _ _ _ _ _ (Set 0) _)  = return ""
printDir dir inheritedPrefix c@(Config _ _ _ _ _ (Set l) _) = do
  entries <- fmap (filter (not . isThisOrParentDir)) (getDirectoryContents dir)
  let lastEntry = last entries
  printed <- forM entries $ \e -> do
    let isLast = e == lastEntry
    -- PREFICES SETTING
    let colonPrefix = if isLast then "    " else "|   "
    let branchPrefix = if isLast then "└── " else "├── "
    let nextPrefix = inheritedPrefix ++ colonPrefix

    let absPath = dir </> e
    isDir <- doesDirectoryExist absPath
    let dirname = if isDir then e ++ "\n" else e
    subdirPrinted <- if isDir then printDir absPath nextPrefix c { level = Set (l - 1) } else return ""
    return $ inheritedPrefix ++ branchPrefix ++ dirname ++ subdirPrinted
  return $ intercalate "\n" printed

displayHelp :: IO String
displayHelp = return "This is help!"

displayVersion :: IO String
displayVersion = return "tree, v. 0.9.0\nAuthor Maros Seleng"



-- | Filters double spaces generated by the directory printing
-- Double spaces occur when passed the empty directory
--
filterDoubleSpaces :: String -> String
filterDoubleSpaces ""             = ""
filterDoubleSpaces [c]            = [c]
filterDoubleSpaces ('\n':'\n':xs) = '\n' : filterDoubleSpaces xs
filterDoubleSpaces (x:xs)         = x : filterDoubleSpaces xs

isThisOrParentDir :: FilePath -> Bool
isThisOrParentDir "."  = True
isThisOrParentDir ".." = True
isThisOrParentDir _    = False

joinLevelOption :: [String] -> [String]
joinLevelOption []          = []
joinLevelOption [x]         = [x]
joinLevelOption ("-L":n:xs) = ("-L" ++ " " ++ n) : joinLevelOption xs
joinLevelOption (x:xs)      = x : joinLevelOption xs
