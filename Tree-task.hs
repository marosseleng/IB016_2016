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
import           Control.Monad      (filterM, forM)
import           Data.List          (intercalate)
import           System.Directory
import           System.Environment (getArgs)
import           System.FilePath
import           Text.Read          (readMaybe)

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
    Config (showHelp c1 || showHelp c2)
           (showVersion c1 || showVersion c2)
           (noreport c1 || noreport c2)
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
              , ("-d", \_ -> defaultConfig { onlyDirs = True })
              , ("-L", \x -> defaultConfig { level = readLevel (tail $ dropWhile (/= ' ') x) })
              , ("--noreport", \_ -> defaultConfig { noreport = True })
              , ("-U", \_ -> defaultConfig { dirsfirst = Set False })
              , ("--dirsfirst", \_ -> defaultConfig { dirsfirst = Set True })
              , ("--help", \_ -> defaultConfig { showHelp = True })
              , ("--version", \_ -> defaultConfig { showVersion = True })
              ]
                where readLevel s = case readMaybe s :: Maybe Int of
                        Just x  -> Set x
                        Nothing -> NotSet

-- | The entry of your program.
main :: IO ()
main = do
  args <- getArgs
  let config = mconcat . map processArg $ joinLevelOption args
  pwd <- getCurrentDirectory
  (dirsCount, filesCount, printed) <- printDir pwd "" config
  let countInfo = if noreport config
                    then ""
                    else "\n\n" ++ show dirsCount ++ " directories, " ++
                    show filesCount ++ " files"
  let currentDirName = takeFileName pwd
  if showHelp config
    then putStrLn $ filterDoubleSpaces printed
    else putStrLn $ currentDirName ++ "\n" ++ filterDoubleSpaces printed ++
         countInfo

-- | Base function for printing the directory content
--
printDir :: FilePath -> String -> Config -> IO (Int, Int, String)
printDir _ _ (Config True _ _ _ _ _ _)                      = displayHelp
printDir _ _ (Config _ True _ _ _ _ _)                      = displayVersion
printDir f p c@(Config _ _ _ _ _ NotSet _)                  = withoutLimit f p c
printDir _ _ (Config _ _ _ _ _ (Set 0) _)                   = return (0, 0, "")
printDir dir inheritedPrefix c@(Config _ _ _ h d (Set l) _) = do
  -- Filter for -a option
  let hiddenFilter = if h then isThisOrParentDir else isDotFile
  filteredHidden <- fmap (filter (not . hiddenFilter)) (getDirectoryContents dir)
  let absPaths = map (dir </>) filteredHidden
  -- Filter for -d option
  let dirFilter = if d then doesDirectoryExist else (\_ -> return True)
  entries <- filterM dirFilter absPaths
  -- Writing directory entries
  let lastEntry = last entries
  printed <- forM entries $ \e -> do
    let isLast = e == lastEntry
    -- Setting prefices for the current line
    let branchPrefix = if isLast then "└── " else "├── "
    let prefixToPrint = inheritedPrefix ++ branchPrefix
    -- Setting prefices for the next line
    let colonPrefix = if isLast then "    " else "|   "
    let nextPrefix = inheritedPrefix ++ colonPrefix
    -- Written file name
    isDir <- doesDirectoryExist e
    let justName = takeFileName e
    let dirname = if isDir then justName ++ "\n" else justName
    -- Print entry's content recursively
    (dirs, files, str) <- if isDir
      then printDir e nextPrefix c { level = Set (l - 1) }
      -- If the current entry is a file
      else return (0, 1, "")
    return (
           -- If the current entry is a directory, add +1 to directories inside
           if isDir then dirs + 1 else dirs,
           files,
           prefixToPrint ++ dirname ++ str
           )
  return (
          sum (map getDirs printed),
          sum (map getFiles printed),
          intercalate "\n" (map getStr printed)
          )
  where isThisOrParentDir s = length s <= 2 && all (== '.') s
        isDotFile ""        = False
        isDotFile s         = head s == '.'
        getDirs (dc, _, _)  = dc
        getFiles (_, f, _)  = f
        getStr (_, _, s)    = s

-- | Recursively prints the directory without the limit
--
withoutLimit :: FilePath -> String -> Config -> IO (Int, Int, String)
withoutLimit d p c = printDir d p c { level = Set (-1) }

-- | Function that returns IO String with the help of the program
-- Using twice as many '\n' character as needed, because the output is filtered
-- by the filterDoubleSpaces Function
displayHelp :: IO (Int, Int, String)
displayHelp = return (0, 0,
  "TREE(1)\n\n\n\n" ++
  "\n\nNAME\n" ++
  "\ttree - list contents of directories in a tree-like format.\n" ++
  "\n\nSYNOPSIS\n" ++
  "\ttree [-a] [-d] [-U] [-L  level] [--noreport] [--dirsfirst]" ++
  " [--version]\n" ++
  "\n\nDESCRIPTION\n" ++
  "\tTree is a recursive directory listing program that " ++
  "produces a depth indented listing of files in the current directory and" ++
  " shows the output to tty. Upon completion of listing all " ++
  "files/directories found, tree returns the total number of files and/or" ++
  " directories listed.\n" ++
  "\n\nOPTIONS\n" ++
  "\tTree understands the following command line switches:\n" ++
  "\n\nLISTING OPTIONS\n" ++
  "\t-a\tAll files are printed. By default tree does not print hidden files" ++
  "(those beginning with a dot `.'). In no event does tree print the file" ++
  " system constructs `.' (current directory) and `..'" ++
  " (previous directory).\n" ++
  "\n\n\t-d\tList directories only.\n" ++
  "\n\n\t-L level\n\t\tMax display depth of the directory tree.\n" ++
  "\n\n\t--noreport\n\t\tOmits printing of the file and directory report at" ++
  "the end of the tree listing.\n" ++
  "\n\nSORTING OPTIONS\n" ++
  "\t-U\tDo not sort. Lists files in directory order. " ++
  "Disables --dirsfirst.\n" ++
  "\n\n\t--dirsfirst\n\t\tList directories before files." ++
  "This option is disabled when -U is used.\n" ++
  "\n\nMISC OPTIONS\n" ++
  "\t--help\tOutputs a verbose usage listing.\n" ++
  "\n\n\t--version\n\t\tOutputs the version of tree.\n" ++
  "\n\nAUTHOR\n" ++
  "\tMaros Seleng (xseleng@fi.muni.cz)" ++
  "\n"
  )

-- | Displays the version of a program and the name of its developer
--
displayVersion :: IO (Int, Int, String)
displayVersion = return (0, 0, "tree v1.0.0 (c) 2016 by Maros Seleng")

-- | Filters double spaces generated by the directory printing
-- Double spaces occur when passed the empty directory
-- >>> filterDoubleSpaces "\n\n"
-- "\n"
--
-- >>> filterDoubleSpaces "\n\n\n\n"
-- "\n\n"
--
-- >>> filterDoubleSpaces "\n\n\n"
-- "\n\n"
--
filterDoubleSpaces :: String -> String
filterDoubleSpaces ""             = ""
filterDoubleSpaces [c]            = [c]
filterDoubleSpaces ('\n':'\n':xs) = '\n' : filterDoubleSpaces xs
filterDoubleSpaces (x:xs)         = x : filterDoubleSpaces xs

-- | Joins "-L" and following option to a single one
-- >>> joinLevelOption ["-L","42"]
-- ["-L 42"]
--
joinLevelOption :: [String] -> [String]
joinLevelOption []          = []
joinLevelOption [x]         = [x]
joinLevelOption ("-L":n:xs) = ("-L" ++ " " ++ n) : joinLevelOption xs
joinLevelOption (x:xs)      = x : joinLevelOption xs
