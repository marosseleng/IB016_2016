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
import           Control.Monad    (forM)
import           Data.List        (intercalate)
import           System.Directory
import           System.FilePath

--data Config = Config
--    { verbose :: Flag Bool
--    , options :: [String]
--    , printer :: Flag2 String
--    } deriving (Eq, Show)

-- | The entry of your program.
main :: IO ()
main = do
  pwd <- getCurrentDirectory
  printed <- printDir pwd ""
  putStrLn $ filterDoubleSpaces printed

printDir :: FilePath -> String -> IO String
printDir dir inheritedPrefix = do
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
    subdirPrinted <- if isDir then printDir absPath nextPrefix else return ""
    return $ inheritedPrefix ++ branchPrefix ++ dirname ++ subdirPrinted
  return $ intercalate "\n" printed


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
