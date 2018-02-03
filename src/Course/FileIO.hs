{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  do a <- getArgs
     case a of
      Nil -> putStrLn "pass args silly"
      h:._ -> run h
      {-
      main =
        do a <- getArgs
           case a of
            Nil -> putStrLn "pass args silly"
            void (sequence (run <$> a))
      main =
        getArgs >>= \s ->
           case s of
            Nil -> putStrLn "pass args silly"
            void (sequence (run <$> a))`

      -}

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run =
  \name ->
  do c <- readFile name
     q <- getFiles (lines c)
     printFiles q

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles = \files -> sequence (getFile <$> files)

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  \name -> (\c -> (name , c)) <$> readFile name
-- or could write lift2 (<$>) (,) readFile
{- getFile =
  \name -> do
    c <- readFile name
    return (name, c)
-}
-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  \list -> void (sequence ((\(n, c) -> printFile n c) <$> list))
-- or could use void . sequence . (uncurry printFile <$>)
-- or could write void . sequence . (<$>) (uncurry printFile)

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fileName contents =
 do putStrLn ("=========== " ++ fileName)
    putStrLn contents
-- putStrLn fileName >>== \_ -> putStrLn contents
