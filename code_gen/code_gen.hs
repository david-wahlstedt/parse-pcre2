{- |

This code is called from Makefile, to generate data types and parser
helper structures for \p{ ... } Unicode properties, based on the
output of

1) pcre2test -LP (for "binary properties") and
2) pcre2test -LS (for unicode script names)

-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char(isAlphaNum, isSpace, toUpper)
import Data.Function(on)
import Data.List(intercalate, groupBy, sort)

--                            Constants

indentation = "  "

binaryPropertyFileName = "pcre2test/pcre2test-LP.txt"
scriptNameFileName     = "pcre2test/pcre2test-LS.txt"

binaryPropertyName = "BinProp"
scriptNameName = "ScriptName"

generatedCodePrefix = "generated"

warningsToSkip
  = unlines ["{-# OPTIONS_GHC -Wno-" ++ w ++ " #-}"
            | w <- ["missing-export-lists", "missing-signatures"]]


--                          Main function

main = do
  genCode binaryPropertyFileName binaryPropertyName
  genCode scriptNameFileName scriptNameName

genCode fileName name = do
  contents <- readFile fileName
  let namesShorts = getNamesAndShorts $ getBody contents
      dataDefs = genDataDef name $ map fst namesShorts
      dataModuleName = "Abs" ++ name
  -- write data type declarations to be imported by AbsPCRE and ParsePCRE
  writeFile (mkHsFilePath dataModuleName) $
    genModule dataModuleName [] dataDefs

  let parserHelper = genParserHelper name namesShorts
      parsModuleName = "ParsHelp" ++ name
  -- write parser helper code to be imported by ParsePCRE
  writeFile (mkHsFilePath parsModuleName) $
    genModule parsModuleName [dataModuleName] parserHelper
  where
    mkHsFilePath moduleName = generatedCodePrefix ++ "/" ++ moduleName ++ ".hs"


--                         Generate module

genModule :: String -> [String] -> String -> String
genModule moduleName importModules code
  = warningsToSkip ++ "\n" ++
    "module " ++ moduleName ++ " where\n\n" ++ imports ++ "\n" ++ code
  where imports = unlines $ map ("import "++) importModules

--                  Generate data type definition

genDataDef :: String -> [String] -> String
genDataDef typeName names
  = "data " ++ typeName ++ "\n" ++
    indentation ++ "= " ++
    intercalate ("\n" ++ indentation ++ "| ") constructorNames ++ "\n" ++
    indentation ++ "deriving Show" ++ "\n"
  where
    constructorNames = map capitalize names


--                 Generate parser helper structure

genParserHelper :: String -> [(String, [String])] -> String
genParserHelper typeName namesShorts
  = let pairs = ["([\"" ++ intercalate "\", \"" (name : shorts) ++ "\"], " ++
                 capitalize name ++ ")"
                | (name, shorts) <- namesShorts
                ]
    in "namesAndCons" ++ typeName ++ " = [\n" ++ indentation ++
       intercalate (",\n" ++ indentation) pairs ++ "]"


--      Collect names and shortcuts from pcre2test -LP or -LS

-- The body of the output appears after the last blank line
getBody :: String -> String
getBody fileData
  = let ls = lines fileData
        (as, _) = break null $ dropWhile null $ reverse ls
    in unlines $ reverse as

-- Collect names and/or shortcuts and sort alphabetically
getNamesAndShorts :: String -> [(String, [String])]
getNamesAndShorts = sort . getNamesAndShorts'
  where
    getNamesAndShorts' body
      = case nextNameShort body of
          Just (nameShort, body') -> nameShort : getNamesAndShorts' body'
          Nothing -> []

-- Gets the next name from the input string, and if present within
-- parentheses, a comma-separated list of its shortcuts.
nextNameShort :: String -> Maybe ((String, [String]), String)
nextNameShort body
  = case span isAlphaNum $ stripSpace body of
      (name@(_ : _), ' ' : '(' : body1) ->
        let (shorts, ')' : body2) = span (/=')') body1
        in Just ((name, splitBy ',' (removeSpace shorts)), body2)
      (name@(_ : _), body1) ->
        Just ((name, []), body1)
      _ ->
        Nothing


--                           Misc helpers

capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs

stripSpace :: String -> String
stripSpace = unwords . words

removeSpace :: String -> String
removeSpace = filter (not . isSpace)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim s = filter (/=[delim]) $ groupOn (==delim) s

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

