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

warningsToSkip
  = unlines ["{-# OPTIONS_GHC -Wno-" ++ w ++ " #-}"
            | w <- ["missing-export-lists", "missing-signatures"]]


--                          Main function

main = do
  -- Binary properties datatype
  propNameFile <- readFile binaryPropertyFileName
  let propNamesShorts = getNamesAndShorts $ getBody propNameFile
      propData = genDataFromNames binaryPropertyName $ map fst propNamesShorts
      propModuleName = "Abs" ++ binaryPropertyName
      propDataBody =
        warningsToSkip ++ "\n" ++
        "module " ++ propModuleName ++ " where\n\n" ++
        propData
  writeFile ("generated/"++ propModuleName ++".hs") propDataBody
  -- Binary properties parser helpers
  let propParserHelpers = genParserHelpers binaryPropertyName propNamesShorts
      propParsModuleName = "ParsHelp" ++ binaryPropertyName
      propParsHelpBody =
        warningsToSkip ++ "\n" ++
        "module " ++ propParsModuleName ++ " where\n\n" ++
        "import " ++ propModuleName ++ "\n\n" ++
        propParserHelpers
  writeFile ("generated/"++ propParsModuleName ++".hs") propParsHelpBody

  -- Script name datatype
  scriptNameFile <- readFile scriptNameFileName
  let scriptNamesShorts = getNamesAndShorts $ getBody scriptNameFile
      scriptData = genDataFromNames scriptNameName $ map fst scriptNamesShorts
      scriptModuleName = "Abs" ++ scriptNameName
      scriptDataBody =
        warningsToSkip ++ "\n" ++
        "module " ++ scriptModuleName ++ " where\n\n" ++
        scriptData
  writeFile ("generated/"++ scriptModuleName ++".hs") scriptDataBody
  -- Script name parser helpers
  let scriptParserHelpers = genParserHelpers scriptNameName scriptNamesShorts
      scriptParsModuleName = "ParsHelp" ++ scriptNameName
      scriptParsHelpBody =
        warningsToSkip ++ "\n" ++
        "module " ++ scriptParsModuleName ++ " where\n\n" ++
        "import " ++ scriptModuleName ++ "\n\n" ++
        scriptParserHelpers
  writeFile ("generated/"++ scriptParsModuleName ++".hs") scriptParsHelpBody


--                  Generate data type definition

genDataFromNames typeName names
  = genData typeName constructorNames
  where
    constructorNames = map capitalize names

genData typeName constructorNames
  = "data " ++ typeName ++ "\n" ++
    indentation ++ "= " ++
    intercalate ("\n" ++ indentation ++ "| ") constructorNames ++ "\n" ++
    indentation ++ "deriving Show" ++ "\n"


--                     Generate parser helpers

genParserHelpers typeName namesShorts = let
  pairs = ["([\"" ++ intercalate "\", \"" (name : shorts) ++ "\"], " ++
           capitalize name ++ ")"
          | (name, shorts) <- namesShorts
          ]
  in "namesAndCons" ++ typeName ++ " = [\n" ++ indentation ++
     intercalate (",\n"++indentation) pairs ++ "]"


--      Collect names and shortcuts from pcre2test -LP or -LS

getBody fileData
  = let ls = lines fileData
        (as, _) = break null $ dropWhile null $ reverse ls
    in unlines $ reverse as

getNamesAndShorts = sort . getNamesAndShorts'
  where
    getNamesAndShorts' body
      = case nextNameShort body of
          Just (nameShort, body') -> nameShort : getNamesAndShorts' body'
          Nothing -> []

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

capitalize (c : cs) = toUpper c : cs

stripSpace = unwords . words

removeSpace = filter (not . isSpace)

splitBy delim s = filter (/=[delim]) $ groupOn (==delim) s

groupOn f = groupBy ((==) `on` f)

