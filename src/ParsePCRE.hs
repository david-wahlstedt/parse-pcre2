{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}

module ParsePCRE where

import Control.Monad.State
    ( get, modify, evalStateT, MonadTrans(lift), StateT(..) )
import Data.Char(chr, isDigit, isAlphaNum, isLetter, isAscii, isControl,
                 isHexDigit, isOctDigit, isPrint, isSpace, toLower)
import qualified Data.HashMap.Strict as HM
import Numeric(readOct, readHex)
import qualified Text.ParserCombinators.ReadP as R

import AbsPCRE

-- Code generated by code_gen.hs
import AbsBinProp ( BinProp )
import ParseHelpBinProp ( namesAndConsBinProp )
import AbsScriptName ( ScriptName )
import ParseHelpScriptName ( namesAndConsScriptName )


------------------------------------------------------------------------------
--                     Parser monad transformer

data Env = Options{
  -- Covers only options that affect parsing
  ignoreWS :: Bool, -- (?x) ignore unescaped whitespace+treat #.*\n as comments
  ignoreWSClasses :: Bool, -- (?xx) like x, but also ignore ws in char classes
  noAutoCapture :: Bool -- (?n)
  }
  deriving Show

type Parser a = StateT [Env] R.ReadP a

runParser :: Env -> String -> [(Re, String)]
runParser initialEnv input =
  -- Start with the initial environment at the top of the stack
  let initialState = [initialEnv]
  in R.readP_to_S (evalStateT (re <* eof) initialState) input


------------------------------------------------------------------------------
--                        Parser entrypoint

parsePCRE :: String -> Maybe Re
parsePCRE input =
  case runParser initialEnv input of
    (result, "") : _ -> Just result
    _                -> Nothing
  where
    initialEnv = Options
      { ignoreWS        = False,
        ignoreWSClasses = False,
        noAutoCapture   = False
      }


--                         Basic constructs

re :: Parser Re
re = resolveOctOrBackrefs <$> alt

alt :: Parser Re
alt = Alt <$> sepBy1 sequencing (char '|')

sequencing :: Parser Re
sequencing = Seq <$> (skippables *> many atom)

atom :: Parser Re
atom = atom' <* skippables

atom' :: Parser Re
atom'
  =   Anchor    <$> anchor
  <|| (\e q m -> quantify m e q) <$> quantifiable <*> quantifier <*> quantMode
  <|| quantify Greedy <$> quantifiable <*> quantifier
  <|| quantifiable
  <|| SetStartOfMatch <$ string "\\K"
  <|| OptSet    <$> optionSetting
  <|| Backtrack <$> backtrackControl
  <|| COut      <$> callout
  where quantify mode e q
          = case splitQuoting e of
              Just (cs, c) ->
                -- we quantify on the last character
                Seq [Quoting cs, Quant mode q (Lit c)]
              Nothing ->
                Quant mode q e
        -- empty quoting is treated as a comment, and should not be
        -- encountered here
        splitQuoting (Quoting "") = Nothing
        splitQuoting (Quoting cs) = Just (init cs, last cs)
        -- singleton sequence or alternation: descend
        splitQuoting (Seq [e']) = splitQuoting e'
        splitQuoting (Alt [e']) = splitQuoting e'
        splitQuoting _ = Nothing

quantifiable :: Parser Re
quantifiable
  =   OctOrBackRef <$> (string "\\"  *> octOrBackRefDigits) -- \\[1-9][0-9]{2}
  <|| Esc <$> escChar
  <|| Ctrl <$> (string "\\c" *> printableAscii) -- \c x, printable ascii
  -- a quoting is not really quantifiable, but its last character will
  -- be quanfitied, and this is handeled by the atom parser
  <|| Quoting   <$> postCheck (not . null) quoting
  <|| localST group
  <|| scriptRun
  <|| lookAround
  <|| Chartype  <$> charType
  <|| Cond      <$> localST conditional
  <|| Charclass <$> charClass
  <|| BackRef   <$> backRef
  <|| SubCall   <$> subCall
  <|| literal


--                               Quoting

quoting :: Parser String
quoting
  =   string "\\Q" *> getUntil "\\E"
  <|| string "\\Q" *> many anyChar <* eof


--                        Escaped characters

escChar :: Parser Char
escChar
  =   '\a'       <$   string "\\a"                   -- \a
  <|| '\ESC'     <$   string "\\e"                   -- \e Esc
  <|| '\f'       <$   string "\\f"                   -- \f FF
  <|| '\n'       <$   string "\\n"                   -- \n
  <|| '\r'       <$   string "\\r"                   -- \r
  <|| '\t'       <$   string "\\t"                   -- \t
  <|| fromOctStr <$> (string "\\0" *> octDigits 0 2) -- \\0[0-7]{0,2}
  <|| fromOctStr <$> (string "\\o{" *> many1 octDigit <* char '}') -- \o{[0-7]+}
  <|| fromHexStr <$> (string "\\N{U+" *> many1 hexDigit <* char '}') -- \N{U+h+}
  <|| 'U'        <$   string "\\U"                                   -- \U
  <|| fromHexStr <$> (string "\\x{" *> many1 hexDigit <* char '}')   -- \x{h+}
  <|| fromHexStr <$> (string "\\x"  *> hexDigits 0 2)                -- \xh{0,2}
  <|| fromHexStr <$> (string "\\u{" *> many1 hexDigit <* char '}')   -- \u{h+}
  <|| fromHexStr <$> (string "\\u"  *> hexDigits 4 4)                -- \uhhhh
  <|| string   "\\" *> nonAlphanumAscii -- \ non alphanum ascii


--                         Character types

charType :: Parser Chartype
charType
  =   charTypeCommon
  <|| CTAny <$ char '.'

charTypeCommon :: Parser Chartype
charTypeCommon
  =   CTCodeUnit   <$ string "\\C"
  <|| CTDigit      <$ string "\\d"
  <|| CTNDigit     <$ string "\\D"
  <|| CTHSpace     <$ string "\\h"
  <|| CTNHSpace    <$ string "\\H"
  <|| CTNNewLine   <$ string "\\N"
  <|| CTNewLineSeq <$ string "\\R"
  <|| CTSpace      <$ string "\\s"
  <|| CTNSpace     <$ string "\\S"
  <|| CTVSpace     <$ string "\\v"
  <|| CTNVSpace    <$ string "\\V"
  <|| CTWordChar   <$ string "\\w"
  <|| CTNWordChar  <$ string "\\W"
  <|| CTUCluster   <$ string "\\X"
  <|| ctProperty

-- \p and \P character type properties
ctProperty :: Parser Chartype
ctProperty
  =   CTProp  <$> ((string "\\P{^" <|| string "\\p{") *> ctBody)
  <|| CTNProp <$> ((string "\\p{^" <|| string "\\P{") *> ctBody)
  <|| CTProp  <$> (string "\\p" *> singleCharGeneral)
  <|| CTNProp <$> (string "\\P" *> singleCharGeneral)

-- Single character general category properties C, L, N, M, P, S, and Z.
singleCharGeneral :: Parser CTProperty
singleCharGeneral = do
  c <- anyChar
  genCatProp [toLower c]

-- Everything inside \p{ } or \P{ } is converted into lower case and
-- stripped from ascii range spaces, hyphens and underscores.

ctBody :: Parser CTProperty
ctBody = do
  body <- getUntil "}"
  case break (`elem` [':','=']) $ normalize body of
    -- Bidi classes
    (prefix, _: subject) | prefix `elem` ["bc", "bidiclass"] ->
      CTBidi <$> bidi subject
    -- Script matching, extensions
    (prefix, _: subject) | prefix `elem` ["scx", "scriptextensions"] ->
      CTScript . Extensions <$> scriptName subject
    -- Script matching, basic
    (prefix, _: subject) | prefix `elem` ["sc", "script"] ->
      CTScript . Basic <$> scriptName subject
    (subject, []) ->
      -- pcre2pattern: If a script name is given without a property type,
      -- for example, \p{Adlam}, it is treated as \p{scx:Adlam}:
      CTScript . Extensions <$> scriptName subject
      <||
      -- "Binary properties"
      CTBinProp <$> binProp subject
      <||
      -- All other \p and \P properties
      genCatProp subject
    _ ->
      pfail
  where normalize
          = map toLower . filter
            (\c -> not (isSpace c && isAscii c || c `elem` ['-','_']))

-- "General category properties for \p and \P" and
-- "PCRE2 special category properties for \p and \P"
genCatProp :: String -> Parser CTProperty
genCatProp s = maybe pfail pure $ HM.lookup s genCatProps

-- namesAndConsScriptName is gnerated from ../pcre2test/pcre2test-LS.txt
scriptName :: String -> Parser ScriptName
scriptName s = maybe pfail pure $ HM.lookup s scriptNames
scriptNames :: HM.HashMap String ScriptName
scriptNames = HM.fromList $ flatNamesAndCons namesAndConsScriptName

-- namesAndConsBinProp is generated from ../pcre2test/pcre2test-LP.txt
binProp :: String -> Parser BinProp
binProp s = maybe pfail pure $ HM.lookup s binProps
binProps :: HM.HashMap String BinProp
binProps = HM.fromList $ flatNamesAndCons namesAndConsBinProp

bidi :: String -> Parser CTBidiClass
bidi s = maybe pfail pure $ lookup s bidiClasses

bidiClasses :: [(String, CTBidiClass)]
bidiClasses =
  [ ("al",  BidiAL)
  , ("an",  BidiAN)
  , ("b",   BidiB)
  , ("bn",  BidiBN)
  , ("cs",  BidiCS)
  , ("en",  BidiEN)
  , ("es",  BidiES)
  , ("et",  BidiET)
  , ("fsi", BidiFSI)
  , ("l",   BidiL)
  , ("lre", BidiLRE)
  , ("lri", BidiLRI)
  , ("lro", BidiLRO)
  , ("nsm", BidiNSM)
  , ("on",  BidiON)
  , ("pdf", BidiPDF)
  , ("pdi", BidiPDI)
  , ("r",   BidiR)
  , ("rle", BidiRLE)
  , ("rli", BidiRLI)
  , ("rlo", BidiRLO)
  , ("s",   BidiS)
  , ("ws",  BidiWS)
  ]

genCatProps :: HM.HashMap String CTProperty
genCatProps = HM.fromList $ concat
  [
    [ ("any", CTUAny) ],
    CTGenProp . Other <<$>>
    [ ("c" , C   ),   -- Other
      ("cc", Cc  ),   -- Control
      ("cf", Cf  ),   -- Format
      ("cn", Cn  ),   -- Unassigned
      ("co", Co  ),   -- Private use
      ("cs", Cs  ) ], -- , -- Surrogate
    CTGenProp . Letter <<$>>
    [ ("l",  L   ),   -- Letter
      ("lc", Lc  ),   -- Ll, Lu, or Lt
      ("l&", Llut),   -- "L&": Ll, Lu, or Lt
      ("ll", Ll  ),   -- Lower case letter
      ("lm", Lm  ),   -- Modifier letter
      ("lo", Lo  ),   -- Other letter
      ("lt", Lt  ),   -- Title case letter
      ("lu", Lu  ) ],   -- Upper case letter
    CTGenProp . Mark <<$>>
    [ ("m",   M  ),   -- Mark
      ("mc",  Mc ),   -- Spacing mark
      ("me",  Me ),   -- Enclosing mark
      ("mn",  Mn ) ], -- Non-spacing mark
    CTGenProp . Number <<$>>
    [ ("n",   N  ),   -- Number
      ("nd",  Nd ),   -- Decimal number
      ("nl",  Nl ),   -- Letter number
      ("no",  No ) ], -- Other number
    CTGenProp . Punctuation <<$>>
    [ ("p",   P  ),   -- Punctuation
      ("pc",  Pc ),   -- Connector punctuation
      ("pd",  Pd ),   -- Dash punctuation
      ("pe",  Pe ),   -- Close punctuation
      ("pf",  Pf ),   -- Final punctuation
      ("pi",  Pi ),   -- Initial punctuation
      ("po",  Po ),   -- Other punctuation
      ("ps",  Ps ) ], -- Open punctuation
    CTGenProp . Symbol <<$>>
    [ ("s",   S  ),   -- Symbol
      ("sc",  Sc ),   -- Currency symbol
      ("sk",  Sk ),   -- Modifier symbol
      ("sm",  Sm ),   -- Mathematical symbol
      ("so",  So ) ], -- Other symbol
    CTGenProp . Separator <<$>>
    [ ("z",   Z  ),   -- Separator
      ("zl",  Zl ),   -- Line separator
      ("zp",  Zp ),   -- Paragraph separator
      ("zs",  Zs ) ], -- Space separator
    CTSpecProp <<$>>
    [ ("xan", Xan),  -- Alphanumeric: union of properties L and N
      ("xps", Xps),  -- POSIX sp: property Z or tab, NL, VT, FF, CR
      ("xsp", Xsp),  -- Perl sp: property Z or tab, NL, VT, FF, CR
      ("xuc", Xuc),  -- Univ.-named character: one that can be ...
      ("xwd", Xwd) ] -- Perl word: property Xan or underscore
  ]
  where infixl 8 <<$>>
        (<<$>>) = fmap . fmap

--                         Character classes

charClass :: Parser Charclass
-- special cases for emtpy classes: should be configurable
charClass
  =   postCheck checkRanges $
      fixQuoting <$> (
      Noneof <$> (stripNullQuotes "[^" *> charClassBody' <* char ']')
  <|| Noneof [] <$ stripNullQuotes "[^]"
  <|| Oneof  <$> (char   '['  *> charClassBody' <* char ']')
  <|| Oneof [] <$ stripNullQuotes "[]")
  where
    charClassBody' = charClassBody <* optEmptyQuoting

    -- Character types and POSIX sets may not be part of ranges:
    checkRanges = charclassCase p p
      where p = all validRange

    validRange (Range (CCCharType _) _             ) = False
    validRange (Range _              (CCCharType _)) = False
    validRange (Range (PosixSet   _) _             ) = False
    validRange (Range _              (PosixSet   _)) = False
    validRange _                                     = True

    fixQuoting = charclassCase (Oneof . f) (Noneof . f)
      where f = removeEmtpyQuotes . fixQuotingRanges

    -- Find range endponits being non-empty quotings, and make the
    -- character adjacent to the hyphen the endpoint. This may create
    -- empty quotings, which are removed in the next pass.
    fixQuotingRanges (r@(Range a1 a2) : items) =
      case (a1, a2) of
        (CCQuoting s1, CCQuoting s2)
          | not (null s1) && not (null s2) ->
            CCAtom (CCQuoting (init s1)) :
            Range (CCLit $ last s1) (CCLit $ head s2) :
            CCAtom (CCQuoting (tail s2)) : fixQuotingRanges items
        (CCQuoting s1, a) | not (null s1) ->
           CCAtom (CCQuoting (init s1)) :
           Range (CCLit $ last s1) a : fixQuotingRanges items
        (a, CCQuoting s2) | not (null s2) ->
           Range a (CCLit $ head s2) :
           CCAtom (CCQuoting (tail s2)) : fixQuotingRanges items
        _ -> r : fixQuotingRanges items
    fixQuotingRanges (item : items) = item : fixQuotingRanges items
    fixQuotingRanges [] = []

    removeEmtpyQuotes = filter noEmptyQuoting

    noEmptyQuoting (CCAtom (CCQuoting "")) = False
    noEmptyQuoting _ = True

charClassBody :: Parser [CharclassItem]
charClassBody
  =   (:) . Range (CCLit ']') <$>
       (optEmptyQuoting <* stripNullQuotes "]-" *> charClassAtom) <*>
       many charClassItem
  <|| (CCAtom (CCLit ']') :) <$>
       (optEmptyQuoting <* char ']' *> many charClassItem)
  <|| many charClassItem

charClassRange :: Parser CharclassItem
charClassRange
  = Range <$>
    charClassAtom <*> ((optEmptyQuoting <* char '-') *> charClassAtom)

charClassItem :: Parser CharclassItem
charClassItem
  =   charClassRange
  <|| CCAtom <$> charClassAtom

charClassAtom :: Parser CharclassAtom
charClassAtom = optEmptyQuoting *> charClassAtom'

charClassAtom' :: Parser CharclassAtom
charClassAtom'
  =   CCQuoting   <$> postCheck (not . null) quoting
  <|| CCBackspace <$  string "\\b"
  <|| CCEsc <$> (char '\\' *> (char '8' <|| char '9'))
  <|| CCEsc . fromOctStr <$> (char '\\' *> octDigits 1 3)
  <|| CCEsc <$> escChar
  <|| CCCtrl <$> (string "\\c" *> printableAscii) -- \c x, printable ascii
  <|| CCCharType  <$> charTypeCommon
  <|| PosixSet    <$> posixSet
  <|| CCLit       <$> ccLit

posixSet :: Parser PosixSet
posixSet
  =   NegSet <$> (string "[:^" *> setName <* string ":]")
  <|| PosSet <$> (string "[:"  *> setName <* string ":]")

setName :: Parser SetName
setName
  =   SetAlnum  <$ string "alnum"  -- alphanumeric
  <|| SetAlpha  <$ string "alpha"  -- alphabetic
  <|| SetAscii  <$ string "ascii"  -- 0-127
  <|| SetBlank  <$ string "blank"  -- space or tab
  <|| SetCntrl  <$ string "cntrl"  -- control character
  <|| SetDigit  <$ string "digit"  -- decimal digit
  <|| SetGraph  <$ string "graph"  -- printing, excluding space
  <|| SetLower  <$ string "lower"  -- lower case letter
  <|| SetPrint  <$ string "print"  -- printing, including space
  <|| SetPunct  <$ string "punct"  -- printing, excluding alphanumeric
  <|| SetSpace  <$ string "space"  -- white space
  <|| SetUpper  <$ string "upper"  -- upper case letter
  <|| SetWord   <$ string "word"   -- same as \w
  <|| SetXdigit <$ string "xdigit" -- hexadecimal digit

ccLit :: Parser Char
ccLit = nonSpecial ccSpecials

-- Set of special characters that need to be escaped inside character classes
ccSpecials :: [Char]
ccSpecials = ['\\', ']']


--                           Quantifiers

quantifier :: Parser Quantifier
quantifier = skippables *> quantifier'

quantifier' :: Parser Quantifier
quantifier'
  =   Option <$ char '?'
  <|| Many   <$ char '*'
  <|| Many1  <$ char '+'
  <|| Rep    <$>
       (char '{' *> skipSpaces *> natural <* skipSpaces <* char '}')
  <|| RepMin <$>
       (char '{' *> skipSpaces *> natural <* skipSpaces <*
        char ',' <* skipSpaces <* char '}')
  <|| RepMax <$>
       (char '{' *> skipSpaces *> char ',' *> natural <*
        skipSpaces <*  char '}')
  <|| RepMinMax <$>
       (char '{' *> skipSpaces *> natural <* skipSpaces <* char ',') <*>
        (skipSpaces *> natural <* skipSpaces <* char '}')

quantMode :: Parser QuantifierMode
quantMode = skippables *> quantMode'

quantMode' :: Parser QuantifierMode
quantMode'
  =   Possessive <$ char '+'
  <|| Lazy <$ char '?'


--                         Anchors

anchor :: Parser Anchor
anchor
  =   StartOfLine          <$ char   '^'
  <|| EndOfLine            <$ char   '$'
  <|| StartOfSubject       <$ string "\\A"
  <|| EndOfSubject         <$ string "\\Z"
  <|| EndOfSubjectAbsolute <$ string "\\z"
  <|| FirstMatchingPos     <$ string "\\G"
  <|| WordBoundary         <$ string "\\b"
  <|| NonWordBoundary      <$ string "\\B"
  <|| StartOfWord          <$ string "[[:<:]]"
  <|| EndOfWord            <$ string "[[:>:]]"


--                         Groups

group :: Parser Re
group
  =   atomicNonCapture
  <|| nonCapture
  <|| nonCaptureOpts
  <|| namedCapture
  <|| nonCaptureReset
  <|| capture

capture :: Parser Re
capture = Group Capture <$> (char '(' *> alt <* char ')')

nonCapture :: Parser Re
nonCapture = Group NonCapture <$>  (string "(?:" *> alt <* char ')')

-- option setting in non-capture group (?OptsOn-OptsOff:...)
nonCaptureOpts :: Parser Re
nonCaptureOpts
  = mkGroup <$>
    (string "(?" *> (internalOpts <* char ':')) <*> alt <* char ')'
  where
    mkGroup (InternalOpts ons offs) = Group (NonCaptureOpts ons offs)

nonCaptureReset :: Parser Re
nonCaptureReset
  = Group NonCaptureReset <$> (string "(?|" *> alt <* char ')')

atomicNonCapture :: Parser Re
atomicNonCapture
  = Group AtomicNonCapture <$
    oneStr ["(?>", "(*atomic:"] <*> alt <* char ')'

namedCapture :: Parser Re
namedCapture
  =   mkNamed <$>
       (string "(?<" *> groupName) <*> (char '>' *> alt <* char ')')
  <|| mkNamed <$>
       (string "(?'" *> groupName) <*> (char '\'' *> alt <* char ')')
  <|| mkNamed <$>
       (string "(?P<" *> groupName) <*> (char '>' *> alt <* char ')' )
  where mkNamed name = Group (NamedCapture name)

groupName :: Parser String
groupName = (:) <$> groupNameChar True <*> many (groupNameChar False)


--                       Comments and skippables

skippables :: Parser ()
skippables = () <$ many skippable

skippable :: Parser String
skippable
  =   string "(?#" *> getUntil ")"
  <|| emptyQuoting
  <|| oneLineComment
  <|| ignoredWS

oneLineComment :: Parser String
oneLineComment = do
  s <- get
  let env = head s
      x = ignoreWS env
      xx = ignoreWSClasses env
  if x || xx
    then char '#' *> (getUntil "\n" <|| manyTill anyChar eof)
    else pfail

ignoredWS = do
  s <- get
  let env = head s
      x = ignoreWS env
      xx = ignoreWSClasses env
  if x || xx
    then "" <$ asciiWhiteSpace
    else pfail

emptyQuoting :: Parser String
emptyQuoting
  =   string "\\Q\\E" -- Ignore empty quotings
  <|| string "\\E"    -- An isolated \E that is not preceded by \Q is ignored.

-- Strips away zero or more consecutive empty quotings
optEmptyQuoting :: Parser String
optEmptyQuoting = "" <$ many emptyQuoting


--                          Option setting

optionSetting :: Parser OptionSetting
optionSetting
  =   StartOpt <$> (string "(*" *> startOpt <* char ')')
  <|| string "(?" *> internalOpts  <* char ')'

internalOpts :: Parser OptionSetting
internalOpts = do
  (onOpts, offOpts) <- optionsOnOff
  modify (applyOptions onOpts offOpts)
  pure (InternalOpts onOpts offOpts)

optionsOnOff :: Parser ([InternalOpt], [InternalOpt])
optionsOnOff
  =   (,)   <$> (many internalOpt <* char '-') <*> many internalOpt
  <|| (,[]) <$> many internalOpt

startOpt :: Parser StartOpt
startOpt
  =   LimitDepth <$ string "LIMIT_DEPTH="     <*> natural
  <|| LimitDepth <$ string "LIMIT_RECURSION=" <*> natural
  <|| LimitHeap  <$ string "LIMIT_HEAP="      <*> natural
  <|| LimitMatch <$ string "LIMIT_MATCH="     <*> natural
  <|| NotemptyAtstart <$ string "NOTEMPTY_ATSTART"
  <|| Notempty        <$ string "NOTEMPTY"
  <|| NoAutoPossess   <$ string "NO_AUTO_POSSESS"
  <|| NoDotstarAnchor <$ string "NO_DOTSTAR_ANCHOR"
  <|| NoJit      <$ string "NO_JIT"
  <|| NoStartOpt <$ string "NO_START_OPT"
  <|| Utf32 <$ string "UTF32"
  <|| Utf16 <$ string "UTF16"
  <|| Utf8  <$ string "UTF8"
  <|| Utf <$ string "UTF"
  <|| Ucp <$ string "UCP"
  -- Newline conventions:
  <|| CR <$ string "CR"
  <|| LF <$ string "LF"
  <|| CRLF <$ string "CRLF"
  <|| ANYCRLF <$ string "ANYCRLF"
  <|| ANY <$ string "ANY"
  <|| NUL <$ string "NUL"
  -- What \R matches:
  <|| BsrAnycrlf <$ string "BSR_ANYCRLF"
  <|| BsrUnicode <$ string "BSR_UNICODE"

internalOpt :: Parser InternalOpt
internalOpt
  =   CaseLess           <$ string  "i"
  <|| AllowDupGrp        <$ string  "J"
  <|| Multiline          <$ string  "m"
  <|| NoAutoCapture      <$ string  "n"
  <|| CaseLessNoMixAscii <$ string  "r"
  <|| SingleLine         <$ string  "s"
  <|| Ungreedy           <$ string  "U"
  <|| IngoreWSClasses    <$ string "xx"
  <|| IngoreWS           <$ string  "x"
  <|| UCPAsciiD          <$ string "aD"
  <|| UCPAsciiS          <$ string "aS"
  <|| UCPAsciiW          <$ string "aW"
  <|| UCPAsciiPosix      <$ string "aP"
  <|| UCPAsciiPosixD     <$ string "aT"
  <|| AllAscii           <$ string  "a"
  <|| UnsetImnrsx        <$ string  "^"


--                            Lookaround

lookAround :: Parser Re
lookAround
  =   Look Ahead Pos <$
       oneStr ["(?=", "(*pla:", "(*positive_lookahead:"]
      <*> alt <* char ')'
  <|| Look Ahead Neg <$
       oneStr ["(?!", "(*nla:", "(*negative_lookahead:"]
        <*> alt <* char ')'
  <|| Look Behind Pos <$
       oneStr ["(?<=", "(*plb:", "(*positive_lookbehind:"]
        <*> alt <* char ')'
  <|| Look Behind Neg <$
       oneStr ["(?<!", "(*nlb:","(*negative_lookbehind:"]
        <*> alt <* char ')'
  <|| Look Ahead NonAtomicPos <$
       oneStr ["(?*", "(*napla:", "(*non_atomic_positive_lookahead:"]
        <*> alt <* char ')'
  <|| Look Behind NonAtomicPos <$
       oneStr ["(?<*", "(*naplb:", "(*non_atomic_positive_lookbehind:"]
        <*> alt <* char ')'


--               Backreferences and subroutine calls

backRef :: Parser BackReference
backRef
  =   ByName   <$> refByName
  <|| Relative <$> refRelative
  <|| ByNumber <$> refByNumber

refByName :: Parser String
refByName
  =   string "(?P=" *> groupName <* char  ')'
  <|| string "\\k{" *> groupName <* char  '}'
  <|| string "\\g{" *> groupName <* char  '}'
  <|| string "\\k'" *> groupName <* char '\''
  <|| string "\\k<" *> groupName <* char  '>'

refRelative :: Parser Int
refRelative
  =   string "\\g{+" *> positive <* char '}'
  <|| negate <$> (string "\\g{-" *> positive <* char '}')
  <|| string "\\g+" *> positive
  <|| negate <$> (string "\\g-"  *> positive)

refByNumber :: Parser Int
refByNumber
  =   string  "\\g{" *> positive <* char '}'
  <|| string  "\\g"  *> positive
  -- \\[1-9][0-9]{0,2} is handled by OctOrBackRef

-- Subroutine references (possibly recursive)
subCall :: Parser SubroutineCall
subCall
  =   (Recurse <$ string "(?R)" )
  <|| CallAbs <$>
       (string "(?"    *> natural <* char ')'
        <||
        string "\\g<"  *> natural <* char '>'
        <||
        string "\\g'"  *> natural <* char '\'')
  <|| CallRel <$>
       (string "(?+"   *> natural <* char ')'
        <||
        string "\\g<+" *> natural <* char '>'
        <||
        string "\\g'+" *> natural <* char '\'')
  <|| CallRel . negate <$>
       (string "(?-"   *> natural <* char ')'
        <||
        string "\\g<-" *> natural <* char '>'
        <||
        string "\\g'-" *> natural <* char '\'')
  <|| CallName <$>
       (string "(?&" *> groupName <* char ')'
        <||
        string "(?P>" *> groupName <* char ')'
        <||
        string "\\g<" *> groupName <* char '>'
        <||
        string "\\g'" *> groupName <* char '\'')


--                           Conditionals

conditional :: Parser Conditional
conditional
  -- sequencing is the category below alt, so we don't get '|' wrong:
  =   CondYesNo <$>
       (string "(?(" *> condition <* char ')') <*>
       sequencing <*> (char '|' *> sequencing <* char ')')
  <|| CondYes <$>
       (string "(?(" *> condition <* char ')') <*> (sequencing <* char ')')

condition :: Parser Condition
condition
  =   AbsRef <$> positive
  <|| RelRef <$> (char '+' *> positive)
  <|| RelRef . negate <$> (char '-' *> positive)
  <|| RecNameGrp <$> (string "R&" *> groupName)
  <|| RecNumGrp <$> (char 'R' *> positive)
  -- From PCRE2 man page: Note the ambiguity of (?(R) and (?(Rn) which
  -- might be named reference conditions or recursion tests. Such a
  -- condition is interpreted as a reference condition if the relevant
  -- named group exists.
  <|| Rec <$ char 'R'
  <|| DefGrp <$ string "DEFINE"
  <|| (\relSymb major dot minor ->
          Version (relSymb ++ major ++ dot ++ minor)) <$>
       (string "VERSION" *> string ">=" <|| string "=") <*>
       many1 digit <*> option "" (string ".") <*> many digit
  <||  NamedRef <$>
       (char '<' *> groupName <* char '>' <||
        char '\'' *> groupName <* char '\'' <||
        groupName)
  <|| (\mCallout e -> Assert mCallout Ahead Pos e) <$>
       option Nothing (Just <$> (string "?" *> calloutBody <* string ")("))
       <*>
       (string "?="  *> alt)
  <|| (\mCallout e -> Assert mCallout Ahead Neg e) <$>
       option Nothing (Just <$> (string "?" *> calloutBody <* string ")("))
       <*>
       (string "?!"  *> alt)
  <|| (\mCallout e -> Assert mCallout Behind Pos e) <$>
       option Nothing (Just <$> (string "?" *> calloutBody <* string ")("))
        <*>
       (string "?<="  *> alt)
  <|| (\mCallout e -> Assert mCallout Behind Neg e) <$>
       option Nothing (Just <$> (string "?" *> calloutBody <* string ")("))
        <*>
        (string "?<!"  *> alt)


--                           Script runs

--  (*script_run:...)           ) script run, can be backtracked into
--  (*sr:...)                   )
--  (*atomic_script_run:...)    ) atomic script run
--  (*asr:...)                  )

scriptRun :: Parser Re
scriptRun
  =   ScriptRun NonAtomic <$
       (string "(*script_run:" <|| string "(*sr:") <*> alt <* char ')'
  <|| ScriptRun Atomic <$
       (string "(*atomic_script_run:" <|| string "(*asr:") <*> alt <* char ')'


--                       Backtracking control

backtrackControl :: Parser BacktrackControl
backtrackControl
  =   Accept <$> (string "(*ACCEPT" *> optName <* char ')')
  <|| Fail   <$> ((string "(*FAIL" <|| string "(*F") *> optName <* char ')')
  <|| MarkName <$>
       ((string "(*MARK:" <|| string "(*:") *> many1 nameChar <* char ')')
  <|| Commit <$> (string "(*COMMIT" *> optName <* char ')')
  <|| Prune <$> (string "(*PRUNE" *> optName <* char ')')
  <|| Skip <$> (string "(*SKIP" *> optName <* char ')')
  <|| Then <$> (string "(*THEN" *> optName <* char ')')
  where optName = option Nothing (Just <$> (char ':' *> many1 nameChar))
                  <||> (Nothing <$ char ':')
        nameChar = satisfy (/=')')


--                             Callouts

callout :: Parser Callout
callout
  = string "(?" *> calloutBody <* string ")"

calloutBody :: Parser Callout
calloutBody
  =   (CalloutN <$> (string "C" *> natural)) -- (?Cn) with numerical data n
  <|| (CalloutS <$> (string "C" *> coutStr)) -- (?C"text") with string data
  <|| (Callout  <$  string "C")              -- (?C) (assumed number 0)

-- delimiters: ` ' " ^ % # $ and { }
coutStr :: Parser String
coutStr
  =   coutStr_ "`"   "`"
  <|| coutStr_ "'"   "'"
  <|| coutStr_ "\"" "\""
  <|| coutStr_ "^"   "^"
  <|| coutStr_ "%"   "%"
  <|| coutStr_ "$"   "$"
  <|| coutStr_ "{"   "}"

-- The close delimiter is escaped by doubling it,
-- e.g. (?C{te{x}}t}) to escape the '}'.
coutStr_ :: String -> String -> Parser String
coutStr_ open close
  = (\ss s -> concatMap (++ close) ss ++ s) <$>
    (string open *> many (getUntil closeTwice))
      <*> option "" (getUntil close)
  where closeTwice = close ++ close


--                             Literals

literal :: Parser Re
literal = Lit <$> do
  s <- get
  let env = head s
      x = ignoreWS env
      xx = ignoreWSClasses env
  if x || xx
    then satisfy -- # and ascii whitespace are now special
         (\c -> c/= '#' &&
                not (isAsciiWhiteSpace c) &&
                c `notElem` topLevelSpecials)
    else nonSpecial topLevelSpecials

nonSpecial :: [Char] -> Parser Char
nonSpecial specials = satisfy (not . flip elem specials)

-- Needs to be escaped on top level (outside character classes):
topLevelSpecials :: [Char]
topLevelSpecials = ['^', '\\', '|', '(', ')', '[', '$', '+', '*', '?', '.']


------------------------------------------------------------------------------
--                 Lifted ReadP parser combinators

-- For efficiency, it is crucial that this operator discards its
-- second argument if the first succeeds: for instance,
-- examples/charclass-hex2-mixed-ranges.txt parses about 2300 times
-- slower using only +++ (or <|> for that sake)! This has to do with
-- the ambiguous grammars, e.g., \x, \xd, \xdd are all valid. This
-- implies also that the order of the alternatives does matter.
infixl 3 <||
(<||) :: Parser a -> Parser a -> Parser a
p1 <|| p2 = StateT $ \s ->
  runStateT p1 s <++ runStateT p2 s
  where (<++) = (R.<++)

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = StateT $ \s ->
  runStateT p1 s +++ runStateT p2 s
  where (+++) = (R.+++)

-- to distinguich it from State's get, we name it anyChar
anyChar :: Parser Char
anyChar = lift R.get

string :: String -> Parser String
string = lift . R.string

char :: Char -> Parser Char
char = lift . R.char

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = scan
  where
    scan = (end >> return []) <|| (:) <$> p <*> scan

count :: Int -> Parser a -> Parser [a]
count n p = lift (R.count n (evalStateT p []))

satisfy :: (Char -> Bool) -> Parser Char
satisfy = lift . R.satisfy

munch :: (Char -> Bool) -> Parser String
munch = lift . R.munch

option :: a -> Parser a -> Parser a
option x p = StateT $ \s ->
  runStateT p s R.+++ return (x, s)

skipSpaces :: Parser ()
skipSpaces = lift R.skipSpaces

many1 :: Parser a -> Parser [a]
many1 p = StateT $ \s -> do
  (a, s') <- runStateT p s
  (as, finalState) <- runStateT (many p) s'
  return (a : as, finalState)

many :: Parser a -> Parser [a]
many p = StateT $ \s -> return ([], s) R.+++ runStateT (many1 p) s

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = StateT $ \s -> do
  (firstItem, s1) <- runStateT p s
  (restItems, finalState) <- runStateT (many (sep >> p)) s1
  return (firstItem : restItems, finalState)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = StateT $ \s ->
  runStateT (return []) s R.+++ runStateT (sepBy1 p sep) s

pfail :: Parser a
pfail = lift R.pfail

eof :: Parser ()
eof = lift R.eof


------------------------------------------------------------------------------
--                          Parser helpers

printableAscii :: Parser Char
printableAscii = satisfy (\c -> isAscii c && isPrint c)

nonAlphanumAscii :: Parser Char
nonAlphanumAscii = satisfy (\c -> not (isAlphaNum c && isAscii c))

asciiWhiteSpace :: Parser Char
asciiWhiteSpace = satisfy isAsciiWhiteSpace

isAsciiWhiteSpace c = isSpace c && isAscii c

groupNameChar :: Bool -> Parser Char
groupNameChar isFirst
  = satisfy (\c -> not isFirst && isDigit c || isLetter c || c == '_')

-- see resolveOctOrBackrefs
octOrBackRefDigits :: Parser String
octOrBackRefDigits = (:) <$> posDigit <*> munch isDigit

-- posDigit is a parser for a positive digit [1-9]
posDigit :: Parser Char
posDigit = satisfy (\c -> '1' <= c && c <= '9')

-- digit is a parser for a digit [0-9]
digit :: Parser Char
digit = satisfy isDigit

-- Parses lo, lo+1, ..., hi decimal digits, trying the longest match first
digits :: Int -> Int -> Parser String
digits lo hi | lo == hi = count lo digit
             | lo  < hi = count hi digit <|| digits lo (hi - 1)
             | otherwise = error $ "invalid range: " ++ show (lo, hi)

octDigit :: Parser Char
octDigit = satisfy (`elem` ['0'..'7'])

-- Parses lo, lo+1, ..., hi octal digits, trying the longest match first
octDigits :: Int -> Int -> Parser String
octDigits lo hi | lo == hi = count lo octDigit
                | lo  < hi = count hi octDigit <|| octDigits lo (hi - 1)
                | otherwise = error $ "invalid range: " ++ show (lo, hi)

-- hexDigit is a parser for a single hex digit
hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

-- Parses lo, lo+1, ..., hi hex digits, trying the longest match first
hexDigits :: Int -> Int -> Parser String
hexDigits lo hi | lo == hi = count lo hexDigit
                | lo  < hi = count hi hexDigit <|| hexDigits lo (hi - 1)
                | otherwise = error $ "invalid range: " ++ show (lo, hi)

natural :: Parser Int
natural = read <$> many1 digit

positive :: Parser Int
positive = postCheck (> 0) natural

-- like string, but strips away empty quotes if they occur between the
-- characters
stripNullQuotes :: String -> Parser String
stripNullQuotes [] = pure ""
stripNullQuotes (c : cs) = (:) <$>
  char c <*> (optEmptyQuoting *> stripNullQuotes cs)

-- Get all characters cs until s appears a substring, consume s and
-- return cs
getUntil :: String -> Parser String
getUntil s = manyTill anyChar (string s)

-- Parse with p, but succeed and comsume input only if isOk p holds.
postCheck :: (a -> Bool) -> Parser a -> Parser a
postCheck isOk p = do
  result <- p
  if isOk result
    then pure result
    else pfail

oneStr :: [String] -> Parser String
oneStr = oneOf . map string

oneOf :: [Parser a] -> Parser a
oneOf = foldr1 (<||)

------------------------------------------------------------------------------
--                      Syntax rebuild helpers

-- Traverse the expression, keeping track of the highest group we've
-- encountered so far, from left to right, in an in-order traversal
-- manner. The OctOrBackRef ds ocurrences are then replaced by either
-- BackRef or EOct followed by possible trailing non octal digit
-- literals, dependent on whether the highest capture group count i so
-- far: length ds == 1 || i > read ds -> BackRef; otherwise Escape EOct.
resolveOctOrBackrefs :: Re -> Re
resolveOctOrBackrefs e = snd $ resolveOBR 0 e

resolveOBR :: Int -> Re -> (Int, Re)
resolveOBR i (Group Capture e) =
  Group (CaptureN i') <$> resolveOBR i' e
  where i' = i + 1
resolveOBR i (Group NonCaptureReset (Alt es)) =
  let ies = map (resolveOBR i) es -- all alternatives start with i
      (is, es') = unzip ies
  in (maximum is, Group NonCaptureReset (Alt es'))
resolveOBR i (OctOrBackRef ds)
  -- The octOrBackRefDigits parser consumes as many digits ds as
  -- available. If the decimal number n, that ds encode, is above the
  -- highest capture group to the left, and ds has a non-empty octal
  -- prefix, ds is considered as an octal escape of length at most 3,
  -- followed by the rest of the digits as literals. In all other
  -- cases ds is considered a backreference, and we don't considere
  -- here whether the reference is too large or not. The octal
  -- reference may also be above 0o377, and this is not allowed in
  -- non-UTF mode. We don't take this into account in the parser.
  | n > i && length ds /= 1 && not (null octs) =
      (i, Seq $ Esc (fromOctStr octs) : map Lit rest)
  | otherwise =
      (i, BackRef $ ByNumber n)
  where
    n = read ds :: Int
    (octs, rest) = span isOctDigit ds
resolveOBR i (Alt es) =
    let (i', es') = resolveOBRs i es
    in (i', Alt es')
resolveOBR i (Seq es) =
    let (i', es') = resolveOBRs i es
    in (i', Seq es')
resolveOBR i (Quant m q e) =
  Quant m q <$> resolveOBR i e
resolveOBR i (Group g e) =
  Group g <$> resolveOBR i e
resolveOBR i (ScriptRun m e) =
  ScriptRun m <$> resolveOBR i e
resolveOBR i (Look d m e) = Look d m <$> resolveOBR i e
resolveOBR i (Cond c) = Cond <$> resolveOBRCondl i c
resolveOBR i e = (i,e)

resolveOBRs :: Int -> [Re] -> (Int, [Re])
resolveOBRs i [] = (i, [])
resolveOBRs i (e:es) =
    let (i' , e' ) = resolveOBR i e
        (i'', es') = resolveOBRs i' es
    in (i'', e' : es')

resolveOBRCondl i (CondYes c e) =
    let (i' , c') = resolveOBRCond i c
    in CondYes c' <$> resolveOBR i' e
resolveOBRCondl i (CondYesNo c e1 e2) =
    let (i' , c') = resolveOBRCond i c
        (i'', e1') = resolveOBR i' e1
    in CondYesNo c' e1' <$> resolveOBR i'' e2

resolveOBRCond i (Assert mc dir m e) = Assert mc dir m <$> resolveOBR i e
resolveOBRCond i cond = (i, cond)

------------------------------------------------------------------------------
--                        State manipultaion

localST :: Parser a -> Parser a
localST p = pushEnv *> p <* popEnv
  where
    pushEnv = modify (\envs -> head envs : envs)
    popEnv  = modify tail

applyOptions :: [InternalOpt] -> [InternalOpt] -> [Env] -> [Env]
applyOptions _ _ [] =
  error "empty state"
applyOptions onOpts offOpts (env : rest) =
  env { ignoreWS        = update ignoreWS        IngoreWS,
        ignoreWSClasses = update ignoreWSClasses IngoreWSClasses,
        noAutoCapture   = update noAutoCapture   NoAutoCapture
       } : env : rest
  where
    update f opt = (f env || (opt `elem` onOpts)) && (opt `notElem` offOpts)


------------------------------------------------------------------------------
--                           Misc helpers

-- Helper to create a an association list of property names generated
-- from pcre2test -LP and -LS.
flatNamesAndCons :: [([a], b)] -> [(a, b)]
flatNamesAndCons namesAndCons = concat
  [[(nameOrAbbrev, constructor)
   | nameOrAbbrev <- nameOrAbbrevs]
  | (nameOrAbbrevs, constructor) <- namesAndCons]

charclassCase :: ([CharclassItem] -> a) -> ([CharclassItem] -> a) -> Charclass -> a
charclassCase fOneof fNoneof charclass =
  case charclass of
    Oneof  items -> fOneof  items
    Noneof items -> fNoneof items

fromOctStr = chr . fst . head . readOct . ("0" <>)
fromHexStr = chr . fst . head . readHex . ("0" <>)
