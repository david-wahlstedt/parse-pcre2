{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AbsPCRE where

-- imports of auto generated code
import AbsBinProp
import AbsScriptName

--                         Basic constructs

data TopLevelRe = TopLevelRe [GlobalOption] Re
  deriving Show

data Re
  = Alt [Re]                           -- alternation: |
  | Seq [Re]                           -- sequencing: juxtaposition
  | Lit Char                           -- character literals
  | Quoting String                     -- \Q <anything without \E> \E
  | Ctrl Char                          -- Ctrl-C, etc
  | Esc  Char                          -- Escaped characters
  | Chartype Chartype                  -- character types and properties
  | Charclass Charclass                -- character classes
  | Quant QuantifierMode Quantifier Re -- quantifiers
  | Anchor Anchor                      -- anchors and simple assertions
  | SetStartOfMatch                    -- \K, reported match point setting
  | Group GroupType Re                 -- capture and non-capture groups
  | OptSet [ScopedOption] [ScopedOption] -- scoped option settings
  | Look Direction LookaroundMode Re   -- lookaround
  | SubScan [GroupId] Re               -- substring scan assertion
  | ScriptRun ScriptRunMode Re         -- script runs
  | BackRef GroupId                    -- back references
  | SubCall SubroutineCall             -- subroutine calls
  | Cond Conditional                   -- conditional patterns
  | Backtrack BacktrackControl         -- backtracking control
  | COut Callout                       -- callouts
  deriving Show


--                         Character types

data Chartype
  = CTAny          -- .   any character except newline
  | CTCodeUnit     -- \C  one code unit, even in UTF mode (best avoided)
  | CTDigit        -- \d  decimal digit
  | CTNDigit       -- \D  character that is not a decimal digit
  | CTHSpace       -- \h  horizontal white space character
  | CTNHSpace      -- \H  character that is not a horizontal white space
  | CTNNewLine     -- \N  character that is not a newline
  | CTNewLineSeq   -- \R  newline sequence
  | CTSpace        -- \s  white space character
  | CTNSpace       -- \S  character that is not a white space character
  | CTVSpace       -- \v  vertical white space character
  | CTNVSpace      -- \V  character that is not a vertical white space
  | CTWordChar     -- \w  "word" character
  | CTNWordChar    -- \W  "non-word" character
  | CTUCluster     -- \X  Unicode extended grapheme cluster
  | CTProp  CTProperty -- \p{xx}
  | CTNProp CTProperty -- \P{xx}
  deriving Show

data CTProperty -- \p{xx} or \P{xx}
  = CTUAny -- \p{Any}
  | CTGenProp CTGeneralProperty
  | CTSpecProp CTSpecialProperty
  | CTBinProp BinProp -- Auto generated in AbsBinProp
  | CTScript CTScriptMatching
  | CTBidi CTBidiClass
  deriving Show

data CTGeneralProperty
  = Other       OtherProp       --  C
  | Letter      LetterProp      --  L
  | Mark        MarkProp        --  M
  | Number      NumberProp      --  N
  | Punctuation PunctuationProp --  P
  | Symbol      SymbolProp      --  S
  | Separator   SeparatorProp   --  Z
  deriving Show

data OtherProp       -- C
  = C  -- Other
  | Cc -- Control
  | Cf -- Format
  | Cn -- Unassigned
  | Co -- Private use
  | Cs -- Surrogate
 deriving Show

data LetterProp      --  L
  = L    -- Letter
  | Ll   -- Lower case letter
  | Lm   -- Modifier letter
  | Lo   -- Other letter
  | Lt   -- Title case letter
  | Lu   -- Upper case letter
  | Lc   -- Ll, Lu, or Lt
  | Llut -- "L&": Ll, Lu, or Lt
 deriving Show

data MarkProp        --  M
 = M  -- Mark
 | Mc -- Spacing mark
 | Me -- Enclosing mark
 | Mn -- Non-spacing mark
 deriving Show

data NumberProp      --  N
 = N  -- Number
 | Nd -- Decimal number
 | Nl -- Letter number
 | No -- Other number
 deriving Show

data PunctuationProp --  P
 = P  -- Punctuation
 | Pc -- Connector punctuation
 | Pd -- Dash punctuation
 | Pe -- Close punctuation
 | Pf -- Final punctuation
 | Pi -- Initial punctuation
 | Po -- Other punctuation
 | Ps -- Open punctuation
 deriving Show

data SymbolProp      --  S
 = S  -- Symbol
 | Sc -- Currency symbol
 | Sk -- Modifier symbol
 | Sm -- Mathematical symbol
 | So -- Other symbol
 deriving Show

data SeparatorProp   --  X
 = Z  -- Separator
 | Zl -- Line separator
 | Zp -- Paragraph separator
 | Zs -- Space separator
 deriving Show

data CTSpecialProperty
  = Xan -- Alphanumeric: union of properties L and N
  | Xps -- POSIX space: property Z or tab, NL, VT, FF, CR
  | Xsp -- Perl space: property Z or tab, NL, VT, FF, CR
  | Xuc -- Universally-named character: one that can be
        -- represented by a Universal Character Name
  | Xwd -- Perl word: property Xan or underscore
  deriving Show

data CTScriptMatching
  = Basic ScriptName      -- \p{sc:name} \p{script:name}
  | Extensions ScriptName -- \p{name} \p{scx:name} \p{script_extensions:name}
  deriving Show

data CTBidiClass
  = BidiAL  -- Arabic letter
  | BidiAN  -- Arabic number
  | BidiB   -- paragraph separator
  | BidiBN  -- boundary neutral
  | BidiCS  -- common separator
  | BidiEN  -- European number
  | BidiES  -- European separator
  | BidiET  -- European terminator
  | BidiFSI -- first strong isolate
  | BidiL   -- left-to-right
  | BidiLRE -- left-to-right embedding
  | BidiLRI -- left-to-right isolate
  | BidiLRO -- left-to-right override
  | BidiNSM -- non-spacing mark
  | BidiON  -- other neutral
  | BidiPDF -- pop directional format
  | BidiPDI -- pop directional isolate
  | BidiR   -- right-to-left
  | BidiRLE -- right-to-left embedding
  | BidiRLI -- right-to-left isolate
  | BidiRLO -- right-to-left override
  | BidiS   -- segment separator
  | BidiWS  -- which space
  deriving Show


--                         Character classes

data Charclass
  = Oneof  [CharclassItem] -- [  ]
  | Noneof [CharclassItem] -- [^ ]
  deriving Show

data CharclassItem
  = CCAtom CharclassAtom
  | Range CharclassAtom CharclassAtom
  deriving Show

data CharclassAtom
  = CCLit Char
  | CCEsc Char
  | CCCtrl Char
  | CCBackspace -- \b
  | CCQuoting String
  -- When a non-empty quoted string appears in a range before '-', the
  -- last character in the string is the lower range bound , and if
  -- the string appears after the '-', the first character in the
  -- string is the lower bound of the range.
  | CCCharType Chartype
  | PosixSet PosixSet
  deriving Show

data PosixSet
  = PosSet SetName -- [:SetName:]
  | NegSet SetName -- [:^SetName:]
  deriving Show

data SetName
  = SetAlnum  -- alphanumeric
  | SetAlpha  -- alphabetic
  | SetAscii  -- 0-127
  | SetBlank  -- space or tab
  | SetCntrl  -- control character
  | SetDigit  -- decimal digit
  | SetGraph  -- printing, excluding space
  | SetLower  -- lower case letter
  | SetPrint  -- printing, including space
  | SetPunct  -- printing, excluding alphanumeric
  | SetSpace  -- white space
  | SetUpper  -- upper case letter
  | SetWord   -- same as \w
  | SetXdigit -- hexadecimal digit
  deriving Show


--                           Quantifiers

data Quantifier
  = Option -- ?
  | Many   -- *
  | Many1  -- +
  | Rep Int -- {N}
  | RepMin Int -- {N,}
  | RepMax Int -- {,N} supported from PCRE2 10.43
  | RepMinMax Int Int -- {M,N}
  deriving Show

data QuantifierMode
  = Greedy
  | Possessive
  | Lazy
  deriving Show


--                         Anchors

data Anchor
  = StartOfLine          -- ^
  | EndOfLine            -- $
  | StartOfSubject       -- \A
  | EndOfSubject         -- \Z
  | EndOfSubjectAbsolute -- \z
  | FirstMatchingPos     -- \G
  | WordBoundary         -- \b
  | NonWordBoundary      -- \B
  | StartOfWord          -- [[:<:]] as \b(?=\w)
  | EndOfWord            -- [[:>:]] as \b(?<=\w)
  deriving Show


--                         Groups

data GroupType
  = Capture Int                                  -- (...) numbered
  | Unnumbered                                   -- (...) unnumbered: (?n) set
  | NonCapture                                   -- (?:...)
  | NonCaptureOpts [ScopedOption] [ScopedOption] -- (?opts:...)
  | NonCaptureReset                              -- (?|...)
  | AtomicNonCapture                             -- (?>...)
  | NamedCapture String                          -- (?<name>...)
  deriving Show


--                          Option setting

data GlobalOption
  = LimitDepth Int  -- (*LIMIT_DEPTH=d)
  | LimitHeap  Int  -- (*LIMIT_HEAP=d)
  | LimitMatch Int  -- (*LIMIT_MATCH=d)
  | Notempty        -- (*NOTEMPTY)
  | NotemptyAtstart -- (*NOTEMPTY_ATSTART)
  | NoAutoPossess   -- (*NO_AUTO_POSSESS)
  | NoDotstarAnchor -- (*NO_DOTSTAR_ANCHOR)
  | NoJit           -- (*NO_JIT)
  | NoStartOpt      -- (*NO_START_OPT)
  | Utf             -- (*UTF)
  | Utf8            -- (*UTF) -- backwards compat
  | Utf16           -- (*UTF) -- backwards compat
  | Utf32           -- (*UTF) -- backwards compat
  | Ucp             -- (*UCP)
  -- Newline conventions:
  | CR              -- (*CR)           carriage return only
  | LF              -- (*LF)           linefeed only
  | CRLF            -- (*CRLF)         carriage return followed by linefeed
  | ANYCRLF         -- (*ANYCRLF)      all three of the above
  | ANY             -- (*ANY)          any Unicode newline sequence
  | NUL             -- (*NUL)          the NUL character (binary zero)
  -- What \R matches:
  | BsrAnycrlf      -- (*BSR_ANYCRLF)  CR, LF, or CRLF
  | BsrUnicode      -- (*BSR_UNICODE)  any Unicode newline sequence
  deriving Show

data ScopedOption
  = AllAscii       -- (?a)  all ASCII options
  | UCPAsciiD      -- (?aD) restrict \d to ASCII in UCP mode
  | UCPAsciiS      -- (?aS) restrict \s to ASCII in UCP mode
  | UCPAsciiW      -- (?aW) restrict \w to ASCII in UCP mode
  | UCPAsciiPosix  -- (?aP) restrict all POSIX classes to ASCII in UCP mode
  | UCPAsciiPosixD -- (?aT) restrict POSIX digit classes to ASCII in UCP mode
  | CaseLess           -- (?i) caseless
  | AllowDupGrp        -- (?J) allow duplicate named groups
  | Multiline          -- (?m) multiline
  | NoAutoCapture      -- (?n) no auto capture
  | CaseLessNoMixAscii -- (?r) restrict caseless to either ASCII or non-ASCII
  | SingleLine         -- (?s) single line (dotall)
  | Ungreedy           -- (?U) default ungreedy (lazy)
  | IgnoreWS           -- (?x) ignore white space except in classes or \Q...\E
  | IgnoreWSClasses    -- (?xx) as (?x) but also ignore space and tab in class
  | UnsetImnrsx        -- (?^) unset imnrsx options
  deriving (Eq, Show)


--                            Lookaround

data LookaroundMode
  = Pos
  | Neg
  | NonAtomicPos
  deriving Show

data Direction
  = Ahead
  | Behind
  deriving Show


--                           Script runs

--  (*script_run:...)           ) script run, can be backtracked into
--  (*sr:...)                   )
--  (*atomic_script_run:...)    ) atomic script run
--  (*asr:...)                  )

data ScriptRunMode
  = Atomic
  | NonAtomic
  deriving Show


--               Backreferences and subroutine calls

data GroupId
  = ByNumber Int  -- \n, \gn, \g{n}
  | Relative Int  -- \g+n, \g-n, \g{+n}, \g{-n}
  | ByName String
  deriving Show

data SubroutineCall
  = Recurse     --     (?R)        recurse whole pattern
  | CallAbs Int --     (?n)        call subroutine by absolute number
  --                   \g<n>        (Oniguruma)
  --                   \g'n'        (Oniguruma)
  | CallRel Int --     (?+n)       call subroutine by relative number
  --                   (?-n)
  --                   \g<+n>       (PCRE2 extension)
  --                   \g'+n'       (PCRE2 extension)
  --                   \g<-n>       (PCRE2 extension)
  --                   \g'-n'       (PCRE2 extension)
  | CallName String -- (?&name)    call subroutine by name (Perl)
  --                   (?P>name)    (Python)
  --                   \g<name>     (Oniguruma)
  --                   \g'name'     (Oniguruma)
  deriving Show


--                           Conditionals

data Conditional
  = CondYes   Condition Re
  | CondYesNo Condition Re Re
  deriving Show

data Condition
  = AbsRef Int                           -- absolute reference condition
  | RelRef Int                           -- relative reference condition
  | NamedRef String                      -- named reference condition
  | Rec                                  -- overall recursion condition
  | RecNumGrp Int                        -- specific numbered group recursion
  | RecNameGrp String                    -- specific named group recursion
  | DefGrp                               -- define groups for reference
  | Version String                       -- test PCRE2 version
  | Assert (Maybe Callout) Direction LookaroundMode Re   -- assertion condition
  deriving Show

{- Assert is one of
Positive Lookahead: (?=...)
Negative Lookahead: (?!...)
Positive Lookbehind: (?<=...)
Negative Lookbehind: (?<!...)

with callout
(?(?C9)(?=)abc|def)
-}


--                       Backtracking control

data BacktrackControl
  = Accept (Maybe String) -- (*ACCEPT) or (*ACCEPT:NAME)
  | Fail (Maybe String)   -- (*FAIL)   or (*FAIL:NAME)
  | MarkName String       -- (*MARK:NAME)
  | Commit (Maybe String) -- (*COMMIT) or (*COMMIT:NAME)
  | Prune (Maybe String)  -- (*PRUNE)  or (*PRUNE:NAME)
  | Skip (Maybe String)   -- (*SKIP)   or (*SKIP:NAME)
  | Then (Maybe String)   -- (*THEN)   or (*THEN:NAME)
  deriving Show

--                             Callouts

data Callout
  = Callout         -- (?C)       callout (assumed number 0)
  | CalloutN Int    -- (?Cn)      callout with numerical data n
  | CalloutS String -- (?C"text") callout with string data
  deriving Show
