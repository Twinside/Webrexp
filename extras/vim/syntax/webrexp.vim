" Vim syntax file
" Language:	    Webrexp
" Maintainer:	Vincent Berthoux <twinside@gmail.com>
" File Types:	.webrexp

"if version < 600
  "syntax clear
"elseif exists("b:current_syntax")
  "finish
"endif

syn match       webrexpOperator     "[*+-=.<>$!~]"
syn match       webrexpStr          "\"\([^\"]\|\\[\"nrt]\)*\""
syn match       webrexpIdent        "[a-zA-Z0-9_-]\+"
syn match       webrexpAttr         "@[a-zA-Z0-9_-]\+"
syn match       webrexpName         "#[a-zA-Z0-9_-]\+"
syn match       webrexpClass        "\.[a-zA-Z0-9_-]\+"
syn match       webrexpSep          ";"
syn match       webrexpComma        ","
syn match       webrexpComment      "--.*"

syn match       webrexpBracketError "]"
syn match       webrexpBraceError   "}"
syn match       webrexpParensError  ")"

" =================================================
" Specific syntax highlighting for actions
" =================================================
syn region      webrexp start="(" end=")"
        \ contains=webrexpOperator,webrexpStr,webrexpIdent,
                \webrexpAttr,webrexpName,webrexpClass,
                \webrexpSep,webrexpComment,webrexpSep,
                \webrexpAction,webrexpRange,webrexpRepeat,
                \webrexp
" =================================================
" Specific syntax highlighting for ranges
" =================================================
syn match       webrexpIndex        "[0-9]\+" contained
syn region      webrexpRange      start="#{" end="}"
                    \ contains=webrexpIndex,webrexpComma,webrexpComment

" =================================================
" Specific syntax highlighting for repetitions
" =================================================
syn region      webrexpRepeat     start="{" end="}"
                    \ contains=webrexpIndex,webrexpComma,webrexpComment

" =================================================
" Specific syntax highlighting for actions
" =================================================
syn match       webrexpActionNum    "[0-9]\+" contained

syn match     webrexpActionOp     ">=" contained
syn match     webrexpActionOp     "<=" contained
syn match     webrexpActionOp     "!=" contained
syn match     webrexpActionOp     "=~" contained
syn match     webrexpActionOp     "~=" contained
syn match     webrexpActionOp     "^=" contained
syn match     webrexpActionOp     "$=" contained
syn match     webrexpActionOp     "|=" contained
syn match     webrexpActionOp     "\&" contained
syn match     webrexpActionOp     "[*+.$-=<>=|]" contained
syn match     webrexpActionSep    ";"             contained
syn match     webrexpAttrAction   "[_a-zA-Z][a-zA-Z0-9_-]\+" contained

syn region    webrexpActionStr    start=+"+ end=+"+ skip=+\\"+ contained
                            \ contains=webrexpSpecialChar
syn match     webrexpSpecialChar  "\\t" contained
syn match     webrexpSpecialChar  "\\n" contained
syn match     webrexpSpecialChar  "\\r" contained
syn match     webrexpSpecialChar  "\\\"" contained

syn region      webrexpAction       start='\[' end='\]'
        \ contains=webrexpActionStr,webrexpActionOp,webrexpActionSep,
                 \webrexpAttrAction,webrexpComment

hi link webrexpBraceError   Error
hi link webrexpBracketError Error
hi link webrexpParensError  Error

hi link webrexpOperator     Operator
hi link webrexpStr          String
hi link webrexpIdent        Identifier   
hi link webrexpSep          Statement
hi link webrexpAttr         Constant
hi link webrexpClass        Structure
hi link webrexpName         Function
hi link webrexpIndex        Number
hi link webrexpComment      Comment

" Id filtering
hi link webrexpRange        Conditional

" Range
hi link webrexpRepeat       Repeat

" Actions
hi link webrexpActionNum    Number
hi link webrexpActionStr    String
hi link webrexpAttrAction   Constant
hi link webrexpActionOp     Operator
hi link webrexpActionSep    Keyword
hi link webrexpAction       Function
hi link webrexpSpecialChar  Special

let b:current_syntax = "webrexp"

" vim: ts=8

