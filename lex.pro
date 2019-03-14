% *************************************************************************
% =========================================================================
% Ada 2012 Compiler in Prolog
% =========================================================================
% *************************************************************************

% Authors: {Nicholas James Roberts (UK), David Botton, Anh Vo}.
%
% Version: Initial experimental.
%
% Copyright: See accompanying file "COPYRIGHT".





% #########################################################################
% =========================================================================
% Lexical Analysis Module
% =========================================================================
% #########################################################################

:- module(lex,[lexes_to/4]).




% =========================================================================
% RM Chapter 2: Lexical Elements
% =========================================================================

% It is assumed that the Prolog interpreter is capable of handling Unicode
% (16-bit) characters, which are equivalent (enough) to the BMP of ISO
% 10646.

% For the time being, however, get0(C) will be assumed to read an 8-bit
% character, which will be assumed to be in the Latin-1 (ISO 8859-1)
% character set. This may change in the future.



% -------------------------------------------------------------------------
% RM 2.1: Character Set
% -------------------------------------------------------------------------

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% character(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with any character which can legally be in an Ada program.
% [2.1(2)]

character(C) :- graphic_character(C).
character(C) :- format_effector(C).
character(C) :- other_control_function(C).

% EG: TRUE: character(10)
% EG: FAIL: character(-1)

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% graphic_character(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with a character which is a graphic_character.
% [2.1(3)]

graphic_character(C) :- identifier_letter(C).
graphic_character(C) :- digit(C).
graphic_character(C) :- space_character(C).
graphic_character(C) :- special_character(C).

% EG: TRUE: graphic_character(0'")
% EG: FAIL: graphic_character(10)

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% identifier_letter(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with a character which is an identifier_letter.
% [2.1(7)]

identifier_letter(C) :- upper_case_identifier_letter(C).
identifier_letter(C) :- lower_case_identifier_letter(C).

% EG: TRUE: identifier_letter(0'A)
% EG: FAIL: identifier_letter(0'@)

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% identifier_letter(C,U)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with a character which is an identifier_letter, and unifies U
% with either C if C is an upper_case_identifier_letter, or with the upper
% case equivalent of C if C is a lower_case_identifier_letter.

identifier_letter(C,C) :- upper_case_identifier_letter(C).
identifier_letter(C,U) :- lower_case_identifier_letter(C),
                          plus(U,0'a-0'A,C).

% EG: TRUE: identifier_letter(0'a,0'A)
% EG: FAIL: identifier_letter(0'b,0'b)

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% upper_case_letter_identifier(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with a character which is an upper_case_letter_identifier.
% [2.1(8)]

upper_case_identifier_letter(C) :- between(0'A,0'Z,C).

% EG: TRUE: upper_case_identifier_letter(0'A)
% EG: FAIL: upper_case_identifier_letter(0'a)

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% lower_case_letter_identifier(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with a character which is a lower_case_letter_identifier.
% [2.1(9)]

lower_case_identifier_letter(C) :- between(0'a,0'z,C).

% EG: TRUE: lower_case_identifier_letter(0'a)
% EG: FAIL: lower_case_identifier_letter(0'A)

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% digit(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with a character which is a digit. [2.1(10)]

digit(C) :- between(0'0,0'9,C).

% EG: TRUE: digit(0'0)
% EG: FAIL: digit(0'A)

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% digit(C,V)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with a character which is a digit, and V with the value of that
% digit (0 to 9).

digit(C,V) :- digit(C), plus(V,0'0,C).

% EG: TRUE: digit(0'2,2)
% EG: FAIL: digit(0'3,2)

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% space_character(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with the space character. [2.1(11)]

space_character(32).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% special_character(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies C with a character which is a special_character.
% [2.1(12)]

special_character(C) :-
    character(C),
	\+ control_character(C),
	\+ space_character(C),
	\+ identifier_letter(C),
	\+ digit(C).

% EG: special_character(0'") TRUE
% EG: special_character(0'A) FAILS

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% control_character(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Succeeds if C unifies with a character which is a control character.

control_character(C) :- between(0,31,C).
control_character(C) :- between(127,159,C).

% EG: control_character(10) TRUE
% EG: control_character(65) FAILS

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% format_effector(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Succeeds if C unifies with a character which is a format effector.
% [2.1(13)]

format_effector(9). % HT (horizontal tabulation)
format_effector(11). % VT (vertical tabulation)
format_effector(13). % CR (carriage return)
format_effector(10). % LF (line feed)
format_effector(12). % FF (form feed)

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% other_control_function(C)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Succeeds if C unifies with a character which is a control character,
% other than a format effector, that is allowed (by our implementation) to
% be put into a comment. At present, there is none, but a dummy clause is
% here to stop the debugger complaining. [2.1(14)]

other_control_function(_) :- fail.

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% <names of characters>
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% The following predicates are defined for two purposes: they allow a
% clear, unambiguous name to be used for characters that can vary in their
% appearance (according to different font designs); they provide for the
% definition of equivalent characters. [2.1(15)]

quotation_mark(0'").
number_sign(0'#).
ampersand(0'&).
apostrophe(0''). tick(0'').
left_parenthesis(0'().
right_parenthesis(0')).
asterisk(0'*). multiply(0'*).
plus_sign(0'+).
comma(0',).
hyphen_minus(0'-). minus(0'-).
full_stop(0'.). dot(0'.). point(0'.).
solidus(0'/). divide(0'/).
colon(0':).
semicolon(0';).
less_than_sign(0'<).
equals_sign(0'=).
greater_than_sign(0'>).
low_line(0'_). underline(0'_).
vertical_line(0'|).
left_square_bracket(0'[).
right_square_bracket(0']).
left_curly_bracket(0'{).
right_curly_bracket(0'}).



% -------------------------------------------------------------------------
% RM95 2.2: Lexical Elements, Separators, and Delimiters
% -------------------------------------------------------------------------

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Fatal Error Descriptors
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% - unrecognised_character(Char)
% - - Char: the offending character
% - assumed_to_be( Wrong, Right, PLE )
% - - Wrong: what the source text was
% - - Right: what we are assuming it to be
% - - PLE: the proper lexical element corresponding to Right

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Lexical Elements
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% - ignore( comment(Txt) )
% - - Txt: text of the comment (including leading "--")
% - lex( PLE )
% - - PLE: proper lexical element
% - fatal( Err, Txt, File, Page, Line, Col1, Col2 )
% - - Err: fatal error descriptor
% - - Txt: raw text from the source file corresponding to error

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Proper Lexical Elements
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% - delimiter(Del)
% - - Del: delimiter
% - 
% - - 
% - 
% - - 
% - 
% - - 
% - 
% - - 
% - 


% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% lexical_element( Lex, Txt, File, Page, Line, Col1, Col2 ) -->
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Lex is a lexical element in the location given by the other parameters.
%
% - Lex: lexical element
% - Txt: raw text from the source file that makes up the lexical element 
% - File: full path name of source text file
% - Page: page number in source text file
% - Line: line numer in page
% - Col1: column number, on line, of start of lexical element 
% - Col2: column number plus one, on line, of end of lexical element
%
% File, Page, Line, Col1 and the input text must be instantiated. [2.2(1)]

lexical_element( Lex, Txt, File, Page, Line, Col1, Col2, TxtIn, TxtOut )
:-
   lexical_element( Lex, Txt, TxtIn, TxtOut ),
   append( Txt, TxtOut, TxtIn ),
   length( Txt, Len ),
   plus( Col1, Len, Col2 ).
   
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% lexical_element(Lex) -->
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Lex is a lexical element. There are also pseudo-elements, that indicate
% errors. 
   
lexical_element(Lex,Txt) --> character_literal(Lex,Txt).
lexical_element(Lex,Txt) --> delimiter(Lex,Txt).
lexical_element(Lex,Txt) --> identifier(Lex,Txt).
lexical_element(Lex,Txt) --> reserved_word(Lex,Txt).
lexical_element(Lex,Txt) --> numeric_literal(Lex,Txt).
lexical_element(Lex,Txt) --> string_literal(Lex,Txt).

lexical_element( ignore( comment(Txt) ) ) --> comment(Txt), !.
% Comments produce the descriptor 'null', making it easy for other
% predicates to simply ignore comments. The cut disposes of comments
% swiftly.

lexical_element( 
   fatal( unrecognised_character(C), Txt, File, Page, Line, Col1, Col2 ), 
   Txt, File, Page, Line, Col1, Col2 )
--> 
   [C], { succ(Col1,Col2) }, !.

% EG: lexical_element(
%        lex( delimiter(less_than_or_equal) ),
%        "<=",
%        "D:\\Devel\\Ada\\Samples\\Foobar_1.ada",
%        1, 23, 14, 16,
%        "<=", "" ) TRUE

% EG: lexical_element(lex(delimiter(less_than_or_equal),loc(10,20),"<="),
%        fatal( assumed_to_be( "=<", "<=", delimiter(less_than_or_equal) ) ),
%        "=<",
%        "D:\\Devel\\Ada\\Samples\\Foobar_1.ada",
%        1, 23, 14, 16,
%        "=<", "" ) TRUE

% EG: lexical_element( _, "$%&", "Foobar_1.ada", 1, 23, 14, 16, "$%&", "" ) FAILS

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% end_of_line -->
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Parses a line termination sequence. Note that all other sequences of
% format effectors (other than HT) will cause an error. These are parsed by
% maximal munch. [2.2(2)]

end_of_line --> [10].    % LF alone
end_of_line --> [13,10]. % CRLF
end_of_line --> [13].    % CR alone
end_of_line --> [10,13]. % LFCR

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% separator -->
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Parses a character which is a separator (ignoring end-of-line, which is
% dealt with separately). [2.2(3-6)]

separator([C|CT],CT) :- space_character(C).
separator --> [9]. % HT

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% lexical_sequence( LexL, File, Page1, Line1, Col1, Page2, Line2, Col2 ) -->
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Parses a piece of text, and unifies LexL with a list of lexical element
% descriptors. Page1/Line1/Col1 point to the start location of the text 
% passed in, and Page2/Line2/Col2 to the start location for the remainder 
% text. 
%
% File, Page1, Line1, Col1, and the input text must be instantiated.
%
% - LexL:
% - File:
% - Page1:
% - Line1:
% - Col1:
% - Page2:
% - Line2:
% - Col2:

lexical_sequence( [], Loc, Loc, [], "", "" ).

lexical_sequence( LexL, File, Page, Line, Col1, Page, Line, Col2 ) -->
    separator, !,
    { succ(Col1,Col2) },
    lexical_sequence( LexL, File, Page, Line, Col1, Page, Line, Col2 ).

lexical_sequence( LexL, File, Page, Line1, Col1, Page, Line2, 1 ) -->
    end_of_line, !,
    { succ(Line1,Line2) },
    lexical_sequence( LexL, File, Page, Line1, Col1, Page, Line2, 1 ).

lexical_sequence(LexL,Loc1,Loc3,ErrL) -->
    lexical_element(null,Loc1,Loc2,ErrL1),
    lexical_sequence(LexL,Loc2,Loc3,ErrL2),
    {append(ErrL1,ErrL2,ErrL)}.

lexical_sequence([Lex|LexT],Loc1,Loc3,ErrL) -->
    lexical_element(Lex,Loc1,Loc2,ErrL1),
    {Lex \= null},
    lexical_sequence(LexT,Loc2,Loc3,ErrL2),
    {append(ErrL1,ErrL2,ErrL)}.

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% lexes_to(ChrL,LexL,Loc,ErrL)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Translates character list ChrL into its corresponding list of lexical
% elements LexL. Loc is the start location of ChrL, and ErrL is unified
% with a list of errors. ChrL and Loc must be instantiated.

lexes_to(ChrL,LexL,Loc,ErrL) :- lexical_sequence(LexL,Loc,_,ErrL,ChrL,"").

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% delimiter(Lex,Loc1,Loc2,ErrL)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Unifies Text with a string that is a legal Ada delimiter (simple or
% compound), and Token with the corresponding token to be put into the
% parse tree. [Paras 8-14]

delimiter(lex(LSD,loc(Lin,Col1),CL),
        loc(Lin,Col1),
        loc(Lin,Col2),
        ErrL) -->
    delimiter_with_errors(LSD,CL,loc(Lin,Col1),ErrL),
    {length(CL,Len),plus(Col1,Len,Col2)}.

delimiter_with_errors(LSD,CL,_,[]) --> good_delimiter(LSD,CL).



good_delimiter(delimiter(ampersand),[C],[C|CT],CT)         :- ampersand(C).
good_delimiter(delimiter(apostrophe),[C],[C|CT],CT)        :- apostrophe(C).
good_delimiter(delimiter(left_parenthesis),[C],[C|CT],CT)  :- left_parenthesis(C).
good_delimiter(delimiter(right_parenthesis),[C],[C|CT],CT) :- right_parenthesis(C).
good_delimiter(delimiter(asterisk),[C],[C|CT],CT)          :- asterisk(C).
good_delimiter(delimiter(plus_sign),[C],[C|CT],CT)         :- plus_sign(C).
good_delimiter(delimiter(comma),[C],[C|CT],CT)             :- comma(C).
good_delimiter(delimiter(minus),[C],[C|CT],CT)             :- minus(C).
good_delimiter(delimiter(dot),[C],[C|CT],CT)               :- dot(C).
good_delimiter(delimiter(solidus),[C],[C|CT],CT)           :- solidus(C).
good_delimiter(delimiter(colon),[C],[C|CT],CT)             :- colon(C).
good_delimiter(delimiter(semicolon),[C],[C|CT],CT)         :- semicolon(C).
good_delimiter(delimiter(less_than_sign),[C],[C|CT],CT)    :- less_than_sign(C).
good_delimiter(delimiter(equals_sign),[C],[C|CT],CT)       :- equals_sign(C).
good_delimiter(delimiter(greater_than_sign),[C],[C|CT],CT) :- greater_than_sign(C).
good_delimiter(delimiter(vertical_line),[C],[C|CT],CT)     :- vertical_line(C).

good_delimiter(delimiter(arrow),[C1,C2]) :-
    equals_sign(C1),
    greater_than_sign(C2).

good_delimiter(delimiter(double_dot),[C1,C2]) :-
    dot(C1),
    dot(C2).

good_delimiter(delimiter(double_star),[C1,C2]) :-
    asterisk(C1),
    asterisk(C2).

good_delimiter(delimiter(assignment),[C1,C2]) :-
    colon(C1),
    equals_sign(C2).

good_delimiter(delimiter(inequality),[C1,C2]) :-
    solidus(C1),
    equals_sign(C2).

good_delimiter(delimiter(greater_than_or_equal),[C1,C2]) :-
    greater_than_sign(C1),
    equals_sign(C2).

good_delimiter(delimiter(less_than_or_equal),[C1,C2]) :-
    less_than_sign(C1),
    equals_sign(C2).

delimiter_with_errors(delimiter(less_than_or_equal),
        [C1,C2],
        Loc,
        [fatal(assumed_to_be([C2,C1]),Loc,[C1,C2])]) :-
    equals_sign(C1),
    less_than_sign(C2).

good_delimiter(delimiter(left_label_bracket),[C1,C2]) :-
    less_than_sign(C1),
    less_than_sign(C2).

good_delimiter(delimiter(right_label_bracket),[C1,C2]) :-
    greater_than_sign(C1),
    greater_than_sign(C2).

good_delimiter(delimiter(box),[C1,C2]) :-
    less_than_sign(C1),
    greater_than_sign(C2).



% -------------------------------------------------------------------------
% RM95 2.3: Identifiers

% Our strategy for lexing identifiers (and reserved words) is first to lex
% them without regard to misuse of underlines, and then to detect underline
% errors afterwards.

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% letter_or_digit(C,U)
%
% Unifies C with a letter or a digit, and U with either the upper case
% version of lower case letter C, or otherwise with C. [Para 3]

letter_or_digit(C,U) :- identifier_letter(C,U).
letter_or_digit(C,C) :- digit(C).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% identifier_character(C,U)
%
% Unifies C with either a letter_or_digit, or an underline, and U with
% either the upper case version of lower case letter C, or otherwise with
% C.

identifier_character(C,U) :- letter_or_digit(C,U).
identifier_character(C,C) :- underline(C).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% all_underlines(Text)
%
% Succeeds if Text is a string composed entirely of underlines. (Fails for
% an empty string.)

all_underlines([C|Tail]) :- underline(C), (Tail=[]; all_underlines(Tail)).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% leading_underlines(Text,N)
%
% Unifies N with the number of leading underlines in string Text, if Text
% has at least one leading underline (i.e. fails if it does not). Text must
% be instantiated. NB: succeeds for Text is all underlines.

leading_underlines([Char|Tail],N) :-
    underline(Char) ->
        (leading_underlines(Tail,TailN) -> N is TailN+1; N=1).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% trailing_underlines(Text,Col,N,At)
%
% Unifies N with the number of trailing underlines in string Text, if Text
% has at least one trailing underline (i.e. fails if it does not). At is
% instantiated to the column number of the start of the trailing
% underlines, given that Text starts at column number Col. Text and Col
% must be instantiated. NB: succeeds for Text is all underlines.

trailing_underline(Text) :-
    append(_,[Char],Text),
    underline(Char).

trailing_underlines(Text,Col,N,At) :-
    append(Lead,Tail,Text),
    all_underlines(Tail),
    not trailing_underline(Lead),
    length(Tail,N),
    length(Lead,LeadLen),
    At is Col+LeadLen.

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% included_underlines(Text,Col,N,At)
%
% For every place where string Text has a contiguous sequence of
% underlines, which are neither leading nor trailing (but included),
% Unifies N with the number of underlines, and At with the column number of
% the start of the underlines, given that Text starts at column number Col.
% Text and Col must be instantiated.

leading_underline([Char|_]) :- underline(Char).

included_underlines(Text,Col,N,At) :-
    append(Lead,MainTail,Text),
    Lead \= [],
    not trailing_underline(Lead),
    append(TailLead,TailTail,MainTail),
    TailTail \= [],
    not leading_underline(TailTail),
    all_underlines(TailLead),
    length(Lead,LeadLen),
    At is Col+LeadLen,
    length(TailLead,N).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% underline_error(Text,Line,Col,Error)
%
% Successively unifies Error with an error token for each error of
% underlining in string Text (i.e. any leading, any trailing, any multiple
% included underlines), assuming Text begins at column number Col on line
% number Line. Text, Line, and Col must be instantiated.

underline_error(Text,Line,Col,
        fatal(lcw(Line,Col,Len),bad_underlines(all))) :-
    all_underlines(Text),
    length(Text,Len).

underline_error(Text,Line,Col,
        fatal(lcw(Line,Col,Len),bad_underlines(leading))) :-
    leading_underlines(Text,Len),
    not all_underlines(Text).

underline_error(Text,Line,Col,
        fatal(lcw(Line,At,Len),bad_underlines(trailing))) :-
    trailing_underlines(Text,Col,Len,At),
    not all_underlines(Text).

underline_error(Text,Line,Col,
        fatal(lcw(Line,At,Len),bad_underlines(included))) :-
    included_underlines(Text,Col,Len,At),
    Len > 1.

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% identifier_errors(Text,Line,Col,Errors)
%
% Unifies Errors with a list of error tokens for identifier string Text,
% which starts at column number Col on line number Line. Text, Line, and 
% Col must be instantiated.

identifier_errors(Text,Line,Col,Errors) :-
    bagof(Error,underline_error(Text,Line,Col,Error),Errors).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% id_or_rw(Text,Value)
%
% Lexes string Text into a proto-identifier/reserved word. The upper case
% version of the lexed string is returned in Value. [Paras 1-5]

id_or_rw([FirstChar|TextTail],[UpperChar|UpperTail]) :-
    identifier_character(FirstChar,UpperChar),
    not digit(FirstChar),
    id_or_rw_tail(TextTail,UpperTail).

id_or_rw_tail([],[]).

id_or_rw_tail([Char|TextTail],[UpperChar|UpperTail]) :-
    identifier_character(Char,UpperChar),
    id_or_rw_tail(TextTail,UpperTail).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% identifier(Text,Line,Col,Value,Errors)
%
% Unifies Value with the upper-case version of a recognisable identifier,
% starting at column number Col, on line number Line, and unifies Errors
% with a list of any errors. (Specifically, this rejects identifiers that
% are reserved words.)

identifier(Text,Line,Col,Value,Errors) :-
    id_or_rw(Text,Value),
    not reserved_word(Value),
    identifier_errors(Text,Line,Col,Errors).



% -------------------------------------------------------------------------
% RM95 2.4: Numeric Literals

% As for identifiers (above), our strategy will be to lex without regard to
% underline misuse, which is then detected later. We have to depart from
% the RM's syntax a bit here, because we must make a distinction between
% integer and real literal.

% Instead of numeric_literals, we declare integer_literals and real_
% literals. Rather than just decimal_literals and based_literals, we
% declare integer_decimal_literals, integer_based_literals, real_decimal_
% literals, and real_based_literals. [Paras 1-2]

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% integer_literal(Text,Value)
%
% Succeeds if Text can be lexed as an integer_literal, and unifies Value
% with its (integer) value.

integer_literal(Text,Value) :- integer_decimal_literal(Text,Value).
integer_literal(Text,Value) :- integer_based_literal(Text,Value).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% real_literal(Text,Value)
%
% Succeeds if Text can be lexed as a real_literal, and unifies Value with 
% its (float) value.

real_literal(Text,Value) :- real_decimal_literal(Text,Value).
real_literal(Text,Value) :- real_based_literal(Text,Value).


% -------------------------------------------------------------------------
% RM95 2.4.1: Decimal Literals

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% numeral(Text,Bare)
%
% Succeeds if string Text is recognisable as a decimal numeral (either an
% integer or the mantissa of a real value). Bare is unified with Text
% without any underlines it may contain. I am assuming a decimal numeral
% must begin with a digit. [Para 3]

numeral([Char|Tail],[Char|BareTail]) :-
    digit(Char),
    numeral_tail(Tail,BareTail).

numeral_tail([],[]).

numeral_tail([Char|Tail],[Char|BareTail]) :-
    digit(Char),
    numeral_tail(Tail,BareTail).

numeral_tail([Char|Tail],BareTail) :-
    underline(Char),
    numeral_tail(Tail,BareTail).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% extended_digit(D,V)
%
% Unifies D with an extended digit, and V with that digit's value (0-15).
% [2.4.2 para 5]

extended_digit(D,V) :- digit(D,V).

extended_digit(D,V) :-
    identifier_letter(D,U),
    between(0'A,0'F,U),
    plus(V1,0'A,U),
    plus(V1,10,V).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% evaluate_integer(Text,Base,Value)
%
% Unifies Value with the value of the digits comprising string Text,
% assuming a number base of Base. Text and Base must be instantiated.

evaluate_integer([D],_,V) :- extended_digit(D,V), V < Base, !.

evaluate_integer([D|Tail],Base,Value) :-

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% integer_numeral(Text,Line,Col,Value,Errors)
%
% Succeeds if string Text is recognisable as an integer decimal numeral,
% starting at column number Col, on line number Line, and unifies Value
% with its integer value, and Errors with any errors. Text, Line, and Col
% must be instantiated.

integer_numeral(Text,Line,Col,Value,Errors) :-
    numeral(Text,Bare),
    evaluate_integer(Bare,10,Value),
    identifier_errors(Text,Line,Col,Errors).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% [no_minus_]exponent(Text,Line,Col,Value,Errors)
%
% Succeeds if Text is a recognisable exponent, starting at column number
% Col, on line number Line, and Value with its integer value, and Errors
% with any errors. Text, Line, and Col must be instantiated. The 'no_minus'
% variant is for use in integer literals, where a minus sign in the
% exponent is forbidden (and thus we generate an error). [Paras 4-5]

no_minus_exponent([FirstChar|TextTail],Line,Col,Value,Errors) :-
    identifier_letter(FirstChar,0'E),
    NextCol is Col+1,
    no_minus_exponent_tail(TextTail,Line,NextCol,Value,Errors).

no_minus_exponent_tail(Text,Line,Col,Value,Errors) :-
    integer_numeral(Text,Line,Col,Value,Errors).

no_minus_exponent_tail([Char|Tail],Line,Col,Value,Errors) :-
    plus_sign(Char),
    NextCol is Col+1,
    integer_numeral(Tail,Line,NextCol,Value,Errors).

no_minus_exponent_tail([Char|Tail],Line,Col,Value,
        [fatal(lcw(Line,Col,1),minus_in_exponent)|Errors]) :-
    minus(Char),
    NextCol is Col+1,
    integer_numeral(Tail,Line,NextCol,AbsVal,Errors),
    Value is -AbsVal.

exponent([FirstChar|TextTail],Line,Col,Value,Errors) :-
    identifier_letter(FirstChar,0'E),
    exponent_tail(TextTail,Line,Col,Value,Errors).

exponent_tail(Text,Line,Col,Value,Errors) :-
    integer_numeral(Text,Line,Col,Value,Errors).

exponent_tail([Char|Tail],Line,Col,Value,Errors) :-
    plus_sign(Char),
    NextCol is Col+1,
    integer_numeral(Tail,Line,NextCol,Value,Errors).

exponent_tail([Char|Tail],Line,Col,Value,Errors) :-
    minus(Char),
    NextCol is Col+1,
    integer_numeral(Tail,Line,NextCol,AbsVal,Errors),
    Value is -AbsVal.

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% integer_decimal_literal(Text,Line,Col,Value,Errors)
%
% Succeeds if Text is a recognisable integer_decimal_literal, which starts
% at column number Col, on line number Line, and unifies Value with its
% integer value, and Errors with any errors. Text, Line, and Col must be
% instantiated. Value may be unified with 'unknown' if its value cannot be
% sensibly computed (due to an error). [Para 2]

integer_decimal_literal(Text,Line,Col,Value,Errors) :-
    integer_numeral(Text,Line,Col,Value,Errors).

integer_decimal_literal(Text,Line,Col,Value,Errors) :-
    append(Lead,Tail,Text),
    integer_numeral(Lead,Line,Col,Factor,Errs1),
    no_minus_exponent(Tail,Line,Col,Index,Errs2),
    (Index < 0 -> Value=unknown; Value is Factor*10^Index),
    append(Errs1,Errs2,Errors).






% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





% =========================================================================
% END-OF-FILE.


