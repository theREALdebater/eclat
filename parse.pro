% *************************************************************************
% =========================================================================
% Ada 2012 compiler in Prolog
% =========================================================================
% *************************************************************************
%
% Authors: {Nicholas James Roberts (UK), David Botton, Anh Vo}.
%
% Version: Initial experimental.
%
% Copyright: See accompanying file "COPYRIGHT".





% #########################################################################
% =========================================================================
% Parsing Module
% =========================================================================
% #########################################################################

% :- module(parse,[parses_to/4]).




% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% el3( 
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% 
% 
% 
% 
% 













% =========================================================================
% RM95 3: Declarations and Types
% =========================================================================



% -------------------------------------------------------------------------
% RM95 3.1: Declarations
% -------------------------------------------------------------------------

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Proper Declaration
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% - type_declaration(  )
% - - :
% - - :
% - subtype_declaration(  )
% - - :
% - - :
% - object_declaration(  )
% - - :
% - - :
% - number_declaration(  )
% - - :
% - - :
% - subprogram_declaration( Prms, Decs, Body, ExHan )
% - - Prms:
% - - Decs:
% - - Body:
% - - ExHan:
% - abstract_subprogram_declaration(  )
% - - :
% - - :
% - package_declaration(  )
% - - :
% - - :
% - renaming_declaration(  )
% - - :
% - - :
% - exception_declaration(  )
% - - :
% - - :
% - generic_declaration(  )
% - - :
% - - :
% - generic_instantiation(  )
% - - :
% - - :

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Declaration
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% - dec( Id, PD )
% - - Id: identifier
% - - PD: proper declaration
% - fatal( Err )
% - - Err: fatal error
% - 
% - 

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% basic_declaration(Dec) -->
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% - Dec: declaration
% 
% 
% 
% 
% 
% 
% 




basic_declaration(Cxt) --> type_declaration(Cxt).
basic_declaration(Cxt) --> subtype_declaration(Cxt).
basic_declaration(Cxt) --> object_declaration(Cxt).
basic_declaration(Cxt) --> number_declaration(Cxt).
basic_declaration(Cxt) --> subprogram_declaration(Cxt).
basic_declaration(Cxt) --> abstract_subprogram_declaration(Cxt).
basic_declaration(Cxt) --> package_declaration(Cxt).
basic_declaration(Cxt) --> renaming_declaration(Cxt).
basic_declaration(Cxt) --> exception_declaration(Cxt).
basic_declaration(Cxt) --> generic_declaration(Cxt).
basic_declaration(Cxt) --> generic_instantiation(Cxt).











% =========================================================================
% RM95 3.2: Types and Subtypes
% =========================================================================










% =========================================================================
% 3.2.1 Type Declarations
% =========================================================================

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% type_declaration(Cxt)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Parses a type_declaration, within context Cxt, and unifies the resulting
% declaration with Dec. [3.2.1(2)]

type_declaration(Cxt,Dec) --> full_type_declaration(Cxt,Dec).
type_declaration(Cxt,Dec) --> incomplete_type_declaration(Cxt,Dec).
type_declaration(Cxt,Dec) --> private_type_declaration(Cxt,Dec).
type_declaration(Cxt,Dec) --> private_extension_declaration(Cxt,Dec).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% full_type_declaration(Cxt)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% Parses a full type declaration, within context Cxt, and unifies the
% resulting declaration with Dec. [3.2.1(3)]

full_type_declaration(Cxt,dec(Id,Def,Loc,CL,ErrL)) -->
    [lex(reserved_word(type),Loc,CL1)],
    defining_identifier(Id,CLL1,ErrL1),
    [lex(reserved_word(is),_,CL2)],
    type_definition(Def,CLL2,ErrL2),
    [lex(delimiter(semicolon),_,CL3)],
    {flatten([CL1,CLL1,CL2,CLL2,CL3],CL),
    append(ErrL1,ErrL2,ErrL)}.

full_type_declaration(Cxt,Dec) --> task_type_declaration(Cxt,Dec).
full_type_declaration(Cxt,Dec) --> protected_type_declaration(Cxt,Dec).



