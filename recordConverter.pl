/*
    @version 2.0.0
    @title Java Record to C++ class converter using SWI-Prolog
    @author Joel Ramirez Vargas
    @since  22 - 11 - 2024
*/

:- use_module(library(pure_input)). 

%%%%%%%%%%%%%%%%%%%%%%%%% File reading - helper functions %%%%%%%%%%%%%%%%%%%%%%%%

read_file(FileName, Records) :- atom_concat(FileName, ".txt", File),
                                open(File, read, Str),
                                read_lines(Str, Records),
                                close(Str)
                                .

read_lines(TextStream, []) :- at_end_of_stream(TextStream), !.

read_lines(TextStream, [LHead | LTail]) :- \+ at_end_of_stream(TextStream),           
                                           read_line_to_string(TextStream, LHead),      
                                           read_lines(TextStream, LTail)
                                           .
each_line([]).
each_line([H | T], [H | RT]) :- each_line(T, RT).

each_line_to_codes([], []).
each_line_to_codes([H | T], [S | R]) :- atom_codes(H, S),
                                        each_line_to_codes(T, R).

cap_first(S, Scf) :- atom_chars(S, [ToCap | T]),
                     upcase_atom(ToCap, Capped),
                     atom_chars(Scf, [Capped | T])
                     .

non_primitive(ToCheck) :- \+ member(ToCheck, ["string", "int", "float", "double", "bool"]).

get_default_val(ToCheck, DVal) :- \+ non_primitive(ToCheck), default_value(ToCheck, DVal).

%%% ======   Default Values for defConStruct   ====== %%%

default_value("string", '"-"').
default_value("int", 0).
default_value("float", 0).
default_value("double", 0).
default_value("bool", false).

%%%%%%%%%%%%%%%%%%%%%%%% Record parsing - Record Grammar %%%%%%%%%%%%%%%%%%%%%%%%%

get_records(UserInput, Records) :- get_records_w_nest(UserInput, RecordsWNests),
                                   process_nested_records(RecordsWNests, Records)  
                                   .                    

get_records_w_nest(UserInput, Records) :- read_file(UserInput, ReadFromFile),
                                          each_line_to_codes(ReadFromFile, LineC), 
                                          process_records(LineC, Records)
                                          .  

process_records([], []).
process_records([RecordCodesH | RecordCodesT], [RecordsH | RecordsT]) :-
    ( phrase(recordStructure([RecordId, RecordArgs, InRecordNested]), RecordCodesH, []) ->
        RecordsH = [RecordId, RecordArgs, InRecordNested]
        ; 
        RecordsH = failed  % If parsing fails, mark as failed
    ),
    process_records(RecordCodesT, RecordsT)
    .

process_nested_records([], []).
process_nested_records([[Id, Args, NestedIn] | T], FL) :- process_nested_records(NestedIn, NestedList),
                                                          process_nested_records(T, NestedRest),
                                                          append([[Id, Args] | NestedList], NestedRest, FL)
                                                          .

/* --------------------------- 'Parsing' Grammar Rules ------------------------- */ 

%%% ======   Record handling   ====== %%%

recordStructure([Record, Args, NestedRecords]) --> ws, recordDeclaration(Record), ws, argumentDeclaration(Args), ws,
                                                   leftCurly, ws, nestedRecords(NestedRecords), ws, rightCurly, ws
                                                   .

nestedRecords([NH | NT]) --> recordStructure(NH), ws, nestedRecords(NT).
nestedRecords([]) --> [].

recordDeclaration(RId) --> ws, recordVisibility, ws, recordKW, ws, id(RId).

recordVisibility --> publicKW, ws, staticKW, ws. 


%%% ======   Argument handling   ====== %%%

argumentDeclaration(Args) --> ws, leftPar, ws, arguments(Args), ws, rightPar, ws.

arguments([Arg | Tail]) --> varDeclaration(Arg), ws, argumentsTail(Tail).
arguments([]) --> [].

argumentsTail([Arg | Tail]) --> comma, ws, varDeclaration(Arg), ws, argumentsTail(Tail).
argumentsTail([]) --> [].


%%% ======   Variable handling   ====== %%%

varDeclaration([VarType, VarId]) --> ws, type(VarType), id(VarId). 


%%% ======   Core building block for our grammar   ====== %%%

id(Id) --> ws, letters(IdS), {IdS \= [], atom_codes(Id, IdS), !}.


%%% ======   Reserved keywords   ====== %%% 

recordKW --> "record".
publicKW --> "public".
staticKW --> "static".
staticKW --> [].


%%% ======   Types   ====== %%%

type(string) --> "String".
type(int) --> "int".
type(float) --> "float".
type(double) --> "double".
type(bool) --> "boolean".
type(Id) --> id(Def), {string_lower(Def, DefL), term_to_atom(Id, DefL)}. 


%%% ======   Symbols and braces   ====== %%%

comma --> ",".
leftPar --> "(".
rightPar --> ")".
leftCurly --> "{".
rightCurly --> "}".


%%% ======   Basic grammar utilities   ====== %%%

numbers([NH | NT]) --> [NH], {code_type(NH, digit)}, letters(NT).
numbers([]) --> [].

letters([LH | LT]) --> [LH], {code_type(LH, alpha)}, letters(LT).
letters([]) --> [].

spChars([CH | CT]) --> [CH], {code_type(CH, ascii)}, spChars(CT).
spChars([]) --> [].

ws --> [C], {code_type(C, space)}, ws.
ws --> []. 


/* ------------------------- 'Generating' Grammar Rules  ------------------------ */ 

%%% ======   Symbols   ====== %%%

tilde --> "~".
lessT --> "<".
greatT --> ">".
arrow --> "-", greatT.
sColon --> ";".
colon --> ":".
sign --> "#".
quote --> "\"".
space --> " ".
newLine --> "\n".
tab --> "\t".
tabSym --> "\\t".
outSym --> lessT, lessT.
comment --> "//".
assign --> "=".


%%% ======   Reserved keywords   ====== %%% 

endlKW --> "endl".
voidKW --> "void".
ifndefKW --> "ifndef".
defineKW --> "define".
endKW --> "endif".
includeKW --> "include".
privateKW --> "private".
constKW --> "const".
classKW --> "class".
newKW --> "new".
thisKW --> "this".
returnKW --> "return".
sstreamKW --> "stringstream".
toStringKW --> "toString".

%%% ======   Default imports and values   ====== %%%

iostreamLib --> libTemplate("iostream").
sstreamLib --> libTemplate("sstream").
stringLib --> libTemplate("string").

std --> "using", space, "namespace", space, "std", sColon.

%%% ======   Complex Structure   ====== %%%

header([Class | [Attributes]]) --> {string_upper(Class, C)},
                                   headerB1(C, Attributes),
                                   headerB2(Class, Attributes),
                                   headerB3_1(Class, Attributes),
                                   headerB3_2(Attributes),
                                   headerB3_3(Attributes),
                                   headerToString, newLine,
                                   headerB4(C)
                                   .
source([Class | [Attributes]]) --> includeClass(Class), newLine, newLine,
                                   sourceB1_1(Class, Attributes),
                                   sourceB1_2(Class, Attributes),
                                   sourceB1_3(Class),
                                   sourceB2(Class, Attributes),
                                   sourceB3(Class, Attributes),
                                   sourceB4(Class, Attributes)
                                   .

%%% ======  Source Structures   ====== %%%

sourceB1_1(C, A) --> addComment("Class constructors and destructor"), newLine, 
                     srcRefToClass(C), paramConStruct(C, A), newLine,
                     tab, tab, colon, space, addMembers(A), space,
                     leftCurly, space, space, rightCurly, newLine, newLine
                     .

sourceB1_2(C, A) --> srcRefToClass(C), defConStruct(C), space, colon, space,
                     capFirstFromAtom(C), space, leftPar, addDefValuesToDefCons(A), rightPar,
                     space, leftCurly, space, space, rightCurly, newLine, newLine
                     .

sourceB1_3(C) --> srcRefToClass(C), tilde, defConStruct(C), space, 
                  leftCurly, space, space, rightCurly, newLine, newLine
                  .

sourceB2(C, A) --> addComment("Setter methods"), newLine,
                   addSettersToSrc(C, A) 
                   .  

sourceB3(C, A) --> addComment("Getter methods"), newLine,
                   addGettersToSrc(C, A) 
                   . 

sourceB4(C, A) --> addComment("toString method"), newLine,
                   "string", space, srcRefToClass(C), toStringKW, leftPar, rightPar,
                   space, constKW, space, leftCurly, newLine, tab, sstreamKW, space, "s",
                   sColon, newLine, newLine, addToSstream(A), newLine,
                   tab, returnKW, space, "s.str()", sColon, newLine, rightCurly
                   .

%%% ======  Header Structures   ====== %%%

headerB1(C, A) --> hIfn(C), newLine, 
                   hDef(C), newLine, 
                   includeExtras(A),
                   defaultLibs, newLine, newLine
                   .
                   
headerB2(C, A) --> classDef(atomToString(C)), newLine,
                   privateKW, colon, newLine, newLine,
                   tab, addComment("Class attributes"), newLine,
                   addAttributes(A), newLine
                   .

headerB3_1(C, A) --> publicKW, colon, newLine, newLine,
                     tab, addComment("Class constructors and destructor"), newLine,
                     tab, defConStruct(C), sColon, newLine,
                     tab, paramConStruct(C, A), sColon, newLine,
                     tab, tilde, defConStruct(C), sColon, newLine, newLine
                     .

headerB3_2(A) --> tab, addComment("Setter methods"), newLine,
                  addSettersToHead(A), newLine
                  .

headerB3_3(A) --> tab, addComment("Getter methods"), newLine,
                  addGettersToHead(A), newLine
                  .

headerToString --> tab, addComment("toString method"), newLine,
                   tab, "string", space, toStringKW, leftPar, rightPar, 
                   space, constKW, sColon, newLine
                   .

headerB4(C) --> rightCurly, sColon, newLine, newLine,
                hEnd(C)
                .


%%% ======   Recursive grammar   ====== %%%

includeExtras([]) --> [], newLine.
includeExtras([[ToCheck, _] | T]) --> {atom_string(ToCheck, Str)},
                                      (
                                        {non_primitive(Str)} -> 
                                        newLine, includeClass(Str)
                                        ; 
                                        [] 
                                      ),
                                      includeExtras(T)
                                      .
addMembers([]) --> [].
addMembers([[_, AtId]]) --> memberStruct(AtId).
addMembers([[_, AtId] | T]) --> memberStruct(AtId), comma, space, addMembers(T).

addDefValuesToDefCons([]) --> [].
addDefValuesToDefCons([[AtType, _]]) --> defVals(AtType).
addDefValuesToDefCons([[AtType, _] | T]) --> defVals(AtType), comma, space, addDefValuesToDefCons(T).

addGettersToHead([]) --> [].
addGettersToHead([[AtType, AtId] | T]) --> tab, capOrNot(AtType), space, getterStruct(AtId), space, constKW, 
                                           sColon, newLine, addGettersToHead(T)
                                           .

addGettersToSrc(_, []) --> [].
addGettersToSrc(Class, [[AtType, AtId] | T]) --> capOrNot(AtType), space, srcRefToClass(Class), getterStruct(AtId), space, 
                                                 constKW, space, leftCurly, space, returnKW, space, thisRefStruct(AtId),
                                                 sColon, space, rightCurly, newLine, newLine, addGettersToSrc(Class, T)
                                                 .

addSettersToHead([]) --> [].
addSettersToHead([[AtType, AtId] | T]) --> tab, voidKW, space, setterStruct(AtType, AtId), sColon, newLine,
                                           addSettersToHead(T)
                                           .

addSettersToSrc(_, []) --> [].
addSettersToSrc(Class, [[AtType, AtId] | T]) --> voidKW, space, srcRefToClass(Class), setterStruct(AtType, AtId), 
                                                 space, leftCurly, space, thisRefAssign(AtId), sColon, space, rightCurly,
                                                 newLine, newLine, addSettersToSrc(Class, T)
                                                 .
addToSstream([]) --> [].
addToSstream([[ToCheck, AtId] | T]) --> tab, "s", space, outSym, space, 
                                  quote, capFirstFromAtom(AtId), colon, tabSym, quote, space, outSym,
                                  space, thisKW, arrow, getterStruct(AtId), 
                                  {atom_string(ToCheck, Str)} ->
                                  ( {non_primitive(Str)} -> 
                                    ".", toStringKW, leftPar, rightPar
                                    ;
                                    []
                                  ),
                                  space, outSym, space, endlKW,
                                  sColon, newLine, addToSstream(T)
                                  .

addAttributes([]) --> [].
addAttributes([[AtType, AtId] | T]) --> tab, capOrNot(AtType),
                                        space, atomToString(AtId), sColon, newLine,
                                        addAttributes(T)
                                        .

addParams([]) --> [].
addParams([[AtType, AtId]]) --> addParam(AtType, AtId).
addParams([[AtType, AtId] | T]) --> addParam(AtType, AtId), comma, space,
                                    addParams(T)
                                    .

defVals(ToCheck) --> {atom_string(ToCheck, Str)} ->
                     ( {get_default_val(Str, Dv)} ->
                       atomToString(Dv) 
                       ; 
                       defConStruct(ToCheck)
                     )
                     .

capOrNot(ToCheck) --> {atom_string(ToCheck, Str)} ->
                      ( {non_primitive(Str)} -> 
                        capFirstFromAtom(ToCheck)
                        ;
                        atomToString(ToCheck) 
                      )
                      .

%%% ======   Building Blocks   ====== %%%

thisRefAssign(AtId) --> thisRefStruct(AtId), space, 
                        assign, space, atomToString(AtId)
                        . 

thisRefStruct(AtId) --> thisKW, arrow, atomToString(AtId).

memberStruct(AtId) --> atomToString(AtId), leftPar, atomToString(AtId), rightPar.

srcRefToClass(Class) --> atomToString(Class), colon, colon.

addParam(AtType, AtId) --> capOrNot(AtType), space, atomToString(AtId).

includeClass(Class) --> sign, includeKW, space, headerTemplate(Class). 

defConStruct(Class) --> capFirstFromAtom(Class), leftPar, rightPar.

paramConStruct(Class, Params) -->  capFirstFromAtom(Class), leftPar, addParams(Params), rightPar.  

getterStruct(AtId) --> "get", capFirstFromAtom(AtId),
                        leftPar, rightPar
                        .
setterStruct(AtType, AtId) --> "set", capFirstFromAtom(AtId), 
                               leftPar, addParam(AtType, AtId), rightPar
                               .

addComment(Comment) --> comment, space, Comment.

capFirstFromAtom(Atom) --> {cap_first(Atom, CAtom)}, atomToString(CAtom). 

atomToString(Atom) --> {atom_string(Atom, AtomStr)}, AtomStr.

classDef(Class) --> classKW, space, Class, space, leftCurly.

headerTemplate(HeaderName) --> quote, capFirstFromAtom(HeaderName), ".h", quote.

libTemplate(LibName) --> lessT, LibName, greatT.

hClassDef(Class) --> Class, "_H".

hIfn(Class) --> sign, ifndefKW, space, hClassDef(Class).
hEnd(Class) --> sign, endKW, space, comment, space, hClassDef(Class).
hDef(Class) --> sign, defineKW, space, hClassDef(Class).

defaultLibs --> sign, includeKW, space, iostreamLib, newLine, 
                sign, includeKW, space, sstreamLib, newLine,  
                sign, includeKW, space, stringLib, newLine,
                std
                .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  File creation logic  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_directory(DirName) :- ( exists_directory(DirName) ->  
                               format("Directory '~w' already exists.~n", [DirName])
                               ; 
                               make_directory(DirName), 
                               format("Directory '~w' created successfully.~n", [DirName])
                             )
                             .

generate_header_src(Name, Files) :- header_file_name(Name, HName), 
                                    source_file_name(Name, SrcName),
                                    Files = [HName, SrcName]
                                    .

write_codes(Stream, Codes) :- atom_codes(ToWrite, Codes), 
                              write(Stream, ToWrite) 
                              .

get_valid_filename(FileName) :- format("Enter the name of the file (without extension): "),
                                read_line_to_string(user_input, Input),
                                atom_string(FileName, Input)
                                .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Header - .h logic  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

header_file_name(CName, HName) :- atom_concat(CName, ".h", HName).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Source - .cpp logic  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_file_name(CName, SName) :- atom_concat(CName, ".cpp", SName).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Main program logic  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

testingHead :- get_records("input", Records),
               open("headTestOutput.txt", write, Stream),
               forall(member(Record, Records), 
                   (   writeln(Record),
                       phrase(header(Record), B1C),
                       write_codes(Stream, B1C),
                       nl(Stream)
                   )
               ),
               close(Stream)
            .  

testingSrc :- get_records("input", Records),
              open("srcTestOutput.txt", write, Stream),
              forall(member(Record, Records), 
                  (   writeln(Record),
                      phrase(source(Record), B1C),
                      write_codes(Stream, B1C),
                      nl(Stream)
                  )
              ),
              close(Stream)
              .      

recordConverter :-  get_valid_filename(InputFile),
                    Directory = "Generated Classes", 
                    create_directory(Directory),
                    get_records(InputFile, Records), 
                    forall(member(Record, Records), 
                        (   
                            nth0(0, Record, Name),

                            % Generating the .h file
                            header_file_name(Name, HName),
                            format(atom(HFullPath), "~w/~w", [Directory, HName]),
                            open(HFullPath, write, HStream),
                            phrase(header(Record), HCodes),
                            write_codes(HStream, HCodes),
                            close(HStream),
                            format("\nHeader File '~w' written successfully.~n", [HFullPath]),

                            % Generating the .cpp file
                            source_file_name(Name, SrcName),
                            format(atom(SrcFullPath), "~w/~w", [Directory, SrcName]),
                            open(SrcFullPath, write, SrcStream),
                            phrase(source(Record), SrcCodes),  
                            write_codes(SrcStream, SrcCodes),
                            close(SrcStream),
                            format("Source file '~w' written successfully.~n", [SrcFullPath]),
                            writeln("--------------------------------------------------------------\n")
                        )    
                    )
                    .    
