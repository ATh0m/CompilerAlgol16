/*
* Tomasz Nanowski
* Metody Programowania, Pracownia nr 3
*
* SWI-Prolog version 7.2.3
*
* Zaimplementowane elementy:
* - podstawowa wersja języka
* - nierekurencyjne procedury
* - możliwość przekazywania parametrów przez wartość
* - możliwość zagnieżdżania procedur
*
*/


/*
*
*   LEXER
*
*/

lexer(Tokens) -->
    white_space,        !, lexer(Tokens) |
    comment,            !, lexer(Tokens) |
    operator(Token),    !, { Tokens = [Token | TokList] }, lexer(TokList) |
    punctuation(Token), !, { Tokens = [Token | TokList] }, lexer(TokList) |
    identifier(Id),     !, { (
                                keyword(Id, Token), !;
                                Token = tokIdentifier(Id)
                              ),
                             Tokens = [Token | TokList] }, lexer(TokList) |
    integer_(Token),    !, { Tokens = [Token | TokList] }, lexer(TokList) |
    unknown(Token),     !, { Tokens = [Token | TokList] }, lexer(TokList) |
    [],                 !, { Tokens = [] }.

/* ------ */

white_space -->
    [Char], { code_type(Char, space) }.

/* ------ */

comment -->
    "(*", !, comment_body.
comment_body -->
    "*)", !.
comment_body -->
    [_], comment_body.

/* ------ */

operator(Token) -->
    ";",            !, { Token = tokSColon  } |
    ":=",           !, { Token = tokAssgn   } |
    "<>",           !, { Token = tokNeq     } |
    "=",            !, { Token = tokEq      } |
    ">=",           !, { Token = tokGeq     } |
    ">",            !, { Token = tokGt      } |
    "<=",           !, { Token = tokLeq     } |
    "<",            !, { Token = tokLt      } |
    "*",            !, { Token = tokTimes   } |
    "-",            !, { Token = tokMinus   } |
    "+",            !, { Token = tokPlus    } .

/* ------ */

punctuation(Token) -->
    ",",            !, { Token = tokComma  } |
    "(",            !, { Token = tokLParen  } |
    ")",            !, { Token = tokRParen  } .

/* ------ */

integer_(Token) -->
    digits(Ds), { \+ Ds = [], number_chars(N, Ds), Token = tokInteger(N) }.

digits([D|Ds]) -->
    digit(D), !, digits(Ds).
digits([]) -->
    [].

digit(D) -->
    [D], { code_type(D, digit) }.

/* ------ */

identifier(Id) -->
    letter(L), characters(Cs), { atom_codes(Id, [L|Cs]) }.

letter(L) -->
    [L], { code_type(L, alpha) }.

characters([A|Cs]) -->
    [A], { code_type(A, alnum); char_code('_', A); char_code('''', A) }, !, characters(Cs).
characters([]) -->
    [].

keyword(Id, Token) :-
    member((Id, Token), [   (and        ,tokAnd),
                            (begin      ,tokBegin),
                            (call       ,tokCall),
                            (div        ,tokDiv),
                            (do         ,tokDo),
                            (done       ,tokDone),
                            (else       ,tokElse),
                            (end        ,tokEnd),
                            (fi         ,tokFi),
                            (if         ,tokIf),
                            (local      ,tokLocal),
                            (mod        ,tokMod),
                            (not        ,tokNot),
                            (or         ,tokOr),
                            (procedure  ,tokProcedure),
                            (program    ,tokProgram),
                            (read       ,tokRead),
                            (return     ,tokReturn),
                            (then       ,tokThen),
                            (value      ,tokValue),
                            (while      ,tokWhile),
                            (write      ,tokWrite)    ]).

/* ------ */

unknown(tokUnknown) -->
    [_].



/*
*
*   PARSER
*
*/

program_(Ast) -->
    [tokProgram], [tokIdentifier(PName)], block_(Block), { Ast = program(PName, Block) }.

/* ------ */

block_(Block) -->
    declarations_(Declarations), [tokBegin], complex_instruction_(CInstruction), [tokEnd], { Block = block(Declarations, CInstruction) }.

/* ------ */

declarations_(Declarations) -->
    declaration_(Declaration),      !, { append(Declaration, RDeclarations, Declarations) }, declarations_(RDeclarations) |
    [],                             !, { Declarations = [] }.

declaration_(Declaration) -->
    declarator_(Declaration),   ! |
    procedure_(Declaration),    ! .

declarator_(Variables) -->
    [tokLocal], variables_(Variables).

variables_(Variables) -->
    variable_(Variable), (
                            [tokComma], !, { Variables = [Variable | RVariables] }, variables_(RVariables) ;
                                        !, { Variables = [Variable] }
                         ).

variable_(variable(VName)) -->
    [tokIdentifier(VName)].

procedure_(Procedure) -->
    [tokProcedure], !, [tokIdentifier(PName)], [tokLParen], formal_arguments_(FArguments), [tokRParen], block_(Block), { Procedure = [procedure(PName, FArguments, Block)] }.

formal_arguments_(FArguments) -->
    formal_arguments_sequence_(FArgumentsSequence),     !, { FArguments = FArgumentsSequence } |
    [],                                                 !, { FArguments = [] }.

formal_arguments_sequence_(FArgumentsSequence) -->
    formal_argument_(FArgument), (
                                    [tokComma], !, { FArgumentsSequence = [FArgument | RFArgumentsSequence] }, formal_arguments_sequence_(RFArgumentsSequence) ;
                                                !, { FArgumentsSequence = [FArgument] }
                                ).

formal_argument_(FArgument) -->
    [tokValue], !, variable_(Variable), { FArgument = value(Variable) } |
                !, variable_(Variable), { FArgument = Variable } .

complex_instruction_(CInstruction) -->
    instruction_(Instruction), (
                                    [tokSColon],    !, complex_instruction_(RCInstruction), { CInstruction =.. [realise, Instruction, RCInstruction] } ;
                                                    !, { CInstruction =.. [realise, Instruction] }
                               ).

instruction_(Instruction) -->
    variable_(Variable), [tokAssgn],    !, arithmetic_expression_(AExpression), { Instruction =.. [assign, Variable, AExpression] } |
    [tokIf],                            !, bool_expression_(Bool), [tokThen], complex_instruction_(ThenPart),   (
                                                                                [tokElse], !, complex_instruction_(ElsePart), [tokFi], { Instruction = if(Bool, ThenPart, ElsePart) };
                                                                                [tokFi], { Instruction = if(Bool, ThenPart) }
                                                                                                                ) |
    [tokWhile],                         !, bool_expression_(Bool), [tokDo], complex_instruction_(Body), [tokDone], { Instruction = while(Bool, Body) } |
    [tokCall],                          !, procedure_call_(Procedure), { Instruction = call(Procedure) } |
    [tokReturn],                        !, arithmetic_expression_(AExpression), { Instruction = return(AExpression) } |
    [tokRead],                          !, variable_(Variable), { Instruction = read(Variable) } |
    [tokWrite],                         !, arithmetic_expression_(AExpression), { Instruction = write(AExpression) }.


arithmetic_expression_(AExpression) -->
    !,
    summand_(Summand), arithmetic_expression_(Summand, AExpression).

arithmetic_expression_(Acc, AExpression) -->
    additive_operator_(AOperator),  !, summand_(Summand), { Acc1 = (AOperator, Acc, Summand) }, arithmetic_expression_(Acc1, AExpression) |
    [],                             !, { Acc = AExpression }.

additive_operator_(Operator) -->
    [tokPlus],      !, { Operator = plus    } |
    [tokMinus],     !, { Operator = minus   }.

summand_(SExpression) -->
    !,
    factor_(Factor), summand_(Factor, SExpression).

summand_(Acc, SExpression) -->
    multiplicative_operator_(MOperator),    !, factor_(Factor), { Acc1 = (MOperator, Acc, Factor) }, summand_(Acc1, SExpression) |
    [],                                     !, { Acc = SExpression }.

multiplicative_operator_(Operator) -->
    [tokTimes],     !, { Operator = times   } |
    [tokDiv],       !, { Operator = div     } |
    [tokMod],       !, { Operator = mod     }.

factor_(FExpression) -->
    [tokMinus],     !, simple_expression_(SExpression), { FExpression = (minus, SExpression) } |
                    !, simple_expression_(SExpression), { FExpression = SExpression }.

simple_expression_(SExpression) -->
    [tokLParen],    !, arithmetic_expression_(SExpression), [tokRParen] |
                    !, atomic_expression_(SExpression).

atomic_expression_(AExpression) -->
    procedure_call_(Procedure),     !, { AExpression = Procedure } |
    variable_(AExpression),         ! |
    [tokInteger(Integer)],          !, { AExpression = constant(Integer) }.

procedure_call_(Procedure) -->
    [tokIdentifier(PName)], [tokLParen], factual_arguments_(PArguments), [tokRParen], { Procedure = procedure(PName, PArguments) }.

factual_arguments_(FArguments) -->
    factual_arguments_sequence_(FArgumentsSequence),    !, { FArguments = FArgumentsSequence } |
    [],                                                 !, { FArguments = [] }.

factual_arguments_sequence_(FArgumentsSequence) -->
    factual_argument_(FArgument),   (
                                        [tokComma], !, { FArgumentsSequence = [FArgument | RFArgumentsSequence] }, factual_arguments_sequence_(RFArgumentsSequence) ;
                                                    !, { FArgumentsSequence = [FArgument] }
                                    ).

factual_argument_(FArgument) -->
    arithmetic_expression_(AExpression), { FArgument = AExpression } .

bool_expression_(Bool) -->
    conjunction_(Conjunction), bool_expression_(Conjunction, Bool).

bool_expression_(Acc, Bool) -->
    [tokOr],    !, conjunction_(Conjunction), { Acc1 = (or, Acc, Conjunction) }, bool_expression_(Acc1, Bool) |
    [],         !, { Acc = Bool }.

conjunction_(Conjunction) -->
    condition_(Condition), conjunction_(Condition, Conjunction).

conjunction_(Acc, Conjunction) -->
    [tokAnd],   !, condition_(Condition), { Acc1 = (and, Acc, Condition) }, conjunction_(Acc1, Conjunction) |
    [],         !, { Acc = Conjunction }.

condition_(Condition) -->
    [tokNot],   !, relative_expression_(RExpression), { Condition = (not, RExpression) } |
                !, relative_expression_(RExpression), { Condition = RExpression }.

relative_expression_(RExpression) -->
    [tokLParen], bool_expression_(RExpression), !, [tokRParen] |
    arithmetic_expression_(AExpressionL), relative_operator_(ROperator), arithmetic_expression_(AExpressionR), { RExpression = (ROperator, AExpressionL, AExpressionR) }.

relative_operator_(Operator) -->
    [tokEq],    !, { Operator = eq  } |
    [tokNeq],   !, { Operator = neq } |
    [tokLt],    !, { Operator = lt  } |
    [tokLeq],   !, { Operator = leq } |
    [tokGt],    !, { Operator = gt  } |
    [tokGeq],   !, { Operator = geq }.


/*
*
*   COMPILER
*
*/

compile_program(program(_, Block), CompiledProgram) :-
    compile_block(([], [], 65535, Return), Block, CompiledBlock),
    append(CompiledBlock, [label(Return), syscall(0)], CompiledProgram).

compile_block(Environment, block(Declarations, CInstruction), CompiledBlock) :-
    compile_declarations(Environment, Declarations, NEnvironment),
    compile_complex_instruction(NEnvironment, CInstruction, CompiledBlock).

compile_declarations(Environment, [variable(VName) | Declarations], (Variables, Procedures, SPointer, Return)) :-
    !,
    compile_declarations(Environment, Declarations, (RVariables, Procedures, RSPointer, Return)),
    Variables = [(VName, RSPointer) | RVariables],
    SPointer is RSPointer - 1.

compile_declarations(Environment, [procedure(PName, FArguments, Block) | Declarations], (Variables, Procedures, SPointer, Return)) :-
    !,
    compile_declarations(Environment, Declarations, (Variables, RProcedures, SPointer, Return)),
    Procedures = [(PName, FArguments, Block) | RProcedures].

compile_declarations(Environment, [], Environment).


compile_procedure((Variables, Procedures, SPointer, Return), procedure(PName, PArguments), CompiledProcedure) :-
    member((PName, FArguments, Block), Procedures),
    length(PArguments, L), length(FArguments, L),
    !,
    compile_procedure_arguments((Variables, Procedures, SPointer, Return), PArguments, FArguments, (NVariables, NProcedures, NSPointer, _), CompiledPArguments),
    compile_block((NVariables, NProcedures, NSPointer, NReturn), Block, CompiledBlock),
    append([CompiledPArguments, CompiledBlock, [const(0), swapd, label(NReturn), swapd]], CompiledProcedure).

compile_procedure_arguments((Variables, Procedures, SPointer, Return), [PArgument | PArguments], [value(variable(VName)) | FArguments], NEnvironment, CompiledPArguments) :-
    !,
    compile_arithmetic_expression((Variables, Procedures, SPointer, Return), PArgument, CompiledPArgument),
    SPointer2 is SPointer - 1,
    compile_procedure_arguments((Variables, Procedures, SPointer2, Return), PArguments, FArguments, (RVariables, RProcedures, RSPointer, Return), RCPArguments),
    append([CompiledPArgument, [swapd, const(SPointer), swapa, swapd, store], RCPArguments], CompiledPArguments),
    NEnvironment = ([(VName, SPointer) | RVariables], RProcedures, RSPointer, Return).

compile_procedure_arguments(Environment, [], [], Environment, []).

compile_complex_instruction(Environment, realise(Instruction, RCInstructions), CompiledCInstruction) :-
    !,
    compile_instruction(Environment, Instruction, CompiledInstruction),
    compile_complex_instruction(Environment, RCInstructions, CompiledRCInstructions),
    append(CompiledInstruction, CompiledRCInstructions, CompiledCInstruction).
compile_complex_instruction(Environment, realise(Instruction), CompiledCInstruction) :-
    compile_instruction(Environment, Instruction, CompiledCInstruction).

compile_instruction((Variables, _, _, _), read(variable(VName)), CompiledInstruction) :-
    member((VName, VAdress), Variables), 
    !,
    CompiledInstruction = [const(VAdress), swapa, syscall(1), store].

compile_instruction(Environment, write(AExpression), CompiledInstruction) :-
    !,
    compile_arithmetic_expression(Environment, AExpression, CompiledAExpression),
    append(CompiledAExpression, [swapd, syscall(2)], CompiledInstruction).

compile_instruction(Environment, call(Procedure), CompiledInstruction) :-
    !,
    compile_procedure(Environment, Procedure, CompiledProcedure),
    CompiledInstruction = CompiledProcedure.

compile_instruction((Variables, Procedures, SPointer, Return), return(AExpression), CompiledInstruction) :-
    !,
    compile_arithmetic_expression((Variables, Procedures, SPointer, Return), AExpression, CompiledAExpression),
    append(CompiledAExpression, [swapd, const(Return), jump], CompiledInstruction).

compile_instruction((Variables, Procedures, SPointer, Return), assign(variable(VName), AExpression), CompiledInstruction) :-
    !,
    compile_arithmetic_expression((Variables, Procedures, SPointer, Return), AExpression, CompiledAExpression),
    member((VName, VAdress), Variables),
    !,
    append(CompiledAExpression, [swapd, const(VAdress), swapa, swapd, store], CompiledInstruction).

compile_instruction(Environment, if(Bool, ThenPart), CompiledInstruction) :-
    !,
    compile_bool_expression(Environment, Bool, CompiledBool),
    compile_complex_instruction(Environment, ThenPart, CompiledCInstruction),
    append([CompiledBool, [swapd, const(FI), swapa, swapd, branchz], CompiledCInstruction, [label(FI)]], CompiledInstruction).

compile_instruction(Environment, if(Bool, ThenPart, ElsePart), CompiledInstruction) :-
    !,
    compile_bool_expression(Environment, Bool, CompiledBool),
    compile_complex_instruction(Environment, ThenPart, CompiledThenPart),
    compile_complex_instruction(Environment, ElsePart, CompiledElsePart),
    append([CompiledBool, [swapd, const(ELSE), swapa, swapd, branchz], CompiledThenPart, [const(FI), jump, label(ELSE)], CompiledElsePart, [label(FI)]], CompiledInstruction).

compile_instruction(Environment, while(Bool, Body), CompiledInstruction) :-
    !,
    compile_bool_expression(Environment, Bool, CompiledBool),
    compile_complex_instruction(Environment, Body, CompiledBody),
    append([[label(WHILE)], CompiledBool, [swapd, const(DONE), swapa, swapd, branchz], CompiledBody, [const(WHILE), jump, label(DONE)]], CompiledInstruction).

compile_arithmetic_expression((Variables, _, _, _), variable(VName), CompiledAExpression) :-
    member((VName, VAdress), Variables),
    !,
    CompiledAExpression = [const(VAdress), swapa, load].

compile_arithmetic_expression(_, constant(Integer), CompiledAExpression) :-
    !,
    CompiledAExpression = [const(Integer)].

compile_arithmetic_expression(Environment, procedure(PName, PArguments), CompiledAExpression) :-
    !,
    compile_procedure(Environment, procedure(PName, PArguments), CompiledProcedure),
    CompiledAExpression = CompiledProcedure.

compile_arithmetic_expression((Variables, Procedures, SPointer, Return), (Operation, AExpressionL, AExpressionR), CompiledAExpression) :-
    member((Operation, Commands),    [
                                        (plus,  [add]),
                                        (minus, [sub]),
                                        (times, [mul]),
                                        (div,   [div]),
                                        (mod,   [div, const(-16), swapd, shift])
                                    ]),
    !,
    compile_arithmetic_expression((Variables, Procedures, SPointer, Return), AExpressionL, CompiledAExpressionL),
    SPointer2 is SPointer - 1,
    compile_arithmetic_expression((Variables, Procedures, SPointer2, Return), AExpressionR, CompiledAExpressionR),
    append([CompiledAExpressionL, [swapd, const(SPointer), swapa, swapd, store], CompiledAExpressionR, [swapd, const(SPointer), swapa, load], Commands], CompiledAExpression).

compile_arithmetic_expression(Environment, (minus, AExpression), CompiledAExpression) :-
    !,
    compile_arithmetic_expression(Environment, AExpression, CAExpression),
    append(CAExpression, [swapd, const(0), sub], CompiledAExpression).


compile_bool_expression((Variables, Procedures, SPointer, Return), (Operation, AExpressionL, AExpressionR), CompiledBoolExpression) :-
    member((Operation, Commands),   [
                                        (eq,    [test(L0, L0), sub, swapd, const(L1), swapa, swapd, branchz, label(L0), const(0), swapd, const(L2), jump, label(L1), const(1), swapd, label(L2), swapd]),
                                        (neq,   [test(L0, L0), sub, swapd, const(L1), swapa, swapd, branchz, label(L0), const(1), swapd, const(L2), jump, label(L1), const(0), swapd, label(L2), swapd]),
                                        (lt,    [test(L1, L0), sub, swapd, const(L1), swapa, swapd, branchn, label(L0), const(0), swapd, const(L2), jump, label(L1), const(1), swapd, label(L2), swapd]),
                                        (leq,   [test(L0, L1), swapd, sub, swapd, const(L1), swapa, swapd, branchn, label(L0), const(1), swapd, const(L2), jump, label(L1), const(0), swapd, label(L2), swapd]),
                                        (gt,    [test(L0, L1), swapd, sub, swapd, const(L1), swapa, swapd, branchn, label(L0), const(0), swapd, const(L2), jump, label(L1), const(1), swapd, label(L2), swapd]),
                                        (geq,   [test(L1, L0), sub, swapd, const(L1), swapa, swapd, branchn, label(L0), const(1), swapd, const(L2), jump, label(L1), const(0), swapd, label(L2), swapd])
                                    ]),
    !,
    compile_arithmetic_expression((Variables, Procedures, SPointer, Return), AExpressionL, CompiledAExpressionL),
    SPointer2 is SPointer - 1,
    compile_arithmetic_expression((Variables, Procedures, SPointer2, Return), AExpressionR, CompiledAExpressionR),
    append([CompiledAExpressionL, [swapd, const(SPointer), swapa, swapd, store], CompiledAExpressionR, [swapd, const(SPointer), swapa, load], Commands], CompiledBoolExpression).


compile_bool_expression((Variables, Procedures, SPointer, Return), (Operation, BExpressionL, BExpressionR), CompiledBoolExpression) :-
    member((Operation, Commands),   [
                                        (or,    [add, swapd, const(L1), swapa, swapd, branchz, const(1), swapd, const(L2), jump, label(L1), const(0), swapd, label(L2), swapd]),
                                        (and,   [add, swapd, const(2), sub, swapd, const(L1), swapa, swapd, branchz, const(0), swapd, const(L2), jump, label(L1), const(1), swapd, label(L2), swapd])
                                    ]),
    !,
    compile_bool_expression((Variables, Procedures, SPointer, Return), BExpressionL, CompiledBExpressionL),
    SPointer2 is SPointer - 1,
    compile_bool_expression((Variables, Procedures, SPointer2, Return), BExpressionR, CompiledBExpressionR),
    append([CompiledBExpressionL, [swapd, const(SPointer), swapa, swapd, store], CompiledBExpressionR, [swapd, const(SPointer), swapa, load], Commands], CompiledBoolExpression).


compile_bool_expression(Environment, (not, BExpression), CompiledBoolExpression) :-
    !,
    compile_bool_expression(Environment, BExpression, CompiledBExpression),
    append(CompiledBExpression, [swapd, const(1), sub], CompiledBoolExpression).

/*
*
*   ASSEMBLER
*
*/

macro_assembler(MacroAssembler) -->
    [syscall(Code)],    !, { append([const(Code), syscall], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler)    |
    [label(Label)],     !, { append([label(Label)], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler)     |
    [const(Constant)],  !, { append([const(Constant)], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler)         |
    [test(R1, R2)],     !, { append([swapa, const(T1), swapa, branchn, swapa, const(T2), jump, label(T1), swapd, swapa, const(E), swapa, branchn, const(R1), jump, label(T2), const(R2), swapa, swapd, branchn, label(E), swapd], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler)    |
    [Command],          !, { append([Command], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler)                 |
    [],                 !, { MacroAssembler = [] }.


/* ------ */

post_macro_assembler([], PostMacroAssembler, Acc, N) :-
    \+ 0 is N mod 4, !, N2 is N + 1, append([nop], RPostMacroAssembler, PostMacroAssembler), post_macro_assembler([], RPostMacroAssembler, Acc, N2).

post_macro_assembler([label(Label) | MacroAssembler], PostMacroAssembler, Acc, N) :-
    \+ 0 is N mod 4, !, N2 is N + 1, append([nop], RPostMacroAssembler, PostMacroAssembler), post_macro_assembler([label(Label) | MacroAssembler], RPostMacroAssembler, Acc, N2).

post_macro_assembler(MacroAssembler, PostMacroAssembler, Acc, N) :-
    0 is N mod 4, \+ Acc = [], !, length(Acc, Length), N2 is N + 4 * Length, append(Acc, RPostMacroAssembler, PostMacroAssembler), post_macro_assembler(MacroAssembler, RPostMacroAssembler, [], N2).

post_macro_assembler([const(Constant) | MacroAssembler], PostMacroAssembler, Acc, N) :-
    !, append(Acc, [Constant], Acc2), N2 is N + 1, append([const], RPostMacroAssembler, PostMacroAssembler), post_macro_assembler(MacroAssembler, RPostMacroAssembler, Acc2, N2).

post_macro_assembler([label(Label) | MacroAssembler], PostMacroAssembler, Acc, N) :-
    !, Label is N div 4, post_macro_assembler(MacroAssembler, PostMacroAssembler, Acc, N).

post_macro_assembler([Command | MacroAssembler], PostMacroAssembler, Acc, N) :-
    !, N2 is N + 1, append([Command], RPostMacroAssembler, PostMacroAssembler), post_macro_assembler(MacroAssembler, RPostMacroAssembler, Acc, N2).

post_macro_assembler([], [], _, _).

/* ------ */

assembler(Assembler) -->
    [Command1], [Command2], [Command3], [Command4],   { Commands =  [
                                                                (nop,       0 ),
                                                                (syscall,   1 ),
                                                                (load,      2 ),
                                                                (store,     3 ),
                                                                (swapa,     4 ),
                                                                (swapd,     5 ),
                                                                (branchz,   6 ),
                                                                (branchn,   7 ),
                                                                (jump,      8 ),
                                                                (const,     9 ),
                                                                (add,       10 ),
                                                                (sub,       11 ),
                                                                (mul,       12 ),
                                                                (div,       13 ),
                                                                (shift,     14 ),
                                                                (nand,      15 )
                                                              ],
                                                  member((Command1, C1), Commands), !,
                                                  member((Command2, C2), Commands), !,
                                                  member((Command3, C3), Commands), !,
                                                  member((Command4, C4), Commands), !,
                                                  C is 16**3 * C1 + 16**2 * C2 + 16**1 * C3 + 16**0 * C4,
                                                  Assembler = [C | RAssembler]
                                                }, assembler(RAssembler)    |
    [Number], !, { convert_number(Number, CNumber), Assembler = [CNumber | RAssembler] }, assembler(RAssembler) |
    [], !, { Assembler = [] }.


convert_number(Number, CNumber) :-
    Number < 0, !, CNumber is 65536 + Number.

convert_number(Number, Number).



/*
*
*   ALGOL16
*
*/

algol16(Source, SextiumBin) :-
    phrase(lexer(TokList), Source),
    phrase(program_(Absynt), TokList),
    compile_program(Absynt, MacroAssembler),
    phrase(macro_assembler(PostMacroAssembler), MacroAssembler),
    post_macro_assembler(PostMacroAssembler, Assembler, [], 0),
    phrase(assembler(SextiumBin), Assembler).



/*
*
*   DEBUGGING
*
*/

algol16_file(File, SextiumBin) :-
    read_file_to_codes(File, Source, []),
    algol16(Source, SextiumBin).

algol16_string(String, SextiumBin) :-
    string_to_list(String, Source),
    algol16(Source, SextiumBin).

main :-
    current_prolog_flag(argv, Argv),
    Argv = [_, File|_],
    algol16_file(File, SextiumBin),
    write_list(SextiumBin),
    halt.


write_list([]) :- !.
write_list([H|T]) :-
    format(atom(Atom), '~`0t~16R~4| ', H), write(Atom), write_list(T).

