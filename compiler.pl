
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

/* ========================== */

program_(Ast) -->
    [tokProgram], [tokIdentifier(PName)], block_(Block), { Ast = program(PName, Block) }.

block_(Block) -->
    declarations_(Declarations), [tokBegin], complex_instruction_(CInstruction), [tokEnd], { Block = block(Declarations, CInstruction) }.

declarations_(Declarations) -->
    declaration_(Declaration),      !, { append(Declaration, RDeclarations, Declarations) }, declarations(RDeclarations) |
    [],                             !, { Declarations = [] }.

declaration_(Declaration) -->
    declarator_(Declaration).

declarator_(Variables) -->
    [tokLocal], variables_(Variables).

variables_(Variables) -->
    variable_(Variable), (
                            [tokComma], !, { Variables = [Variable | RVariables] }, variables_(RVariables) ;
                                        !, { Variables = [Variable] }
                         ).

variable_(variable(VName)) -->
    [tokIdentifier(VName)].

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
    [tokRead],                          !, variable_(Variable), { Instruction = read_(Variable) } |
    [tokWrite],                         !, arithmetic_expression_(AExpression), { Instruction = write_(AExpression) }.


arithmetic_expression_(AExpression) -->
    summand_(Summand), arithmetic_expression_(Summand, AExpression).

arithmetic_expression_(Acc, AExpression) -->
    additive_operator_(AOperator),  !, summand_(Summand), { Acc1 = (AOperator, Acc, Summand) }, arithmetic_expression_(Acc1, AExpression) |
    [],                             !, { Acc = AExpression }.

additive_operator_(Operator) -->
    [tokPlus],      !, { Operator = plus    } |
    [tokMinus],     !, { Operator = minus   }.

summand_(SExpression) -->
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
    variable_(AExpression),     ! |
    [tokInteger(Integer)],      !, { AExpression = constant(Integer) }.


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
    [tokLParen], !, bool_expression_(RExpression), [tokRParen] |
    arithmetic_expression_(AExpressionL), relative_operator_(ROperator), arithmetic_expression_(AExpressionR), { RExpression = (ROperator, AExpressionL, AExpressionR) }.

relative_operator_(Operator) -->
    [tokEq],    !, { Operator = eq  } |
    [tokNeq],   !, { Operator = neq } |
    [tokLt],    !, { Operator = lt  } |
    [tokLeq],   !, { Operator = leq } |
    [tokGt],    !, { Operator = gt  } |
    [tokGeq],   !, { Operator = geq }.


/* ========================== */

compile_program(program(_, Block), CompiledProgram) :-
    compile_block(Block, CompiledProgram).

compile_block(block(Declarations, CInstruction), CompiledBlock) :-
    compile_declarations(Declarations, Environment),
    compile_complex_instruction(Environment, CInstruction, CompiledBlock).

compile_declarations([variable(VName) | Declarations], (Variables, SPointer)) :-
    !,
    compile_declarations(Declarations, (RVariables, RSPointer)),
    Variables = [(VName, RSPointer) | RVariables],
    SPointer is RSPointer - 1.

compile_declarations([], ([], 65535)).


compile_complex_instruction(Environment, realise(Instruction, RCInstructions), CompiledCInstruction) :-
    !,
    compile_instruction(Environment, Instruction, CompiledInstruction),
    compile_complex_instruction(Environment, RCInstructions, CompiledRCInstructions),
    append(CompiledInstruction, CompiledRCInstructions, CompiledCInstruction).
compile_complex_instruction(Environment, realise(Instruction), CompiledCInstruction) :-
    compile_instruction(Environment, Instruction, CompiledCInstruction).

compile_instruction((Variables, _), read_(variable(VName)), CompiledInstruction) :-
    !,
    member((VName, VAdress), Variables),
    CompiledInstruction = [const(VAdress), swapa, syscall(1), store].

compile_instruction(Environment, write_(AExpression), CompiledInstruction) :-
    !,
    compile_arithmetic_expression(Environment, AExpression, CompiledAExpression),
    append(CompiledAExpression, [swapd, syscall(2)], CompiledInstruction).

compile_instruction((Variables, SPointer), assign(variable(VName), AExpression), CompiledInstruction) :-
    !,
    compile_arithmetic_expression((Variables, SPointer), AExpression, CompiledAExpression),
    member((VName, VAdress), Variables),
    append(CompiledAExpression, [swapd, const, VAdress, swapa, swapd, store], CompiledInstruction).

compile_arithmetic_expression((Variables, _), variable(VName), CompiledAExpression) :-
    !,
    member((VName, VAdress), Variables),
    CompiledAExpression = [const(VAdress), swapa, load].

compile_arithmetic_expression(_, constant(Integer), CompiledAExpression) :-
    !,
    CompiledAExpression = [const(Integer)].

compile_arithmetic_expression((Variables, SPointer), (Operation, AExpressionL, AExpressionR), CompiledAExpression) :-
    member((Operation, Commands),    [
                                        (plus,  [add]),
                                        (minus, [sub]),
                                        (times, [mul]),
                                        (div,   [div]),
                                        (mod,   [div, const(16), swapd, const(0), sub, shift])
                                    ]),
    !,
    compile_arithmetic_expression((Variables, SPointer), AExpressionL, CompiledAExpressionL),
    SPointer2 is SPointer - 1,
    compile_arithmetic_expression((Variables, SPointer2), AExpressionR, CompiledAExpressionR),
    append([CompiledAExpressionL, [swapd, const(SPointer), swapa, swapd, store], CompiledAExpressionR, [swapd, const(SPointer), swapa, load], Commands], CompiledAExpression).

compile_arithmetic_expression(Environment, (minus, AExpression), CompiledAExpression) :-
    !,
    compile_arithmetic_expression(Environment, AExpression, CAExpression),
    append(CAExpression, [swapd, const(0), sub], CompiledAExpression).

compile_bool_expression.


/* ========================== */

macro_assembler(MacroAssembler) -->
    [const(Constant)],  !, { append([const, Constant], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler)         |
    [syscall(Code)],    !, { append([const, Code, syscall], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler)    |
    [store(Register)],  !, { member((Register, Adress), [(r1, 65535), (r2, 65534), (r3, 65533), (sp, 65532)]), append([swapd, const, Adress, swapa, swapd, store], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler) |
    [load(Register)],   !, { member((Register, Adress), [(r1, 65535), (r2, 65534), (r3, 65533), (sp, 65532)]), append([const, Adress, swapa, load], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler) |
    [push],             !, { append([swapd, const, 65532, swapa, load, swapd, swapa, const, 1, swapd, sub, swapa, store, swapd, const, 65532, swapa, store], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler) |
    [pop],              !, { append([const, 65532, swapa, load, swapa, load, swapa, swapd, const, 1, add, swapd, const, 65532, swapa, swapd, store, swapd], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler) |
    [Command],          !, { append([Command], RMacroAssembler, MacroAssembler) }, macro_assembler(RMacroAssembler)                 |
    [],                 !, { MacroAssembler = [] }.

assembler(Assembler) -->
    [Command], !, {(
                    member((Command, Symbol),   [
                                                (syscall,   "SYSCALL NOP NOP NOP "   ),
                                                (load,      "LOAD NOP NOP NOP "      ),
                                                (store,     "STORE NOP NOP NOP "     ),
                                                (swapa,     "SWAPA NOP NOP NOP "     ),
                                                (swapd,     "SWAPD NOP NOP NOP "     ),
                                                (branchz,   "BRANCHZ NOP NOP NOP "   ),
                                                (branchn,   "BRANCHN NOP NOP NOP "   ),
                                                (jump,      "JUMP NOP NOP NOP "      ),
                                                (const,     "CONST NOP NOP NOP "     ),
                                                (add,       "ADD NOP NOP NOP "       ),
                                                (sub,       "SUB NOP NOP NOP "       ),
                                                (mul,       "MUL NOP NOP NOP "       ),
                                                (div,       "DIV NOP NOP NOP "       ),
                                                (shift,     "SHIFT NOP NOP NOP "     )
                                            ]), !;
                    format(atom(Atom), '~`0t~16r~4| ', [Command]), atom_string(Atom, Symbol)
                  )},
    assembler(RAssembler), { string_concat(Symbol, RAssembler, Assembler) } |
    [], { Assembler = "" }.

/* ========================== */

algol16(Source, SextiumBin) :-
    phrase(lexer(TokList), Source),
    phrase(program_(Absynt), TokList),
    compile_program(Absynt, CompiledProgram),
    SextiumBin = CompiledProgram.

/* ========================== */

algol16_file(File, SextiumBin) :-
    read_file_to_codes(File, Source, []),
    algol16(Source, SextiumBin).

/* ========================== */

test(String, Assembler) :-
    string_to_list(String, L),
    phrase(lexer(T), L),
    phrase(complex_instruction_(I), T),
    compile_complex_instruction(([(a, 65535)], 65534), I, PMA),
    append(PMA, [syscall(0)], PMA2),
    phrase(macro_assembler(MA), PMA2),
    phrase(assembler(Assembler), MA).
