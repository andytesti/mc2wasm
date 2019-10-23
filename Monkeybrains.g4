/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

grammar Monkeybrains;

IfToken : 'if';
ElseToken : 'else';
WhileToken : 'while';
DoToken : 'do';
ForToken : 'for';
VarToken : 'var';
ConstToken: 'const';
ClassToken: 'class';
FunctionToken: 'function';
ModuleToken: 'module';
EnumToken: 'enum';
SwitchToken: 'switch';
DefaultToken: 'default';
CaseToken: 'case';
InstanceOf: 'instanceof';
Has: 'has';
TryToken: 'try';
CatchToken: 'catch';
FinallyToken: 'finally';
ThrowToken: 'throw';
BlingToken: '$';

//Num : [-]?[0-9]+ | '0x'[0-9a-fA-F]+;

HexNumber: '0x'[0-9a-fA-F]+[lL]?;
OctalNumber: [0][0-9]*[lL]?;
FloatNumber
    : ( ([0])('f'|'d')? | ([1-9][0-9]*))([\\.][0-9]+)?FloatExponent?('f'|'d')?
    | [\\.] [0-9]+ FloatExponent?('f'|'d')?
    ;
fragment FloatExponent: [Ee][+-]?[0-9]+;
Id: ([a-zA-Z_]|'\u0080'..'\ufffe')([a-zA-Z0-9_]|'\u0080'..'\ufffe')*;
WS : [ \r\n\t]+ -> skip;
fragment NEWLINE: '\r' '\n' | '\n' | '\r';
DoxyComment: '//!' ~[\r\n]* '\r'? (NEWLINE | EOF) -> channel(HIDDEN);
Comment: '//' ~[\r\n]* '\r'? (NEWLINE | EOF) -> skip ;
MultiLineComment: '/*' .*? '*/' -> skip;
String: '"' ( EscapeSequence | ~[\\"] )* '"';
Char: '\'' ( EscapeSequence | ~[\\"] ) '\'';
fragment HexDigit: [0-9a-fA-F];
fragment EscapeSequence: '\\' [nrtbf"'\\] | UnicodeSequence;
fragment UnicodeSequence : '\\' 'u' HexDigit HexDigit HexDigit HexDigit;

/*
 * A program assumes you are defining the global object, where you are
 * definiing functions, variables, classes, and modules.
 */
program : moduleDeclaration * EOF;

/*
 * Modules are objects that are instantiated at startup.
 */
moduleDeclaration
    : annotations? flags=declarationFlags* (classdef | moduledef | functiondef | enumdef | variabledef )
    | usingdef
    ;
/*
 * A module is a class that automaticall instantiates at runtime
 * it is defined as
 * module Id
 * {
 *    (class/variable/enum/function definition) *
 * }
 */
moduledef
    : ModuleToken id=symbol '{' moduleDeclaration* '}'
    ;
/*
 * A hidden field is not visible from outside the field.
 * A final field is constant.
 */
declarationFlags
    : 'hidden'
    | 'static'
    | 'native'
    | 'public'
    | 'private'
    | 'protected'
    ;
/*
 * Attribution is a way of associating symbols with a class member. It allows for
 * communication with the compiler without adding new types or keywords, opening a way
 * to add new features without changing the language
 */
annotations
    : '(' (symbolref (',')?)* ')'
    ;
/*
 * A class is defined as
 * class ID (extends ClassId)
 * {
 *   (var/function/class/enum) *
 * }
 */
classdef
    : ClassToken id=symbol ( 'extends' extendsValue=moduleId )? '{' classDeclaration* '}'
    ;
/*
 * Classes are objects. You cannot define a module inside a class
 */
classDeclaration
    : annotations? flags=declarationFlags* (classdef | functiondef | variabledef | enumdef )
    | usingdef
    ;
/*
 * An enum is a module without functions and everything is final. It is defined as
 * enum Id
 * {
 *  symbol ( = initializer )?
 * }
 */
enumDeclaration
    : id=symbol  ( '=' initializer=variabledefInitializers)?
    ;
enumdef
    : EnumToken '{' enumDeclaration (',' enumDeclaration)* ','? '}'
    ;
/*
 * Functions are defined as
 * function Id ( Param, param...)
 * {
 *   statements*
 * }
 */
functiondef
    : FunctionToken id=symbol paramDecl (codeBlock | ';')
    ;
paramDecl
    : '(' ')'
    | '('  Id ( ',' Id )*  ')'
    ;
/*
 * Define a variable in a class
 */
variabledef
    : type=VarToken variabledefPair ( ',' variabledefPair ) * ';'
    | type=ConstToken variabledefPair ( ',' variabledefPair ) * ';'
    ;
variabledefPair
    : id=symbol ( '=' initializer=variabledefInitializers )?
    ;
variabledefInitializers
    : number
    | string
    | expression
    ;
usingdef
    : 'using' usingModule=moduleId ('as' name=symbol)? ';'
    ;
moduleId
    : symbol
    | moduleId '.' symbol
    ;
/*
 * The statements are the high level operations in a function.
 */
statement
    : assignment ';'
    | procedureCall ';'
    | varDeclaration ';'
    | ifStatement
    | whileStatement
    | doStatement ';'
    | forStatement
    | switchStatement
    | breakStatement ';'
    | continueStatement ';'
    | throwStatement ';'
    | tryStatement
    | returnStatement
    ;
/* Assignments are setting one value equal to another */
assignment
    : lvalue op=assignmentOperator expression
    | incrDecr lvalue
    | lvalue incrDecr
    ;
/* Assignment Operators */
assignmentOperator
    : '='
    | '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '>>='
    | '<<='
    | '&='
    | '^='
    | '|='
    ;
/* Handle pre and post increment/decrement */
incrDecr
    : '++'
    | '--'
    ;
/* Procedure calls are when a call is made without capturing it's return value */
procedureCall
    : factor
    ;
/* lValues are the left hand value of an assignment */
lvalue
    : (BlingToken op='.')? symbol lvalueExtension?
    | (BlingToken op='.')? symbol invocation lvalueExtension
    ;
lvalueExtension
    : op='.' symbol lvalueExtension?
    | op='[' expression ']' lvalueExtension?
    | op='.' symbol invocation lvalueExtension
    ;
/* Variable definitions.
 * def Id = x;
 */
varDeclaration
    : VarToken varDef ( ',' varDef )*
    ;
varDef
    : Id ( '=' expression )?
    ;
/* If statements */
ifStatement
    : IfToken '(' expression ')' ifCase = codeBlock ( ElseToken ( elseIfCase=ifStatement | elseCase=codeBlock ) )?
    ;
/* While statement */
whileStatement
    :  WhileToken '(' expression')' whileCode=codeBlock
    ;
/* Do/while statement */
doStatement
    : DoToken whileCode=codeBlock WhileToken '(' expression ')'
    ;
/* For loops */
forStatement
    : ForToken '(' initialize=forInitialize? ';' test=expression? ';' increment=forIncrement? ')' forCode=codeBlock
    ;
forInitialize
    : assignment ( ',' assignment )*
    | varDeclaration
    ;
forIncrement
    : assignment ( ',' assignment )*
    ;
/* Break */
breakStatement
    : 'break'
    ;
/* Continue */
continueStatement
    : 'continue'
    ;
/* Switch/case statements */
switchStatement
    : SwitchToken '(' expression ')' '{' ( caseStatement )+ '}'
    ;
caseStatement
    : CaseToken expression ':' caseStmt = statementSequenceEntry
    | CaseToken InstanceOf lvalue ':' caseStmt = statementSequenceEntry
    | DefaultToken ':' caseStmt = statementSequenceEntry
    ;
/* Return values */
returnStatement
    : 'return' op=expression? ';'
    ;
/* Code blocks */
statementSequenceEntry
    : statementSequence?
    | codeBlock
    ;
statementSequence
    : statement statementSequence?
    | codeBlock statementSequence?
    ;
codeBlock
    :  '{' '}'
    | '{' statementSequence '}'
    ;
/* try/catch/finally blocks */
tryStatement
    : tryBlock catchTypeBlock+ catchBlock? finallyBlock?
    | tryBlock catchTypeBlock* catchBlock finallyBlock?
    | tryBlock catchTypeBlock* catchBlock? finallyBlock
    ;
tryBlock: TryToken codeBlock;
catchTypeBlock: CatchToken '(' symbol InstanceOf lvalue ')' codeBlock;
catchBlock: CatchToken '(' symbol ')' codeBlock;
finallyBlock: FinallyToken codeBlock;
/* throw statement */
throwStatement: ThrowToken ( lvalue | creation );


/*
 * Expressions are mathematical, logical, or other operation.
 */
expression
    : conditionalExpression
    ;
conditionalExpression
    : conditionalOrExpression
    | conditionalOrExpression op='?' expression ':' conditionalExpression
    ;
conditionalOrExpression
    : conditionalAndExpression
    | conditionalOrExpression op=('||' | 'or') conditionalOrExpression
    ;
conditionalAndExpression
    : equalityExpression
    | conditionalAndExpression op=('&&' | 'and') conditionalAndExpression
    ;
equalityExpression
    : simpleExpression ( op=expressionOps simpleExpression)*
    ;
expressionOps
    : '=='
    | '<'
    | '<='
    | '>'
    | '>='
    | '!='
    ;
simpleExpression
    : pn=( '+' | '-')? term ( simpleExpressionOps term )*
    ;
simpleExpressionOps
    : '+'
    | '-'
    | '|'
    | '^'
    | 'instanceof'
    ;
term
    : factor ( op=termOps factor )*
    ;
termOps: '*'
    | '/'
    | '%'
    | '&'
    | '<<'
    | '>>'
    | InstanceOf
    | Has
    ;
factor
    : primary
    | op='~' factor
    | op='!' factor
    ;
primary
    : literal
    | '(' expression ')'
    | symbol invocation
    | primary op='.' symbol invocation?
    | primary arrayAccess
    | creation
    ;
literal
    : symbol
    | symbolref
    | number
    | string
    | bool
    | nullReference
    | character
    | BlingToken
    ;
invocation
    : '(' argc+=expression (',' argc+=expression)* ')'
    | '(' ')'
    ;
arrayAccess
    : '[' expression ']'
    ;
keyValuePair
    : key=expression '=>' value=expression
    ;
creation
    : op='new' lvalue invocation
    | op='new' '[' expression ']'
    | op='new' '[' expression ']b'
    | op='[' (expression ',')* expression? ']'
    | op='[' (expression ',')* expression? ']b'
    | op='{' (keyValuePair ',')* keyValuePair? '}'
    ;
bool
    : 'true'
    | 'false'
    ;
nullReference
    : 'null'
    ;
string
    : String
    ;
character
    : Char
    ;
number
    : signValue=sign? integerLiteral
    | signValue=sign? floatLiteral
    ;
symbol
    : Id
    ;
symbolref
    : ':' symbolrefId
    ;
symbolrefId
    : Id
    | Id '(' symbol ')'
    | Id '(' string ')'
    | Id '(' bool ')'
    | Id '(' nullReference ')'
    | Id '(' number ')'
    | Id '(' character ')'
    | symbolrefIdArray
    ;
symbolrefIdArray
    : Id '(' '[' symbol (',' symbol)* ']' ')'
    ;
sign
    : '+'
    | '-'
    ;
integerLiteral
    : IntNumber
    | HexNumber
    | OctalNumber
    ;

floatLiteral
    : FloatNumber
    | 'NaN'
    ;
