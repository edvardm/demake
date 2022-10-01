/* Assign lower precedence to NL. */
/* %precedence NL */
/* %precedence COMMENT "ifdef" "ifndef" "ifeq" "ifneq" */

makefile: statements "end of file"
        | "end of file"

statements: br
        | statement
        | statements br
        | statements statement

conditional: if_eq_kw condition statements_opt "endif" comment_opt br
        | if_eq_kw condition statements_opt "else" statements_opt "endif" comment_opt br
        | if_eq_kw condition statements_opt "else" conditional
        | if_def_kw identifier statements_opt "endif" comment_opt br
        | if_def_kw identifier statements_opt "else" statements_opt "endif" comment_opt br
        | if_def_kw identifier statements_opt "else" conditional
conditional_in_recipe: if_eq_kw condition recipes_opt "endif" comment_opt
                | if_eq_kw condition recipes_opt "else" recipes_opt "endif" comment_opt
                | if_eq_kw condition recipes_opt "else" conditional_in_recipe
                | if_def_kw identifier recipes_opt "endif" comment_opt
                | if_def_kw identifier recipes_opt "else" recipes_opt "endif" comment_opt
                | if_def_kw identifier recipes_opt "else" conditional_in_recipe
condition: '(' expressions_opt ',' expressions_opt ')'
        | SLIT SLIT
define: "define" pattern definition "endef" br
| specifiers "define" pattern definition "endef" br
| "define" pattern ASSIGN_OP definition "endef" br
| specifiers "define" pattern ASSIGN_OP definition "endef" br
definition: comment_opt br
        | comment_opt br exprs_in_def br
include: "include" expressions br
statements_opt: comment_opt br
        | comment_opt br statements
if_def_kw: "ifdef"
        | "ifndef"
if_eq_kw: "ifeq"
        | "ifneq"
statement: COMMENT
        | assignment br
        | function br
        | rule
        | conditional
        | define
        | include
        | export br
export: "export"
| "unexport"
| assignment_prefix
| assignment_prefix WS targets
assignment: pattern ASSIGN_OP comment_opt
        | pattern ASSIGN_OP exprs_in_assign comment_opt
        | assignment_prefix ASSIGN_OP comment_opt
        | assignment_prefix ASSIGN_OP exprs_in_assign comment_opt
assignment_prefix: specifiers pattern
specifiers: "override"
        | "export"
        | "unexport"
        | "override" "export"
        | "export" "override"
        | "undefine"
        | "override" "undefine"
        | "undefine" "override"
expressions_opt: %empty
                | expressions
expressions: expression
        | expressions WS expression
exprs_nested: expr_nested
        | exprs_nested WS expr_nested
exprs_in_assign: expr_in_assign
                | exprs_in_assign WS expr_in_assign
exprs_in_def: first_expr_in_def
        | br
        | br first_expr_in_def
        | exprs_in_def br
        | exprs_in_def WS expr_in_recipe
        | exprs_in_def br first_expr_in_def
first_expr_in_def: char_in_def expr_in_recipe
                | function expr_in_recipe
                | char_in_def
                | function
exprs_in_recipe: expr_in_recipe
                | exprs_in_recipe WS expr_in_recipe
expression: expression_text
        | expression_function
expr_nested: expr_text_nested
        | expr_func_nested
expr_in_assign: expr_text_in_assign
        | expr_func_in_assign
expr_in_recipe: expr_text_in_recipe
        | expr_func_in_recipe
expression_text: text
                | expression_function text
expr_text_nested: text_nested
                | expr_func_nested text_nested
expr_text_in_assign: text_in_assign
                | expr_func_in_assign text_in_assign
expr_text_in_recipe: text_in_recipe
                | expr_func_in_recipe text_in_recipe
expression_function: function
                | '(' exprs_nested ')'
                | expression_text function
                | expression_function function
expr_func_nested: function
                | '(' exprs_nested ')'
                | expr_func_nested function
                | expr_text_nested function
expr_func_in_assign: function
                | expr_func_in_assign function
                | expr_text_in_assign function
expr_func_in_recipe: function
                | expr_func_in_recipe function
                | expr_text_in_recipe function
function: VAR
        | "$(" function_name ")"
        | "$(" function_name WS arguments ")"
        | "$(" function_name ',' arguments ")"
        | "$(" function_name ':' expressions ")"
        | "$(" function_name ASSIGN_OP expressions ")"
function_name: function_name_text
        | function_name_function
function_name_text: function_name_piece
                | function_name_function function_name_piece
function_name_piece: CHARS
                | function_name_piece CHARS
function_name_function: function
                | function_name_text function
arguments: %empty
        | argument
        | arguments ','
        | arguments ',' argument
argument: expressions
rule: targets colon prerequisites NL
| targets colon prerequisites recipes NL
| targets colon assignment NL
target: pattern
pattern: pattern_text
        | pattern_function
pattern_text: identifier
        | pattern_function identifier
pattern_function: function
                | pattern_text function
                | pattern_function function
prerequisites: %empty
        | targets
targets: target
        | targets WS target
recipes: recipe
        | recipes recipe
recipes_opt: comment_opt NL
        | comment_opt recipes NL
recipe: LEADING_TAB exprs_in_recipe
| NL conditional_in_recipe
| NL COMMENT
identifier: CHARS
        | ','
        | '('
        | ')'
        | identifier CHARS
        | identifier keywords
        | identifier ','
        | identifier '('
        | identifier ')'
text: char
| text char
text_nested: char_nested
        | text_nested char_nested
text_in_assign: char_in_assign
        | text_in_assign char_in_assign
text_in_recipe: char_in_recipe
        | text_in_recipe char_in_recipe
char: CHARS
| SLIT
| ASSIGN_OP
| ':'
char_nested: char
        | ','
char_in_assign: char_nested
        | '('
        | ')'
        | keywords
char_in_def: char
        | '('
        | ')'
        | ','
        | COMMENT
        | "include"
        | "override"
        | "export"
        | "unexport"
        | "ifdef"
        | "ifndef"
        | "ifeq"
        | "ifneq"
        | "else"
        | "endif"
        | "define"
        | "undefine"
char_in_recipe: char_in_assign
        | COMMENT
keywords: "include"
        | "override"
        | "export"
        | "unexport"
        | "ifdef"
        | "ifndef"
        | "ifeq"
        | "ifneq"
        | "else"
        | "endif"
        | "define"
        | "endef"
        | "undefine"
br: NL
| LEADING_TAB
colon: ':'
| ':' ':'
comment_opt: %empty
        | COMMENT
