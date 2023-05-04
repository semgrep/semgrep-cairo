(* Generated by ocaml-tree-sitter. *)
(*
   cairo grammar

   entrypoint: source_file
*)

open! Sexplib.Conv
open Tree_sitter_run

type number_suffix = Token.t (* pattern _[a-z][a-z0-9]+ *)

type unit_type = Token.t

type binary = Token.t (* pattern 0b[01]+ *)

type integer = Token.t (* pattern [0-9]+ *)

type name = Token.t (* pattern [a-zA-Z][a-zA-Z0-9_]* *)

type semgrep_var = Token.t (* pattern \$[A-Z_][A-Z_0-9]* *)

type octal = Token.t (* pattern 0o[0-7]+ *)

type hex = Token.t (* pattern 0x[0-9a-f]+ *)

type string_ = Token.t

type path = (
    name (*tok*)
  * (Token.t (* "::" *) * name (*tok*)) list (* zero or more *)
  * Token.t (* "::" *) option
)

type modifier = [
    `Modi_ref of Token.t (* "ref" *)
  | `Modi_mut of Token.t (* "mut" *)
]

type pattern_var = [ `Wild of Token.t (* "_" *) | `Name of name (*tok*) ]

type literal_expression = [
    `True of Token.t (* "true" *)
  | `False of Token.t (* "false" *)
  | `Num of (
        [
            `Int of integer (*tok*)
          | `Hex of hex (*tok*)
          | `Octal of octal (*tok*)
          | `Bin of binary (*tok*)
        ]
      * number_suffix (*tok*) option
    )
  | `Str of string_ (*tok*)
  | `Unit of (Token.t (* "(" *) * Token.t (* ")" *))
]

type type_argument_list = (
    Token.t (* "<" *)
  * literal_expression list (* zero or more *)
  * Token.t (* ">" *)
)

type attribute_argument = [
    `Choice_true of literal_expression
  | `Name of name (*tok*)
  | `Path_COLON_choice_true of (
        path * Token.t (* ":" *) * literal_expression
    )
  | `Path_COLON_attr_arg_list of (
        path * Token.t (* ":" *) * attribute_argument_list
    )
]

and attribute_argument_list = (
    Token.t (* "(" *)
  * attribute_argument
  * (Token.t (* "," *) * attribute_argument) list (* zero or more *)
  * Token.t (* "," *) option
  * Token.t (* ")" *)
)

type qualified_name_segment = (name (*tok*) * type_argument_list option)

type attribute = (path * attribute_argument_list option)

type qualified_name = (
    (qualified_name_segment * Token.t (* "::" *)) list (* zero or more *)
  * name (*tok*)
  * (Token.t (* "::" *) * type_argument_list) option
)

type attribute_list = (
    Token.t (* "#" *)
  * Token.t (* "[" *)
  * attribute list (* one or more *)
  * Token.t (* "]" *)
)

type pattern = [
    `Choice_wild of pattern_var
  | `Pat_struct of (
        qualified_name
      * Token.t (* "{" *)
      * pattern_struct_binding
      * (Token.t (* "," *) * pattern_struct_binding) list (* zero or more *)
      * Token.t (* "," *) option
      * Token.t (* "}" *)
    )
  | `Pat_enum of (
        qualified_name
      * Token.t (* "(" *)
      * pattern
      * (Token.t (* "," *) * pattern) list (* zero or more *)
      * Token.t (* "," *) option
      * Token.t (* ")" *)
    )
  | `Pat_tuple of (
        Token.t (* "(" *)
      * pattern
      * (Token.t (* "," *) * pattern) list (* zero or more *)
      * Token.t (* "," *) option
      * Token.t (* ")" *)
    )
]

and pattern_struct_binding = [
    `Name of name (*tok*)
  | `Name_COLON_choice_choice_wild of (
        name (*tok*) * Token.t (* ":" *) * pattern
    )
]

type type_ = [
    `Type_tuple of (
        Token.t (* "(" *)
      * (type_ * Token.t (* "," *)) list (* zero or more *)
      * type_ option
      * Token.t (* ")" *)
    )
  | `Unit_type of unit_type (*tok*)
  | `Type_id of (qualified_name * type_parameters option)
]

and type_parameters = (
    Token.t (* "<" *)
  * (type_ * Token.t (* "," *)) list (* zero or more *)
  * type_ option
  * Token.t (* ">" *)
)

type match_case_pattern = [
    `Choice_choice_wild of pattern
  | `Choice_true of literal_expression
]

type member_declaration = [
    `Rep_attr_list_name_COLON_choice_type_tuple of (
        attribute_list list (* zero or more *)
      * name (*tok*)
      * Token.t (* ":" *)
      * type_
    )
  | `Ellips of Token.t (* "..." *)
]

type parameter_declaration = (
    modifier list (* zero or more *)
  * name (*tok*)
  * Token.t (* ":" *)
  * type_
)

type argument_list = (
    Token.t (* "(" *)
  * (expression * Token.t (* "," *)) list (* zero or more *)
  * expression option
  * Token.t (* ")" *)
)

and binary_expression = [
    `Choice_tuple_exp_choice_STAR_choice_tuple_exp of (
        expression
      * [
            `STAR of Token.t (* "*" *)
          | `SLASH of Token.t (* "/" *)
          | `PERC of Token.t (* "%" *)
        ]
      * expression
    )
  | `Choice_tuple_exp_choice_PLUS_choice_tuple_exp of (
        expression
      * [ `PLUS of Token.t (* "+" *) | `DASH of Token.t (* "-" *) ]
      * expression
    )
  | `Choice_tuple_exp_choice_EQEQ_choice_tuple_exp of (
        expression
      * [
            `EQEQ of Token.t (* "==" *)
          | `BANGEQ of Token.t (* "!=" *)
          | `LT of Token.t (* "<" *)
          | `LTEQ of Token.t (* "<=" *)
          | `GT of Token.t (* ">" *)
          | `GTEQ of Token.t (* ">=" *)
        ]
      * expression
    )
  | `Choice_tuple_exp_AMPAMP_choice_tuple_exp of (
        expression * Token.t (* "&&" *) * expression
    )
  | `Choice_tuple_exp_BARBAR_choice_tuple_exp of (
        expression * Token.t (* "||" *) * expression
    )
]

and block = (
    Token.t (* "{" *)
  * statement list (* zero or more *)
  * expression option
  * Token.t (* "}" *)
)

and call_expression = ([ `Choice_tuple_exp of expression ] * argument_list)

and expression = [
    `Tuple_exp of tuple_expression
  | `Blk of block
  | `Un_exp of unary_expression
  | `Bin_exp of binary_expression
  | `If_exp of if_expression
  | `Loop_exp of loop_expression
  | `Match_exp of match_expression
  | `Sele_exp of selector_expression
  | `Call_exp of call_expression
  | `Qual_name of qualified_name
  | `Choice_true of literal_expression
  | `Semg_var of semgrep_var (*tok*)
  | `Ellips of Token.t (* "..." *)
  | `Deep_ellips of (
        Token.t (* "<..." *) * expression * Token.t (* "...>" *)
    )
]

and if_expression = (
    Token.t (* "if" *)
  * simple_expression
  * block
  * (Token.t (* "else" *) * [ `If_exp of if_expression | `Blk of block ])
      option
)

and loop_expression = (
    Token.t (* "loop" *) * Token.t (* "{" *) * expression * Token.t (* "}" *)
)

and match_case = (match_case_pattern * Token.t (* "=>" *) * expression)

and match_cases = (
    Token.t (* "{" *)
  * (match_case * Token.t (* "," *)) list (* zero or more *)
  * match_case option
  * Token.t (* "}" *)
)

and match_expression = (Token.t (* "match" *) * expression * match_cases)

and selector_expression = (expression * Token.t (* "." *) * name (*tok*))

and simple_expression = [
    `Tuple_exp of tuple_expression
  | `Un_exp of unary_expression
  | `Bin_exp of binary_expression
  | `If_exp of if_expression
  | `Loop_exp of loop_expression
  | `Match_exp of match_expression
  | `Sele_exp of selector_expression
  | `Call_exp of call_expression
  | `Qual_name of qualified_name
  | `Choice_true of literal_expression
]

and statement = [
    `Let_stmt of (
        Token.t (* "let" *)
      * Token.t (* "mut" *) option
      * pattern
      * (Token.t (* ":" *) * type_) option
      * Token.t (* "=" *)
      * expression
      * Token.t (* ";" *)
    )
  | `Assign_stmt of (
        [ `Name of name (*tok*) | `Wild of Token.t (* "_" *) ]
      * [
            `STAREQ of Token.t (* "*=" *)
          | `SLASHEQ of Token.t (* "/=" *)
          | `PERCEQ of Token.t (* "%=" *)
          | `PLUSEQ of Token.t (* "+=" *)
          | `DASHEQ of Token.t (* "-=" *)
          | `EQ of Token.t (* "=" *)
        ]
      * expression
      * Token.t (* ";" *)
    )
  | `Ret_stmt of (
        Token.t (* "return" *)
      * expression option
      * Token.t (* ";" *)
    )
  | `Brk_stmt of (
        Token.t (* "break" *)
      * expression option
      * Token.t (* ";" *)
    )
  | `If_exp of if_expression
  | `Loop_exp of loop_expression
  | `Match_exp of match_expression
  | `Choice_tuple_exp_SEMI of (expression * Token.t (* ";" *))
]

and tuple_expression = (
    Token.t (* "(" *)
  * expression
  * (Token.t (* "," *) * expression) list (* zero or more *)
  * Token.t (* "," *) option
  * Token.t (* ")" *)
)

and unary_expression = (
    [
        `BANG of Token.t (* "!" *)
      | `STAR of Token.t (* "*" *)
      | `DASH of Token.t (* "-" *)
    ]
  * expression
)

type member_declaration_list = (
    Token.t (* "{" *)
  * (member_declaration * Token.t (* "," *)) list (* zero or more *)
  * member_declaration option
  * Token.t (* "}" *)
)

type parameter_list = (
    Token.t (* "(" *)
  * (parameter_declaration * Token.t (* "," *)) list (* zero or more *)
  * parameter_declaration option
  * Token.t (* ")" *)
)

type type_parameter_declaration = [
    `Name of name (*tok*)
  | `Type_const_decl of (
        Token.t (* "const" *) * name (*tok*) * Token.t (* ":" *) * type_
    )
  | `Type_impl_decl of (
        Token.t (* "impl" *) * name (*tok*) * Token.t (* ":" *) * type_
    )
]

type type_parameter_list = (
    Token.t (* "<" *)
  * type_parameter_declaration
  * (Token.t (* "," *) * type_parameter_declaration) list (* zero or more *)
  * Token.t (* "," *) option
  * Token.t (* ">" *)
)

type function_signature = (
    attribute_list list (* zero or more *)
  * Token.t (* "fn" *)
  * name (*tok*)
  * type_parameter_list option
  * parameter_list
  * (Token.t (* "->" *) * type_) option
)

type trait_function = (
    function_signature
  * [ `Opt_blk of block option | `SEMI of Token.t (* ";" *) ]
)

type trait_body = (
    Token.t (* "{" *)
  * trait_function list (* zero or more *)
  * Token.t (* "}" *)
)

type declaration = [
    `Import_decl of (
        Token.t (* "use" *)
      * path
      * (Token.t (* "as" *) * name (*tok*)) option
      * Token.t (* ";" *)
    )
  | `Module_decl of (
        attribute_list list (* zero or more *)
      * Token.t (* "mod" *)
      * name (*tok*)
      * [ `SEMI of Token.t (* ";" *) | `Module_body of module_body ]
    )
  | `Typeas_decl of (
        Token.t (* "type" *)
      * name (*tok*)
      * type_parameter_list option
      * Token.t (* "=" *)
      * type_
      * Token.t (* ";" *)
    )
  | `Const_decl of (
        attribute_list list (* zero or more *)
      * Token.t (* "const" *)
      * name (*tok*)
      * Token.t (* ":" *)
      * type_
      * Token.t (* "=" *)
      * expression
      * Token.t (* ";" *)
    )
  | `Trait_decl of (
        Token.t (* "trait" *)
      * name (*tok*)
      * type_parameter_list option
      * trait_body
    )
  | `Struct_decl of (
        attribute_list list (* zero or more *)
      * Token.t (* "struct" *)
      * name (*tok*)
      * type_parameter_list option
      * [
            `Member_decl_list of member_declaration_list
          | `SEMI of Token.t (* ";" *)
        ]
    )
  | `Enum_decl of (
        attribute_list list (* zero or more *)
      * Token.t (* "enum" *)
      * name (*tok*)
      * type_parameter_list option
      * member_declaration_list
    )
  | `Choice_impl_base of impl_declaration
  | `Func_decl of (function_signature * block)
  | `Ellips of Token.t (* "..." *)
]

and impl_body = (
    Token.t (* "{" *)
  * declaration list (* zero or more *)
  * Token.t (* "}" *)
)

and impl_declaration = [
    `Impl_base of (
        attribute_list list (* zero or more *)
      * Token.t (* "impl" *)
      * name (*tok*)
      * type_parameter_list option
      * impl_body
    )
  | `Impl_trait of (
        attribute_list list (* zero or more *)
      * Token.t (* "impl" *)
      * name (*tok*)
      * type_parameter_list option
      * Token.t (* "of" *)
      * qualified_name
      * impl_body
    )
]

and module_body = (
    Token.t (* "{" *)
  * declaration list (* zero or more *)
  * Token.t (* "}" *)
)

type source_file = [
    `Rep_choice_import_decl of declaration list (* zero or more *)
  | `Semg_exp of (Token.t (* "__SEMGREP_EXPRESSION" *) * expression)
  | `Semg_stmt of (
        Token.t (* "__SEMGREP_STATEMENT" *)
      * statement list (* one or more *)
    )
]

type ellipsis (* inlined *) = Token.t (* "..." *)

type modifier_mut (* inlined *) = Token.t (* "mut" *)

type unit_ (* inlined *) = (Token.t (* "(" *) * Token.t (* ")" *))

type modifier_ref (* inlined *) = Token.t (* "ref" *)

type wildcard (* inlined *) = Token.t (* "_" *)

type comment (* inlined *) = Token.t

type false_ (* inlined *) = Token.t (* "false" *)

type true_ (* inlined *) = Token.t (* "true" *)

type number (* inlined *) = (
    [
        `Int of integer (*tok*)
      | `Hex of hex (*tok*)
      | `Octal of octal (*tok*)
      | `Bin of binary (*tok*)
    ]
  * number_suffix (*tok*) option
)

type import_declaration (* inlined *) = (
    Token.t (* "use" *)
  * path
  * (Token.t (* "as" *) * name (*tok*)) option
  * Token.t (* ";" *)
)

type pattern_enum (* inlined *) = (
    qualified_name
  * Token.t (* "(" *)
  * pattern
  * (Token.t (* "," *) * pattern) list (* zero or more *)
  * Token.t (* "," *) option
  * Token.t (* ")" *)
)

type pattern_struct (* inlined *) = (
    qualified_name
  * Token.t (* "{" *)
  * pattern_struct_binding
  * (Token.t (* "," *) * pattern_struct_binding) list (* zero or more *)
  * Token.t (* "," *) option
  * Token.t (* "}" *)
)

type pattern_tuple (* inlined *) = (
    Token.t (* "(" *)
  * pattern
  * (Token.t (* "," *) * pattern) list (* zero or more *)
  * Token.t (* "," *) option
  * Token.t (* ")" *)
)

type type_identifier (* inlined *) = (
    qualified_name
  * type_parameters option
)

type type_tuple (* inlined *) = (
    Token.t (* "(" *)
  * (type_ * Token.t (* "," *)) list (* zero or more *)
  * type_ option
  * Token.t (* ")" *)
)

type type_impl_declaration (* inlined *) = (
    Token.t (* "impl" *) * name (*tok*) * Token.t (* ":" *) * type_
)

type type_const_declaration (* inlined *) = (
    Token.t (* "const" *) * name (*tok*) * Token.t (* ":" *) * type_
)

type assignment_statement (* inlined *) = (
    [ `Name of name (*tok*) | `Wild of Token.t (* "_" *) ]
  * [
        `STAREQ of Token.t (* "*=" *)
      | `SLASHEQ of Token.t (* "/=" *)
      | `PERCEQ of Token.t (* "%=" *)
      | `PLUSEQ of Token.t (* "+=" *)
      | `DASHEQ of Token.t (* "-=" *)
      | `EQ of Token.t (* "=" *)
    ]
  * expression
  * Token.t (* ";" *)
)

type break_statement (* inlined *) = (
    Token.t (* "break" *)
  * expression option
  * Token.t (* ";" *)
)

type deep_ellipsis (* inlined *) = (
    Token.t (* "<..." *) * expression * Token.t (* "...>" *)
)

type let_statement (* inlined *) = (
    Token.t (* "let" *)
  * Token.t (* "mut" *) option
  * pattern
  * (Token.t (* ":" *) * type_) option
  * Token.t (* "=" *)
  * expression
  * Token.t (* ";" *)
)

type return_statement (* inlined *) = (
    Token.t (* "return" *)
  * expression option
  * Token.t (* ";" *)
)

type const_declaration (* inlined *) = (
    attribute_list list (* zero or more *)
  * Token.t (* "const" *)
  * name (*tok*)
  * Token.t (* ":" *)
  * type_
  * Token.t (* "=" *)
  * expression
  * Token.t (* ";" *)
)

type semgrep_expression (* inlined *) = (
    Token.t (* "__SEMGREP_EXPRESSION" *) * expression
)

type semgrep_statement (* inlined *) = (
    Token.t (* "__SEMGREP_STATEMENT" *)
  * statement list (* one or more *)
)

type typealias_declaration (* inlined *) = (
    Token.t (* "type" *)
  * name (*tok*)
  * type_parameter_list option
  * Token.t (* "=" *)
  * type_
  * Token.t (* ";" *)
)

type enum_declaration (* inlined *) = (
    attribute_list list (* zero or more *)
  * Token.t (* "enum" *)
  * name (*tok*)
  * type_parameter_list option
  * member_declaration_list
)

type struct_declaration (* inlined *) = (
    attribute_list list (* zero or more *)
  * Token.t (* "struct" *)
  * name (*tok*)
  * type_parameter_list option
  * [
        `Member_decl_list of member_declaration_list
      | `SEMI of Token.t (* ";" *)
    ]
)

type function_declaration (* inlined *) = (function_signature * block)

type trait_declaration (* inlined *) = (
    Token.t (* "trait" *)
  * name (*tok*)
  * type_parameter_list option
  * trait_body
)

type impl_base (* inlined *) = (
    attribute_list list (* zero or more *)
  * Token.t (* "impl" *)
  * name (*tok*)
  * type_parameter_list option
  * impl_body
)

type impl_trait (* inlined *) = (
    attribute_list list (* zero or more *)
  * Token.t (* "impl" *)
  * name (*tok*)
  * type_parameter_list option
  * Token.t (* "of" *)
  * qualified_name
  * impl_body
)

type module_declaration (* inlined *) = (
    attribute_list list (* zero or more *)
  * Token.t (* "mod" *)
  * name (*tok*)
  * [ `SEMI of Token.t (* ";" *) | `Module_body of module_body ]
)
