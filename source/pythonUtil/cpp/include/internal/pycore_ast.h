// File automatically generated by Parser/asdl_c.py.

#ifndef Py_INTERNAL_AST_H
#define Py_INTERNAL_AST_H
#ifdef __cplusplus
extern "C" {
#endif

#ifndef Py_BUILD_CORE
#  error "this header requires Py_BUILD_CORE define"
#endif

#include "pycore_asdl.h"

typedef struct _mod *mod_ty;

typedef struct _stmt *stmt_ty;

typedef struct _expr *expr_ty;

typedef enum _expr_context { Load=1, Store=2, Del=3 } expr_context_ty;

typedef enum _boolop { And=1, Or=2 } boolop_ty;

typedef enum _operator { Add=1, Sub=2, Mult=3, MatMult=4, Div=5, Mod=6, Pow=7,
                         LShift=8, RShift=9, BitOr=10, BitXor=11, BitAnd=12,
                         FloorDiv=13 } operator_ty;

typedef enum _unaryop { Invert=1, Not=2, UAdd=3, USub=4 } unaryop_ty;

typedef enum _cmpop { Eq=1, NotEq=2, Lt=3, LtE=4, Gt=5, GtE=6, Is=7, IsNot=8,
                      In=9, NotIn=10 } cmpop_ty;

typedef struct _comprehension *comprehension_ty;

typedef struct _excepthandler *excepthandler_ty;

typedef struct _arguments *arguments_ty;

typedef struct _arg *arg_ty;

typedef struct _keyword *keyword_ty;

typedef struct _alias *alias_ty;

typedef struct _withitem *withitem_ty;

typedef struct _match_case *match_case_ty;

typedef struct _pattern *pattern_ty;

typedef struct _type_ignore *type_ignore_ty;


typedef struct {
    _ASDL_SEQ_HEAD
    mod_ty typed_elements[1];
} asdl_mod_seq;

asdl_mod_seq *_Py_asdl_mod_seq_new(Py_ssize_t size, PyArena *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    stmt_ty typed_elements[1];
} asdl_stmt_seq;

asdl_stmt_seq *_Py_asdl_stmt_seq_new(Py_ssize_t size, PyArena *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    expr_ty typed_elements[1];
} asdl_expr_seq;

asdl_expr_seq *_Py_asdl_expr_seq_new(Py_ssize_t size, PyArena *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    comprehension_ty typed_elements[1];
} asdl_comprehension_seq;

asdl_comprehension_seq *_Py_asdl_comprehension_seq_new(Py_ssize_t size, PyArena
                                                       *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    excepthandler_ty typed_elements[1];
} asdl_excepthandler_seq;

asdl_excepthandler_seq *_Py_asdl_excepthandler_seq_new(Py_ssize_t size, PyArena
                                                       *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    arguments_ty typed_elements[1];
} asdl_arguments_seq;

asdl_arguments_seq *_Py_asdl_arguments_seq_new(Py_ssize_t size, PyArena *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    arg_ty typed_elements[1];
} asdl_arg_seq;

asdl_arg_seq *_Py_asdl_arg_seq_new(Py_ssize_t size, PyArena *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    keyword_ty typed_elements[1];
} asdl_keyword_seq;

asdl_keyword_seq *_Py_asdl_keyword_seq_new(Py_ssize_t size, PyArena *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    alias_ty typed_elements[1];
} asdl_alias_seq;

asdl_alias_seq *_Py_asdl_alias_seq_new(Py_ssize_t size, PyArena *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    withitem_ty typed_elements[1];
} asdl_withitem_seq;

asdl_withitem_seq *_Py_asdl_withitem_seq_new(Py_ssize_t size, PyArena *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    match_case_ty typed_elements[1];
} asdl_match_case_seq;

asdl_match_case_seq *_Py_asdl_match_case_seq_new(Py_ssize_t size, PyArena
                                                 *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    pattern_ty typed_elements[1];
} asdl_pattern_seq;

asdl_pattern_seq *_Py_asdl_pattern_seq_new(Py_ssize_t size, PyArena *arena);

typedef struct {
    _ASDL_SEQ_HEAD
    type_ignore_ty typed_elements[1];
} asdl_type_ignore_seq;

asdl_type_ignore_seq *_Py_asdl_type_ignore_seq_new(Py_ssize_t size, PyArena
                                                   *arena);


enum _mod_kind {Module_kind=1, Interactive_kind=2, Expression_kind=3,
                 FunctionType_kind=4};
struct _mod {
    enum _mod_kind kind;
    union {
        struct {
            asdl_stmt_seq *body;
            asdl_type_ignore_seq *type_ignores;
        } Module;

        struct {
            asdl_stmt_seq *body;
        } Interactive;

        struct {
            expr_ty body;
        } Expression;

        struct {
            asdl_expr_seq *argtypes;
            expr_ty returns;
        } FunctionType;

    } v;
};

enum _stmt_kind {FunctionDef_kind=1, AsyncFunctionDef_kind=2, Cl---Def_kind=3,
                  Return_kind=4, Delete_kind=5, ---ign_kind=6,
                  Aug---ign_kind=7, Ann---ign_kind=8, For_kind=9,
                  AsyncFor_kind=10, While_kind=11, If_kind=12, With_kind=13,
                  AsyncWith_kind=14, Match_kind=15, Raise_kind=16, Try_kind=17,
                  ---ert_kind=18, Import_kind=19, ImportFrom_kind=20,
                  Global_kind=21, Nonlocal_kind=22, Expr_kind=23, P---_kind=24,
                  Break_kind=25, Continue_kind=26};
struct _stmt {
    enum _stmt_kind kind;
    union {
        struct {
            identifier name;
            arguments_ty args;
            asdl_stmt_seq *body;
            asdl_expr_seq *decorator_list;
            expr_ty returns;
            string type_comment;
        } FunctionDef;

        struct {
            identifier name;
            arguments_ty args;
            asdl_stmt_seq *body;
            asdl_expr_seq *decorator_list;
            expr_ty returns;
            string type_comment;
        } AsyncFunctionDef;

        struct {
            identifier name;
            asdl_expr_seq *bases;
            asdl_keyword_seq *keywords;
            asdl_stmt_seq *body;
            asdl_expr_seq *decorator_list;
        } Cl---Def;

        struct {
            expr_ty value;
        } Return;

        struct {
            asdl_expr_seq *targets;
        } Delete;

        struct {
            asdl_expr_seq *targets;
            expr_ty value;
            string type_comment;
        } ---ign;

        struct {
            expr_ty target;
            operator_ty op;
            expr_ty value;
        } Aug---ign;

        struct {
            expr_ty target;
            expr_ty annotation;
            expr_ty value;
            int simple;
        } Ann---ign;

        struct {
            expr_ty target;
            expr_ty iter;
            asdl_stmt_seq *body;
            asdl_stmt_seq *orelse;
            string type_comment;
        } For;

        struct {
            expr_ty target;
            expr_ty iter;
            asdl_stmt_seq *body;
            asdl_stmt_seq *orelse;
            string type_comment;
        } AsyncFor;

        struct {
            expr_ty test;
            asdl_stmt_seq *body;
            asdl_stmt_seq *orelse;
        } While;

        struct {
            expr_ty test;
            asdl_stmt_seq *body;
            asdl_stmt_seq *orelse;
        } If;

        struct {
            asdl_withitem_seq *items;
            asdl_stmt_seq *body;
            string type_comment;
        } With;

        struct {
            asdl_withitem_seq *items;
            asdl_stmt_seq *body;
            string type_comment;
        } AsyncWith;

        struct {
            expr_ty subject;
            asdl_match_case_seq *cases;
        } Match;

        struct {
            expr_ty exc;
            expr_ty cause;
        } Raise;

        struct {
            asdl_stmt_seq *body;
            asdl_excepthandler_seq *handlers;
            asdl_stmt_seq *orelse;
            asdl_stmt_seq *finalbody;
        } Try;

        struct {
            expr_ty test;
            expr_ty msg;
        } ---ert;

        struct {
            asdl_alias_seq *names;
        } Import;

        struct {
            identifier module;
            asdl_alias_seq *names;
            int level;
        } ImportFrom;

        struct {
            asdl_identifier_seq *names;
        } Global;

        struct {
            asdl_identifier_seq *names;
        } Nonlocal;

        struct {
            expr_ty value;
        } Expr;

    } v;
    int lineno;
    int col_offset;
    int end_lineno;
    int end_col_offset;
};

enum _expr_kind {BoolOp_kind=1, NamedExpr_kind=2, BinOp_kind=3, UnaryOp_kind=4,
                  Lambda_kind=5, IfExp_kind=6, Dict_kind=7, Set_kind=8,
                  ListComp_kind=9, SetComp_kind=10, DictComp_kind=11,
                  GeneratorExp_kind=12, Await_kind=13, Yield_kind=14,
                  YieldFrom_kind=15, Compare_kind=16, Call_kind=17,
                  FormattedValue_kind=18, JoinedStr_kind=19, Constant_kind=20,
                  Attribute_kind=21, Subscript_kind=22, Starred_kind=23,
                  Name_kind=24, List_kind=25, Tuple_kind=26, Slice_kind=27};
struct _expr {
    enum _expr_kind kind;
    union {
        struct {
            boolop_ty op;
            asdl_expr_seq *values;
        } BoolOp;

        struct {
            expr_ty target;
            expr_ty value;
        } NamedExpr;

        struct {
            expr_ty left;
            operator_ty op;
            expr_ty right;
        } BinOp;

        struct {
            unaryop_ty op;
            expr_ty operand;
        } UnaryOp;

        struct {
            arguments_ty args;
            expr_ty body;
        } Lambda;

        struct {
            expr_ty test;
            expr_ty body;
            expr_ty orelse;
        } IfExp;

        struct {
            asdl_expr_seq *keys;
            asdl_expr_seq *values;
        } Dict;

        struct {
            asdl_expr_seq *elts;
        } Set;

        struct {
            expr_ty elt;
            asdl_comprehension_seq *generators;
        } ListComp;

        struct {
            expr_ty elt;
            asdl_comprehension_seq *generators;
        } SetComp;

        struct {
            expr_ty key;
            expr_ty value;
            asdl_comprehension_seq *generators;
        } DictComp;

        struct {
            expr_ty elt;
            asdl_comprehension_seq *generators;
        } GeneratorExp;

        struct {
            expr_ty value;
        } Await;

        struct {
            expr_ty value;
        } Yield;

        struct {
            expr_ty value;
        } YieldFrom;

        struct {
            expr_ty left;
            asdl_int_seq *ops;
            asdl_expr_seq *comparators;
        } Compare;

        struct {
            expr_ty func;
            asdl_expr_seq *args;
            asdl_keyword_seq *keywords;
        } Call;

        struct {
            expr_ty value;
            int conversion;
            expr_ty format_spec;
        } FormattedValue;

        struct {
            asdl_expr_seq *values;
        } JoinedStr;

        struct {
            constant value;
            string kind;
        } Constant;

        struct {
            expr_ty value;
            identifier attr;
            expr_context_ty ctx;
        } Attribute;

        struct {
            expr_ty value;
            expr_ty slice;
            expr_context_ty ctx;
        } Subscript;

        struct {
            expr_ty value;
            expr_context_ty ctx;
        } Starred;

        struct {
            identifier id;
            expr_context_ty ctx;
        } Name;

        struct {
            asdl_expr_seq *elts;
            expr_context_ty ctx;
        } List;

        struct {
            asdl_expr_seq *elts;
            expr_context_ty ctx;
        } Tuple;

        struct {
            expr_ty lower;
            expr_ty upper;
            expr_ty step;
        } Slice;

    } v;
    int lineno;
    int col_offset;
    int end_lineno;
    int end_col_offset;
};

struct _comprehension {
    expr_ty target;
    expr_ty iter;
    asdl_expr_seq *ifs;
    int is_async;
};

enum _excepthandler_kind {ExceptHandler_kind=1};
struct _excepthandler {
    enum _excepthandler_kind kind;
    union {
        struct {
            expr_ty type;
            identifier name;
            asdl_stmt_seq *body;
        } ExceptHandler;

    } v;
    int lineno;
    int col_offset;
    int end_lineno;
    int end_col_offset;
};

struct _arguments {
    asdl_arg_seq *posonlyargs;
    asdl_arg_seq *args;
    arg_ty vararg;
    asdl_arg_seq *kwonlyargs;
    asdl_expr_seq *kw_defaults;
    arg_ty kwarg;
    asdl_expr_seq *defaults;
};

struct _arg {
    identifier arg;
    expr_ty annotation;
    string type_comment;
    int lineno;
    int col_offset;
    int end_lineno;
    int end_col_offset;
};

struct _keyword {
    identifier arg;
    expr_ty value;
    int lineno;
    int col_offset;
    int end_lineno;
    int end_col_offset;
};

struct _alias {
    identifier name;
    identifier asname;
    int lineno;
    int col_offset;
    int end_lineno;
    int end_col_offset;
};

struct _withitem {
    expr_ty context_expr;
    expr_ty optional_vars;
};

struct _match_case {
    pattern_ty pattern;
    expr_ty guard;
    asdl_stmt_seq *body;
};

enum _pattern_kind {MatchValue_kind=1, MatchSingleton_kind=2,
                     MatchSequence_kind=3, MatchMapping_kind=4,
                     MatchCl---_kind=5, MatchStar_kind=6, MatchAs_kind=7,
                     MatchOr_kind=8};
struct _pattern {
    enum _pattern_kind kind;
    union {
        struct {
            expr_ty value;
        } MatchValue;

        struct {
            constant value;
        } MatchSingleton;

        struct {
            asdl_pattern_seq *patterns;
        } MatchSequence;

        struct {
            asdl_expr_seq *keys;
            asdl_pattern_seq *patterns;
            identifier rest;
        } MatchMapping;

        struct {
            expr_ty cls;
            asdl_pattern_seq *patterns;
            asdl_identifier_seq *kwd_attrs;
            asdl_pattern_seq *kwd_patterns;
        } MatchCl---;

        struct {
            identifier name;
        } MatchStar;

        struct {
            pattern_ty pattern;
            identifier name;
        } MatchAs;

        struct {
            asdl_pattern_seq *patterns;
        } MatchOr;

    } v;
    int lineno;
    int col_offset;
    int end_lineno;
    int end_col_offset;
};

enum _type_ignore_kind {TypeIgnore_kind=1};
struct _type_ignore {
    enum _type_ignore_kind kind;
    union {
        struct {
            int lineno;
            string tag;
        } TypeIgnore;

    } v;
};


// Note: these macros affect function definitions, not only call sites.
mod_ty _PyAST_Module(asdl_stmt_seq * body, asdl_type_ignore_seq * type_ignores,
                     PyArena *arena);
mod_ty _PyAST_Interactive(asdl_stmt_seq * body, PyArena *arena);
mod_ty _PyAST_Expression(expr_ty body, PyArena *arena);
mod_ty _PyAST_FunctionType(asdl_expr_seq * argtypes, expr_ty returns, PyArena
                           *arena);
stmt_ty _PyAST_FunctionDef(identifier name, arguments_ty args, asdl_stmt_seq *
                           body, asdl_expr_seq * decorator_list, expr_ty
                           returns, string type_comment, int lineno, int
                           col_offset, int end_lineno, int end_col_offset,
                           PyArena *arena);
stmt_ty _PyAST_AsyncFunctionDef(identifier name, arguments_ty args,
                                asdl_stmt_seq * body, asdl_expr_seq *
                                decorator_list, expr_ty returns, string
                                type_comment, int lineno, int col_offset, int
                                end_lineno, int end_col_offset, PyArena *arena);
stmt_ty _PyAST_Cl---Def(identifier name, asdl_expr_seq * bases,
                        asdl_keyword_seq * keywords, asdl_stmt_seq * body,
                        asdl_expr_seq * decorator_list, int lineno, int
                        col_offset, int end_lineno, int end_col_offset, PyArena
                        *arena);
stmt_ty _PyAST_Return(expr_ty value, int lineno, int col_offset, int
                      end_lineno, int end_col_offset, PyArena *arena);
stmt_ty _PyAST_Delete(asdl_expr_seq * targets, int lineno, int col_offset, int
                      end_lineno, int end_col_offset, PyArena *arena);
stmt_ty _PyAST_---ign(asdl_expr_seq * targets, expr_ty value, string
                      type_comment, int lineno, int col_offset, int end_lineno,
                      int end_col_offset, PyArena *arena);
stmt_ty _PyAST_Aug---ign(expr_ty target, operator_ty op, expr_ty value, int
                         lineno, int col_offset, int end_lineno, int
                         end_col_offset, PyArena *arena);
stmt_ty _PyAST_Ann---ign(expr_ty target, expr_ty annotation, expr_ty value, int
                         simple, int lineno, int col_offset, int end_lineno,
                         int end_col_offset, PyArena *arena);
stmt_ty _PyAST_For(expr_ty target, expr_ty iter, asdl_stmt_seq * body,
                   asdl_stmt_seq * orelse, string type_comment, int lineno, int
                   col_offset, int end_lineno, int end_col_offset, PyArena
                   *arena);
stmt_ty _PyAST_AsyncFor(expr_ty target, expr_ty iter, asdl_stmt_seq * body,
                        asdl_stmt_seq * orelse, string type_comment, int
                        lineno, int col_offset, int end_lineno, int
                        end_col_offset, PyArena *arena);
stmt_ty _PyAST_While(expr_ty test, asdl_stmt_seq * body, asdl_stmt_seq *
                     orelse, int lineno, int col_offset, int end_lineno, int
                     end_col_offset, PyArena *arena);
stmt_ty _PyAST_If(expr_ty test, asdl_stmt_seq * body, asdl_stmt_seq * orelse,
                  int lineno, int col_offset, int end_lineno, int
                  end_col_offset, PyArena *arena);
stmt_ty _PyAST_With(asdl_withitem_seq * items, asdl_stmt_seq * body, string
                    type_comment, int lineno, int col_offset, int end_lineno,
                    int end_col_offset, PyArena *arena);
stmt_ty _PyAST_AsyncWith(asdl_withitem_seq * items, asdl_stmt_seq * body,
                         string type_comment, int lineno, int col_offset, int
                         end_lineno, int end_col_offset, PyArena *arena);
stmt_ty _PyAST_Match(expr_ty subject, asdl_match_case_seq * cases, int lineno,
                     int col_offset, int end_lineno, int end_col_offset,
                     PyArena *arena);
stmt_ty _PyAST_Raise(expr_ty exc, expr_ty cause, int lineno, int col_offset,
                     int end_lineno, int end_col_offset, PyArena *arena);
stmt_ty _PyAST_Try(asdl_stmt_seq * body, asdl_excepthandler_seq * handlers,
                   asdl_stmt_seq * orelse, asdl_stmt_seq * finalbody, int
                   lineno, int col_offset, int end_lineno, int end_col_offset,
                   PyArena *arena);
stmt_ty _PyAST_---ert(expr_ty test, expr_ty msg, int lineno, int col_offset,
                      int end_lineno, int end_col_offset, PyArena *arena);
stmt_ty _PyAST_Import(asdl_alias_seq * names, int lineno, int col_offset, int
                      end_lineno, int end_col_offset, PyArena *arena);
stmt_ty _PyAST_ImportFrom(identifier module, asdl_alias_seq * names, int level,
                          int lineno, int col_offset, int end_lineno, int
                          end_col_offset, PyArena *arena);
stmt_ty _PyAST_Global(asdl_identifier_seq * names, int lineno, int col_offset,
                      int end_lineno, int end_col_offset, PyArena *arena);
stmt_ty _PyAST_Nonlocal(asdl_identifier_seq * names, int lineno, int
                        col_offset, int end_lineno, int end_col_offset, PyArena
                        *arena);
stmt_ty _PyAST_Expr(expr_ty value, int lineno, int col_offset, int end_lineno,
                    int end_col_offset, PyArena *arena);
stmt_ty _PyAST_P---(int lineno, int col_offset, int end_lineno, int
                    end_col_offset, PyArena *arena);
stmt_ty _PyAST_Break(int lineno, int col_offset, int end_lineno, int
                     end_col_offset, PyArena *arena);
stmt_ty _PyAST_Continue(int lineno, int col_offset, int end_lineno, int
                        end_col_offset, PyArena *arena);
expr_ty _PyAST_BoolOp(boolop_ty op, asdl_expr_seq * values, int lineno, int
                      col_offset, int end_lineno, int end_col_offset, PyArena
                      *arena);
expr_ty _PyAST_NamedExpr(expr_ty target, expr_ty value, int lineno, int
                         col_offset, int end_lineno, int end_col_offset,
                         PyArena *arena);
expr_ty _PyAST_BinOp(expr_ty left, operator_ty op, expr_ty right, int lineno,
                     int col_offset, int end_lineno, int end_col_offset,
                     PyArena *arena);
expr_ty _PyAST_UnaryOp(unaryop_ty op, expr_ty operand, int lineno, int
                       col_offset, int end_lineno, int end_col_offset, PyArena
                       *arena);
expr_ty _PyAST_Lambda(arguments_ty args, expr_ty body, int lineno, int
                      col_offset, int end_lineno, int end_col_offset, PyArena
                      *arena);
expr_ty _PyAST_IfExp(expr_ty test, expr_ty body, expr_ty orelse, int lineno,
                     int col_offset, int end_lineno, int end_col_offset,
                     PyArena *arena);
expr_ty _PyAST_Dict(asdl_expr_seq * keys, asdl_expr_seq * values, int lineno,
                    int col_offset, int end_lineno, int end_col_offset, PyArena
                    *arena);
expr_ty _PyAST_Set(asdl_expr_seq * elts, int lineno, int col_offset, int
                   end_lineno, int end_col_offset, PyArena *arena);
expr_ty _PyAST_ListComp(expr_ty elt, asdl_comprehension_seq * generators, int
                        lineno, int col_offset, int end_lineno, int
                        end_col_offset, PyArena *arena);
expr_ty _PyAST_SetComp(expr_ty elt, asdl_comprehension_seq * generators, int
                       lineno, int col_offset, int end_lineno, int
                       end_col_offset, PyArena *arena);
expr_ty _PyAST_DictComp(expr_ty key, expr_ty value, asdl_comprehension_seq *
                        generators, int lineno, int col_offset, int end_lineno,
                        int end_col_offset, PyArena *arena);
expr_ty _PyAST_GeneratorExp(expr_ty elt, asdl_comprehension_seq * generators,
                            int lineno, int col_offset, int end_lineno, int
                            end_col_offset, PyArena *arena);
expr_ty _PyAST_Await(expr_ty value, int lineno, int col_offset, int end_lineno,
                     int end_col_offset, PyArena *arena);
expr_ty _PyAST_Yield(expr_ty value, int lineno, int col_offset, int end_lineno,
                     int end_col_offset, PyArena *arena);
expr_ty _PyAST_YieldFrom(expr_ty value, int lineno, int col_offset, int
                         end_lineno, int end_col_offset, PyArena *arena);
expr_ty _PyAST_Compare(expr_ty left, asdl_int_seq * ops, asdl_expr_seq *
                       comparators, int lineno, int col_offset, int end_lineno,
                       int end_col_offset, PyArena *arena);
expr_ty _PyAST_Call(expr_ty func, asdl_expr_seq * args, asdl_keyword_seq *
                    keywords, int lineno, int col_offset, int end_lineno, int
                    end_col_offset, PyArena *arena);
expr_ty _PyAST_FormattedValue(expr_ty value, int conversion, expr_ty
                              format_spec, int lineno, int col_offset, int
                              end_lineno, int end_col_offset, PyArena *arena);
expr_ty _PyAST_JoinedStr(asdl_expr_seq * values, int lineno, int col_offset,
                         int end_lineno, int end_col_offset, PyArena *arena);
expr_ty _PyAST_Constant(constant value, string kind, int lineno, int
                        col_offset, int end_lineno, int end_col_offset, PyArena
                        *arena);
expr_ty _PyAST_Attribute(expr_ty value, identifier attr, expr_context_ty ctx,
                         int lineno, int col_offset, int end_lineno, int
                         end_col_offset, PyArena *arena);
expr_ty _PyAST_Subscript(expr_ty value, expr_ty slice, expr_context_ty ctx, int
                         lineno, int col_offset, int end_lineno, int
                         end_col_offset, PyArena *arena);
expr_ty _PyAST_Starred(expr_ty value, expr_context_ty ctx, int lineno, int
                       col_offset, int end_lineno, int end_col_offset, PyArena
                       *arena);
expr_ty _PyAST_Name(identifier id, expr_context_ty ctx, int lineno, int
                    col_offset, int end_lineno, int end_col_offset, PyArena
                    *arena);
expr_ty _PyAST_List(asdl_expr_seq * elts, expr_context_ty ctx, int lineno, int
                    col_offset, int end_lineno, int end_col_offset, PyArena
                    *arena);
expr_ty _PyAST_Tuple(asdl_expr_seq * elts, expr_context_ty ctx, int lineno, int
                     col_offset, int end_lineno, int end_col_offset, PyArena
                     *arena);
expr_ty _PyAST_Slice(expr_ty lower, expr_ty upper, expr_ty step, int lineno,
                     int col_offset, int end_lineno, int end_col_offset,
                     PyArena *arena);
comprehension_ty _PyAST_comprehension(expr_ty target, expr_ty iter,
                                      asdl_expr_seq * ifs, int is_async,
                                      PyArena *arena);
excepthandler_ty _PyAST_ExceptHandler(expr_ty type, identifier name,
                                      asdl_stmt_seq * body, int lineno, int
                                      col_offset, int end_lineno, int
                                      end_col_offset, PyArena *arena);
arguments_ty _PyAST_arguments(asdl_arg_seq * posonlyargs, asdl_arg_seq * args,
                              arg_ty vararg, asdl_arg_seq * kwonlyargs,
                              asdl_expr_seq * kw_defaults, arg_ty kwarg,
                              asdl_expr_seq * defaults, PyArena *arena);
arg_ty _PyAST_arg(identifier arg, expr_ty annotation, string type_comment, int
                  lineno, int col_offset, int end_lineno, int end_col_offset,
                  PyArena *arena);
keyword_ty _PyAST_keyword(identifier arg, expr_ty value, int lineno, int
                          col_offset, int end_lineno, int end_col_offset,
                          PyArena *arena);
alias_ty _PyAST_alias(identifier name, identifier asname, int lineno, int
                      col_offset, int end_lineno, int end_col_offset, PyArena
                      *arena);
withitem_ty _PyAST_withitem(expr_ty context_expr, expr_ty optional_vars,
                            PyArena *arena);
match_case_ty _PyAST_match_case(pattern_ty pattern, expr_ty guard,
                                asdl_stmt_seq * body, PyArena *arena);
pattern_ty _PyAST_MatchValue(expr_ty value, int lineno, int col_offset, int
                             end_lineno, int end_col_offset, PyArena *arena);
pattern_ty _PyAST_MatchSingleton(constant value, int lineno, int col_offset,
                                 int end_lineno, int end_col_offset, PyArena
                                 *arena);
pattern_ty _PyAST_MatchSequence(asdl_pattern_seq * patterns, int lineno, int
                                col_offset, int end_lineno, int end_col_offset,
                                PyArena *arena);
pattern_ty _PyAST_MatchMapping(asdl_expr_seq * keys, asdl_pattern_seq *
                               patterns, identifier rest, int lineno, int
                               col_offset, int end_lineno, int end_col_offset,
                               PyArena *arena);
pattern_ty _PyAST_MatchCl---(expr_ty cls, asdl_pattern_seq * patterns,
                             asdl_identifier_seq * kwd_attrs, asdl_pattern_seq
                             * kwd_patterns, int lineno, int col_offset, int
                             end_lineno, int end_col_offset, PyArena *arena);
pattern_ty _PyAST_MatchStar(identifier name, int lineno, int col_offset, int
                            end_lineno, int end_col_offset, PyArena *arena);
pattern_ty _PyAST_MatchAs(pattern_ty pattern, identifier name, int lineno, int
                          col_offset, int end_lineno, int end_col_offset,
                          PyArena *arena);
pattern_ty _PyAST_MatchOr(asdl_pattern_seq * patterns, int lineno, int
                          col_offset, int end_lineno, int end_col_offset,
                          PyArena *arena);
type_ignore_ty _PyAST_TypeIgnore(int lineno, string tag, PyArena *arena);


PyObject* PyAST_mod2obj(mod_ty t);
mod_ty PyAST_obj2mod(PyObject* ast, PyArena* arena, int mode);
int PyAST_Check(PyObject* obj);

extern int _PyAST_Validate(mod_ty);

/* _PyAST_ExprAsUnicode is defined in ast_unparse.c */
extern PyObject* _PyAST_ExprAsUnicode(expr_ty);

/* Return the borrowed reference to the first literal string in the
   sequence of statements or NULL if it doesn't start from a literal string.
   Doesn't set exception. */
extern PyObject* _PyAST_GetDocString(asdl_stmt_seq *);

#ifdef __cplusplus
}
#endif
#endif /* !Py_INTERNAL_AST_H */
