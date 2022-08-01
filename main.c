#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include "external/mpc.h"
#include <math.h>

#define BUFFER_SIZE 2048

#define LASSERT(args, cond, fmt, ...) \
   if (!(cond)) { \
      lval *err = lval_err(fmt, ##__VA_ARGS__); \
      lval_del(args); \
      return err; \
   }

int max(int a, int b) { return a > b ? a : b; }

static char input[BUFFER_SIZE];

mpc_parser_t* Number;
mpc_parser_t* Symbol;
mpc_parser_t* String;
mpc_parser_t* Comment;
mpc_parser_t* Sexpr;
mpc_parser_t* Qexpr;
mpc_parser_t* Expr;
mpc_parser_t* Lispy;

struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

// number types
typedef enum {
   LVAL_NUM,
   LVAL_ERR,
   LVAL_SYM,
   LVAL_STR,
   LVAL_SEXPR,
   LVAL_QEXPR,
   LVAL_FUN,
} NUMBER_TYPE;

typedef lval*(*lbuiltin)(lenv*, lval*);

struct lval {
   int type;

   double num;
   char *err;
   char *sym;
   char *str;

   lbuiltin builtin;
   lenv *env;
   lval *formals;
   lval *body;

   int count;
   struct lval** cell;
};

struct lenv {
   lenv *par;
   int count; // number of entries in syms and vals
   char **syms;
   lval **vals;
};

// number type lval
lval *lval_num(double x) {
   lval* v = malloc(sizeof(lval));
   v->type = LVAL_NUM;
   v->num = x;
   return v;
}

// error type lval
lval *lval_err(char *fmt, ...) {
   lval *v = malloc(sizeof(lval));
   v->type = LVAL_ERR;

   va_list va;
   va_start(va, fmt);

   v->err = malloc(512);
   vsnprintf(v->err, 511, fmt, va);

   // reduce allocated amount to actually used
   v->err = realloc(v->err, strlen(v->err) + 1);
   va_end(va);

   return v;
}

// symbol type lval
lval *lval_sym(char *s) {
   lval *v = malloc(sizeof(lval));
   v->type = LVAL_SYM;
   v->sym = malloc(strlen(s) + 1);
   strcpy(v->sym, s);
   return v;
}

// str type lval
lval *lval_str(char *s) {
   lval *v = malloc(sizeof(lval)); 
   v->type = LVAL_STR;
   v->str = malloc(strlen(s) + 1);
   strcpy(v->str, s);
   return v;
}

// sexpr type lval
lval *lval_sexpr() {
   lval *v = malloc(sizeof(lval));
   v->type = LVAL_SEXPR;
   v->count = 0;
   v->cell = NULL;
   return v;
}

// qexpr type lval
lval *lval_qexpr() {
   lval *v = malloc(sizeof(lval));
   v->type = LVAL_QEXPR;
   v->count = 0;
   v->cell = NULL;
   return v;
}

lenv *lenv_new() {
   lenv *e = malloc(sizeof(lenv));
   e->par = NULL;
   e->count = 0;
   e->syms = NULL;
   e->vals = NULL;
   return e;
}

lval *lval_fun(lbuiltin func) {
   lval *v = malloc(sizeof(lval));
   v->type = LVAL_FUN;
   v->builtin = func;
   return v;
}

lval *lval_lambda(lval *formals, lval *body) {
   lval *v = malloc(sizeof(lval));
   v->type = LVAL_FUN;
   v->builtin = NULL;
   v->env = lenv_new();
   v->formals = formals;
   v->body = body;
   return v;
}

void lval_del(lval *v);

void lenv_del(lenv *e) {
   for (int i = 0; i < e->count; i++) {
      free(e->syms[i]);
      lval_del(e->vals[i]);
   }
   free(e->syms);
   free(e->vals);
   free(e);
}
 
void lval_del(lval *v) {
   switch(v->type) {
      case LVAL_NUM: break;
      case LVAL_ERR: free(v->err); break;
      case LVAL_SYM: free(v->sym); break; 
      case LVAL_STR: free(v->str); break; 
      case LVAL_SEXPR:
      case LVAL_QEXPR:
         for (int i = 0; i < v->count; i++)
            lval_del(v->cell[i]);
         free(v->cell);
         break;
      case LVAL_FUN:
         if (!v->builtin) {
            lenv_del(v->env);
            lval_del(v->formals);
            lval_del(v->body);
         }
         break;
   }
   free(v);
} 

// read the number type
lval *lval_read_num(mpc_ast_t *t) {
   errno = 0;
   double x = strtod(t->contents, NULL);
   return errno != ERANGE ?
      lval_num(x) : lval_err("invalid number");
}

lval *lval_add(lval *v, lval *x) {
   v->count++;
   v->cell = realloc(v->cell, sizeof(lval*) * v->count);
   v->cell[v->count-1] = x;
   return v;
}

lval *lval_read_str(mpc_ast_t *t) {
   t->contents[strlen(t->contents) - 1] = '\0';
   char *unescaped = malloc(strlen(t->contents + 1) + 1);
   strcpy(unescaped, t->contents + 1);
   unescaped = mpcf_unescape(unescaped);
   lval *str = lval_str(unescaped);
   free(unescaped);
   return str;
}

lval *lval_read(mpc_ast_t *t) {
   if (strstr(t->tag, "number")) return lval_read_num(t);
   if (strstr(t->tag, "symbol")) return lval_sym(t->contents); 
   if (strstr(t->tag, "string")) return lval_read_str(t);

   lval *x = NULL;
   if (strcmp(t->tag, ">") == 0) x = lval_sexpr();
   if (strstr(t->tag, "sexpr"))  x = lval_sexpr();
   if (strstr(t->tag, "qexpr"))  x = lval_qexpr();

   for (int i = 0; i < t->children_num; i++) {
      if (strcmp(t->children[i]->contents, "}") == 0) continue;
      if (strcmp(t->children[i]->contents, "{") == 0) continue;
      if (strcmp(t->children[i]->contents, "(") == 0) continue;
      if (strcmp(t->children[i]->contents, ")") == 0) continue;
      if (strcmp(t->children[i]->tag, "regex") == 0)  continue;
      if (strstr(t->children[i]->tag, "comment"))     continue;
      x = lval_add(x, lval_read(t->children[i]));
   }
   return x;
}

void lval_print(lval *v);

void lval_expr_print(lval *v, char open, char close) {
   putchar(open);
   for (int i = 0; i < v->count; i++) {
      lval_print(v->cell[i]);
      if (i != v->count - 1)
         putchar(' ');
   }
   putchar(close);
}

void lval_print_str(lval *v) {
   char *escaped = malloc(strlen(v->str) + 1);
   strcpy(escaped, v->str);
   escaped = mpcf_escape(escaped);
   printf("\"%s\"", escaped);
   free(escaped);
}

void lval_function_print(lbuiltin f);

// print lval type 
void lval_print(lval *v) {
   switch (v->type) {
      case LVAL_NUM: printf("%g", v->num); break;
      case LVAL_SYM: printf("%s", v->sym); break;
      case LVAL_STR: lval_print_str(v); break;
      case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break;
      case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break;
      case LVAL_FUN: 
         if (v->builtin) { 
            lval_function_print(v->builtin); 
         } else {
            printf("(\\ ");
            lval_print(v->formals);
            putchar(' ');
            lval_print(v->body);
            putchar(' ');
         }
         break;
      case LVAL_ERR: printf("Error: %s", v->err); break;
   }
}

// print lval followed with a newline
void lval_println(lval *v) {
   lval_print(v);
   putchar('\n');
}

lval *lval_pop(lval *v, int i) {
   // fint the item at i'th index
   lval *x = v->cell[i];

   // shift memory
   memmove(&v->cell[i], &v->cell[i+1], sizeof(lval*) * (v->count-i-1));

   v->count--;

   v->cell = realloc(v->cell, sizeof(lval*) * v->count);
   return x;
}

lval *lval_take(lval *v, int i) {
   lval* x = lval_pop(v, i);
   lval_del(v);
   return x;
}

char *ltype_name(int t) {
   switch (t) {
      case LVAL_FUN:    return "Function";
      case LVAL_NUM:    return "Number";
      case LVAL_ERR:    return "Error";
      case LVAL_SYM:    return "Symbol";
      case LVAL_STR:    return "String";
      case LVAL_SEXPR:  return "S-Expression";
      case LVAL_QEXPR:  return "Q-Expression";
      default:          return "Unknown";
   }
}

lval *lval_eval(lenv *e, lval *v);

lval *builtin_load(lenv *e, lval *a) {
   LASSERT(a, a->count == 1,
      "Function 'load' passed too many arguments. "
      "Got %i, expected %i.", 
      a->count, 1);

   LASSERT(a, a->cell[0]->type == LVAL_STR, 
      "Function 'load' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_STR));

   mpc_result_t r;
   if (mpc_parse_contents(a->cell[0]->str, Lispy, &r)) {
      lval *expr = lval_read(r.output);
      mpc_ast_delete(r.output);

      while (expr->count) {
         lval *x = lval_eval(e, lval_pop(expr, 0));
         lval_println(x);
         lval_del(x);
      }
      
      lval_del(expr);
      lval_del(a);

      return lval_sym("ok");

   } else {
      char *err_msg = mpc_err_string(r.error);
      lval *err = lval_err("Could not load Library %s", err_msg);
      free(err_msg);
      lval_del(a);
      return err;
   }
}

lval *builtin_print(lenv *e, lval *a) {
   for (int i = 0; i < a->count; i++) {
      lval_print(a->cell[i]);
      putchar(' ');
   }

   putchar('\n');
   lval_del(a);
   
   return lval_sym("ok");
}

lval *builtin_read(lenv *e, lval *a) {
   // convert string to a q-expression
   // read "hello" -> { "hello" }
   LASSERT(a, a->count == 1,
      "Function 'read' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 1);

   LASSERT(a, a->cell[0]->type == LVAL_STR,
      "Function 'read' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_STR));

   lval *v = lval_pop(a, 0);
   lval *x = lval_qexpr();
   x = lval_add(x, v);
   lval_del(a);
   return x;
}

lval *builtin_error(lenv *e, lval *a) {
   LASSERT(a, a->count == 1,
      "Function 'error' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 1);

   LASSERT(a, a->cell[0]->type == LVAL_STR,
      "Function 'error' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_STR));

   lval *err = lval_err(a->cell[0]->str);
   lval_del(a);
   return err;
}

lval *builtin_op(lenv *e, lval* a, char *op) {
   for (int i = 0; i < a->count; i++) {
      LASSERT(a, a->cell[i]->type == LVAL_NUM, 
      "Function '%s' passed incorrect type "
      "for argument %i. Got %s, expected %s.", op, i, 
      ltype_name(a->cell[i]->type), ltype_name(LVAL_NUM));
   }

   // pop first element
   lval *x = lval_pop(a, 0);

   // if no args and sub then unary negation
   if ((strcmp(op, "-") == 0) && a->count == 0)
      x->num = -x->num;

   while (a->count > 0) {
      lval *y = lval_pop(a, 0);

      if (strcmp(op, "+") == 0) x->num += y->num;           
      if (strcmp(op, "-") == 0) x->num -= y->num;
      if (strcmp(op, "*") == 0) x->num *= y->num;
      if (strcmp(op, "/") == 0) {
         if (y->num == 0) {
            lval_del(x);
            lval_del(y);
            x = lval_err("Division by zero!"); break;
         }
         x->num /= y->num;
      }
      if (strcmp(op, "%") == 0) {
         if (y->num == 0) {
            lval_del(x);
            lval_del(y);
            x = lval_err("Division by zero!"); break;
         }
         x->num = fmod(x->num, y->num);
      }
      if (strcmp(op, "^") == 0) {
         if (y->num == 0 && x->num == 0) {
            lval_del(x);
            lval_del(y);
            lval_err("0^0 is undefined!"); break;
         }
         x->num = pow(x->num, y->num);
      }
      lval_del(y);
   }
   lval_del(a);
   return x;
}

lval *builtin_add(lenv *e, lval *a) {
   return builtin_op(e, a, "+");
}

lval *builtin_sub(lenv *e, lval *a) {
   return builtin_op(e, a, "-");
}

lval *builtin_mul(lenv *e, lval *a) {
   return builtin_op(e, a, "*");
}

lval *builtin_div(lenv *e, lval *a) {
   return builtin_op(e, a, "/");
}

lval *builtin_mod(lenv *e, lval *a) {
   return builtin_op(e, a, "%");
}

lval *builtin_pow(lenv *e, lval *a) {
   return builtin_op(e, a, "^");
}

lval *builtin_ord(lenv *e, lval *a, char *op) {
   LASSERT(a, a->count == 2,
      "1");
   for (int i = 0; i < a->count; i++)
      LASSERT(a, a->cell[i]->type == LVAL_NUM,
         "2");

   int r;
   if (strcmp(op, ">") == 0)
      r = (a->cell[0]->num > a->cell[1]->num);
   if (strcmp(op, "<") == 0)
      r = (a->cell[0]->num < a->cell[1]->num);
   if (strcmp(op, ">=") == 0)
      r = (a->cell[0]->num >= a->cell[1]->num);
   if (strcmp(op, "<=") == 0)
      r = (a->cell[0]->num <= a->cell[1]->num);
   
   lval_del(a);
   return lval_num(r);
}

lval *builtin_gt(lenv *e, lval* a) {
   return builtin_ord(e, a, ">");
}

lval *builtin_lt(lenv *e, lval* a) {
   return builtin_ord(e, a, "<");
}
lval *builtin_le(lenv *e, lval* a) {
   return builtin_ord(e, a, "<=");
}
lval *builtin_ge(lenv *e, lval* a) {
   return builtin_ord(e, a, ">=");
}

// check if two lvals are equal
int lval_eq(lval *x, lval *y) {
   if (x->type != y->type)
      return 0;

   switch(x->type) {
      case LVAL_NUM: return x->num == y->num;
      case LVAL_ERR: return (strcmp(x->err, y->err) == 0);
      case LVAL_SYM: return (strcmp(x->sym, y->sym) == 0);
      case LVAL_STR: return (strcmp(x->str, y->str) == 0);
      case LVAL_FUN:
         if (x->builtin || y->builtin)
            return x->builtin == y->builtin;
         else
            // comparse function arguments and body
            return lval_eq(x->formals, y->formals) &&
                   lval_eq(x->body, y->body);
      case LVAL_SEXPR:
      case LVAL_QEXPR: 
         if (x->count != y->count) 
            return 0;
         for (int i = 0; i < x->count; i++)
            if (!lval_eq(x->cell[i], y->cell[i])) 
               return 0;
         return 1;
   }
   return 0;
}

lval *builtin_cmp(lenv *e, lval *a, char *op) {
   LASSERT(a, a->count == 2, 
      "Function 'cmp' passed incorrect number of arguments. "
      "Got %i, expected %i.",
      a->count, 2);

      int r;
      if (strcmp(op, "==") == 0)
         r = lval_eq(a->cell[0], a->cell[1]);
      if (strcmp(op, "!=") == 0)
         r = !lval_eq(a->cell[0], a->cell[1]);

      lval_del(a);
      return lval_num(r);
}

lval *builtin_eq(lenv *e, lval *a) {
   return builtin_cmp(e, a, "==");
}

lval *builtin_ne(lenv *e, lval *a) {
   return builtin_cmp(e, a, "!=");
}


lval *builtin_if(lenv *e, lval *a) {
   LASSERT(a, a->count == 3,
      "Function 'if' passed incorrect number of arguments. "
      "Got %s, expected %s.",
      a->count, 3);

   for (int i = 0; i < a->count; i++) {
      int type = (i == 0) ? LVAL_NUM : LVAL_QEXPR;
      LASSERT(a, a->cell[i]->type == type, 
         "Function 'if' passed incorrect type for argument %i. "
         "Got %s, expected %s.",
         i, ltype_name(a->cell[i]->type), ltype_name(type));
   }

   lval *x;
   a->cell[1]->type = LVAL_SEXPR;
   a->cell[2]->type = LVAL_SEXPR;

   if (a->cell[0]->num)
      x = lval_eval(e, lval_pop(a, 1));
   else
      x = lval_eval(e, lval_pop(a, 2));

   lval_del(a);
   return x;
}

void lenv_put(lenv *e, lval *k, lval *v);

void lenv_add_var(lenv *e, char *name, double x) {
   lval *k = lval_sym(name);
   lval *v = lval_num(x);
   lenv_put(e, k, v);
   lval_del(k);
   lval_del(v);
}

void lenv_print(lenv *e);

lval *builtin_env(lenv *e, lval *a) {
   lenv_print(e);
   return NULL;
}

lval *builtin_exit(lenv *e, lval *a) {
   exit(1);
   return NULL;
}

lval *builtin_lambda(lenv *e, lval *a) {
   LASSERT(a, a->count == 2, 
      "Function 'lambda' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 2);

   LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
      "Function 'lambda passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));
 
   LASSERT(a, a->cell[1]->type == LVAL_QEXPR,
      "Function 'lambda passed incorrect type for argument 1. "
      "Got %s, expected %s.",
      ltype_name(a->cell[1]->type), ltype_name(LVAL_QEXPR));

   for (int i = 0; i < a->cell[0]->count; i++)
      LASSERT(a, a->cell[0]->cell[i]->type == LVAL_SYM,
         "Cannot define non-symbol. Got %s, expected %s.",
         ltype_name(a->cell[0]->cell[i]->type), ltype_name(LVAL_SYM));

   lval *formals = lval_pop(a, 0);
   lval *body = lval_pop(a, 0);
   lval_del(a);

   return lval_lambda(formals, body);
}

lval *builtin_not(lenv *e, lval *a) {
   LASSERT(a, a->count == 1,
      "Function 'not' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 1);

   LASSERT(a, a->cell[0]->type == LVAL_NUM,
      "Function 'not' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_NUM));

      lval *x = lval_pop(a, 0);
      x->num = !(x->num);
      lval_del(a);
      return x;
}

lval *builtin_or(lenv *e, lval *a) {
   LASSERT(a, a->count == 2,
      "Function 'not' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 2);

   for (int i = 0; i < a->count; i++) {
      LASSERT(a, a->cell[i]->type == LVAL_NUM,
         "Function 'not' passed incorrect type for argument %i. "
         "Got %s, expected %s.",
         i, ltype_name(a->cell[i]->type), ltype_name(LVAL_NUM));
   }
      lval *x = lval_pop(a, 0);
      lval *y = lval_pop(a, 0);
      x->num = x->num || y->num;
      lval_del(y);
      lval_del(a);
      return x;
}

lval *builtin_and(lenv *e, lval *a) {
   LASSERT(a, a->count == 2,
      "Function 'not' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 2);

   for (int i = 0; i < a->count; i++) {
      LASSERT(a, a->cell[i]->type == LVAL_NUM,
         "Function 'not' passed incorrect type for argument %i. "
         "Got %s, expected %s.",
         i, ltype_name(a->cell[i]->type), ltype_name(LVAL_NUM));
   }
      lval *x = lval_pop(a, 0);
      lval *y = lval_pop(a, 0);
      x->num = x->num && y->num;
      lval_del(y);
      lval_del(a);
      return x;
}

lval *builtin_head(lenv *e, lval* a) {
   LASSERT(a, a->count == 1, 
      "Function 'head' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 1);

   LASSERT(a, a->cell[0]->type == LVAL_QEXPR || a->cell[0]->type == LVAL_STR, 
      "Function 'head' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));

   if (a->cell[0]->type == LVAL_QEXPR) 
      LASSERT(a, a->cell[0]->count != 0,
         "Function 'head' passed {}!");

   // take first element
   lval *v = lval_take(a, 0);

   if (v->type == LVAL_STR) {
      if (strlen(v->str) > 0) {
         v->str = realloc(v->str, 2);
         v->str[1] = '\0';
      }
      return v;
   }

   // remove every element until one remains
   while (v->count > 1)
      lval_del(lval_pop(v, 1));
   return v;
}

lval *builtin_tail(lenv *e, lval *a) {
   LASSERT(a, a->count == 1, 
      "Function 'tail' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 1);

   LASSERT(a, a->cell[0]->type == LVAL_QEXPR || a->cell[0]->type == LVAL_STR,
      "Function 'tail' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));

   if (a->cell[0]->type == LVAL_QEXPR)
      LASSERT(a, a->cell[0]->count != 0,
         "Function 'tail' passed {}!");

   // take first element
   lval *v = lval_take(a, 0);
   if (v->type == LVAL_STR)
      // remove first character from the string
      memmove(v->str, v->str + 1, strlen(v->str));
   else
      lval_del(lval_pop(v, 0));
   return v;
}

void lenv_def(lenv *e, lval *k, lval *v);

// add easier way for creaeting functions
lval *builtin_fun(lenv *e, lval *a) {
   // fun {name args...} {body}
   LASSERT(a, a->count == 2,
      "Function 'fun' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 2);

   for (int i = 0; i < a->count; i++)
      LASSERT(a, a->cell[i]->type == LVAL_QEXPR,
         "Function 'fun' passed incorrect type for argument %i. "
         "Got %s, expected %s.",
         i, ltype_name(a->cell[i]->type), ltype_name(LVAL_QEXPR));

   lval *name = lval_pop(a->cell[0], 0);
   lval *args = a->cell[0];
   lval *body = a->cell[1];
   lenv_def(e, name, lval_lambda(args, body));
   lval_del(a);
   return lval_sym("ok");
}

// convert lval to QEXPR
lval *builtin_list(lenv *e, lval *a) {
   a->type = LVAL_QEXPR;
   return a;
}

lval *lval_join(lval *x, lval *y) {
   if (x->type == LVAL_STR && y->type == LVAL_STR) {
      // concatenate y string into x string
      strcat(x->str, y->str);
   } else { 
      while(y->count)
         x = lval_add(x, lval_pop(y, 0));
   }

   lval_del(y);
   return x;
}

// join lists together
lval *builtin_join(lenv *e, lval *a) {
   for (int i = 0; i < a->count; i++)
      LASSERT(a, a->cell[i]->type == LVAL_QEXPR || 
         a->cell[i]->type == LVAL_STR,
         "Function 'join' passed incorrect type. "
         "Got %s, expected %s.",
         ltype_name(a->cell[i]->type), ltype_name(LVAL_QEXPR));

   // pop first argument
   lval *x = lval_pop(a, 0);
   while (a->count)
      x = lval_join(x, lval_pop(a, 0));

   lval_del(a);
   return x;
}

// TODO: Add a builtin function cons that takes a value and a 
// Q-Expression and appends it to the front.
lval *builtin_cons(lenv *e, lval *a) {
   LASSERT(a, a->count == 2,
      "Function 'cons' passed incorrect number of arguments!");

   a->cell[0] = builtin_list(e, a->cell[0]);
   a->cell[0]->count = 1;
   printf("%g\n", a->cell[0]->num);
   printf("%d\n", a->cell[0]->count);
   lval *v = builtin_join(e, a);
   return v;

}

// return the length of a list
lval *builtin_len(lenv *e, lval *a) {
   LASSERT(a, a->count == 1,
      "Function 'len' passed too many arguments. "
      "Got %s, expected %s.",
      a->count, 1); 

   LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
      "Function 'len' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR)); 

   LASSERT(a, a->cell[0]->count != 0,
      "Function 'len' passed {}!"); 

   lval *v = lval_take(a, 0);
   v->cell[0]->num = v->count;

   // remove every element until one remains
   while (v->count > 1)
      lval_del(lval_pop(v, 1));
   return v;
}

// return a list without the last element
lval *builtin_init(lenv *e, lval *a) {
   LASSERT(a, a->count == 1,
      "Function 'init' passed too many arguments. "
      "Got %s, expected %s.",
      a->count, 1);

   LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
      "Function 'init' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));

   LASSERT(a, a->cell[0]->count != 0,
      "Function 'init' passed {}!");
   
   lval *v = lval_take(a, 0);
   lval_del(lval_pop(v, v->count - 1));
   return v;
}

lval *builtin_var(lenv *e, lval *a, char *func);

lval *builtin_def(lenv *e, lval *a) {
   return builtin_var(e, a, "def");
}

lval *builtin_put(lenv *e, lval *a) {
   return builtin_var(e, a, "=");
}


lval *builtin_var(lenv *e, lval *a, char *func) {
   LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
      "Function 'def' passed incorrect type for argument 0. "
      "Got %s, expected %s",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));

   lval *syms = a->cell[0];
   for (int i = 0; i < syms->count; i++)
      LASSERT(a, syms->cell[i]->type == LVAL_SYM,
         "Function 'def' cannot define non-symbol. "
         "Got %s, expected %s.",
         ltype_name(syms->cell[i]->type), ltype_name(LVAL_SYM));

   LASSERT(a, syms->count == a->count - 1,
      "Function 'def' cannot define incorrect "
      "number of values to symbols. "
      "Got %i, expected %i.",
      syms->count, a->count - 1);

   for (int i = 0; i < syms->count; i++) {
      // if 'def' define globally, if 'put' define locally
      if (strcmp(func, "def") == 0)
         lenv_def(e, syms->cell[i], a->cell[i+1]);
      if (strcmp(func, "=") == 0)
         lenv_put(e, syms->cell[i], a->cell[i+1]);
   }

   lval_del(a); 
   return lval_sym("ok");
}

lenv *lenv_copy(lenv *e);

lval *lval_copy(lval *v) {
   lval *x = malloc(sizeof(lval));
   x->type = v->type;

   switch (v->type) {
      case LVAL_FUN: 
         if (v->builtin) {
            x->builtin = v->builtin; 
         } else {
            x->builtin = NULL;
            x->env = lenv_copy(v->env);
            x->formals = lval_copy(v->formals);
            x->body = lval_copy(v->body);
         }
      break;

      case LVAL_NUM: x->num = v->num; break;

      // copy strings
      case LVAL_ERR:
         x->err = malloc(strlen(v->err) + 1);
         strcpy(x->err, v->err);
         break;

      case LVAL_SYM:
         x->sym = malloc(strlen(v->sym) + 1);
         strcpy(x->sym, v->sym);
         break;
      
      case LVAL_STR:
         x->str = malloc(strlen(v->str) + 1);
         strcpy(x->str, v->str);
         break;

      // copy lists
      case LVAL_SEXPR:
      case LVAL_QEXPR:
         x->count = v->count;
         x->cell = malloc(sizeof(lval*) * x->count);
         for (int i = 0; i < x->count; i++)
            x->cell[i] = lval_copy(v->cell[i]);
         break;
   }
   return x;
}

lenv *lenv_copy(lenv *e) {
   lenv *n = malloc(sizeof(lenv));
   n->count = e->count;
   n->par = e->par;
   n->syms = malloc(sizeof(char*) * n->count);
   n->vals = malloc(sizeof(lval*) * n->count);
   for (int i = 0; i < e->count; i++) {
      n->syms[i] = malloc(strlen(e->syms[i]) + 1);
      strcpy(n->syms[i], e->syms[i]);
      n->vals[i] = lval_copy(e->vals[i]);
   }
   return n;

}

// get lval from the environment 
lval *lenv_get(lenv *e, lval *k) {
   for (int i = 0; i < e->count; i++)
      if (strcmp(e->syms[i], k->sym) == 0)
         return lval_copy(e->vals[i]);

   if (e->par)
      return lenv_get(e->par, k);
   else
      return lval_err("Unbound Symbol '%s'", k->sym);
}

// put symbol k as lval v into the LOCAL environment e
void lenv_put(lenv *e, lval *k, lval *v) {
   for (int i = 0; i < e->count; i++) {
      if (strcmp(e->syms[i], k->sym) == 0) {
         lval_del(e->vals[i]);
         e->vals[i] = lval_copy(v);
         return;
      }
   }

   e->count++;
   e->vals = realloc(e->vals, sizeof(lval*) * e->count);
   e->syms = realloc(e->syms, sizeof(char*) * e->count);

   e->vals[e->count - 1] = lval_copy(v);
   e->syms[e->count - 1] = malloc(strlen(k->sym) + 1);
   strcpy(e->syms[e->count - 1], k->sym);
}

// put symbol k as lval v into the GLOBAL environment of e 
void lenv_def(lenv *e, lval *k, lval *v) {
   // iterate to outermost environment
   while (e->par)
      e = e->par;

   lenv_put(e, k, v);
}

// add builtin func to the environment e as name
void lenv_add_builtin(lenv *e, char *name, lbuiltin func) {
   lval *k = lval_sym(name); 
   lval *v = lval_fun(func);
   lenv_put(e, k, v);
   lval_del(k);
   lval_del(v);
}

lval *builtin_eval(lenv *e, lval *a);

lval *lval_call(lenv *e, lval *f, lval* a) {
   if (f->builtin)
      return f->builtin(e, a);

   int given = a->count;
   int total = f->formals->count;
   while (a->count) {
      if (f->formals->count == 0) {
         lval_del(a);
         return lval_err("Function passed too many arguments. "
            "Got %i, expected %i.", given, total);
      }
      lval *sym = lval_pop(f->formals, 0);
      if (strcmp(sym->sym, "&") == 0) {
         if (f->formals->count != 1) {
            lval_del(a);
            return lval_err("Function format invalid. "
               "Symbol '&' not followed by single symbol.");
         }
         lval *nsym = lval_pop(f->formals, 0); 
         lenv_put(f->env, nsym, builtin_list(e, a));
         lval_del(sym);
         lval_del(nsym);
         break;
      }
      lval *val = lval_pop(a, 0);
      lenv_put(f->env, sym, val);
      lval_del(sym);
      lval_del(val);
   }

   lval_del(a);
   if (f->formals->count > 0 && 
      strcmp(f->formals->cell[0]->sym, "&") == 0) {
      
      if (f->formals->count != 2)
         return lval_err("Function format invalid. "
            "Symbol '&' not folloewd by single symbol.");

      lval_del(lval_pop(f->formals, 0));
      lval *sym = lval_pop(f->formals, 0);
      lval *val = lval_qexpr();

      lenv_put(f->env, sym, val);
      lval_del(sym);
      lval_del(val);
   }

   if (f->formals->count == 0) {
      f->env->par = e;
      return builtin_eval(f->env, lval_add(lval_sexpr(), lval_copy(f->body)));
   } 
   else
      return lval_copy(f);
}

void lenv_add_builtins(lenv *e) {
   // list functions
   lenv_add_builtin(e, "list", builtin_list);
   lenv_add_builtin(e, "head", builtin_head);
   lenv_add_builtin(e, "tail", builtin_tail);
   lenv_add_builtin(e, "eval", builtin_eval);
   lenv_add_builtin(e, "join", builtin_join);
   lenv_add_builtin(e, "cons", builtin_cons);
   lenv_add_builtin(e, "len", builtin_len);
   lenv_add_builtin(e, "init", builtin_init);
   lenv_add_builtin(e, "read", builtin_read);

   // math functions
   lenv_add_builtin(e, "+", builtin_add);
   lenv_add_builtin(e, "-", builtin_sub);
   lenv_add_builtin(e, "*", builtin_mul);
   lenv_add_builtin(e, "/", builtin_div);
   lenv_add_builtin(e, "%", builtin_mod);
   lenv_add_builtin(e, "^", builtin_pow);

   // variable functions
   lenv_add_builtin(e, "def", builtin_def);
   lenv_add_builtin(e, "=", builtin_put);
   lenv_add_builtin(e, "env", builtin_env);
   lenv_add_builtin(e, "\\", builtin_lambda);

   lenv_add_var(e, "pi", acos(-1));
   lenv_add_var(e, "e", exp(1));
   lenv_add_var(e, "true", true);
   lenv_add_var(e, "false", false);

   lenv_add_builtin(e, "exit", builtin_exit);
   lenv_add_builtin(e, "fun", builtin_fun);

   lenv_add_builtin(e, "if", builtin_if);
   lenv_add_builtin(e, "==", builtin_eq);
   lenv_add_builtin(e, "!=", builtin_ne);
   lenv_add_builtin(e, ">", builtin_gt);
   lenv_add_builtin(e, "<", builtin_lt);
   lenv_add_builtin(e, ">=", builtin_ge);
   lenv_add_builtin(e, "<=", builtin_le);
   
   lenv_add_builtin(e, "!", builtin_not);
   lenv_add_builtin(e, "||", builtin_or);
   lenv_add_builtin(e, "&&", builtin_and);

   lenv_add_builtin(e, "load", builtin_load);
   lenv_add_builtin(e, "error", builtin_error);
   lenv_add_builtin(e, "print", builtin_print);
}

void lenv_print(lenv *e) {
   for (int i = 0; i < e->count; i++) {
      printf("%s: ", e->syms[i]);
      lval_println(e->vals[i]);
   }
}


lval *builtin_eval(lenv *e, lval *a) {
   LASSERT(a, a->count == 1, 
      "Funciton 'eval' passed too many arugments. "
      "Got %s, expected %s.",
      a->count, 1);

   LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
      "Function 'eval' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));

   lval *x = lval_take(a, 0);
   x->type = LVAL_SEXPR;
   return lval_eval(e, x);
}

void lval_function_print(lbuiltin f) {
   if (f == builtin_add)  printf("<function '+'>");
   if (f == builtin_sub)  printf("<function '-'>");
   if (f == builtin_mul)  printf("<function '*'>");
   if (f == builtin_div)  printf("<function '/'>");
   if (f == builtin_mod)  printf("<function '%%'>");
   if (f == builtin_pow)  printf("<function '^'>");

   if (f == builtin_list)  printf("<function 'list'>");
   if (f == builtin_head)  printf("<function 'head'>");
   if (f == builtin_tail)  printf("<function 'tail'>");
   if (f == builtin_join)  printf("<function 'join'>");
   if (f == builtin_cons)  printf("<function 'cons'>");
   if (f == builtin_len)   printf("<function 'len'>");
   if (f == builtin_init)  printf("<function 'init'>");
   if (f == builtin_eval)  printf("<function 'eval'>");

   if (f == builtin_def)      printf("<function 'def'>");
   if (f == builtin_put)      printf("<function '='>");
   if (f == builtin_env)      printf("<function 'env'>");
   if (f == builtin_lambda)   printf("<function 'lambda'>");
   if (f == builtin_fun)      printf("<function 'fun'>");

   if (f == builtin_exit)  printf("<function 'exit'>");
 
   if (f == builtin_if) printf("<function 'if'>");
   if (f == builtin_eq) printf("<function 'eq'>");
   if (f == builtin_ne) printf("<function 'ne'>");
   if (f == builtin_gt) printf("<function 'gt'>");
   if (f == builtin_lt) printf("<function 'lt'>");
   if (f == builtin_ge) printf("<function 'ge'>");
   if (f == builtin_le) printf("<function 'le'>");

   if (f == builtin_not)   printf("<function 'not'>");
   if (f == builtin_or)    printf("<function 'or'>");
   if (f == builtin_and)   printf("<function 'and'>");

   if (f == builtin_load)   printf("<function 'load'>");
   if (f == builtin_error)   printf("<function 'error'>");
   if (f == builtin_print)   printf("<function 'print'>");
}

lval *lval_eval_sexpr(lenv *e, lval *v) {
   // eval children
   for (int i = 0; i < v->count; i++) 
      v->cell[i] = lval_eval(e, v->cell[i]); 
   
   // error check
   for (int i = 0; i < v->count; i++)
      if (v->cell[i]->type == LVAL_ERR)
         return lval_take(v, i); 

   // empty expresison
   if (v->count == 0)
      return v;

   // single expression 
   if (v->count == 1)
      return lval_take(v, 0);

   // ensure first element is a symbol
   lval *f = lval_pop(v, 0);
   if (f->type != LVAL_FUN) {
      lval *err = lval_err(
         "S-Expresison starts with incorrect type. "
         "Got %s, expected %s.",
         ltype_name(f->type), ltype_name(LVAL_FUN));
      lval_del(f);
      lval_del(v);
      return err;
   }

   // call builtin with operator
   lval *res = lval_call(e, f, v);
   lval_del(f);
   return res;
}

lval *lval_eval(lenv *e, lval *v) {
   if (v->type == LVAL_SEXPR)
      return lval_eval_sexpr(e, v);

   if (v->type == LVAL_SYM) {
      lval *x = lenv_get(e, v);
      if (x->type == LVAL_FUN) {
         if (x->builtin == builtin_exit) // exit
            x->builtin(e, v);
         if (x->builtin == builtin_env) // list env
            x->builtin(e, v);
      }
      lval_del(v);
      return x;
   }

   return v;
}

#if 0
   /** exercises **/
int number_of_nodes(mpc_ast_t* t) {
   if (t->children_num == 0) return 1;
   else {
      int total = 1;
      for (int i = 0; i < t->children_num; i++)
         total += number_of_nodes(t->children[i]);
      return total;
   }
   return 0;
}

int number_of_leaves(mpc_ast_t *t) {
   if (t->children_num == 0) return 1;
   else { 
      int total = 0;
      for (int i = 0; i < t->children_num; i++)
         total += number_of_leaves(t->children[i]);
      return total;
   }
   return 0;
}

int number_of_branches(mpc_ast_t *t) {
   if (t->children_num == 0) return 0;
   else {
      int total = 0;
      for (int i = 0; i < t->children_num; i++)
         total += 1 + number_of_branches(t->children[i]); 
      return total;
   }
   return 0;
}

int max_children_from_branch(mpc_ast_t *t) {
   if (t->children_num == 0) return 1;
   else {
      int total = 0;
      for (int i = 0; i < t->children_num; i++)
          total = max(total, 1 + max_children_from_branch(t->children[i])); 
      return total;
   }
   return 0;
}
#endif

int main(int argc, char *argv[]) {
   Number = mpc_new("number");
   Symbol = mpc_new("symbol");
   String = mpc_new("string");
   Comment = mpc_new("comment");
   Sexpr = mpc_new("sexpr");
   Qexpr = mpc_new("qexpr");
   Expr = mpc_new("expr");
   Lispy = mpc_new("lispy"); 

   mpca_lang(MPCA_LANG_DEFAULT,
    "                                                                  \
      number   : /-?[0-9]+(\\.[0-9]+)?/ ;                              \
      symbol   : \"list\" | \"head\" | \"tail\" | \"join\" | \"eval\"  \
               | \"len\" | \"init\" | \"cons\"                         \
               | /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&|%^]+/ ;                 \
      string   : /\"(\\\\.|[^\"])*\"/ ;                                \
      comment  : /;[^\\r\\n]*/ ;                                       \
      sexpr    : '(' <expr>* ')' ;                                     \
      qexpr    : '{' <expr>* '}' ;                                     \
      expr     : <number> | <symbol> | <string> | <comment> | <sexpr>  \
               | <qexpr> ;                                             \
      lispy    : /^/ <expr>* /$/ ;                                     \
    ",
    Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lispy);
  
   lenv *e = lenv_new();
   lenv_add_builtins(e);
   if (argc == 1) {
      puts("Press Ctrl+C to Exit\n");

      // load standard library
      lval *a = lval_add(lval_sexpr(), lval_str("prelude.lspy"));
      lval *x = builtin_load(e, a); 
      lval_del(x);
      while (true) {
         fputs("> ", stdout);
         fgets(input, BUFFER_SIZE, stdin);

         mpc_result_t r;
         if (mpc_parse("<stdin>", input, Lispy, &r)) {
            lval *res = lval_eval(e, lval_read(r.output));
            lval_println(res);
            lval_del(res);
            mpc_ast_delete(r.output);
         } else {
            mpc_err_print(r.error);
            mpc_err_delete(r.error);
         }
      }
   }
   
   if (argc >= 2) {
      for (int i = 1; i < argc; i++) {
         lval *args = lval_add(lval_sexpr(), lval_str(argv[i]));
         lval *x = builtin_load(e, args);
         if (x->type == LVAL_ERR)
            lval_println(x);
         lval_del(x);
      }
   }

   mpc_cleanup(8, Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lispy);
   lenv_del(e);
   return 0;
}
