#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include "external/mpc.h"

#define BUFFER_SIZE 2048

#define LASSERT(args, cond, fmt, ...) \
   if (!(cond)) { \
      lval *err = lval_err(fmt, ##__VA_ARGS__); \
      lval_del(args); \
      return err; \
   }

int max(int a, int b) { return a > b ? a : b; }

static char input[BUFFER_SIZE];

struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

// number types
typedef enum {
   LVAL_NUM,
   LVAL_ERR,
   LVAL_SYM,
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
   lbuiltin fun;
   int count;
   struct lval** cell;
};

struct lenv {
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
   e->count = 0;
   e->syms = NULL;
   e->vals = NULL;
   return e;
}

lval *lval_fun(lbuiltin func) {
   lval *v = malloc(sizeof(lval));
   v->type = LVAL_FUN;
   v->fun = func;
   return v;
}

void lval_del(lval *v) {
   switch(v->type) {
      case LVAL_NUM: break;
      case LVAL_ERR: free(v->err); break;
      case LVAL_SYM: free(v->sym); break; 
      case LVAL_SEXPR:
      case LVAL_QEXPR:
         for (int i = 0; i < v->count; i++)
            lval_del(v->cell[i]);
         free(v->cell);
         break;
      case LVAL_FUN: break;
   }
   free(v);
} 

void lenv_del(lenv *e) {
   for (int i = 0; i < e->count; i++) {
      free(e->syms[i]);
      lval_del(e->vals[i]);
   }
   free(e->syms);
   free(e->vals);
   free(e);
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

lval *lval_read(mpc_ast_t *t) {
  if (strstr(t->tag, "number")) return lval_read_num(t);
  if (strstr(t->tag, "symbol")) return lval_sym(t->contents); 

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

// print lval type 
void lval_print(lval *v) {
   switch (v->type) {
      case LVAL_NUM: printf("%g", v->num); break;
      case LVAL_SYM: printf("%s", v->sym); break;
      case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break;
      case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break;
      case LVAL_FUN: printf("<function>"); break;
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

lval *builtin_op(lenv *e, lval* a, char *op) {
   for (int i = 0; i < a->count; i++) {
      if (a->cell[i]->type != LVAL_NUM) {
         lval_del(a);
         return lval_err("Cannot operate on non-number '%s'.", op);
      }
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

lval *builtin_add(lenv *e, lval* a) {
   return builtin_op(e, a, "+");
}

lval *builtin_sub(lenv *e, lval* a) {
   return builtin_op(e, a, "-");
}

lval *builtin_mul(lenv *e, lval* a) {
   return builtin_op(e, a, "*");
}

lval *builtin_div(lenv *e, lval* a) {
   return builtin_op(e, a, "/");
}

char *ltype_name(int t) {
   switch (t) {
      case LVAL_FUN:    return "Function";
      case LVAL_NUM:    return "Number";
      case LVAL_ERR:    return "Error";
      case LVAL_SYM:    return "Symbol";
      case LVAL_SEXPR:  return "S-Expression";
      case LVAL_QEXPR:  return "Q-Expression";
      default:          return "Unknown";
   }
}

lval *builtin_head(lenv *e, lval* a) {
   LASSERT(a, a->count == 1, 
      "Function 'head' passed too many arguments. "
      "Got %i, expected %i.",
      a->count, 1);

   LASSERT(a, a->cell[0]->type == LVAL_QEXPR, 
      "Function 'head' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));

   LASSERT(a, a->cell[0]->count != 0,
      "Function 'head' passed {}!");

   // take first element
   lval *v = lval_take(a, 0);

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

   LASSERT(a, a->cell[0]->type == LVAL_QEXPR, 
      "Function 'tail' passed incorrect type for argument 0. "
      "Got %s, expected %s.",
      ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));

   LASSERT(a, a->cell[0]->count != 0,
      "Function 'tail' passed {}!");

   // take first element
   lval *v = lval_take(a, 0);
   lval_del(lval_pop(v, 0));
   return v;
}

// convert lval to QEXPR
lval *builtin_list(lenv *e, lval *a) {
   a->type = LVAL_QEXPR;
   return a;
}

lval *lval_join(lval *x, lval *y) {
   while(y->count)
      x = lval_add(x, lval_pop(y, 0));

   lval_del(y);
   return x;
}

lval *builtin_join(lenv *e, lval *a) {
   for (int i = 0; i < a->count; i++)
      LASSERT(a, a->cell[i]->type == LVAL_QEXPR,
         "Function 'join' passed incorrect type. "
         "Got %s, expected %s.",
         ltype_name(a->cell[i]->type), ltype_name(LVAL_QEXPR));

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

// return length of a list
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

void lenv_put(lenv *e, lval *k, lval *v);

lval *builtin_def(lenv *e, lval *a) {
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

   for (int i = 0; i < syms->count; i++)
      lenv_put(e, syms->cell[i], a->cell[i+1]);

   lval_del(a); 
   return lval_sexpr();
}

lval *lval_copy(lval *v) {
   lval *x = malloc(sizeof(lval));
   x->type = v->type;

   switch (v->type) {
      case LVAL_FUN: x->fun = v->fun; break;
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

// get lval from the environment 
lval *lenv_get(lenv *e, lval *k) {
   for (int i = 0; i < e->count; i++) {
      if (strcmp(e->syms[i], k->sym) == 0)
         return lval_copy(e->vals[i]);
   }
   return lval_err("Unbound Symbol '%s'", k->sym);
}

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

// add builtin func to the environment e as name
void lenv_add_builtin(lenv *e, char *name, lbuiltin func) {
   lval *k = lval_sym(name); 
   lval *v = lval_fun(func);
   lenv_put(e, k, v);
   lval_del(k);
   lval_del(v);
}

lval *builtin_eval(lenv *e, lval *a);

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

   // math functions
   lenv_add_builtin(e, "+", builtin_add);
   lenv_add_builtin(e, "-", builtin_sub);
   lenv_add_builtin(e, "*", builtin_mul);
   lenv_add_builtin(e, "/", builtin_div);

   // variable functions
   lenv_add_builtin(e, "def", builtin_def);
}

lval *lval_eval(lenv *e, lval *v);

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

lval *builtin(lenv *e, lval *a, char *func) {
   if (strcmp("list", func) == 0) return builtin_list(e, a);
   if (strcmp("head", func) == 0) return builtin_head(e, a);
   if (strcmp("tail", func) == 0) return builtin_tail(e, a);
   if (strcmp("join", func) == 0) return builtin_join(e, a);
   if (strcmp("cons", func) == 0) return builtin_cons(e, a);
   if (strcmp("len", func) == 0) return builtin_len(e, a);
   if (strcmp("init", func) == 0) return builtin_init(e, a);
   if (strcmp("eval", func) == 0) return builtin_eval(e, a);
   if (strstr("+-/*%^", func)) return builtin_op(e, a, func);
   lval_del(a);
   return lval_err("Unknown function %s.", func);
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
      lval_del(f);
      lval_del(v);
      return lval_err("first element %s is not a function",
      ltype_name(f->type));
   }

   // call builtin with operator
   lval *res = f->fun(e, v);
   lval_del(f);
   return res;
}

lval *lval_eval(lenv *e, lval *v) {
   if (v->type == LVAL_SEXPR)
      return lval_eval_sexpr(e, v);

   if (v->type == LVAL_SYM) {
      lval *x = lenv_get(e, v);
      lval_del(v);
      return x;
   }

   return v;
}

/** exercises **/
#if 0
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
lispy>
lispy>
lispy>


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
   mpc_parser_t* Number = mpc_new("number");
   mpc_parser_t* Symbol = mpc_new("symbol");
   mpc_parser_t* Sexpr = mpc_new("sexpr");
   mpc_parser_t* Qexpr = mpc_new("qexpr");
   mpc_parser_t* Expr = mpc_new("expr");
   mpc_parser_t* Lispy = mpc_new("lispy"); 
   mpca_lang(MPCA_LANG_DEFAULT,
    "                                                                  \
      number   : /-?[0-9]+(\\.[0-9]+)?/ ;                              \
      symbol   : \"list\" | \"head\" | \"tail\" | \"join\" | \"eval\"  \
               | \"len\" | \"init\" | \"cons\"                         \
               | /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&%^]+/ ;                   \
      sexpr    : '(' <expr>* ')' ;                                     \
      qexpr    : '{' <expr>* '}' ;                                     \
      expr     : <number> | <symbol> | <sexpr> | <qexpr> ;             \
      lispy    : /^/ <expr>* /$/ ;                                     \
    ",
    Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
  
   lenv *e = lenv_new();
   lenv_add_builtins(e);

   while (true) {
      fputs("> ", stdout);
      fgets(input, BUFFER_SIZE, stdin);

      mpc_result_t r;
      if (mpc_parse("<stdin>", input, Lispy, &r)) {
         /*
         mpc_ast_t *a = r.output;
         printf("Tag: %s\n", a->tag);
         printf("Contents: %s\n", a->contents);
         printf("Number of children: %i\n", a->children_num);

         printf("Tag: %s\n", a->children[2]->tag);
         printf("Contents: %s\n", a->children[2]->contents);
         printf("Number of children: %i\n", a->children[2]->children_num);
         */

         lval *res = lval_eval(e, lval_read(r.output));
         lval_println(res);
         lval_del(res);
         mpc_ast_delete(r.output);
      } else {
         mpc_err_print(r.error);
         mpc_err_delete(r.error);
      }
   }
   mpc_cleanup(6, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
   lenv_del(e);
   return 0;
}
