#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include "mpc.h"

#define BUFFER_SIZE 2048

int max(int a, int b) { return a > b ? a : b; }

static char input[BUFFER_SIZE];

// number types
typedef enum {
   LVAL_NUM,
   LVAL_ERR
} NUMBER_TYPE;

// error types
typedef enum {
   LERR_DIV_ZERO,
   LERR_BAD_OP,
   LERR_BAD_NUM
} ERROR_TYPE;

typedef union {
   double num;
   int err;
} value;

typedef struct {
   int type;
   value val;
} lval;

// number type lval
lval lval_num(double x) {
   lval v;
   v.type = LVAL_NUM;
   v.val.num = x;
   return v;
}

// error type lval
lval lval_err(ERROR_TYPE x) {
   lval v;
   v.type = LVAL_ERR;
   v.val.err = x;
   return v;
}

// print lval type
void lval_print(lval v) {
   switch (v.type) {
      case LVAL_NUM: printf("%g", v.val.num); break;
      case LVAL_ERR: 
         switch (v.val.err) {
            case LERR_DIV_ZERO: printf("Error: division by zero!"); break;
            case LERR_BAD_OP:   printf("Error: invalid operator!"); break;
            case LERR_BAD_NUM:  printf("Error: invalid number!");   break;
         }
         break;
   }
}

// print lval followed with a newline
void lval_println(lval v) {
   lval_print(v);
   putchar('\n');
}

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

lval eval_op(lval x, char *op, lval y) {
   if (x.type == LVAL_ERR)
      return x;
   else if (y.type == LVAL_ERR)
      return y;

   if (strcmp(op, "+") == 0) return lval_num(x.val.num + y.val.num);
   if (strcmp(op, "-") == 0) return lval_num(x.val.num - y.val.num);
   if (strcmp(op, "*") == 0) return lval_num(x.val.num * y.val.num);
   if (strcmp(op, "^") == 0) return lval_num(pow(x.val.num, y.val.num));
   if (strcmp(op, "/") == 0)
      return y.val.num == 0 
        ? lval_err(LERR_DIV_ZERO) 
        : lval_num(x.val.num / y.val.num);
   if (strcmp(op, "%") == 0) 
      return y.val.num == 0
        ? lval_err(LERR_DIV_ZERO)
        : lval_num(fmod(x.val.num, y.val.num));

   return lval_err(LERR_BAD_OP);
}

lval eval(mpc_ast_t* t) {
   if (strstr(t->tag, "number")) {
      errno = 0;
      double x = strtod(t->contents, NULL);
      return errno != ERANGE ? lval_num(x) : lval_err(LERR_BAD_NUM);
   }

   char *op = t->children[1]->contents;
   lval x = eval(t->children[2]);
   
   if (strcmp(op, "-") == 0 && t->children_num < 5)
      return lval_num(0 - x.val.num);

   int i = 3;
   for (; strstr(t->children[i]->tag, "expr"); i++)
      x = eval_op(x, op, eval(t->children[i]));
   return x;
}

/** exercises **/

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

int main(int argc, char *argv[]) {
   mpc_parser_t* Number = mpc_new("number");
   mpc_parser_t* Operator = mpc_new("operator");
   mpc_parser_t* Expr = mpc_new("expr");
   mpc_parser_t* Lispy = mpc_new("lispy"); 
   mpca_lang(MPCA_LANG_DEFAULT,
    "                                                     \
      number   : /-?[0-9]+(\\.[0-9]+)?/ ;                 \
      operator : '+' | '-' | '*' | '/' | '%' | '^' ;      \
      expr     : <number> | '(' <operator> <expr>+ ')' ;  \
      lispy    : /^/ <operator> <expr>+ /$/ ;             \
    ",
    Number, Operator, Expr, Lispy);
  
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

         lval res = eval(r.output);
         lval_println(res);
         mpc_ast_delete(r.output);
      } else {
         mpc_err_print(r.error);
         mpc_err_delete(r.error);
      }
   }
   mpc_cleanup(4, Number, Operator, Expr, Lispy);
   return 0;
}
