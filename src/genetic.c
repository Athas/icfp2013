#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

#define arrlen(x) (sizeof(x) / sizeof(x[0]))
#define PROGSIZE 11

typedef enum _term_t {
  Zero,
  One,
  Arg,
  Byte,
  Acc,
  Fold,
  If,
  Not,
  Shl1,
  Shr1,
  Shr4,
  Shr16,
  And,
  Or,
  Xor,
  Plus,
} term_t;
#define TERM_TYPES 16

char *term_strings[] = {
  "0", "1", "arg", "byte", "acc",
  "(fold ", "(if0 ",
  "(not ", "(shl1 ", "(shr1 ", "(shr4 ", "(shr16 ",
  "(and ", "(or ", "(xor ", "(plus "
};


static int printExpr(term_t *terms, int remaining) {
  if(remaining <= 0) return -1;
  fputs(term_strings[terms[--remaining]], stdout);

  switch(terms[remaining]) {
  case Zero:
  case One:
  case Arg:
  case Byte:
  case Acc:
    break;

  case Fold:
    remaining = printExpr(terms, remaining);
    putchar(' ');
    remaining = printExpr(terms, remaining-1);
    fputs(" (lambda (byte acc) ", stdout);
    remaining = printExpr(terms, remaining);
    fputs("))", stdout);
    break;

  case If:
    remaining = printExpr(terms, remaining);
    putchar(' ');
  case And:
  case Or:
  case Xor:
  case Plus:
    remaining = printExpr(terms, remaining);
    putchar(' ');
  case Not:
  case Shl1:
  case Shr1:
  case Shr4:
  case Shr16:
    remaining = printExpr(terms, remaining);
    putchar(')');
    break;

  }
  return remaining;
}

static void printProg(term_t *terms) {
  fputs("(lambda (arg) ", stdout);
  printExpr(terms, PROGSIZE);
  fputs(")\n", stdout);
}

typedef struct _eval_ret {
  uint64_t value;
  int remaining;
} eval_ret;

static eval_ret eval_helper(term_t *terms, int remaining, uint64_t state[5]) {
  eval_ret foo;
  term_t cur;
  int n;
  uint64_t saved_value0, saved_value1;
  int saved_pos;

  foo.remaining = remaining;

  switch(cur = terms[--foo.remaining]) {
  case Zero:
  case One:
  case Arg:
  case Byte:
  case Acc:
    foo.value = state[cur];
    break;

  case Fold:
    foo = eval_helper(terms, foo.remaining, state);
    saved_value0 = foo.value;
    foo = eval_helper(terms, foo.remaining, state);
    state[Acc] = foo.value;
    saved_pos = foo.remaining-1;
    for(n = 0; n < 8; n++) {
      state[Byte] = saved_value0 & 0xff;
      saved_value0 >>= 8;
      foo = eval_helper(terms, saved_pos, state);
      state[Acc] = foo.value;
    }
    break;

  case If:
    foo = eval_helper(terms, foo.remaining, state);
    saved_value0 = foo.value;
    foo = eval_helper(terms, foo.remaining, state);
    saved_value1 = foo.value;
    foo = eval_helper(terms, foo.remaining, state);
    if(!saved_value0) foo.value = saved_value1;
    break;

  case Not:
    foo = eval_helper(terms, foo.remaining, state);
    foo.value = ~foo.value;

  case Shl1:
    foo = eval_helper(terms, foo.remaining, state);
    foo.value = foo.value << 1;
    break;

  case Shr1:
    foo = eval_helper(terms, foo.remaining, state);
    foo.value = foo.value >> 1;
    break;

  case Shr4:
    foo = eval_helper(terms, foo.remaining, state);
    foo.value = foo.value >> 4;
    break;

  case Shr16:
    foo = eval_helper(terms, foo.remaining, state);
    foo.value = foo.value >> 16;
    break;

  case And:
    foo = eval_helper(terms, foo.remaining, state);
    saved_value0 = foo.value;
    foo = eval_helper(terms, foo.remaining, state);
    foo.value &= saved_value0;
    break;

  case Or:
    foo = eval_helper(terms, foo.remaining, state);
    saved_value0 = foo.value;
    foo = eval_helper(terms, foo.remaining, state);
    foo.value |= saved_value0;
    break;

  case Xor:
    foo = eval_helper(terms, foo.remaining, state);
    saved_value0 = foo.value;
    foo = eval_helper(terms, foo.remaining, state);
    foo.value ^= saved_value0;
    break;

  case Plus:
    foo = eval_helper(terms, foo.remaining, state);
    saved_value0 = foo.value;
    foo = eval_helper(terms, foo.remaining, state);
    foo.value += saved_value0;
    break;
  }

  return foo;
}

static uint64_t eval(term_t *terms, uint64_t value) {
  uint64_t state[5] = {0, 1, 0, 0, 0};
  eval_ret foo = eval_helper(terms, PROGSIZE, state);
  return foo.value;
}

typedef struct _program_status {
  int inside_fold;
  int type_error;
} program_status;

int typecheck_helper(term_t *terms, int remaining, program_status *status) {
  if(remaining <= 0) {
    if (status->type_error == 0) {
      status->type_error = 1;
    }
    return 0;
  }

  switch(terms[--remaining]) {
  case Fold:
    if (status->inside_fold != 0) {
      status->type_error = 2;
      return 0;
    }
    remaining = typecheck_helper(terms, remaining, status);
    remaining = typecheck_helper(terms, remaining, status);
    status->inside_fold = 1;
    remaining = typecheck_helper(terms, remaining, status);
    status->inside_fold = 2;
    break;
  case If:
    remaining = typecheck_helper(terms, remaining, status);
  case And:
  case Or:
  case Xor:
  case Plus:
    remaining = typecheck_helper(terms, remaining, status);
  case Not:
  case Shl1:
  case Shr1:
  case Shr4:
  case Shr16:
    remaining = typecheck_helper(terms, remaining, status);
    break;
  case Acc:
  case Byte:
    if (status->inside_fold != 1) {
      status->type_error = 3;
      return 0;
    }
    break;
  case Zero:
  case One:
  case Arg:
    break;
  }

  return remaining;
}


static int typecheck(term_t *terms) {
  program_status status = {0, 0};
  typecheck_helper(terms, PROGSIZE, &status);

  return status.type_error;
}

uint64_t fitness(term_t *terms) {
  uint64_t test_values[10] = {0x17, 0x48, 0xff, 0x500, 0x513513};
  uint64_t test_results[10] = {0x18, 0x49, 0x100, 0x501, 0x513514};
  int n;
  uint64_t fitness = 0, cur;

  for(n = 0; n < arrlen(test_values); n++) {
    cur = eval(terms, test_values[n]);
    if(cur < test_results[n]) {
      cur = test_results[n] - cur;
    } else {
      cur = cur - test_results[n];
    }
    if(fitness + cur < fitness) {
      fitness = 0xffffffffffffffff;
    } else {
      fitness += cur;
    }
  }
  return fitness;
}

int main() {
  int k, good = 0;
  term_t foo[PROGSIZE];
  int values[PROGSIZE] = {0};
  term_t ok[] = {Zero, One, Arg, If, Not, Plus, Shr1};
  int broke;

  srand(time(NULL));

  while(1) {
    if(typecheck(foo) == 0) {
      if(fitness(foo) < 5323638) {
        printf("%lu\n", fitness(foo));
        printProg(foo);
      }
    }
    broke = 0;
    for(k = 0; k < arrlen(values); k++) {
      values[k]++;
      foo[arrlen(foo)-k-1] = ok[values[k]];
      if(values[k] < arrlen(values)) {
        broke = 1;
        break;
      }
      values[k] = 0;
    }
    if(!broke) break;
  }

  return 0;
}
