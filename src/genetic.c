#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <string.h>

#define FIT_BITDIST
//#define FIT_NUMDIFF

#define arrlen(x) (sizeof(x) / sizeof(x[0]))

typedef enum _term_t {
  Zero,
  One,
  Arg,
  Byte,
  Acc,
  Fold,
  If0,
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

#include "data.h"

typedef struct _prog_and_fitness {
  term_t prog[PROGSIZE];
  uint64_t fitness;
} prog_and_fitness;

prog_and_fitness arena[1024+64];

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
    remaining = printExpr(terms, remaining);
    fputs(" (lambda (byte acc) ", stdout);
    remaining = printExpr(terms, remaining-1);
    fputs("))", stdout);
    break;

  case If0:
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

  case If0:
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
    break;

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
  uint64_t state[5] = {0, 1, value, 0, 0};
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
    status->inside_fold = 2;
    remaining = typecheck_helper(terms, remaining, status);
    remaining = typecheck_helper(terms, remaining, status);
    status->inside_fold = 1;
    remaining = typecheck_helper(terms, remaining-1, status);
    status->inside_fold = 2;
    break;
  case If0:
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

#ifdef FIT_NUMDIFF

uint64_t fitness(term_t *terms) {
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
      fitness = -1L;
    } else {
      fitness += cur;
    }
  }
  return fitness;
}

#else

uint64_t fitness(term_t *terms) {

  int n,i;
  uint64_t fitness = 0, cur = 0, res;

  for (n = 0; n < arrlen(test_values); n++) {
    res = eval(terms, test_values[n]);
    res = test_results[n] ^ res;

    for (i = 0; i < 64; i++) {
      cur += res >> i & 0x1;
    }

    if (fitness + cur < fitness) {
      fitness = -1L;
    } else {
      fitness += cur;
    }
    /* printf("fitness: %d\n", fitness); */
  }
  return fitness;
}

#endif

static void mutate(prog_and_fitness *terms, uint8_t chance) {
  int n;

  for(n = 0; n < PROGSIZE; n++) {
    if((rand()&0xff) < chance) {
      terms->prog[n] = ok[rand() % arrlen(ok)];
    }
  }
}

static void mate(prog_and_fitness *dst, prog_and_fitness *src) {
  int n;

  for(n = 0; n < PROGSIZE; n++) {
    if(rand()&1) {
      dst->prog[n] = src->prog[n];
      dst->fitness = 0;
    }
  }
}

int compare(const void *p1_, const void *p2_) {
  const prog_and_fitness *p1 = p1_;
  const prog_and_fitness *p2 = p2_;
  if(p1->fitness < p2->fitness) {
    return -1;
  } else if(p1->fitness > p2->fitness) {
    return 1;
  } else {
    return 0;
  }
}

static void update(prog_and_fitness *progs, int count) {
  int n;

  for(n = 0; n < count; n++) {
#ifdef TFOLD
      progs[n].prog[PROGSIZE-1] = Fold;
      progs[n].prog[PROGSIZE-2] = Arg;
      progs[n].prog[PROGSIZE-3] = Zero;
#endif
    if(progs[n].fitness == 0) {
      if(typecheck(progs[n].prog) != 0) {
        progs[n].fitness = -1L;
      } else {
        progs[n].fitness = fitness(progs[n].prog);
      }
    }
  }
  qsort(progs, count, sizeof(progs[0]), compare);
}

int main() {
  int n, k;
  time_t end;

  /* term_t foo[PROGSIZE] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Arg, 0, 0, 0, Fold}; */
  /* printProg(foo); */
  /* printf("%lu\n", eval(foo, 0x12345678)); */
  /* return 0; */

  srand(time(NULL));


  while(1) {
    for(n = 0; n < arrlen(arena); n++) {
      for(k = 0; k < PROGSIZE; k++) {
        arena[n].prog[k] = ok[rand() % arrlen(ok)];
      }
      arena[n].fitness = 0;
    }
    update(arena, arrlen(arena));
    end = time(NULL) + RETRY_TIME;
    /* printf("Retrying\n"); */
    while(time(NULL) < end) {
      for(k = 0; k < 1024; k++) {
        //    memcpy(&arena[arrlen(arena)-64], &arena[0], 64);
        for(n = arrlen(arena); n > 256; n--) {
          mate(&arena[n], &arena[rand() % n / 2]);
        }
        update(arena, arrlen(arena));
        for(n = arrlen(arena); n > 64; n--) {
          mutate(&arena[n], n / 32);
        }
        update(arena, arrlen(arena));

        if(arena[0].fitness == 0) {
          printProg(arena[0].prog);
          return 0;
        }
      }
      for(n = 0; n < 32; n++) {
        printf("%lu ", arena[n].fitness);
      }
      printf("\n");
    }
  }

  return 0;
}
