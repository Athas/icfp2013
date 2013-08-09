#define PROGSIZE 25
term_t ok[] = {Zero, One, Arg, And, Fold, Acc, Byte, If, Not, Plus, Shr1, Shr16, Shr4, Xor};
uint64_t test_values[]  = {0, 0x100, 0x1234123412341234, 0x5634123412341234, 0x6789123412341234};
uint64_t test_results[] = {0, 0,     0x1234123412,       0x5634123412,       0x67891234121};
#define RETRY_TIME 10
