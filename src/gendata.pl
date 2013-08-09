#!/usr/bin/env perl
use strict;
use warnings;

die("Give me a program!") if (@ARGV < 1);
my $program = $ARGV[0];

open(my $f, '>', 'data.h');

# #define PROGSIZE 25
# term_t ok[] = {Zero, One, Arg, And, Fold, Acc, Byte, If, Not, Plus, Shr1, Shr16, Shr4, Xor};
# uint64_t test_values[]  = {0, 0x100, 0x1234123412341234, 0x5634123412341234, 0x6789123412341234};
# uint64_t test_results[] = {0, 0,     0x1234123412,       0x5634123412,       0x67891234121};
# #define RETRY_TIME 10

my $progsize = trim(`runhaskell Main.hs size "$program"`);

sub trim {
       return $_[0] =~ s/^\s+|\s+$//rg;
   }

sub genVals {
    my $n = shift;

    my @values = ();
    push(@values, "0x" . join "", map { unpack "H*", chr(rand(256)) } 1..8) for (1..$n);

    return @values;
}

my @vals = ("0x0", "0x12345678", "0x1234567800000000", "0x1234567890ABCDEF", genVals(10));
my @ress = map { trim(`runhaskell Main.hs eval "$program" $_`) } @vals;

my $values = join(', ', @vals);
my $results = join(', ', @ress);

my $operators = trim(`runhaskell Main.hs operators "$program"`);
$operators =~ s/[^\w,]//g;
$operators =~ s/(\w+)/\u$1/g;

my $tfold = "";
if ($operators =~ /Tfold/) {
    $tfold = "#define TFOLD";
    $operators =~ s/Tfold/Fold/g;
}

$operators =~ s/Fold/Fold,Acc,Byte/g;

print $f <<EOF;
#define PROGSIZE $progsize
term_t ok[] = {Zero,One,Arg,$operators};
uint64_t test_values[]  = {$values};
uint64_t test_results[] = {$results};
#define RETRY_TIME 10
$tfold
EOF

close($f);
