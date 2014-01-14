#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;
use FindBin;

my $exe = "$FindBin::Bin/../sudoku-solver";
my $tdir = "$FindBin::Bin/";

is( `$exe < $tdir/sudoku-problem1.txt`, <<EOF );
---
| 5, 3, 4 | 6, 7, 8 | 9, 1, 2 |
| 6, 7, 2 | 1, 9, 5 | 3, 4, 8 |
| 1, 9, 8 | 3, 4, 2 | 5, 6, 7 |
---
| 8, 5, 9 | 7, 6, 1 | 4, 2, 3 |
| 4, 2, 6 | 8, 5, 3 | 7, 9, 1 |
| 7, 1, 3 | 9, 2, 4 | 8, 5, 6 |
---
| 9, 6, 1 | 5, 3, 7 | 2, 8, 4 |
| 2, 8, 7 | 4, 1, 9 | 6, 3, 5 |
| 3, 4, 5 | 2, 8, 6 | 1, 7, 9 |
---
EOF

is( `$exe < $tdir/sudoku-problem2.txt`, <<EOF );
---
| 3, 8, 9 | 7, 1, 4 | 6, 2, 5 |
| 6, 4, 2 | 3, 9, 5 | 7, 8, 1 |
| 5, 1, 7 | 8, 2, 6 | 4, 3, 9 |
---
| 4, 2, 6 | 1, 7, 9 | 8, 5, 3 |
| 7, 5, 3 | 4, 8, 2 | 1, 9, 6 |
| 1, 9, 8 | 6, 5, 3 | 2, 4, 7 |
---
| 8, 6, 5 | 9, 4, 7 | 3, 1, 2 |
| 9, 7, 4 | 2, 3, 1 | 5, 6, 8 |
| 2, 3, 1 | 5, 6, 8 | 9, 7, 4 |
---
EOF

is( `$exe < $tdir/sudoku-problem3.txt`, <<EOF );
---
---
EOF

done_testing();
