#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;
use FindBin;

my $exe = "$FindBin::Bin/../sudoku";
my $tdir = "$FindBin::Bin/";

is( `$exe < $tdir/sudoku-example1.txt`, <<EOF );
Case #1: Yes
Case #2: No
Case #3: No
EOF

is( `$exe < $tdir/sudoku-example-2x2.txt`, <<EOF );
Case #1: Yes
Case #2: No
Case #3: No
EOF

is( `$exe < $tdir/a-small-practice.txt`, <<EOF );
Case #1: Yes
Case #2: No
Case #3: No
Case #4: No
Case #5: Yes
Case #6: No
Case #7: No
Case #8: Yes
Case #9: Yes
Case #10: Yes
Case #11: No
Case #12: Yes
Case #13: Yes
Case #14: Yes
Case #15: Yes
Case #16: Yes
Case #17: Yes
Case #18: Yes
Case #19: Yes
Case #20: Yes
Case #21: No
Case #22: No
Case #23: Yes
Case #24: Yes
Case #25: No
Case #26: Yes
Case #27: Yes
Case #28: Yes
Case #29: Yes
Case #30: No
Case #31: Yes
Case #32: Yes
Case #33: Yes
Case #34: Yes
Case #35: No
Case #36: No
Case #37: No
Case #38: Yes
Case #39: Yes
Case #40: Yes
Case #41: Yes
Case #42: Yes
Case #43: No
Case #44: Yes
Case #45: No
Case #46: No
Case #47: No
Case #48: No
Case #49: No
Case #50: Yes
Case #51: No
Case #52: Yes
Case #53: Yes
Case #54: Yes
Case #55: Yes
Case #56: No
Case #57: Yes
Case #58: Yes
Case #59: Yes
Case #60: Yes
Case #61: Yes
Case #62: Yes
Case #63: Yes
Case #64: Yes
Case #65: No
Case #66: Yes
Case #67: Yes
Case #68: No
Case #69: Yes
Case #70: Yes
Case #71: Yes
Case #72: No
Case #73: Yes
Case #74: Yes
Case #75: Yes
Case #76: No
Case #77: No
Case #78: Yes
Case #79: Yes
Case #80: Yes
Case #81: Yes
Case #82: Yes
Case #83: No
Case #84: No
Case #85: Yes
Case #86: Yes
Case #87: Yes
Case #88: No
Case #89: Yes
Case #90: Yes
Case #91: Yes
Case #92: Yes
Case #93: Yes
Case #94: Yes
Case #95: No
Case #96: No
Case #97: Yes
Case #98: Yes
Case #99: No
Case #100: Yes
EOF

is( `$exe < $tdir/a-large-practice.txt`, <<EOF );
Case #1: Yes
Case #2: No
Case #3: No
Case #4: No
Case #5: Yes
Case #6: No
Case #7: No
Case #8: Yes
Case #9: Yes
Case #10: Yes
Case #11: No
Case #12: Yes
Case #13: Yes
Case #14: Yes
Case #15: Yes
Case #16: Yes
Case #17: Yes
Case #18: Yes
Case #19: Yes
Case #20: Yes
Case #21: No
Case #22: No
Case #23: Yes
Case #24: Yes
Case #25: No
Case #26: Yes
Case #27: Yes
Case #28: Yes
Case #29: Yes
Case #30: No
Case #31: Yes
Case #32: Yes
Case #33: Yes
Case #34: Yes
Case #35: No
Case #36: No
Case #37: No
Case #38: Yes
Case #39: Yes
Case #40: Yes
Case #41: Yes
Case #42: Yes
Case #43: No
Case #44: Yes
Case #45: No
Case #46: No
Case #47: No
Case #48: No
Case #49: No
Case #50: Yes
Case #51: No
Case #52: Yes
Case #53: Yes
Case #54: Yes
Case #55: Yes
Case #56: No
Case #57: Yes
Case #58: Yes
Case #59: Yes
Case #60: Yes
Case #61: Yes
Case #62: Yes
Case #63: Yes
Case #64: Yes
Case #65: No
Case #66: Yes
Case #67: Yes
Case #68: No
Case #69: Yes
Case #70: Yes
Case #71: Yes
Case #72: No
Case #73: Yes
Case #74: Yes
Case #75: Yes
Case #76: No
Case #77: No
Case #78: Yes
Case #79: Yes
Case #80: Yes
Case #81: Yes
Case #82: Yes
Case #83: No
Case #84: No
Case #85: Yes
Case #86: Yes
Case #87: Yes
Case #88: No
Case #89: Yes
Case #90: Yes
Case #91: Yes
Case #92: Yes
Case #93: Yes
Case #94: Yes
Case #95: No
Case #96: No
Case #97: Yes
Case #98: Yes
Case #99: No
Case #100: Yes
EOF

done_testing();
