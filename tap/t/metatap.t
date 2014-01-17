#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;

use File::Temp qw( tempdir );

is_haskell_tap( <<HASKELL, <<EXPECT, 'simple pass' );
TAP.pass Nothing
TAP.pass \$ Just "pass"
done_testing
HASKELL
ok 1
ok 2 - pass
1..2
EXPECT

is_haskell_tap( <<HASKELL, <<EXPECT, 'simple fail' );
TAP.fail Nothing
TAP.fail \$ Just "fail"
done_testing
HASKELL
not ok 1
not ok 2 - fail
1..2
# Looks like you failed 2 tests of 2.
EXPECT

is_haskell_tap( <<HASKELL, <<EXPECT, 'pass and fail' );
TAP.pass \$ Just "pass"
TAP.fail \$ Just "fail"
done_testing
HASKELL
ok 1 - pass
not ok 2 - fail
1..2
# Looks like you failed 1 test of 2.
EXPECT

is_haskell_tap( <<HASKELL, <<EXPECT, 'ok' );
ok True \$ Just "pass"
ok False \$ Just "fail"
ok (1 == 1) Nothing
done_testing
HASKELL
ok 1 - pass
not ok 2 - fail
ok 3
1..3
# Looks like you failed 1 test of 3.
EXPECT

is_haskell_tap( <<HASKELL, <<EXPECT, 'is/isnt' );
TAP.is 1 1 \$ Just "is pass"
TAP.is 1 2 \$ Just "is fail"
TAP.isnt 1 2 \$ Just "isnt pass"
TAP.isnt 1 1 \$ Just "isnt fail"
done_testing
HASKELL
ok 1 - is pass
not ok 2 - is fail
#          got: '1'
#     expected: '2'
ok 3 - isnt pass
not ok 4 - isnt fail
#          got: '1'
#     expected: anything else
1..4
# Looks like you failed 2 tests of 4.
EXPECT

is_haskell_tap( <<HASKELL, <<EXPECT, 'or' );
TAP.pass (Just "pass") `TAP.or` do diag "pass diag"
TAP.fail (Just "fail") `TAP.or` do diag "fail diag"
done_testing
HASKELL
ok 1 - pass
not ok 2 - fail
# fail diag
1..2
# Looks like you failed 1 test of 2.
EXPECT

done_testing();

sub is_haskell_tap
{
    my ( $haskell, $expect, $message ) = @_;

    my $tmpdir = tempdir( CLEANUP => 1 );

    my $fname = "$tmpdir/tap.t.hs";
    my $exe_fname = "$tmpdir/tap.t";

    open my $fh, '>', $fname
      or return fail( "could not open $fname for writing: $!" );

    $haskell =~ s/(^|\n)/$1            /g; # indent to "do"

    $haskell = <<HASKELL;
module Main where

import TAP

mainTests = do
$haskell

main = runTests mainTests
HASKELL

    print {$fh} $haskell;

    close $fh
      or return fail( "error closing $fname: $!" );

    open my $compile_pipe, '-|', 'bash', '-c', "ghc $fname 2>&1"
      or return fail( "can't open pipe 'ghc $fname'" );

    note( $_ ) while <$compile_pipe>;

    $? == 0 or do { fail( "error compiling $fname" ); diag( $haskell ); return; };

    open my $run_pipe, '-|', 'bash', '-c', "$exe_fname 2>&1"
      or return fail( "can't open pipe '$exe_fname'" );

    my $got = join '', <$run_pipe>;

    subtest $message => sub
    {
        is( $?, 0, "exitval" );
        is_deeply( [ split /\n/, $got ], [ split /\n/, $expect ], 'output' );
    };
}
