#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;

use File::Temp qw( tempdir );
use List::Util qw( max );

is_haskell_tap( <<HASKELL, <<PERL, 'empty' );
done_testing
HASKELL
done_testing
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'pass, noplan' );
pass Nothing
HASKELL
pass
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'simple pass' );
pass Nothing
pass \$ Just "pass"
done_testing
HASKELL
pass();
pass( "pass" );
done_testing();
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'simple fail' );
TAP.fail Nothing
TAP.fail \$ Just "fail"
done_testing
HASKELL
fail();
fail( "fail" );
done_testing();
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'pass and fail' );
pass \$ Just "pass"
TAP.fail \$ Just "fail"
done_testing
HASKELL
pass( "pass" );
fail( "fail" );
done_testing()
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'ok' );
ok True \$ Just "pass"
ok False \$ Just "fail"
ok (1 == 1) Nothing
done_testing
HASKELL
ok( 1, "pass" );
ok( undef, "fail" );
ok( 1 == 1 );
done_testing();
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'is/isnt' );
is 1 1 \$ Just "is pass"
is 1 2 \$ Just "is fail"
isnt 1 2 \$ Just "isnt pass"
isnt 1 1 \$ Just "isnt fail"
done_testing
HASKELL
is( 1, 1, "is pass" );
is( 1, 2, "is fail" );
isnt( 1, 2, "isnt pass" );
isnt( 1, 1, "isnt fail" );
done_testing();
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'or' );
pass (Just "pass") `TAP.or` do diag "pass diag"
TAP.fail (Just "fail") `TAP.or` do diag "fail diag"
done_testing
HASKELL
pass( "pass" ) or diag( "pass diag" );
fail( "fail" ) or diag( "fail diag" );
done_testing();
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'noplan' );
plan NoPlan
pass Nothing
HASKELL
plan 'no_plan';
pass;
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'good plan' );
plan \$ Tests 1
pass Nothing
HASKELL
plan tests => 1;
pass;
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'bad plan short' );
plan \$ Tests 2
pass Nothing
HASKELL
plan tests => 2;
pass;
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'bad plan long' );
plan \$ Tests 1
pass Nothing
pass Nothing
HASKELL
plan tests => 1;
pass;
pass;
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'double done - no intervening' );
pass Nothing
done_testing
done_testing
HASKELL
pass;
done_testing();
done_testing();
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'double done - intervening pass' );
pass Nothing
done_testing
pass Nothing
done_testing
HASKELL
pass;
done_testing();
pass;
done_testing();
PERL

is_haskell_tap( <<HASKELL, <<PERL, 'double done - intervening fail' );
pass Nothing
done_testing
TAP.fail Nothing
done_testing
HASKELL
pass;
done_testing();
fail;
done_testing();
PERL

done_testing();

sub diagf { my $fmt = shift; diag( sprintf $fmt, @_ ) }

sub is_haskell_tap
{
    my ( $haskell, $perl, $message ) = @_;

    my $tmpdir = tempdir( CLEANUP => 1 );

    subtest $message => sub
    {
        my ( $got, $got_exit ) = run_haskell( $haskell, $tmpdir );
        my ( $expect, $expect_exit ) = run_perl( $perl, $tmpdir );

        my @got_lines = split /\n/, $got;
        my @expect_lines = split /\n/, $expect;
        my $linecount = max( $#got_lines, $#expect_lines );

        is( $got_exit, $expect_exit, "exitval" );
        ok( $got eq $expect, sprintf( "output (%d lines)", $linecount ) )
            or do
            {
                my $max_got_width = max map { length } @got_lines;

                my $fmt = "%${max_got_width}s %s %s";
                diagf( $fmt, "HASKELL", '|', "PERL" );
                for my $line ( 0 .. $linecount )
                {
                    my $got_line = $got_lines[$line] // '';
                    my $expect_line = $expect_lines[$line] // '';
                    diagf
                    (
                        $fmt,
                        $got_line,
                        ($got_line eq $expect_line ? '|' : '!'),
                        $expect_line,
                    );
                }
            };
    };
}

sub run_haskell
{
    my ( $haskell, $tmpdir ) = @_;

    my $exe_fname = "$tmpdir/tap.t";
    my $hs_fname = "$tmpdir/tap.t.hs";

    open my $hs_fh, '>', $hs_fname
      or do { fail( "could not open $hs_fname for writing: $!" ); return; };

    $haskell =~ s/(^|\n)/$1            /g; # indent to "do"

    $haskell = <<HASKELL;
module Main where

import TAP

mainTests = do
$haskell

main = runTests mainTests
HASKELL

    print {$hs_fh} $haskell;

    close $hs_fh
      or do { fail( "error closing $hs_fname: $!" ); return; };

    open my $compile_pipe, '-|', 'bash', '-c', "ghc $hs_fname 2>&1"
      or do { fail( "can't open pipe 'ghc $hs_fname'" ); return; };

    note( $_ ) while <$compile_pipe>;

    $? == 0 or do { fail( "error compiling $hs_fname" ); diag( $haskell ); return; };

    open my $run_hs_pipe, '-|', 'bash', '-c', "$exe_fname 2>&1"
      or do { fail( "can't open pipe '$exe_fname'" ); return; };

    my $output = join '', <$run_hs_pipe>;
    my $exitval = $?;

    return ( $output, $exitval );
}

sub run_perl
{
    my ( $perl, $tmpdir ) = @_;

    my $pl_fname = "$tmpdir/tap.t.pl";

    open my $pl_fh, '>', $pl_fname
      or do { fail( "could not open $pl_fname for writing: $!" ); return; };

    $perl = <<PERL;
#!/usr/bin/env perl

use strict;
use warnings;
use Test::More;

$perl
PERL

    print {$pl_fh} $perl;

    close $pl_fh
      or do { fail( "error closing $pl_fname: $!" ); return; };

    open my $run_pl_pipe, '-|', 'bash', '-c', "perl $pl_fname 2>&1"
      or do { fail( "can't open pipe 'perl $pl_fname'" ); return; };

    my $output = join '', <$run_pl_pipe>;
    my $exitval = $?;

    $output =~ s/ at .* line \d+\.?//g;
    $output =~ s/(^|\n)\s*\#\s*\n/$1/g;

    return ( $output, $exitval );
}
