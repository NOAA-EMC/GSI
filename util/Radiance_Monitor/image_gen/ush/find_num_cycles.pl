#! /usr/bin/perl

#-----------------------------------------------------------------------
#  find_num_cycles.pl
#
#    Given two dates in YYYYMMDDHH format determine the number of cycles
#      between the two.  Each cycle is assumed to be 6 hours, and the
#      input times are assumed to have HH = 00,06,12, or 18.
#
#    Print the number of cycles to stdout for calling scripts to capture.
#
#    Note to self:  There should be no output other than cycles.  Anything
#    else sent to stdout will confuse calling scripts.
#-----------------------------------------------------------------------

use strict;
use warnings;
use Time::Local;
use Time::ParseDate;

#use DateTime;

#print $#ARGV, "\n";

if( $#ARGV != 1 ) {
   exit
}

my $tmstr1 = $ARGV[0];
my $tmstr2 = $ARGV[1];

my $yyyy1  = substr $tmstr1, 0, 4;
my $mm1    = substr $tmstr1, 4, 2;
my $dd1    = substr $tmstr1, 6, 2;
my $hh1    = substr $tmstr1, 8, 2;
my $seconds1 = parsedate("$yyyy1-$mm1-$dd1 $hh1:00");

my $yyyy2  = substr $tmstr2, 0, 4;
my $mm2    = substr $tmstr2, 4, 2;
my $dd2    = substr $tmstr2, 6, 2;
my $hh2    = substr $tmstr2, 8, 2;
my $seconds2 = parsedate("$yyyy2-$mm2-$dd2 $hh2:00");

my $diff   = $seconds2 - $seconds1;

# assuming 60 sec/min, 60 min/hr, 6 hrs/cycle
my $cycles = abs($diff / 21600); 

print $cycles;

exit;
