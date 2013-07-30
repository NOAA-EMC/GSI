#! /usr/bin/perl

#-------------------------------------------------------------------
#  cycle_delta.pl
#
#  This determines the delta in cycles between start date and end 
#  date, assuming 4 cycles per day.  The input dates must be in 
#  YYYYMMDDHH format.  The HH values should be in the set 
#  (00, 06, 12, 18), or the HH will be ignored and the returned 
#  delta value will only consider the YYYYMMDD.
#
#  Calling sequence:             
#
#    cycle_delta.pl start_date end_date
#
#  Return value:
#
#    Cycle delta between the two input dates will be printed to
#    stdout.  If this value is negative then end date preceeds
#    start date.
#
#    If an error occurs there will be no output.
#-------------------------------------------------------------------

    use strict;
    use warnings;
    use Date::Calc qw( Delta_Days );

    if( $#ARGV != 1 ){
       exit;
    }   

    my $start_dt = $ARGV[0];
    my $end_dt   = $ARGV[1];

#   parse start date
#
    my $year1  = substr( $start_dt, 0, 4 );
    my $month1 = substr( $start_dt, 4, 2 );
    my $day1   = substr( $start_dt, 6, 2 );
    my $hr1    = substr( $start_dt, 8, 2 );


#   parse end date
#   
    my $year2  = substr( $end_dt, 0, 4 );
    my $month2 = substr( $end_dt, 4, 2 );
    my $day2   = substr( $end_dt, 6, 2 );
    my $hr2    = substr( $end_dt, 8, 2 );

    my $Dy;
    my $Dm; 
    my $Dd;


#   convert the hour values into cycle numbers
#
#     Note: any hour value not in the set (00, 06, 12, 18) is treated as 
#           zero. 
#
    my $cycle1 = 0;
    my $cycle2 = 0;

    if( $hr1 eq "00" ){
       $cycle1 = 1;
    } elsif( $hr1 eq "06" ){
       $cycle1 = 2;
    } elsif( $hr1 eq "12" ){
       $cycle1 = 3;
    } elsif( $hr1 eq "18" ){
       $cycle1 = 4;
    }

    if( $hr2 eq "00" ){
       $cycle2 = 1;
    } elsif( $hr2 eq "06" ){
       $cycle2 = 2;
    } elsif( $hr2 eq "12" ){
       $cycle2 = 3;
    } elsif( $hr2 eq "18" ){
       $cycle2 = 4;
    }


#   Cycle delta is delta in days * 4 plus the hr_delta. 
#   Print out cycle delta.
#
    my $hr_delta = $cycle2 - $cycle1;

    ($Dy) = Delta_Days($year1,$month1,$day1,
                $year2,$month2,$day2);

    my $cycle_delta = ($Dy * 4) + $hr_delta;

    print "$cycle_delta";
