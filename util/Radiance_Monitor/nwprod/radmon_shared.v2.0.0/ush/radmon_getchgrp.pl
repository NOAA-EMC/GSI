#! /usr/bin/perl

################################################################################
####  Perl Script Documentation Block
#                      .                                             .
# Script name:         radmon_getchgrp.pl
# Script description:  Find the channel grouping number for a given setllite and
#                      channel.
#
# Author:        Ed  Safford       Org: NP23         Date: 2012-02-02
#
# Abstract:  This script determines the channel grouping number for a given    
#            satellite and channel number.  Most, but not all satellite sources
#            have consecutively numbered channels.  This script locates the 
#            channel and determines in which block of images that channel would be 
#            located.
#
#
# Script history log:
# 2012-02-02  Safford  initial script
#
# Usage:  radmon_getchgrp.pl ctl_file channel
#
#   Input script positional parameters:
#     ctl_file          meta-data file 
#                       required
#     channel           satellite/instrument channel number
#                       required
#
#     output data: $chan_grp
#
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#  Control variable resolution priority
#    1 Command line argument.
#
# Attributes:
#   Language: Perl script
#   Machine: IBM SP
####################################################################

$numArgs = $#ARGV + 1;

if( $numArgs < 2 ) {
   print"  Usage: \n";
   print"    radmon_gtchgrp.pl sat_control_file, channel \n"; 
   exit(1);
}
else {
   $ctl_file = @ARGV[0];
   $channel = @ARGV[1];
}

#----------------------------------------------------------
#  Load the control file
#----------------------------------------------------------
open( CTLFILE, $ctl_file ) or die "Can't open $ctl_file \n";

$chan_grp = 0;
$ctr = 0;

while( $line = <CTLFILE>) {
   if ( $line =~ m/iuse/ ) {
      $ctr+=1; 
      @fields = split ' ', $line;
      if( $fields[4] == $channel ) {
         $chan_grp = int( $ctr/4 );
         if( $ctr % 4 > 0 ) {
            $chan_grp +=1;
            break;
         }
      }
   }
} 

print "$chan_grp\n";
close CTLFILE;
