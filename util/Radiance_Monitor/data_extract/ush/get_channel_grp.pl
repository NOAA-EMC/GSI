#! /usr/bin/perl

#-----------------------------------------------------------------------------
#  get_channel_grp.pl
#
#  Find the channel grouping number for a given satellite and channel.
#
#
#  Log
#  ===
#  04/2010  safford  initial coding
#-----------------------------------------------------------------------------

$numArgs = $#ARGV + 1;

if( $numArgs < 2 ) {
   print"  Usage: \n";
   print"    mk_drift.pl sat_control_file, channel \n"; 
   exit( -1);
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
