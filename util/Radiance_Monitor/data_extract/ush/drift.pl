#! /usr/bin/perl

#-----------------------------------------------------------------------------
#  drift.pl
#
#  Generate the drift report data.  Compare the 7 day average files to the
#  established "normal" [satype].base files (which average data over a 30 
#  day period).  
#
#  Perl was used for this script because of the need to compare floats in 
#  scientific notation.
#
#
#  Log
#  ===
#  03/2010	Safford		Removed lower bound check on penalty values.
#  03/2010	Safford		Reworded to ">,< than norm"
#  05/2010      Safford         Reduce report to just region 1 (global).
#  07/2010      Safford		rm check on obs -- summer sndrd* variances in
#                               obs counts make this very difficult to judge
#                               what is a bad count value
#-----------------------------------------------------------------------------

$numArgs = $#ARGV + 1;

if( $numArgs < 1 ) {
   print"  No satname specified\n";
   print" \n";
   print"  Usage: \n";
   print"    mk_drift.pl satname \n"; 
   exit( -1);
}
else {
   $satname = @ARGV[0];
   print"  $satname \n" ;
}

$base_file = sprintf( "%s%s", $satname, ".base" );
$test_file = sprintf( "%s%s", $satname, ".test" );
$url       = sprintf( "%s",  "http://www.emc.ncep.noaa.gov/gmb/gdas/radiance/esafford/opr/index.html?" );

#----------------------------------------------------------
#  Load the base file for just region 1.
#----------------------------------------------------------
open( BFILE, $base_file ) or die "Can't open $base_file \n";
$line = <BFILE>;
@fields = split ' ' , $line;
$nchan = $fields[3];
$nregn = $fields[4];

while( $line = <BFILE>) {
   @fields = split ' ', $line;
   if( $fields[1] == 1 ) {
      push @base, [$fields[0], $fields[1], $fields[2], $fields[3], $fields[4], $fields[5]], $fields[6], $fields[7], $fields[8], $fields[9];
   }
} 
close BFILE;

#----------------------------------------------------------
#  Load the test file for just region 1.
#----------------------------------------------------------
open( TFILE, $test_file ) or die "Can't open $test_file \n";
$line = <TFILE>;
@fields = split ' ', $line;

while( $line = <TFILE>) {
   @fields = split ' ', $line;
   if( $fields[1] == 1 ) {
      push @test, [$fields[0], $fields[1], $fields[2], $fields[3], $fields[4], $fields[5]], $fields[6], $fields[7], $fields[8], $fields[9];
   }
} 
close TFILE;

$nbase = scalar( @base );
$ntest = scalar( @test );

if( $ntest == $nbase ) {

#   open ( OUTFILE, ">>drift_obs.txt" );
#   printf( OUTFILE " \n" );

   #----------------------------------------------------------
   #  Evaluate the obs values
   #----------------------------------------------------------
#   for $i( 0 .. $ntest ) {

#      $base_obs_avg = $base[$i][2];
#      $base_obs_sdv = $base[$i][3];

      #-------------------------------------------------------
      #  If base_obs_avg < 0 the channel is not assimilated
      #-------------------------------------------------------
#      if( $base_obs_avg > 0.0 ) {
#         $found_problem = 0;

#         $hi = $base_obs_avg + (1 * $base_obs_sdv);
#         $lo = $base_obs_avg - (1 * $base_obs_sdv);

#         $hi_limit = $hi + ( 0.15 * $hi );
#         $lo_limit = $lo - ( 0.15 * $lo );

#         $test_obs = $test[$i][2];

#         if( $test_obs > $hi_limit ) {
#            $found_problem = 1;
#            $problem = sprintf( "%s", "> than norm" );
#         } 
#         elsif( $test_obs < $lo_limit ) {
#            $found_problem = 1;
#            $problem = sprintf( "%s", "< than norm" );
#         }
#         if( $found_problem > 0 ) {
#            $ctlfile = sprintf( "%s%s", $satname, ".ctl" );
#            my $changrp = `/u/wx20es/home/radweb_global_dev/scripts/get_channel_grp.pl $ctlfile $test[$i][0]`;

#            $link = sprintf( "%s%s%s%s%s%s%d%s", $url, "sat=", $satname, "&region=region", $test[$i][1], "&channel=", $changrp, "&stat=obs" );

#            printf( OUTFILE "$satname:  channel = $test[$i][0], region = $test[$i][1], $problem		$test_obs  $lo\n" );
#            printf( OUTFILE "   $link\n" );
#         }
#      }
#   }
#   close OUTFILE;

   #----------------------------------------------------------
   #  Evaluate the pen values
   #----------------------------------------------------------
   open ( OUTFILE, ">>drift_pen.txt" );
   printf( OUTFILE " \n" );

   for $i( 0 .. $ntest ) {
      $base_pen_avg = $base[$i][6];
      $base_pen_sdv = $base[$i][7];

      #-------------------------------------------------------
      #  If base_pen_avg < 0 the channel is not assimilated
      #-------------------------------------------------------
      if( $base_pen_avg > 0.0 ) {
         $hi = ( $base_pen_avg + (1 * $base_pen_sdv) );
         $hi_limit = $hi + ( 0.15 * $hi );
 
         $test_pen = $test[$i][6];
         if( $test_pen > $hi_limit ) {

            $ctlfile = sprintf( "%s%s", $satname, ".ctl" );
            my $changrp = `/u/wx20es/home/radweb_global_dev/scripts/get_channel_grp.pl $ctlfile $test[$i][0]`;
            
            $link = sprintf( "%s%s%s%s%s%s%d%s", $url, "sat=", $satname, "&region=region", $test[$i][1], "&channel=", $changrp, "&stat=pen" );

            printf( OUTFILE "$satname:  channel = $test[$i][0], region = $test[$i][1], > than norm	$test_pen  $hi\n" );
            printf( OUTFILE "   $link\n" );
         } 
      }
   }
   close OUTFILE;
}
else {
   open( OUTFILE, ">>drift_err.txt" );
   printf( OUTFILE " ERROR:  Mismatch record count between $base_file and $test_file \n" );
   close OUTFILE;
}


