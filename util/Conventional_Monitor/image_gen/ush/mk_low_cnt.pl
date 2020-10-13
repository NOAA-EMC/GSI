#! /usr/bin/perl

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#  mk_low_cnt.pl
#
#    Arguments: 
#       --dir     : Required string value containing path to $TANKdir
#                     (including nbns/stats).  Required input.
#       --cyc     : 10 digit cycle time.  Required input.
#       --net     : data source idenifier.  Default is GFS.
#       --run     : Run name, generally 'gdas' or 'gfs'. gdas is the  
#		    default value.
#
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

   use strict;
   use warnings;
   use Getopt::Long;
   use Scalar::Util qw(looks_like_number);


   #---------------------------------------------------------
   #  subroutine trim
   #
   #  strip leading and trailing white space from a string
   #---------------------------------------------------------
   sub  trim {
      my $s = shift;
      $s =~ s/^\s+|\s+$//g;
      return $s
   };
 

   ##------------------------------------------------------------------
   ##------------------------------------------------------------------
   ##------------------------------------------------------------------
   ##
   ##  begin main 
   ##
   ##------------------------------------------------------------------
   ##------------------------------------------------------------------
   ##------------------------------------------------------------------

   my $net  = 'GFS';
   my $run  = 'gdas';
   my $cyc  = '';
   my $dir  = '';

   GetOptions( 'net:s' => \$net,
               'run:s' => \$run,
               'cyc:s' => \$cyc,
               'dir:s' => \$dir);

   print " net = $net\n";
   print " run = $run\n";
   print " cyc = $cyc\n";
   print " dir = $dir\n";
              
 
   #------------------------------ 
   #  locate nobs.ges.$pdate file
   #
   my $pdy = substr $cyc, 0, 8;
   my $hr  = substr $cyc, 8, 2;
   print " pdy = $pdy \n";
   print " hr  = $hr \n";

   my $nobs_ges = "${dir}/${net}/${run}.${pdy}/${hr}/conmon/horz_hist/ges/nobs.ges.${cyc}";
   print "nobs_ges = $nobs_ges\n";

   my $comp_nobs = "${nobs_ges}.gz";
   if( -e $comp_nobs ){
      system( "gunzip ${comp_nobs}" );
   }

   #-----------------------
   #  read in nobs file
   #
   open( FILE, "${nobs_ges}" ) or die( "Unable to open ${nobs_ges}" );
   my @nobs_data = <FILE>;
   close( FILE );

   #--------------------------------------------
   # This is a simple csv file, so break on the 
   # comma and push to %nobs_hash.
   #
   my %nobs_hash;
   foreach ( @nobs_data ) {
      my @words = split /,/, $_;
      my $key = trim( $words[0] ) . "_" . trim( $words[1] );
      my $value = trim( $words[2] );
      $nobs_hash{ $key } = $value;
   }

   #-----------------------------------------------
   #  locate gdas_conmon_base.txt, load into hash   
   #
   my $home = "HOME" . ${run} . "_conmon";
   my $base_file = $ENV{ "$home" };
   $base_file=${base_file} . "/fix/" . ${run} . "_conmon_base.txt";
   print "base_file = ${base_file} \n";

   #------------------------------------------------
   #  read in gdas_conmon_base.txt, load into hash
   #
   open( FILE, "${base_file}" ) or die( "Unable to open ${base_file}" );
   my @base_data = <FILE>;
   close( FILE );

   my %base_hash;
   foreach ( @base_data ) {
      my @words = split /,/, $_;
      my $key = trim( $words[0] );
      my $value = trim( $words[1] );
      $base_hash{ $key } = $value;
   }
   foreach my $key (keys %base_hash)
   {
     my $value = $base_hash{$key};
 #    print "  $key , $value\n";
   } 


   #--------------------------------------
   #  Compare nobs count to base.
   #  Create entry in low_cnt.txt file. 
   #  If count is low write to low_cnt.
   #
   my $low_cnt = "${dir}/${net}/${run}.${pdy}/${hr}/conmon/horz_hist/ges/low_cnt.ges.${cyc}";
   open( FILE, '>', "${low_cnt}" ) or die( "Unable to open ${low_cnt}" );

   foreach my $key (keys %nobs_hash)
   {
     my $nobs  = $nobs_hash{ $key };
     my $bound = $base_hash{ $key }; 
     $bound = $bound * 0.75;

     print "key, nobs, bound = $key, $nobs, $bound \n";
     if( $nobs < $bound ) {
        print "RED LIGHT!\n";
        print FILE, "$key, $nobs, $bound\n";
     }
   } 
   close( FILE );

