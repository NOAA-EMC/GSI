#! /usr/bin/perl

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#  mk_low_cnt.pl
#
#    Arguments: 
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

   my $net   = 'GFS';
   my $run   = 'gdas';
   my $cyc   = '';
   my $nobsf = '';
   my $lcntf = '';
   my $basef = '';

   GetOptions( 'net:s'   => \$net,
               'run:s'   => \$run,
               'cyc:s'   => \$cyc,
               'nobsf:s' => \$nobsf,
               'lcntf:s' => \$lcntf,
               'basef:s' => \$basef );

   print " net   = $net\n";
   print " run   = $run\n";
   print " cyc   = $cyc\n";
   print " nobsf = $nobsf\n";
   print " lcntf = $lcntf\n";
   print " basef = $basef\n";

   if( length( $cyc ) != 10 ){
      print "Error:  cyc value of $cyc is missing or invalid\n";              
      exit 1
   }
   
   #----------------------------- 
   #  uncompress the nobs file
   #
   my $compressed_nobs = "${nobsf}.gz";
   if( -e $compressed_nobs ){
      system( "gunzip ${compressed_nobs}" );
   }


   #-----------------------
   #  read in nobs file
   #
   open( FILE, "${nobsf}" ) or die( "Unable to open ${nobsf}" );
   my @nobs_data = <FILE>;
   close( FILE );

   my $hr = substr( $cyc, -2 );
 
   #--------------------------------------------
   # This is a simple csv file, so break on the 
   # comma and push to %nobs_hash.
   #
   my %nobs_hash;
   foreach ( @nobs_data ) {
      my @words = split /,/, $_;
      print " words = @words\n";
      my $key = trim( $words[0] ) . "_" . trim( $words[1] );
      my $value = trim( $words[2] );
      $nobs_hash{ $key } = $value;
   }

   #------------------------------------------------
   #  read in gdas_conmon_base.txt, load into hash
   #
   open( FILE, "${basef}" ) or die( "Unable to open ${basef}" );
   my @base_data = <FILE>;
   close( FILE );

   my %base_hash;
   foreach ( @base_data ) {
      my @words = split /,/, $_;
      print " words = @words\n";
      my $base_hr = trim( $words[0] );
      my $key = trim( $words[1] );
      my $value = trim( $words[2] );

      print "base_hr, hr = $base_hr, $hr \n" ;

      if( $base_hr == $hr ) {
         print " adding $value to hash with key $key\n";
         $base_hash{ $key } = $value;
      }
      else {
         print " not this hour\n";
      }
   }


   foreach my $key (keys %base_hash) {
     my $value = $base_hash{$key};
   } 


   #--------------------------------------
   #  Compare nobs count to base.
   #  Create entry in low_cnt.txt file. 
   #  If count is low write to low_cnt.
   #
   open( FILE, '>', "${lcntf}" ) or die( "Unable to open ${lcntf}" );

   foreach my $key (keys %nobs_hash)
   {
     my $nobs  = $nobs_hash{ $key };
     my $bound = $base_hash{ $key }; 
     my $avg   = $bound;

     #--------------------------------------------
     #  start with a gross check of 75% of average
     #
     $bound = $bound * 0.75;

     if( $nobs < $bound ) {
        print FILE "$key, $nobs, $bound, $avg\n";
     }
   } 
   close( FILE );

