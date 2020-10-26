#! /usr/bin/perl

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#  mk_err_rpt.pl
#
#    Arguments: 
#       --net          : Data source idenifier.  Default is GFS.
#       --run          : Run name, generally 'gdas' or 'gfs'. gdas 
#		         is the default value.
#       --lcf          : Low count file for the current cycle. 
#       --plcf         : Low count file for the previous cycle.
#       --cyc0         : Current cycle time.
#       --cyc1         : Previous cycle time.
#       --errf         : Output error file.
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
   my $lcf   = '';
   my $plcf  = '';
   my $cyc0  = '';
   my $cyc1  = '';
   my $errf  = '';

   GetOptions( 'net:s'  => \$net,
               'run:s'  => \$run,
               'lcf:s'  => \$lcf,
               'plcf:s' => \$plcf,
               'cyc0:s' => \$cyc0, 
               'cyc1:s' => \$cyc1,
               'errf:s' => \$errf );

   print " net  = $net\n";
   print " run  = $run\n";
   print " lcf  = $lcf\n";
   print " plcf = $plcf\n";
   print " cyc0 = $cyc0\n";
   print " cyc1 = $cyc1\n";
   print " errf = $errf\n";


   #-------------------------------------------------
   #  read in lcf and create a hash to an array 
   #  of its contents
   #
   open( FILE, "${lcf}" ) or die( "Unable to open low count file ${lcf}" );
   my @low_cnt = <FILE>;
   close( FILE );

   my %lcf_hash;
   foreach( @low_cnt ) {
      my @words = split /,/, $_;
      my $key   = trim( $words[0] );

      my @vals  = trim( $words[1] );
      push @vals ,trim( $words[2] );
      push @vals ,trim( $words[3] );

      $lcf_hash{ $key } = \@vals; 
   }


   #------------------------------------------------
   #  read in plcf and create hash to oan array
   #  of its contents
   #
   open( FILE, "${plcf}" ) or die( "Unable to open prev low count file ${plcf}" );
   my @prev_low_cnt = <FILE>;
   close( FILE );

   my %plcf_hash;
   foreach( @prev_low_cnt ) {
      my @words = split /,/, $_;
      my $key   = trim( $words[0] );

      my @vals  = trim( $words[1] );
      push @vals ,trim( $words[2] );
      push @vals ,trim( $words[3] );

      $plcf_hash{ $key } = \@vals; 
   }


   
   open( FILE, '>', "${errf}" ) or die( "Unable to open error file ${errf}" );
   print FILE "ConMon low count warning report\n";
   print FILE "\n";
   print FILE "\n";
   print FILE "     Net:   $net\n";
   print FILE "     Run:   $run\n";
   print FILE "   Cycle:   $cyc0\n";
   print FILE "\n";
   print FILE "\n";

   #-----------------------------------------------------------------
   #  Traverse the keys to lcf_hash and determine if the same key is 
   #  found in the plcf_hash.  If so write to warning file.
   #
   foreach my $key ( keys %lcf_hash ){
      if( exists $plcf_hash{ $key }){

         my @lcf_vals  = @{ $lcf_hash{ $key }};
         my @plcf_vals = @{ $plcf_hash{ $key }};

         print FILE " $key \n";
         print FILE "            $cyc1  count:  $plcf_vals[0]   bound: $lcf_vals[1]\n";
         print FILE "            $cyc0  count:  $lcf_vals[0]   \n\n";
#         print "https://www.emc.ncep.noaa.gov/gmb/gdas/es_conv/v16rt2/index_time.html\n";
      }
   }
   close( FILE );

exit 0 
