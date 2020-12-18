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
   my $ctr = 0;

   foreach my $key ( keys %lcf_hash ){
      if( exists $plcf_hash{ $key }){
          
         $ctr++;
         my @lcf_vals  = @{ $lcf_hash{ $key }};
         my @plcf_vals = @{ $plcf_hash{ $key }};

         #------------------------------------------------------------------
         #  Note there's a mismatch between how the type and subtype values
         #  are used in nbns and how they are used by GrADS to produce the
         #  resulting image file names.  The net result is that the key is 
         #  in formate $type_$subtype and we need to pass a value that 
         #  matches the image file name, so it's $type-$subtype (note hyphen
         #  not underscore) and subtype 00 needs to become 0.  This value is
         #  then used in the hyperlink as the value for 'src'. 
         #
         my $type_str = $key;
         $type_str =~ s/_00/-0/;
         $type_str =~ s/_03/-3/;
         $type_str =~ s/_04/-4/;
         $type_str =~ s/_05/-5/;
         $type_str =~ s/_/-/;

         #-----------------------------------------------------
         # All ps plots, and some of the q, t and uv types use
         # surface time series plots instead of time series.
         #
         my $ptype = "time";
         if( $type_str =~ m/ps/   || 
             $type_str =~ m/q18/  || $type_str =~ m/t18/ ||
             $type_str =~ m/uv28/ || $type_str =~ m/uv29/ ) {
            $ptype = "surftime"; 
         }

         print FILE " $key \n";
         printf FILE "   cycle: % d", $cyc1;  
         printf FILE "   count: %6s", $plcf_vals[0];
         printf FILE "   bound: %10.1f", $plcf_vals[1];
         printf FILE "     avg: %10.1f \n", $plcf_vals[2];

         printf FILE "   cycle: % d", $cyc0;
         printf FILE "   count: %6s", $lcf_vals[0];
         printf FILE "   bound: %10.1f", $lcf_vals[1];
         printf FILE "     avg: %10.1f \n", $lcf_vals[2];
         print FILE  "      https://www.emc.ncep.noaa.gov/gmb/gdas/es_conv/index.html?net=$net&run=$run&src=$type_str&vtype=count&ptype=$ptype\n\n";
      }
   }

   #-------------------
   #  report footer
   #  
   print FILE  "\n\n\n"; 
   print FILE  "*********************** WARNING ***************************\n";
   print FILE  "This is an automated email.  Replies to sender will not be \n";
   print FILE  'received.  Please direct replies to edward.safford@noaa.gov';
   print FILE  " \n";
   print FILE  "*********************** WARNING ***************************\n";

   close( FILE );


   #--------------------------------------------
   # if no keys overlapped then delete the errf
   #
   if( $ctr <= 0 ){
     unlink $errf or warn "Could not unlink $errf: $!"; 
   }

exit 0 
