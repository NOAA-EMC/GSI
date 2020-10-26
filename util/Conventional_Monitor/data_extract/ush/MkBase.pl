#! /usr/bin/perl

#-----------------------------------------------------------------------
#  MkBase.pl
#
#    Arguments: 
#       --dir     : Required string value containing  $TANKdir/$NET.
#       --net     : Identifying name of data source (aka suffix)
#       --run     : Run name, generally 'gdas' or 'gfs'.  
#		    If not specified 'gdas' will be used.
#-----------------------------------------------------------------------

   use strict;
   use warnings;
   use Getopt::Long;
   use Scalar::Util qw(looks_like_number);

   my %sums;


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


   #---------------------------------------------------------
   #  subroutine process_nobs_file
   #
   #  Open the input file name and read the contents.
   #
   #  Using $type_$subtype as a key, push the nobs value
   #  into the %sums hash (a hash of arrays).
   #---------------------------------------------------------
   sub process_nobs_file {
      print "--> process_nobs \n";

      #-----------------------------------
      # check number of arguments passed.
      #  
      my $nargs = scalar(@_);

      if( $nargs <= 0 || $nargs > 1 ){
         print "error:  wrong number of args passed \n"; 
      }
      else {

         my $nobs_file = shift;

         print " nobs_file = $nobs_file \n";

         open(FH, '<', $nobs_file) or die $!;

         while(<FH>){
            my @line = split /,/, $_;

            my $type    = trim( $line[0] );
            my $subtype = trim( $line[1] );
            my $count   = trim( $line[2] );

            print "type    = $type \n";
            print "subtype = $subtype \n";
            print "count   = $count \n";

            #-------------------------------------------------------
            # insert into hash %sums using $type_$subtype as the key
            #
            my $key = $type . "_" . $subtype;

            if( exists( $sums{$key} )){
               push @{ $sums{$key} }, $count;
            } 
            else {
               my @counts = [ $count ];
               $sums{$key} = \@counts;
            }
         }
         close(FH);
      }

      print "<-- process_nobs \n";
   };

   #---------------------------------------------------------
   #  subroutine calc_avgs
   #
   #  Take each key in the %sums hash and determine the avg
   #  for the array of counts and dump to new base file.
   #
   #---------------------------------------------------------
   sub calc_avgs {

      print "--> calc_avgs \n";
      my $dir = shift;
      my $net = shift;
      my $run = shift;

      print "   dir = $dir\n";
      print "   net = $net\n";
      print "   run = $run\n";

      my $key;

      my $filename = "${dir}/${net}/info/${run}_conmon_base.txt";
      print "filename = $filename\n";
      open(FH, '>', $filename) or die $!;

      foreach $key (sort keys %sums) {
#         print " $key ==>\n";

         my $total = 0;
         my $nrecs = 0;

         for my $count (@{ $sums{ $key } }) {
            if( looks_like_number( $count )) {
               $nrecs = $nrecs + 1;
               $total = $total + $count;
            }
         }
        
         my $avg = $total/$nrecs;
         my $rounded = sprintf("%.2f", $avg);
         print FH "${key}, ${rounded}\n";
      }

      close( FH );
      print "<-- calc_avgs \n";
   }

   ###------------------------------------------------------------------
   ###------------------------------------------------------------------
   ###
   ###  begin main 
   ###
   ###------------------------------------------------------------------
   ###------------------------------------------------------------------


   my $run  = 'gdas';
   my $dir  = '';
   my $net  = '';
   my $err  = 0;

   GetOptions( 'net:s' => \$net,
               'run:s' => \$run,
               'dir=s' => \$dir );

   if( length($net) == 0 ){
      print "Error:  Missing net value \n";
      $err = 1;
      exit $err
   }

   if( length($dir) == 0 ){
      print "Error:  Missing dir value \n";
      $err = 2;
      exit $err
   }

   print " net   = $net\n";
   print " run   = $run\n";
   print " dir   = $dir\n";
    
   my @alldirs;
   my $dirpath = $dir . '/' . $net;
   print "dirpath = $dirpath\n";

   #-------------------------------------------------------------------- 
   #  Get list of $run.* directories which contain conmon subdirectories
   #
   opendir(DIR, $dirpath) or die "Cannot open directory $!";
   while (my $file = readdir(DIR)) {
        next unless (-d "$dirpath/$file");
        push( @alldirs, $file );
   }
   closedir DIR;

   my @rundirs = grep { /$run/ } @alldirs;

   #-----------------------------------------------------------------------   
   #  If there are no $run.yyyymmdd subdirectories, then exit without 
   #    returning any date string.
   #
   if( $#rundirs < 0 ) {
      print "exiting with 0 rundirs\n"; 
      $err = 3;
      exit $err;
   } 


   #-----------------------------------------------------------------------
   #  Sort the rundirs array and loop through it from end to beginning
   #
   my @sortrun = sort( @rundirs );


   #-----------------------------------------------------------------------
   #  Access the nobs files backwards starting from the latest cycle.
   #
   my $idx     = $#sortrun + 1;
   my $end_ctr = 0;
   my @hrs     = qw( 00 06 12 18 );


   #----------------------------------------------------------------
   #  Start with the latest directory and attempt to locate the 
   #  latest 120 (30 days worth of) monitor subdirectories
   #  containing an nobs.$run.$pdate file.
   #
   my $exit_flag   = 0;
   my $found_cycle = 0;
   my $cycle_ctr         = 0;

   do {
      $idx = $idx -1;
      my $hr_ctr = $#hrs + 1;

      my @spl = split /\./, ${sortrun[$idx]};
      my $date = ${spl[1]};


      #---------------------------------------------
      #  use this loop over the hrs array to build
      #  the $obs_file names
      #
      do {
 
         $hr_ctr = $hr_ctr - 1;

         my $cycle = $date . ${hrs[$hr_ctr]};
 
         my $obs_file = "${dirpath}/${sortrun[$idx]}/${hrs[$hr_ctr]}/conmon/horz_hist/anl/nobs.anl.$cycle";
         my $comp_obs_file = $obs_file . '.gz';

         if( -e $comp_obs_file ) {
           system( "gunzip", $comp_obs_file );
         }

         if( -e $obs_file ) {
            process_nobs_file( $obs_file );
            system( "gzip", $obs_file );
         }

      } while $hr_ctr > 0;

      $cycle_ctr = $cycle_ctr + 1;

   } while $idx > 0 && $cycle_ctr < 120;

   calc_avgs( $dir, $net, $run );

   exit 0
