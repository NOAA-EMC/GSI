#! /usr/bin/perl

#-----------------------------------------------------------------------
#  mk_data_rpt.pl
#
#    Arguments: 
#       --dir     : Required string value containing  $TANKdir/$SUFFIX.
#       --cyc     : Optional integer value:
#       		1 = last cycle  (default)
#       		0 = first cycle
#       --run     : Run name, generally 'gdas' or 'gfs'.  
#		    If not specified 'gdas' will be used.
#
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
              
  
   #  locate nobs.anl|ges.$pdate file
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
   #  read in nobs file, locate count total
   open( FILE, "${nobs_ges}" ) or die( "Unable to open ${nobs_ges}" );
   my @nobs_data = <FILE>;
   close( FILE );

   # This is a simple csv file, so break on the comma and 
   # push to %nobs_hash.
   my %nobs_hash;
   foreach ( @nobs_data ) {
      my @words = split /,/, $_;
      my $key = trim( $words[0] ) . "_" . trim( $words[1] );
      my $value = trim( $words[2] );
      $nobs_hash{ $key } = $value;
   }

   #  locate gdas_conmon_base.txt, load into hash   
   my $home = "HOME" . ${run} . "_conmon";
   my $base_file = $ENV{ "$home" };
   $base_file=${base_file} . "/fix/" . ${run} . "_conmon_base.txt";
   print "base_file = ${base_file} \n";

   #  read in gdas_conmon_base.txt, load into hash
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
     print "  $key , $value\n";
   } 


   #  Compare nobs count to base
   #  generate warning if count is low
   
#   my @alldirs;
#   my $dirpath = $dir;

   #-------------------------------------------------------------------- 
   #  Get list of $run.* directories which contain conmon subdirectories
   #
#   opendir(DIR, $dirpath) or die "Cannot open directory $!";
#   while (my $file = readdir(DIR)) {
#        next unless (-d "$dirpath/$file");
#        push( @alldirs, $file );
#   }
#   closedir DIR;

#   my $search_string;

#   if( length($run) == 0 ){
#      $search_string = $lcm;
#   } else {
#      $search_string = $run;
#   }
#
#   my @mmdirs = grep { /$search_string/ } @alldirs;
#   #-----------------------------------------------------------------------   
#   #  If there are no $run.yyyymmdd subdirectories, then exit without 
#   #    returning any date string.
#   #
#   if( $#mmdirs < 0 ) {
#      print "exiting with 0 mmdirs\n";
#      exit;
#   } 
#
#
#   #-----------------------------------------------------------------------
#   #  Sort the mmdirs array and loop through it from end to beginning
#   #
#
#   my @sortmm = sort( @mmdirs );
#
#   my $ctr;
#   my $incr;
#   my $end_ctr;
#   my @hrs;
#
#   #-----------------------------------------------------------------------
#   #  Arrange the logic here for accessing either the first or last
#   #  cycle.  If we're after the first cycle the directories will be 
#   #  processed from 0 to max.  Note below the cycle hours are processed
#   #  from max to 0, so the cycle order is reversed (18..00) when looking
#   #  for the first cycle.
#   # 
#   if( $cyc == 0 ){  
#      $ctr = -1;
#      $incr = 1;
#      $end_ctr = $#sortmm;
#      @hrs = qw( 18 12 06 00 );
#   } else { 
#      $ctr = $#sortmm + 1;
#      $incr = -1;
#      @hrs = qw( 00 06 12 18 );
#      $end_ctr = 0;
#   }
#
#
#   my $found_cycle = 0;
#
#   #  Start with the latest directory and attempt to locate monitor 
#   #  subdirectories.
#   #
#
#   my $exit_flag = 0;
#
#   do {
#      $ctr = $ctr + $incr;
# 
#      #  In each subdirectory attempt to locate all *stas* files
#      #  and parse out all unique date values.  The latest is the answer
#      #  we're looking for. 
#      #
#      #  If there are no *stas* files, step to the next iteration.
#      #
#
#      my $newdir; 
#      my $hr_ctr = $#hrs + 1;
#
#      do {
# 
#         $hr_ctr = $hr_ctr - 1;
#         
#         $newdir = "${dirpath}/${sortmm[$ctr]}/${hrs[$hr_ctr]}/${lcm}/time_vert";
#
#
#         if( -d $newdir ) {
#            opendir DIR, $newdir or die "Cannot open the current directory: $!";
#
#            my @timefiles = grep { /stas/ && !/ctl/ } readdir DIR;
#
#            if( $#timefiles >= 0 ) {
#               my @sorttime = sort( @timefiles );
#               my @times;
#               my $idx = 0;
#
#               #  Find the first string of 10 digits; that's the date.  Use that 
#               #  $idx number to process all files.
#               #
#               my @vals = split( '\.', $timefiles[0] ); 
#               for ( my $ii=$#vals; $ii >= 0; $ii-- ) {
#                  if( looks_like_number( $vals[$ii] ) && length($vals[$ii] ) == 10 ){
#                     $idx = $ii;
#                  }
#               }
#
#               for ( my $ii=$#sorttime; $ii >= 0; $ii-- ) {
#                  my $teststr = $sorttime[$ii];
#
#                  my @values = split( '\.', $teststr );
#                  if( length($values[$idx] ) == 10 ){   
#                     push( @times, $values[$idx] );
#                  }
#               }
#
#               if ( $#times >= 0 ) {
#                  my @utimes = sort( uniq( @times ) );
#                  if( $cyc == 1 ) {
#                     print "$utimes[$#utimes]";
#                     $found_cycle = 1;
#                  } elsif( $cyc == 2 && $#utimes >= 1 ) {
#                     print "$utimes[$#utimes-1]";
#                     $found_cycle = 1;
#                  } else {
#                     print "$utimes[0]";
#                     $found_cycle = 1;
#                  }
#               }
#            }
#
#         } 
#          
#      } while $hr_ctr > 0 && $found_cycle == 0;
#
#
#      if( $cyc == 0 && $ctr >= $end_ctr ){  
#         $exit_flag = 1;
#      } elsif( $cyc == 1 && $ctr <= $end_ctr ){
#         $exit_flag = 1;
#      }
#   
#
#   } while $found_cycle == 0 && $exit_flag == 0;

