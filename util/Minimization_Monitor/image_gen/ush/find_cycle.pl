#! /usr/bin/perl

#-----------------------------------------------------------------------
#  find_cycle.pl
#
#    Arguments: 
#       --dir     : Required string value containing  $TANKdir/$SUFFIX.
#       --cyc     : Optional integer value:
#       		1 = last cycle  (default)
#       		0 = first cycle
#       --run     : Run name, generally 'gdas' or 'gfs'.  
#		    If not specified 'gdas' will be used.
#
#    Return that first/last cycle as a text string in YYYYMMDDHH format,
#      or return nothing if none of the expected data files are found.
#
#    Note that this is designed to be used by a shell script which will
#    pick up the returned cycle string.  If debug statements are left 
#    in this perl script then the calling shell script will have 
#    problems.
#-----------------------------------------------------------------------

   use strict;
   use warnings;
   use Getopt::Long;
   use Scalar::Util qw(looks_like_number);


   #-------------------------------------------------------------------
   #
   #  Subroutine uniq
   # 
   #    Given an input array, return all unique values in an array.
   # 
   #-------------------------------------------------------------------
   sub uniq {
      my %seen;
      return grep { !$seen{$_}++ } @_;
   }


   ##------------------------------------------------------------------
   ##------------------------------------------------------------------
   ##
   ##  begin main 
   ##
   ##------------------------------------------------------------------
   ##------------------------------------------------------------------

   my $run  = 'gdas';
   my $dir  = '';
   my $lcm  = 'minmon';
   my $cyc  = '1';

   GetOptions( 'cyc:i' => \$cyc,
               'run:s' => \$run,
               'dir=s' => \$dir,
               'lcm:s' => \$lcm );  
  

   my @alldirs;
   my $dirpath = $dir;

   #-------------------------------------------------------------------- 
   #  Get list of $run.* directories which contain minmon subdirectories
   #
   opendir(DIR, $dirpath) or die "Cannot open directory $!";
   while (my $file = readdir(DIR)) {
        next unless (-d "$dirpath/$file");
        push( @alldirs, $file );
   }
   closedir DIR;

   my $search_string;

   if( length($run) == 0 ){
      $search_string = $lcm;
   } else {
      $search_string = $run;
   }

   my @mmdirs = grep { /$search_string/ } @alldirs;

   #-----------------------------------------------------------------------   
   #  If there are no $run.yyyymmdd subdirectories, then exit without 
   #    returning any date string.
   #
   if( $#mmdirs < 0 ) {
      print "exiting with 0 mmdirs\n";
      exit;
   } 


   #-----------------------------------------------------------------------
   #  Sort the mmdirs array and loop through it from end to beginning
   #

   my @sortmm = sort( @mmdirs );

   my $ctr;
   my $incr;
   my $end_ctr;
   my @hrs;

   #-----------------------------------------------------------------------
   #  Arrange the logic here for accessing either the first or last
   #  cycle.  If we're after the first cycle the directories will be 
   #  processed from 0 to max.  Note below the cycle hours are processed
   #  from max to 0, so the cycle order is reversed (18..00) when looking
   #  for the first cycle.
   # 
   if( $cyc == 0 ){  
      $ctr = -1;
      $incr = 1;
      $end_ctr = $#sortmm;
      @hrs = qw( 18 12 06 00 );
   } else { 
      $ctr = $#sortmm + 1;
      $incr = -1;
      @hrs = qw( 00 06 12 18 );
      $end_ctr = 0;
   }


   my $found_cycle = 0;

   #  Start with the latest directory and attempt to locate monitor 
   #  subdirectories.
   #

   my $exit_flag = 0;

   do {
      $ctr = $ctr + $incr;
 
      #  In each subdirectory attempt to locate all *ieee_d files
      #  and parse out all unique date values.  The latest is the answer
      #  we're looking for. 
      #
      #  If there are no time.*ieee_d* files, step to the next iteration.
      #

      my $newdir; 
      my $hr_ctr = $#hrs + 1;

      do {
 
         $hr_ctr = $hr_ctr - 1;
         
         $newdir = "${dirpath}/${sortmm[$ctr]}/${hrs[$hr_ctr]}/${lcm}";
#         print " newdir = $newdir \n";


         if( -d $newdir ) {
            opendir DIR, $newdir or die "Cannot open the current directory: $!";

            my @timefiles = grep { /ieee_d/ } readdir DIR;

            if( $#timefiles >= 0 ) {
               my @sorttime = sort( @timefiles );
               my @times;
               my $idx = 0;

               #  Find the first string of 10 digits; that's the date.  Use that 
               #  $idx number to process all files.
               #
               my @vals = split( '\.', $timefiles[0] ); 
               for ( my $ii=$#vals; $ii >= 0; $ii-- ) {
                  if( looks_like_number( $vals[$ii] ) && length($vals[$ii] ) == 10 ){
                     $idx = $ii;
                  }
               }

               for ( my $ii=$#sorttime; $ii >= 0; $ii-- ) {
                  my $teststr = $sorttime[$ii];

                  my @values = split( '\.', $teststr );
                  push( @times, $values[$idx] );

               }
               if ( $#times >= 0 ) {
                  my @utimes = sort( uniq( @times ) );
                  if( $cyc == 1 ) {
                     print "$utimes[$#utimes]";
                     $found_cycle = 1;
                  } elsif( $cyc == 2 && $#utimes >= 1 ) {
                     print "$utimes[$#utimes-1]";
                     $found_cycle = 1;
                  } else {
                     print "$utimes[0]";
                     $found_cycle = 1;
                  }
               }
            }

         } 
          
      } while $hr_ctr > 0 && $found_cycle == 0;

#      print " found_cycle, ctr, end_ctr = $found_cycle, $ctr, $end_ctr \n";

      if( $cyc == 0 && $ctr >= $end_ctr ){  
#         print " exiting from if\n";
         $exit_flag = 1;
      } elsif( $cyc == 1 && $ctr <= $end_ctr ){
#         print " exiting from elsif\n";
         $exit_flag = 1;
      }
   

   } while $found_cycle == 0 && $exit_flag == 0;

