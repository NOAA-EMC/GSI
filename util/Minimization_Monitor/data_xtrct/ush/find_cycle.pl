#! /usr/bin/perl

#-----------------------------------------------------------------------
#  find_cycle.pl
#
#    Given a directory containing ${SUFFIX}_minmon.YYYYMMDDHH 
#    subdirectories, determine the first or last cycle for which ieee_d 
#    data files  exist. 
#
#    Return that first/last cycle as a text string in YYYYMMDDHH format,
#      or return nothing if none of the expected data files are found.
#-----------------------------------------------------------------------

    use strict;
    use warnings;

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

   if ($#ARGV != 2 ) {
	print "usage: find_cycle.pl  suffix 0/1 /path_to_directory/containing/minmon.YYYYMMDDHH subdirectories. \n";
        print "                           0 = first, 1 = last \n";
	exit;
   }
   my $suffix = $ARGV[0];
   my $target = $ARGV[1];
   my $dirpath = $ARGV[2];
   my @alldirs;

   
   #  Get list of minmon.* sub-directories 
   #
   opendir(DIR, $dirpath) or die "Cannot open directory $!";
   while (my $file = readdir(DIR)) {
        next unless (-d "$dirpath/$file");
        push( @alldirs, $file );
   }
   closedir DIR;
   my @mmdirs = grep { /minmon/ } @alldirs;

   
   #  If there are no minmon* subdirectories, then exit without 
   #    returning any date string.
   #
   if( $#mmdirs < 0 ) {
      print "exiting with 0 mmdirs\n";
      exit;
   } 
   
   #  Sort the mmdirs array and loop through it from end to beginning
   #
   if( $target == 1 ){			# search is for latest date/time

      my @sortmm = sort( @mmdirs );
      my $ctr = $#sortmm + 1;

      my $found_cycle = 0;

      #  Start with the latest directory and attempt to locate the 
      #  gnorm_data.txt file.  The last line will contain the
      #  latest cycle processed. 
      do {
      
         $ctr--;

      
         #  In each subdirectory attempt to locate the last
         #  and parse out all unique date values.  The oldest is the answer
         #  we're looking for. 
         #
         #  If there are no time.*ieee_d* files, step to the next iteration.
         #
         my $newdir = "${dirpath}/${sortmm[$ctr]}";
         opendir DIR, $newdir or die "Cannot open the current directory: $!";

#         my @tfiles = grep { /time/ } readdir DIR;
         my @timefiles = grep { /costs.txt/ } readdir DIR;
#         my @timefiles = grep { /ieee_d/ } @tfiles;

         if( $#timefiles >= 0 ) {
            my @sorttime = sort( @timefiles );
            my @times;
            my $idx = 0;

            #  Find the first string of 10 digits; that's the date.  Use that $idx
            #  number to process all files.
            #
            my @vals = split( '\.', $timefiles[0] ); 
            for ( my $ii=$#vals; $ii >= 0; $ii-- ) {
               if( looks_like_number( $vals[$ii] )  && length($vals[$ii] ) == 10 ){
                     $idx = $ii;
               }
            }
 
            for ( my $ii=$#sorttime; $ii >= 0; $ii-- ) {
               my $teststr = $sorttime[$ii];

               my @values = split( '\.', $teststr );
               push( @times, $values[$idx] );

            }

            if ( $#times >= 0 ) {
               $found_cycle = 1;
               my @utimes = sort( uniq( @times ) );
              print "$utimes[$#utimes]";
            }
         }

      } while $found_cycle == 0 && $ctr > 0;
   }
   else {				# search is for earliest date/time

      my @sortmm = sort( @mmdirs );
      my $ctr = -1;

      my $found_cycle = 0;
      do {
      
         $ctr++;

      
         #  In each subdirectory build a list of time.*ieee_d* files
         #  and parse out all unique date values.  The oldest is the answer
         #  we're looking for. 
         #
         #  If there are no time.*ieee_d* files, step to the next iteration.
         #
         my $newdir = "${dirpath}/${sortmm[$ctr]}";
         opendir DIR, $newdir or die "Cannot open the current directory: $!";

         my @timefiles = grep { /costs.txt/ } readdir DIR;
#         my @tfiles = grep { /time/ } readdir DIR;
#         my @timefiles = grep { /ieee_d/ } @tfiles;

         if( $#timefiles >= 0 ) {
            my @sorttime = sort( @timefiles );
            my @times;
   
            for ( my $ii=0; $ii <= $#sorttime; $ii++ ) {
               my $teststr = $sorttime[$ii];

               #  Find the first string of 10 digits; that's the date.  
               #
               my @vals = split( '\.', $sorttime[$ii] );
#               my @vals = split( '\.', $timefiles[0] ); 
               for ( my $ii=$#vals; $ii >= 0; $ii-- ) {
                  if( looks_like_number( $vals[$ii] )  && length($vals[$ii] ) == 10 ){
                     push( @times, $vals[$ii] );
                  }
               }
            }

            if ( $#times >= 0 ) {
               $found_cycle = 1;
               my @utimes = sort( uniq( @times ) );
               print "$utimes[0]";
            }
         }

      } while $found_cycle == 0 && $ctr < $#sortmm ;
   }

   
