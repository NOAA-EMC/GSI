#! /usr/bin/perl

#-----------------------------------------------------------------------
#  find_cycle.pl
#
#    Given a directory containing radmon.YYYYMMDDHH subdirectories,
#      determine the first or last cycle for which ieee_d data files 
#      exist. 
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

   if ($#ARGV != 1 ) {
	print "usage: find_cycle.pl  0/1 /path_to_directory/containing/radmon.YYYYMMDDHH subdirectories. \n";
        print "                           0 = first, 1 = last \n";
	exit;
   }
   my $target = $ARGV[0];
   my $dirpath = $ARGV[1];
   my @alldirs;

   
   #  Get list of radmon.* sub-directories 
   #
   opendir(DIR, $dirpath) or die "Cannot open directory $!";
   while (my $file = readdir(DIR)) {
        next unless (-d "$dirpath/$file");
        push( @alldirs, $file );
   }
   closedir DIR;

   my @raddirs = grep { /radmon/ } @alldirs;

   
   #  If there are no radmon.* subdirectories, then exit without 
   #    returning any date string.
   #
   if( $#raddirs < 0 ) {
      print "exiting with 0 raddirs\n";
      exit;
   }
  
   
   #  Sort the raddirs array and loop through it from end to beginning
   #
   if( $target == 1 ){			# search is for latest date/time

      my @sortrad = sort( @raddirs );
      my $ctr = $#sortrad + 1;

      my $found_cycle = 0;

      do {
      
         $ctr--;

      
         #  In each subdirectory build a list of time.*ieee_d* files
         #  and parse out all unique date values.  The oldest is the answer
         #  we're looking for. 
         #
         #  If there are no time.*ieee_d* files, step to the next iteration.
         #
         my $newdir = "${dirpath}/${sortrad[$ctr]}";
         opendir DIR, $newdir or die "Cannot open the current directory: $!";

         my @tfiles = grep { /time/ } readdir DIR;
         my @timefiles = grep { /ieee_d/ } @tfiles;

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

      my @sortrad = sort( @raddirs );
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
         my $newdir = "${dirpath}/${sortrad[$ctr]}";
         opendir DIR, $newdir or die "Cannot open the current directory: $!";

         my @tfiles = grep { /time/ } readdir DIR;
         my @timefiles = grep { /ieee_d/ } @tfiles;

         if( $#timefiles >= 0 ) {
            my @sorttime = sort( @timefiles );
            my @times;
   
            for ( my $ii=0; $ii <= $#sorttime; $ii++ ) {
               my $teststr = $sorttime[$ii];

               my @values = split( '\.', $sorttime[$ii] );
               push( @times, $values[2] );
            }

            if ( $#times >= 0 ) {
               $found_cycle = 1;
               my @utimes = sort( uniq( @times ) );
              print "$utimes[0]";
            }
         }

      } while $found_cycle == 0 && $ctr < $#sortrad ;
   }

   exit;
