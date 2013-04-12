#! /usr/bin/perl

#-----------------------------------------------------------------------
#  find_last_cycle.pl
#
#    Given a directory containing radmon.YYYYMMDDHH subdirectories,
#      determine the latest cycle for which data exists. 
#    Return that last cycle as a text string in YYYYMMDDHH format,
#      or return nothing if none of the expected data files are found.
#
#-----------------------------------------------------------------------

    use strict;
    use warnings;

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

   if ($#ARGV != 0 ) {
	print "usage: find_last_cycle.pl  /path_to_directory/containing/radmon.YYYYMMDDHH subdirectories. \n";
	exit;
   }
   my $dirpath = $ARGV[0];
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

         for ( my $ii=$#sorttime; $ii >= 0; $ii-- ) {
            my $teststr = $sorttime[$ii];

            my @values = split( '\.', $sorttime[$ii] );
            push( @times, $values[2] );
         }

         if ( $#times >= 0 ) {
           $found_cycle = 1;
           my @utimes = sort( uniq( @times ) );
           print "$utimes[$#utimes]";
         }
      }

   } while $found_cycle == 0 && $ctr > 0;

   exit;
