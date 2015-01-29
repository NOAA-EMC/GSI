#! /usr/bin/perl

#-----------------------------------------------------------------------
#  find_cycle.pl
#
#    Given a directory containing conventional monitor data files
#      determine the first or last cycle for which data files exist.
#
#    Note that the normal directory path is ${TANKDIR}/${SUFFIX} which 
#      will most often work out to ~/nbns/stats/convweb/${SUFFIX}.  This
#      script will then probe the time_vert directory and use the 
#      ges_uv_stas.* files to determine the last/first cycle processed.
#
#    Return that first/last cycle as a text string in YYYYMMDDHH format,
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

   if ($#ARGV != 1 ) {
	print "usage: find_cycle.pl  0/1 /path_to_directory/containing/conv/data/files/ending.YYYYMMDDHH\n";
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

   my @tvdirs = grep { /time_vert/ } @alldirs;

   
   #  If there are no radmon.* subdirectories, then exit without 
   #    returning any date string.
   #
   if( $#tvdirs < 0 ) {
      print "exiting with 0 tvdirs\n";
      exit;
   }
  
   
   #  Sort the tvdirs array and loop through it from end to beginning
   #

   my @sortrad = sort( @tvdirs );
   my $ctr = $#sortrad + 1;
      
   my $found_cycle = 0;

   do {
      
      $ctr--;

      #  In each subdirectory build a list of ges_uv_stas* files
      #  and parse out all unique date values.  The oldest is the answer
      #  we're looking for. 
      #
      #  If there are no ges_uv_stas* files, step to the next iteration.
      #
      my $newdir = "${dirpath}/${sortrad[$ctr]}";
      opendir DIR, $newdir or die "Cannot open the current directory: $!";

      my @timefiles = grep { /ges_uv_stas/ } readdir DIR;

      if( $#timefiles >= 0 ) {
         my @sorttime = sort( @timefiles );
         my @times;
   
         for ( my $ii=$#sorttime; $ii >= 0; $ii-- ) {
            my $teststr = $sorttime[$ii];

            my @values = split( '\.', $sorttime[$ii] );
            push( @times, $values[1] );

         }

         if ( $#times >= 0 ) {
            $found_cycle = 1;
            my @utimes = sort( uniq( @times ) );

            if( $target == 1 ){			# return latest date/time
               print "$utimes[$#utimes]";
            } else {				# return earliest date/time
               print "$utimes[0]";
            }     
         }
      }

   } while $found_cycle == 0 && $ctr > 0;


   exit;
