#! /usr/bin/perl

#-----------------------------------------------------------------------
#  find_cycle.pl
#
#    Arguments:
#       --dir     : Required string value containing  $TANKdir/$SUFFIX.
#       --cyc     : Optional integer value:
#                       1 = last cycle  (default)
#                       2 = 2nd to last cycle
#                       0 = first cycle
#       --run     : Optional run name, generally 'gdas' or 'gfs'.
#                   This should be used if $TANK_USE_RUN is set to 1.
#
#    Return the requested cycle time or nothing if none of the expected 
#    	    data files are found.
#
#    NOTE:  This version has been modified to add case 2 returning
#           the 2nd to latest cycle time.  This is to counter a timing 
#	    problem we've encountered on the crays.
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
   my $lcm  = 'radmon';
   my $cyc  = '1';

   GetOptions( 'cyc:i' => \$cyc,
               'run:s' => \$run,
               'dir=s' => \$dir,
               'lcm:s' => \$lcm );

   my $target  = $cyc;
   my $dirpath = $dir;
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
   if( $#raddirs < 0 ) {
      @raddirs = grep { /gdas/ } @alldirs;
   }
 
   #  If there are no radmon.* subdirectories, then exit without 
   #    returning any date string.
   #
   if( $#raddirs < 0 ) {
      print "exiting with 0 raddirs\n";
      exit;
   }
  
   
   #  Sort the raddirs array and loop through it from end to beginning
   #
   if( $target == 1 || $target == 2 ){		# search is for latest date/time

      my @sortrad = sort( @raddirs );
      my $ctr = $#sortrad + 1;

      my $found_cycle = 0;
      my @times;

      do {
      
         $ctr--;

      
         #  In each subdirectory build a list of time.*ieee_d* files
         #  and parse out all unique date values.  The oldest is the answer
         #  we're looking for. 
         #
         #  If there are no time.*ieee_d* files, step to the next iteration.
         #
         my $newdir;
         my @tfiles;
         my @timefiles;
         if( -d "${dirpath}/${sortrad[$ctr]}/radmon" ){
            $newdir = "${dirpath}/${sortrad[$ctr]}/radmon";
            opendir DIR, $newdir;

            @tfiles = grep { /time/ } readdir DIR;
            @timefiles = grep { /ieee_d/ } @tfiles;
         }
         elsif( -d "${dirpath}/${sortrad[$ctr]}" ){
            $newdir = "${dirpath}/${sortrad[$ctr]}";
            opendir DIR, $newdir;
            @tfiles = grep { /time/ } readdir DIR;
            @timefiles = grep { /ieee_d/ } @tfiles;
         }

         if( $#timefiles >= 0 ) {
            my @sorttime = sort( @timefiles );
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
               if( looks_like_number( $values[$idx] ) && length( $values[$idx] ) == 10 ) {
                  push( @times, $values[$idx] );
               }
            }

            #------------------------------------------------------------------
            #  Added a check on $ctr < $#sortrad to ensure we look
	    #  at least 2 directories.  In order to potentially rerturn the 2nd
            #  to the last time here on the crays.
            #------------------------------------------------------------------
            if ( $#times >= 0 && $ctr <= $#sortrad ) {
               $found_cycle = 1;
               my @utimes = sort( uniq( @times ) );
               if ( $target == 2 ) {				# 2nd to last time
                  print "$utimes[$#utimes -1]";
               } 
               else {
                  print "$utimes[$#utimes]";			# last time
               }
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
         my $newdir;
         my @tfiles;
         my @timefiles;

         if( -d "${dirpath}/${sortrad[$ctr]}" ){
            $newdir = "${dirpath}/${sortrad[$ctr]}";
            opendir DIR, $newdir or die "Cannot open the current directory: $!";
            @tfiles = grep { /time/ } readdir DIR;
            @timefiles = grep { /ieee_d/ } @tfiles;
         }
         elsif( -d "${dirpath}/${sortrad[$ctr]}/radmon" ){ 
            $newdir = "${dirpath}/${sortrad[$ctr]}/radmon";
            opendir DIR, $newdir or die "Cannot open the current directory: $!";
            @tfiles = grep { /time/ } readdir DIR;
            @timefiles = grep { /ieee_d/ } @tfiles;
         }

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
