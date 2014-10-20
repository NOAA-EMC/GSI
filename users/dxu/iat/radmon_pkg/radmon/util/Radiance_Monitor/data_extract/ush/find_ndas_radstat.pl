#! /usr/bin/perl

#-----------------------------------------------------------------------
#  find_ndas_radstat.pl
#
#    Given a regional data source directory containing ndas.YYYYMMDDHH 
#      subdirectories, determine the first or last cycle for which 
#      radstat files exist.
#
#    Return that first/last cycle as a text string in YYYYMMDDHH format,
#      or return nothing if no radstat files are found.
#
#-----------------------------------------------------------------------

    use strict;
    use warnings;
    use Date::Calc qw(Add_Delta_Days);

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
	print "usage: find_radstat.pl  first_last /path_to_directory/containing/[gdas/ndas].YYYYMMDDHH subdirectories. \n";
        print "                           first_last:          0 = first, 1 = last \n";
	exit;
   }
   my $target = $ARGV[0];
   my $dirpath = $ARGV[1];
   my @alldirs;
   my @raddirs;
   
   my $rad_area = "rgn";

   #  Get list of gdas/ndas.* sub-directories 
   #
   opendir(DIR, $dirpath) or die "Cannot open directory $!";
   while (my $file = readdir(DIR)) {
        next unless (-d "$dirpath/$file");
        push( @alldirs, $file );
   }

   my $filter = "ndas";
   if ( $rad_area eq "glb" ) {
      $filter = "gdas";
   }

   @raddirs = grep { /$filter/ } @alldirs;
   
   #  If there are no "$filter.* subdirectories, then exit without 
   #    returning any date string.
   #
   if( $#raddirs < 0 ) {
      print "exiting with 0 raddirs\n";
      exit;
   }


   #  Sort the raddirs array and loop through it from end to beginning
   #
   my $target_cycle ="";

   if( $target == 1 ){			# search is for latest date/time

      my @sortrad = sort( @raddirs );
      my $ctr = $#sortrad + 1;

      my $found_cycle = 0;

      do {
      
         $ctr--;


 
         #  In each subdirectory build a list of radstat files
         #  and parse out all unique date values.  The latest date is the answer
         #  we're looking for. 
         #
         #  If there are no radstat files, step to the next iteration.
         #
         my $newdir = "${dirpath}/${sortrad[$ctr]}";
         my @datestr = split( '\.', $newdir );
         
         my $year  = substr ${datestr[1]}, 0, 4;
         my $month = substr ${datestr[1]}, 4, 2;
         my $day   = substr ${datestr[1]}, 6, 2;

         my $tday = sprintf( "%4d%02d%02d", $year, $month, $day  );

         ($year, $month, $day) = Add_Delta_Days($year,$month,$day,-1);
         my $pday = sprintf( "%4d%02d%02d", $year, $month, $day  );

         my $hh     = "";
         opendir DIR, $newdir or die "Cannot open the current directory: $!";
         my @rfiles = grep { /radstat.tm12/ } readdir DIR;

         if( $#rfiles >= 0 ) {

            my @sfiles = sort( @rfiles );
            my @split_sfile = split( '\.', ${sfiles[$#sfiles]} );
            my $hr = substr( ${split_sfile[1]}, 1, 2 );

            if( $hr eq "18" ){
               $hh = "06";
               $target_cycle = sprintf( "%s%s", $tday, $hh );
               $found_cycle = 1;
            }
            elsif( $hr eq "12" ){
               $hh = "00";
               $target_cycle = sprintf( "%s%s", $tday, $hh );
               $found_cycle = 1;
            }
            elsif( $hr eq "06" ){
               $hh = "18";
               $target_cycle = sprintf( "%s%s", $pday, $hh );
               $found_cycle = 1;
            }
            elsif( $hr eq "00" ){
               $hh = "12";
               $target_cycle = sprintf( "%s%s", $pday, $hh );
               $found_cycle = 1;
            }
         }
         closedir DIR, $newdir or die "Cannot close the current directory: $!";

      } while $found_cycle == 0 && $ctr > 0;
   }
   else {				# search is for earliest date/time
      my @sortrad = sort( @raddirs );
      my $ctr = -1;

      my $found_cycle = 0;

      do {
      
         $ctr++;

         #  In each subdirectory build a list of radstat files
         #  and parse out all unique date values.  The earliest date is the answer
         #  we're looking for. 
         #
         #  If there are no radstat files, step to the next iteration.
         #
         my $newdir = "${dirpath}/${sortrad[$ctr]}";
         my @datestr = split( '\.', $newdir );
         
         my $year  = substr ${datestr[1]}, 0, 4;
         my $month = substr ${datestr[1]}, 4, 2;
         my $day   = substr ${datestr[1]}, 6, 2;

         my $tday = sprintf( "%4d%02d%02d", $year, $month, $day  );

         ($year, $month, $day) = Add_Delta_Days($year,$month,$day,-1);
         my $pday = sprintf( "%4d%02d%02d", $year, $month, $day  );

         my $hh     = "";
         opendir DIR, $newdir or die "Cannot open the current directory: $!";
         my @rfiles = grep { /radstat.tm12/ } readdir DIR;

         if( $#rfiles >= 0 ) {
            my @sfiles = sort( @rfiles );
            my @split_sfile = split( '\.', ${sfiles[0]} );
            my $hr = substr( ${split_sfile[1]}, 1, 2 );

            if( $hr eq "00" ){
               $hh = "12";
               $target_cycle = sprintf( "%s%s", $pday, $hh );
               $found_cycle = 1;
            }
            elsif( $hr eq "06" ){
               $hh = "18";
               $target_cycle = sprintf( "%s%s", $pday, $hh );
               $found_cycle = 1;
            }
            elsif( $hr eq "12" ){
               $hh = "00";
               $target_cycle = sprintf( "%s%s", $tday, $hh );
               $found_cycle = 1;
            }
            elsif( $hr eq "18" ){
               $hh = "06";
               $target_cycle = sprintf( "%s%s", $tday, $hh );
               $found_cycle = 1;
            }
         }
         closedir DIR, $newdir or die "Cannot close the current directory: $!";

      } while $found_cycle == 0 && $ctr <= $#raddirs ;


   }


   print( $target_cycle );
   exit;
