#!/usr/bin/perl
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

 
#--------------------
#  Main begins here 
#--------------------

print "--> rm_img_files.pl\n";

#--------------------------------
#  load command line argument(s)
#--------------------------------
my $dir  = './';  # directory to be cleaned up (~/nbns/imgn/NET/RUN/monitor/pngs)
my $nfl  = 20;    # number of files to keep

GetOptions( 'dir=s' => \$dir,
            'nfl=i' => \$nfl );

#-----------------------------------
# get directory $dir/hist contents
#-----------------------------------
opendir my $target_dir, "$dir/summary" or die "Cannot open directory: $!";
my @files = readdir $target_dir;
closedir $target_dir;

#-----------------------------------------------------
# Break up file names and get the unique cycle times 
# 
# Sort the unique list in reverse order so the latest
# cycle time is first.   
#-----------------------------------------------------
my @times = ();
foreach my $file ( @files ) {
   my @spl = split( '\.', $file ); 
   if( looks_like_number( $spl[1] ) && length($spl[1] ) == 10 ) {
      push( @times, $spl[1] );
   }
}

my @unique = ();
if ( $#times >= 0 ) {
   @unique = sort{ $b <=> $a }( uniq( @times ));
}


#-------------------------------------
# Identify cycle times to be removed
#-------------------------------------
my @del_list = ();
my @sdir_list = qw( summary );

if( $#unique >= $nfl ) {
   my $ii = $nfl;
   my $end = $#unique;

   foreach my $time ( @unique ) {
      print "$time\n";
   }
   print " ii, end = $ii, $end\n";

   do {
      push( @del_list, $unique[$ii] );
      $ii++;
   } while $ii <= $end; 


   #---------------------------------------------
   #  step through the del_list and dir_list to
   #  remove old image files
   #---------------------------------------------
   print "del_list = @del_list\n";

   foreach my $sdir ( @sdir_list ) {

      foreach my $del ( @del_list ) {
         my $rm_cmd = "rm -f $dir/$sdir/*$del*";
         print "RM:  $rm_cmd\n";
         system( $rm_cmd ) == 0
            or die "system $rm_cmd failed: $?";
      }
   }
}

print "<-- rm_img_files.pl\n";
