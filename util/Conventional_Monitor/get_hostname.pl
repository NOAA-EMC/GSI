#! /usr/bin/perl

#-----------------------------------------------------------------------
#  get_hostname.pl
#
#  This script determines the hostname of the current machine.  The
#  possiblities are wcoss, wcoss_c, wcoss_d, or hera.  A null 
#  string ("") if the machine is not recognized. 
#
#  NOTE:  shell scripts call this and then read the output string
#  	  which is in $machine.  So don't leave uncommented debug 
#	  statements in place here; that will confuse calling scripts.
#-----------------------------------------------------------------------

   my $machine = "";

   if (-d "/dcom" and -d "/hwrf") {
      $machine = "wcoss";
   } 
   elsif( -d "/cm" ) {
      $machine = "wcoss_c";
   }
   elsif( -d "/ioddev_dell" ) {
      $machine = "wcoss_d";
   }
   elsif( -d "/scratch1" ) {
      $machine = "hera";
   }
   elsif( -d "/lfs/h2" ) {
      $machine = "wcoss2";
   }
 
   print "$machine";

   exit 0;
