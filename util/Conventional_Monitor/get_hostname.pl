#! /usr/bin/perl

#-----------------------------------------------------------------------
#  get_hostname.pl
#
#  This script determines the hostname of the current machine.  The
#  possiblities are wcoss(ibm), (wcoss)cray, or theia.  A null 
#  string ("") if the host is not one of those three. 
#
#  NOTE:  shell scripts call this and then read the output string
#  	  which is in $machine.  So don't leave uncommented debug 
#	  statements in place here; that will confuse calling scripts.
#-----------------------------------------------------------------------

   my $machine = "";
  
   #----------------------------------------------------
   # use `hostname` command to determine the platform
   # we're on:
   #
   #   - theia login nodes are tfe1-fe8,
   #   - wcoss(ibm) login nodes are [t|s][14|10]a[1|2]
   #   - (wcoss)cray login nodes are [s|t]login[1|2]
   #
   my $host_zeus  = 0;
   my $host = "";
   $host = ` hostname `;
   chomp( $host );

   if( $host =~ /\./ ) {
      my @hostnames = split( '\.', $host );     
      $host = $hostnames[0];
   }

   if( $host =~ /tfe/ ) {
      $machine = "theia";
   } 
   elsif( $host =~ /llogin/ || $host =~/slogin/ ){  
      $machine = "cray";			
   }
   elsif( $host =~ /t/ || $host =~ /g/ ){
      $machine = "wcoss";
   }

   print "$machine";

   exit 0;
