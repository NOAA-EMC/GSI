#! /usr/bin/perl

#-------------------------------------------------------------------
#  get_hostname.pl
#
#  This script determines the hostname of the current machine.  The
#  possiblities are wcoss, wcoss_d, cray, hera or ""  if the host is 
#  determined to not be one of those four.
#-------------------------------------------------------------------


   my $machine = "";
  
   #
   # Hera login nodes are he1-he8, and hostname command only returns the node name,
   # while ccs and (perhaps) wcoss return [hostname].ncep.noaa.gov.  Keep only the
   # actual hostname and see if it matches the node names for zeus, tide, or gyre.
   #
   my $host = "";
   $host = ` hostname `;
   chomp( $host );

   if( $host =~ /\./ ) {
      my @hostnames = split( '\.', $host );     
      $host = $hostnames[0];
   }

   if( $host =~ /hfe/ ) { 
      $machine = "hera";
   } 
   elsif( $host =~ /login/ ) {
      $machine = "cray";
   }
   elsif( $host =~ /t/ || $host =~ /g/ ){	# wcoss nodes are tXXaY and gXXaY
      $machine = "wcoss";
   }
   elsif( $host =~ /m/ || $host =~ /v/ ){	# wcoss_d nodes are mXXaY and vXXaY
      $machine = "wcoss_d";
   }

   print "$machine";

   exit 0;

