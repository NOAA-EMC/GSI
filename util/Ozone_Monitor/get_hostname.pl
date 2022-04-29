#! /usr/bin/perl

#-------------------------------------------------------------------
#  get_hostname.pl
#
#  This script determines the hostname of the current machine.  The
#  possiblities are wcoss_c, wcoss_d, hera, or "" if the host is not  
#  one of those three.
#-------------------------------------------------------------------


   my $machine = "";
  
   #
   # hera login nodes are hfe1-hfeN, and hostname command only returns the node name,
   # while wcoss_c and wcoss_d return [hostname].ncep.noaa.gov.  Keep only the
   # actual hostname and see if it matches the node names for hera, wcoss_d, or wcoss_c.
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
   elsif( $host =~ /clogin0/ || $host =~ /dlogin0/ ){
      $machine = "wcoss2"
   }
   elsif( $host =~ /login/ ) {
      $machine = "wcoss_c";
   }
   elsif( $host =~ /v/ || $host =~ /m/ ){	# wcoss_d nodes are vXXaY and mXXaY
      $machine = "wcoss_d";
   }

   print "$machine";

   exit 0;

