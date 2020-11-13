#! /usr/bin/perl

#-------------------------------------------------------------------
#  get_hostname.pl
#
#  This script determines the hostname of the current machine.  The
#  possiblities are ccs, zeus, wcoss or "" if the host is not  
#  one of those three.
#-------------------------------------------------------------------

#   use IO::File;
#   use File::Copy qw(move);

   my $arch;
   $arch = ` uname -s | tr '[:upper:]' '[:lower:]' `;
   $arch =~ s/^\s+|\s+$//g;
   my $my_os = "export MY_OS=$arch";

   #
   #  Determine if installation is on cray, wcoss_d, or hera
   #
   if( $arch ne "linux" && $arch ne "aix" ) {
      die( "only linux and aix are supported, $arch is not\n" );
   }


   my $machine = "";
  
   #
   # zeus login nodes are fe1-fe8, and hostname command only returns the node name,
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
   elsif( $host =~ /m/ || $host =~ /v/ ){
      $machine = "wcoss_d";			# dell machines are mXXaY/vXXaY
   }
   elsif( $host =~ /t/ || $host =~ /g/ ){	# wcoss nodes are tXXaY and gXXaY
      $machine = "wcoss";
   }

   print "$machine";

   exit 0;

