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
   #  Determine if installation is on CCS, WCOSS, or Zeus.
   #  If the arch is aix, CCS is assumed.
   #
   if( $arch ne "linux" && $arch ne "aix" ) {
      die( "only linux and aix are supported, $arch is not\n" );
   }
#   print "\n";
#   print "arch = $arch\n";

   my $machine = "";
  
   if( $arch eq "aix" ) {
      $machine = "ccs";
   }  
   elsif( $arch eq "linux" ) {

      #
      # zeus login nodes are fe1-fe8, and hostname command only returns the node name,
      # while ccs and (perhaps) wcoss return [hostname].ncep.noaa.gov.  Keep only the
      # actual hostname and see if it matches the node names for zeus, tide, or gyre.
      #
      my $host_zeus  = 0;
      my $host = "";
      $host = ` hostname `;
      chomp( $host );

      if( $host =~ /\./ ) {
         my @hostnames = split( '\.', $host );     
         $host = $hostnames[0];
      }

      if( $host =~ /fe/ ) { 
         $machine = "zeus";
      } 
      elsif( $host =~ /t/ || $host =~ /g/ ){	# wcoss nodes are tXXaY and gXXaY
         $machine = "wcoss";
      }
   } 

   print "$machine";

   exit 0;

