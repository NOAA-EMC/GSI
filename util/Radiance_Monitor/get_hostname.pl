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
      # zeus login nodes are fe1-fe8
      #
      my $host_zeus  = 0;
      $host_zeus  = ` hostname | gawk '{split($0,a,"."); print a[1]}' | grep fe | wc -l`;   
      if( $host_zeus == 1 ) {
         $machine = "zeus";
      } 
      else {
         my $host_tide = 0;
         $host_tide = ` hostname | gawk '{split($0,a,"."); print a[1]}' | grep t | wc -l`;
         my $host_gyre = 0;
         $host_gyre = ` hostname | gawk '{split($0,a,"."); print a[1]}' | grep g | wc -l`;

         if( $host_tide == 1 || $host_gyre == 1 ) {
            $machine = "wcoss";
         }
      }
   } 

   print "$machine";

   exit 0;

