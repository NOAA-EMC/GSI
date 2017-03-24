#! /usr/bin/perl

#-------------------------------------------------------------------
#  CMon_install.pl
#
#  This script makes sets all necessary configuration definitions
#  and calls the makeall.sh script to build all the necessary
#  executables.  This script works for wcoss, theia, and cray 
#  machines.
#-------------------------------------------------------------------

   use IO::File;
   use File::Copy qw(move);

   print "--> CMon_install.sh\n";
   my $machine = `/usr/bin/perl ./get_hostname.pl`;
   my $my_machine="export MY_MACHINE=$machine";

   if( $machine ne "theia" && $machine ne "wcoss" && $machine ne "cray" ) {
      die( "ERROR --- Unrecognized machine hostname, $machine.  Exiting now...\n" );
   }
   else {
      print "machine = $machine\n";
   }

   #---------------------------------------------------------------------------------
   #  All 3 currently supported platforms are little endian machines and linux OSes.
   #    I'm keeping these switches though because that will surely change at some
   #    point and I'll just have to re-introduce the same switches.
   #    
   my $little_endian = "export LITTLE_ENDIAN=1";

   my $os = "linux";
   my $my_os = "export MY_OS=$os";


   #
   #  Idenfity basedir location of package
   #
   print "\n";
   print "locating and saving CMon package location\n";
   my $cmondir;
   $cmondir = `dirname $0`;
   $cmondir =~ s/^\s+|\s+$//g;

   if( $cmondir eq "." ) {
      $cmondir = `pwd`;
      $cmondir =~ s/^\s+|\s+$//g;
   }
   my $my_cmon = "export MY_CMON=$cmondir";
   print "my_cmon = $my_cmon \n";
   print"\n\n";

   sleep( 1 );

   #
   #  TANKDIR location
   #
   my $user_name = $ENV{ 'USER' };
   if( $machine eq "theia" ) {
      $tankdir = "/scratch4/NCEPDEV/da/save/$user_name/nbns";
   }
   elsif( $machine eq "wcoss" ) {
      $tankdir = "/global/save/$user_name/nbns";
   }
   elsif( $machine eq "cray" ) {
      $tankdir = "/gpfs/hps/emc/da/noscrub/$user_name"
   }

   print "Please specify TANKDIR location for storage of data and image files.\n";
   print "  Return to accept default location or enter new location now.\n";
   print "\n";
   print "  Default TANKDIR:  $tankdir \n";
   print "     ?\n";
   my $new_tankdir = <>;
   $new_tankdir =~ s/^\s+|\s+$//g;

   if( length($new_tankdir ) > 0 ) {
      $tankdir = $new_tankdir;
   }
   my $my_tankdir="export CMON_TANKDIR=$tankdir";
   print "my_tankdir = $my_tankdir\n";
   print "\n\n";
   sleep( 1 );


   #
   #  Web sever name
   #
   my $server = "emcrzdm";
   print "Please specify web server name.\n";
   print "  Return to accept default server name or enter new server name.\n";
   print " \n";
   print "  Default web server:  $server\n";
   print "    ?\n";
   my $new_server =<>;
   $new_server =~ s/^\s+|\s+$//g;
   if( length($new_server ) > 0 ) {
      $server = $new_server;
   }
   my $my_server="export WEBSVR=$server";
   print "my_server = $my_server\n";
   print "\n\n";
   sleep( 1 );


   #
   #  Web server user name 
   #
   my $webuser = $ENV{ 'USER' };
   print "Please specify your user name on the $server server.\n";
   print "  Return to accept default user name or enter new user name.\n";
   print " \n";
   print "  Default user name on $server:  $webuser\n";
   print "    ?\n";
   my $new_webuser =<>;
   $new_webuser =~ s/^\s+|\s+$//g;
   if( length($new_webuser ) > 0 ) {
      $webuser = $new_webuser;
   }
   my $my_webuser="export WEBUSER=$webuser";
   print "my_webuser = $my_webuser\n";
   print "\n\n";
   sleep( 1 );


   #
   #  Web directory
   #
   my $webdir = "/home/people/emc/www/htdocs/gmb/gdas/radiance/${webuser}";
   my $webdir = "/home/people/emc/www/htdocs/gmb/gdas";
   print "Please specify the top level web site directory $server.\n";
   print "  Return to accept default directory location or enter new location.\n";
   print " \n";
   print "  Default directory on $server:  $webdir\n";
   print "    ?\n";
   my $new_webdir =<>;
   $new_webdir =~ s/^\s+|\s+$//g;
   if( length($new_webdir ) > 0 ) {
      $webdir = $new_webdir;
   }
   my $my_webdir="export WEBDIR=$webdir";
   print "my_webdir = $my_webdir\n";
   print "\n\n";
   sleep( 1 );


   #
   #  Set up ptmp and stmp locations according to $arch.
   #
   my $ptmp    = "/ptmpd1";
   my $stmp    = "/stmpd1";
   my $my_ptmp = "export C_PTMP=\${C_PTMP:-$ptmp}";
   my $my_stmp = "export C_STMP=\${C_STMP:-$stmp}";

   if( $machine eq "theia" ) {
      $my_ptmp="export C_PTMP=\${C_PTMP:-/scratch4/NCEPDEV/stmp4}";
      $my_stmp="export C_STMP=\${C_STMP:-/scratch4/NCEPDEV/stmp3}";
   }
   elsif( $machine eq "cray" ) {
      $my_ptmp="export C_PTMP=\${C_PTMP:-/gpfs/hps/ptmp/$user_name}";
      $my_stmp="export C_STMP=\${C_STMP:-/gpfs/hps/stmp/$user_name}";
   } 
   else {
      print "Please specify PTMP location.  This is used for temporary work space.\n";
      print "  Available options are: \n";
      print "      /ptmpd1  (default)\n";
      print "      /ptmpd2\n";
      print "      /ptmpd3\n";
      print "      /ptmpp1\n";
      print "      /ptmpp2\n";

      print "  Return to accept default location or enter new location now.\n";
      print "\n";
      print "  Default PTMP:  $ptmp \n";
      print "     ?\n";
      my $new_ptmp = <>;
      $new_ptmp =~ s/^\s+|\s+$//g;

      if( length($new_ptmp ) > 0 ) {
         $ptmp = $new_ptmp;
      }
      my $my_ptmp="export C_PTMP=\${C_PTMP:-$ptmp}";
      print "\n\n";
      sleep( 1 );

      print "Please specify STMP location.  This is used for temporary work space.\n";
      print "  Available options are: \n";
      print "      /stmpd1  (default)\n";
      print "      /stmpd2\n";
      print "      /stmpd3\n";
      print "      /stmpp1\n";
      print "      /stmpp2\n";

      print "  Return to accept default location or enter new location now.\n";
      print "\n";
      print "  Default STMP:  $stmp \n";
      print "     ?\n";
      my $new_stmp = <>;
      $new_stmp =~ s/^\s+|\s+$//g;

      if( length($new_stmp ) > 0 ) {
         $stmp = $new_stmp;
      }
      my $my_stmp="export C_STMP=\${C_STMP:-$stmp}";
      print "my_stmp = $my_stmp\n";
      print "\n\n";
      sleep( 1 );
   }

   print "my_ptmp = $my_ptmp\n";
   print "my_stmp = $my_stmp\n";
   print "\n";


   my $account = "export ACCOUNT=\${ACCOUNT:-glbss}";
   if( $machine ne "theia" ) {
      $account = "export ACCOUNT=\${ACCOUNT:-}";
   }

   #
   #  Update the conv_conf with the configuration information
   #
   my $conv_conf = "parm/CMon_config";
   open my $in,  '<',  $conv_conf      or die "Can't read $conv_conf: $!";
   open my $out, '>', "$conv_conf.new" or die "Can't write $conv_conf.new: $!";

   while( <$in> ) {
      if( $_ =~ "MY_CMON=" ) {
         print $out "$my_cmon\n";
      }
      elsif( $_ =~ "ACCOUNT=" ) {
         print $out "$account\n";
      }
      elsif( $_ =~ "CMON_TANKDIR=" ) {
         print $out "$my_tankdir\n";
      }
      elsif( $_ =~ "WEBSVR=" ) {
         print $out "$my_server\n";
      }
      elsif( $_ =~ "WEBUSER=" ) {
         print $out "$my_webuser\n";
      }
      elsif( $_ =~ "WEBDIR=" ) {
         print $out "$my_webdir\n";
      }
      elsif( $_ =~ "LITTLE_ENDIAN=" ) {
         print $out "$little_endian\n";
      }
      elsif( $_ =~ "MY_OS=" ) {         
         print $out "$my_os\n";
      }      
      elsif( $_ =~ "MY_MACHINE=" ) {
         print $out "$my_machine\n";
      }
      elsif( $_ =~ "C_PTMP=" ) {
         print $out "$my_ptmp\n";
      }
      elsif( $_ =~ "C_STMP=" ) {
         print $out "$my_stmp\n";
      }
      else {
         print $out $_;
      }
   }
   close $out;
   close $in;
   move "$conv_conf.new", $conv_conf;

   print "building executables\n"; 
   `./makeall.sh clean`;
   `./makeall.sh`;

   #     
   #   Update the default account settings in the data_map.xml file.
   #      
#   print "updating defaults in data_map.xml \n";
#   my $glbl_account = "GDAS-MTN";
#   if( $machine eq "zeus" ) {
#      $glbl_account = "ada"; 
#   }
#   elsif( $machine eq "wcoss" ) {
#      $glbl_account = "dev";
#   }

#   `/usr/bin/perl ./scripts/update_data_map.pl ./parm/data_map.xml global_default account $glbl_account`;


   print "<-- CMon_install.sh\n";

exit 0;
