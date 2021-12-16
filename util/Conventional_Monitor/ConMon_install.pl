#! /usr/bin/perl

#-------------------------------------------------------------------
#
#  ConMon_install.pl
#
#  This script makes sets all necessary configuration definitions
#  and calls the makeall.sh script to build all the necessary
#  executables.  This script works for wcoss, theia, and cray 
#  machines.
#-------------------------------------------------------------------

   use IO::File;
   use File::Copy qw(move);

   print "--> ConMon_install.sh\n";

   my $machine = `/usr/bin/perl ./get_hostname.pl`;
   my $my_machine="export MY_MACHINE=$machine";

   if( $machine ne "hera" && $machine ne "wcoss" && $machine ne "wcoss_c" 
       && $machine ne "wcoss_d" && $machine ne "wcoss2" ) {
      die( "ERROR --- Unrecognized machine hostname, $machine.  Exiting now...\n" );
   }
   else {
      print "machine = $machine\n";
   }

   #---------------------------------------------------------------------------------
   #
   #  All 3 currently supported platforms are little endian machines and linux OSes.
   #    I'm keeping these switches though because that will surely change at some
   #    point and I'll just have to re-introduce the same switches.
   #    
   my $little_endian = "export LITTLE_ENDIAN=1";

   my $os = "linux";
   my $my_os = "export MY_OS=$os";


   #---------------------------------------------------------------
   #
   #  Idenfity basedir location of package
   #
   print "\n";
   print "locating and saving ConMon package location\n";
   my $conmon_dir;
   $conmon_dir = `dirname $0`;
   $conmon_dir =~ s/^\s+|\s+$//g;

   if( $conmon_dir eq "." ) {
      $conmon_dir = `pwd`;
      $conmon_dir =~ s/^\s+|\s+$//g;
   }
   my $my_conmon = "export MY_CONMON=$conmon_dir";
   print "my_conmon = $my_conmon \n";
   print"\n\n";

   sleep( 1 );

   #---------------------------------------------------------------
   #
   #  TANKDIR location
   #
   my $user_name = $ENV{ 'USER' };

   if( $machine eq "hera" ) {
      $tankdir = "/scratch1/NCEPDEV/da/$user_name/save/nbns";
   }
   elsif( $machine eq "wcoss" ) {
      $tankdir = "/global/save/$user_name/nbns";
   }
   elsif( $machine eq "wcoss_c" ) {
      $tankdir = "/gpfs/hps/emc/da/noscrub/$user_name"
   }
   elsif( $machine eq "wcoss_d" ) {
      $tankdir = "/gpfs/dell2/emc/modeling/noscrub/$user_name/nbns";
   }
   elsif( $machine eq "wcoss2" ){
      $tankdir = "/lfs/h2/emc/da/noscrub/$user_name/nbns";
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
   my $my_tankdir="export CONMON_TANKDIR=$tankdir";
   print "my_tankdir = $my_tankdir\n";
   print "\n\n";
   sleep( 1 );


   #---------------------------------------------------------------
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


   #---------------------------------------------------------------
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


   #---------------------------------------------------------------
   #
   #  Web directory
   #
   my $webdir = "/home/people/emc/www/htdocs/gmb/gdas/es_conv";
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


   #----------------------------------------------------
   #  Set up ptmp and stmp locations according to $arch.
   #
   my $ptmp    = "/ptmpd1";
   my $stmp    = "/stmpd1";
   my $my_ptmp = "export C_PTMP=\${C_PTMP:-$ptmp}";
   my $my_stmp = "export C_STMP=\${C_STMP:-$stmp}";

   if( $machine eq "hera" ) {
      $my_ptmp="export C_PTMP=\${C_PTMP:-/scratch2/NCEPDEV/stmp3}";
      $my_stmp="export C_STMP=\${C_STMP:-/scratch2/NCEPDEV/stmp1}";
   }
   elsif( $machine eq "wcoss_c" ) {
      $my_ptmp="export C_PTMP=\${C_PTMP:-/gpfs/hps/ptmp}";
      $my_stmp="export C_STMP=\${C_STMP:-/gpfs/hps/stmp}";
   } 
   elsif( $machine eq "wcoss_d" ) {
      $my_ptmp="export C_PTMP=\${C_PTMP:-/gpfs/dell2/ptmp}";
      $my_stmp="export C_STMP=\${C_STMP:-/gpfs/dell2/stmp}";
   }
   elsif( $machine eq "wcoss2" ){
      $my_ptmp="export MY_PTMP=\${MY_PTMP:-/lfs/h2/emc/ptmp}";
      $my_stmp="export MY_STMP=\${MY_STMP:-/lfs/h2/emc/stmp}";
   }

   #---------------------------------------
   #
   #  wcoss has several options available:
   #
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
      $my_ptmp="export C_PTMP=\${C_PTMP:-$ptmp}";
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
      $my_stmp="export C_STMP=\${C_STMP:-$stmp}";
      print "my_stmp = $my_stmp\n";
      print "\n\n";
      sleep( 1 );
   }

   print "my_ptmp = $my_ptmp\n";
   print "my_stmp = $my_stmp\n";
   print "\n";


   my $account = "export ACCOUNT=\${ACCOUNT:-}";
   if( $machine eq "hera" ) {
      $account = "export ACCOUNT=\${ACCOUNT:-fv3-cpu}";
   } elsif( $machine eq "wcoss2" ){
      $account = "export ACCOUNT=\${ACCOUNT:-GFS-DEV}";
   }

   my $project = "export PROJECT=\${PROJECT:-GDAS-T2O}";
   if( $machine eq "wcoss2" ){
      $project = "export PROJECT=\${PROJECT:-GDAS-DEV}";
   } elsif( $machine ne "wcoss" && $machine ne "cray" && $machine ne "wcoss_d" ) {
      $project="export PROJECT=";
   }

   my $job_queue="export JOB_QUEUE=";
   if( $machine eq "cray" || $machine eq "wcoss2" ) {
      $job_queue="export JOB_QUEUE=\${JOB_QUEUE:-dev}";
   } elsif( $machine eq "wcoss" || $machine eq "wcoss_d" ){
      $job_queue = "export JOB_QUEUE=\${JOB_QUEUE:-dev_shared}";
   }
   
   #------------------------------------------------------------
   #
   #  Update the config file with the configuration information
   #
   my $config = "parm/ConMon_config";
   open my $in,  '<',  $config      or die "Can't read $config $!";
   open my $out, '>', "$config.new" or die "Can't write $config.new: $!";

   while( <$in> ) {
      if( $_ =~ "MY_CONMON=" ) {
         print $out "$my_conmon\n";
      }
      elsif( $_ =~ "ACCOUNT=" ) {
         print $out "$account\n";
      }
      elsif( $_ =~ "PROJECT=" ) {
         print $out "$project\n";
      }
      elsif( $_ =~ "JOB_QUEUE=" ) {
         print $out "$job_queue\n";
      }
      elsif( $_ =~ "CONMON_TANKDIR=" ) {
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

   move "$config.new", $config;


   print "building executables\n"; 
   `./build_ConMon_cmake.sh`;


   print "<-- ConMon_install.sh\n";

exit 0;
