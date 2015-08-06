#! /usr/bin/perl

#-------------------------------------------------------------------
#  MinMon_install.pl
#
#  This script makes sets all configuration definitions
#  and calls the makeall.sh script to build all the necessary
#  executables.  This script works for zeus and wcoss machines.
#
#-------------------------------------------------------------------

   use IO::File;
   use File::Copy qw(move);

   my $machine = `/usr/bin/perl get_hostname.pl`;
   my $my_machine="export MY_MACHINE=$machine";

   if( $machine ne "zeus" && $machine ne "wcoss" ) {
      die( "ERROR --- Unrecognized machine hostname, $machine.  Exiting now...\n" );
   }
   else {
      print "machine = $machine\n";
   }

   print" ---------------------------\n";
   print"  Installing MinMon Package \n";
   print" ---------------------------\n";
   sleep( 2 );

   #
   #  Idenfity basedir location of package
   #
   print "\n";
   print "locating and saving MinMon package location\n"; 
   my $minmon;
   $minmon = `dirname $0`;
   $minmon =~ s/^\s+|\s+$//g;
 
   if( $minmon eq "." ) {
      $minmon = `pwd`;
      $minmon =~ s/^\s+|\s+$//g;
   }
   my $my_minmon = "export MY_MINMON=\${MY_MINMON:-$minmon}";
   print "my_minmon = $my_minmon \n";
   print"\n\n";

   sleep( 1 );

   #
   #  TANKDIR location
   #
   my $user_name = $ENV{ 'USER' };
   if( $mahine eq "zeus" ) {
      $tankdir = "/scratch2/portfolios/NCEPDEV/global/save/$user_name/nbns";
   } 
   else {
      $tankdir = "/global/save/$user_name/nbns";
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
   my $my_tankdir="export MY_TANKDIR=\${MY_TANKDIR:-$tankdir}";
   print "my_tankdir = $my_tankdir\n";
   print "\n\n";
   sleep( 1 );


   #
   #  Set ptmp and stmp locations
   #
   #	(note I'll need to do something here for Zeus defaults)
   #
   $ptmp = "/ptmpd1";
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
   my $my_ptmp="export MY_PTMP=\${MY_PTMP:-$ptmp}";
   print "my_ptmp = $my_ptmp\n";
   print "\n\n";
   sleep( 1 );


   $stmp = "/stmpd1";
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
   my $my_stmp="export MY_STMP=\${MY_STMP:-$stmp}";
   print "my_stmp = $my_stmp\n";
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
   my $my_server="export WEBSERVER=\${WEBSERVER:-$server}";
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
   my $my_webuser="export WEBUSER=\${WEBUSER:-$webuser}";
   print "my_webuser = $my_webuser\n";
   print "\n\n";
   sleep( 1 );
 
 
   #
   #  Web directory
   #
   my $webdir = "/home/people/emc/www/htdocs/gmb/gdas/radiance/${webuser}/gsi_stat/pngs";
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
   my $my_webdir="export WEBDIR=\${WEBDIR:-$webdir}";
   print "my_webdir = $my_webdir\n";
   print "\n\n";
   sleep( 1 );

   my $infile  = "./parm/MinMon_config";
   my $outfile = "./parm/MinMon_config.new";
   open my $in,  '<', $infile      or die "Can't read $infile: $!";
   open my $out, '>', $outfile     or die "Can't write $outfile: $!";

   while( <$in> ) {
      if( $_ =~ "export MY_MINMON=" ) {
         print $out "$my_minmon\n";
      }
      elsif( $_ =~ "export MY_TANKDIR=" ) {
         print $out "$my_tankdir\n";
      }
      elsif( $_ =~ "export WEBSERVER=" ) {
         print $out "$my_server\n";
      }
      elsif( $_ =~ "export WEBUSER=" ) {
         print $out "$my_webuser\n";
      }
      elsif( $_ =~ "export WEBDIR=" ) {
         print $out "$my_webdir\n";
      }
      elsif( $_ =~ "export MY_MACHINE=" ) {
         print $out "$my_machine\n";
      }
      elsif( $_ =~ "export MY_PTMP=" ) {
         print $out "$my_ptmp\n";
      }
      elsif( $_ =~ "export MY_STMP=" ) {
         print $out "$my_stmp\n";
      }
      else {
         print $out $_;
      }
   } 
   close $out;
   close $in;
   move $outfile, $infile;


   # 
   #   Update the default account settings in the MinMon_user_settings script.
   #
   print "\n"; print "\n";
   print "\n"; print "\n";
   print "----------------------------------\n";
   print "Updating parm/MinMon_user_settings\n";
   print "----------------------------------\n";
   print "\n"; print "\n";
   sleep( 2 );
 
   my $account = "export ACCOUNT=\${ACCOUNT:-glbss}";
   if( $machine ne "zeus" ) {
      $account = "export ACCOUNT=\${ACCOUNT:-}";
   }

   #
   #  project definition
   #
   my $project = "GDAS-T2O";
   my $my_project = "";

   if( $machine ne "wcoss" ) {
      $project="export PROJECT=";
   } else {
      print "Please specify the PROJECT setting for job submissions from this package.\n";
      print "  Return to accept default PROJECT or enter new project.\n";
      print " \n";
      print "  Default PROJECT:  $project\n";
      print "    ?\n";
      my $new_project =<>;
      $new_project =~ s/^\s+|\s+$//g;
      if( length($new_project ) > 0 ) {
         $project = $new_project;
      }
      $my_project="export PROJECT=\${PROJECT:-$project}";
      print "my_project = $my_project\n";
      print "\n\n";
      sleep( 1 );
   }


   #
   #  job queue definition
   #
   my $job_queue = "dev_shared";
   my $my_job_queue = "";

   if( $machine ne "wcoss" ) {
      $job_queue="export JOB_QUEUE=";
   } else {
      print "Please specify the JOB_QUEUE for job submissions from this package.\n";
      print "  Return to accept default JOB_QUEUE or enter job queue.\n";
      print " \n";
      print "  Default JOB_QUEUE:  $job_queue\n";
      print "    ?\n";
      my $new_queue =<>;
      $new_queue =~ s/^\s+|\s+$//g;
      if( length($new_queue ) > 0 ) {
         $job_queue = $new_queue;
      }
      $my_job_queue="export JOB_QUEUE=\${JOB_QUEUE:-$job_queue}";
      print "my_job_queue = $my_job_queue\n";
      print "\n\n";
      sleep( 1 );

   } 


   my $outfile = "./parm/MinMon_user_settings.new";
   my $infile  = "./parm/MinMon_user_settings";
   
   open my $in,  '<', $infile      or die "Can't read $infile: $!";
   open my $out, '>', $outfile     or die "Can't write $outfile: $!";

   while( <$in> ) {
      if( $_ =~ "export ACCOUNT" ) {
         print $out "$account\n";
      } 
      elsif( $_ =~ "export PROJECT" ){
         print $out "$my_project\n";
      }
      elsif( $line =~ "export JOB_QUEUE" ){
         print $out "$my_job_queue\n";
      }
      else {
         print $out $_;
      }
   } 

   close $out;    
   close $in;

   move $outfile, $infile;


   exit 0;

