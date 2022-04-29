#! /usr/bin/perl

#-------------------------------------------------------------------
#  OznMon_install.pl
#
#  This script makes sets all necessary configuration definitions
#  and calls the makeall.sh script to build all the necessary
#  executables.  This script works for hera, wcoss_c, and
#  wcoss_d machines.
#
#-------------------------------------------------------------------

   use IO::File;
   use File::Copy qw(move);

   my $machine = `/usr/bin/perl get_hostname.pl`;
   my $my_machine="export MY_MACHINE=$machine";

   if( $machine ne "wcoss_c" && $machine ne "hera" && $machine ne "wcoss_d" && 
       $machine ne "wcoss2" ) {
      die( "ERROR --- Unrecognized machine hostname, $machine.  Exiting now...\n" );
   }
   else {
      print "machine = $machine\n";
   }

   #
   #  hera, wcoss_c, wcoss_d are all little endian machines, and all run linux
   # 
   my $little_endian = "export LITTLE_ENDIAN=\${LITTLE_ENDIAN:-0}";
   my $my_os = "linux";


   #
   #  Idenfity basedir location of package
   #
   print "\n";
   print "locating and saving OznMon package location\n"; 
   my $oznmon;
   $oznmon = `dirname $0`;
   $oznmon =~ s/^\s+|\s+$//g;
 
   if( $oznmon eq "." ) {
      $oznmon = `pwd`;
      $oznmon =~ s/^\s+|\s+$//g;
   }
   my $my_oznmon = "export MY_OZNMON=\${MY_OZNMON:-$oznmon}";
   print "my_oznmon = $my_oznmon \n";
   print"\n\n";

   sleep( 1 );

   #
   #  TANKDIR location
   #
   my $user_name = $ENV{ 'USER' };
   if( $machine eq "hera" ){
      $tankdir = "/scratch1/NCEPDEV/da/$user_name/nbns";
   }
   elsif( $machine eq "wcoss_d" ){
      $tankdir = "/gpfs/dell2/emc/modeling/noscrub/$user_name/nbns";
   }
   elsif( $machine eq "wcoss_c" ){
      $tankdir = "/gpfs/hps/emc/da/noscrub/$user_name/nbns";
   }
   elsif( $machine eq "wcoss2" ){
      $tankdir = "/lfs/h2/emc/da/noscrub/$user_name/nbns";
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
   my $my_tankdir="export OZN_TANKDIR=\${OZN_TANKDIR:-$tankdir}";
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
   my $my_server="export WEB_SVR=\${WEB_SVR:-$server}";
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
   my $my_webuser="export WEB_USER=\${WEB_USER:-$webuser}";
   print "my_webuser = $my_webuser\n";
   print "\n\n";
   sleep( 1 );
 
 
   #
   #  Web directory
   #
   my $webdir = "/home/people/emc/www/htdocs/gmb/gdas/es_ozn";

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

   #
   #  Set up ptmp and stmp locations according to $arch.
   #
   #
   my $my_ptmp;
   my $my_stmp;

   if( $machine eq "wcoss" ) {
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
      $my_ptmp="export OZN_PTMP=\${OZN_PTMP:-$ptmp}";
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
      $my_stmp="export OZN_STMP=\${OZN_STMP:-$stmp}";
      print "my_stmp = $my_stmp\n";
      print "\n\n";
      sleep( 1 );

   }
   elsif( $machine eq "wcoss_d" ) {
      $my_ptmp="export OZN_PTMP=\${OZN_PTMP:-/gpfs/dell2/ptmp}";
      $my_stmp="export OZN_STMP=\${OZN_STMP:-/gpfs/dell2/stmp}";
   }
   elsif( $machine eq "wcoss_c" ) {
      $my_ptmp="export OZN_PTMP=\${OZN_PTMP:-/gpfs/hps2/ptmp}";
      $my_stmp="export OZN_STMP=\${OZN_STMP:-/gpfs/hps2/stmp}";
   }
   elsif( $machine eq "wcoss2" ){
      $my_ptmp="export MY_PTMP=\${MY_PTMP:-/lfs/h2/emc/ptmp}";
      $my_stmp="export MY_STMP=\${MY_STMP:-/lfs/h2/emc/stmp}";
   }
   elsif( $machine eq "hera" ){
      $ptmp = "/scratch2/NCEPDEV/stmp3";

      print "Please specify PTMP location.  This is used for temporary work space.\n";
      print "\n";
      print "  Return to accept default location or enter new location now.\n";
      print "\n";
      print "  Default PTMP:  $ptmp \n";
      print "     ?\n";
      my $new_ptmp = <>;
      $new_ptmp =~ s/^\s+|\s+$//g;

      if( length($new_ptmp ) > 0 ) {
         $ptmp = $new_ptmp;
      }
      $my_ptmp="export OZN_PTMP=\${OZN_PTMP:-$ptmp}";
      print "my_ptmp = $my_ptmp\n";
      print "\n\n";
      sleep( 1 );


      $stmp = "/scratch2/NCEPDEV/stmp1";

      print "Please specify STMP location.  This is used for temporary work space.\n";
      print "\n";
      print "  Return to accept default location or enter new location now.\n";
      print "\n";
      print "  Default STMP:  $stmp \n";
      print "     ?\n";
      my $new_stmp = <>;
      $new_stmp =~ s/^\s+|\s+$//g;

      if( length($new_stmp ) > 0 ) {
         $stmp = $new_stmp;
      }
      $my_stmp="export OZN_STMP=\${OZN_STMP:-$stmp}";
      print "my_stmp = $my_stmp\n";
      print "\n\n";
      sleep( 1 );

   } 

   print "my_ptmp = $my_ptmp\n";
   print "my_stmp = $my_stmp\n";

   my $oznmon_config = "parm/OznMon_config";
   open my $in,  '<',  $oznmon_config      or die "Can't read $oznmon_config: $!";
   open my $out, '>', "$oznmon_config.new" or die "Can't write $oznmon_config.new: $!";

   while( <$in> ) {
      if( $_ =~ "MY_OZNMON=" ) {
         print $out "$my_oznmon\n";
      }
      elsif( $_ =~ "OZN_TANKDIR=" ) {
         print $out "$my_tankdir\n";
      }
      elsif( $_ =~ "WEB_SVR=" ) {
         print $out "$my_server\n";
      }
      elsif( $_ =~ "WEB_USER=" ) {
         print $out "$my_webuser\n";
      }
      elsif( $_ =~ "WEBDIR=" ) {
         print $out "$my_webdir\n";
      }
      elsif( $_ =~ "LITTLE_ENDIAN=" ) {
         print $out "$little_endian\n";
      }
      elsif( $_ =~ "MY_MACHINE=" ) {
         print $out "$my_machine\n";
      }
      elsif( $_ =~ "OZN_PTMP=" ) {
         print $out "$my_ptmp\n";
      }
      elsif( $_ =~ "OZN_STMP=" ) {
         print $out "$my_stmp\n";
      }
      else {
         print $out $_;
      }
   } 
   close $out;
   close $in;
   move "$oznmon_config.new", $oznmon_config;

   sleep( 1 );

   print "\n\n";

   my $rpt = 0;
   print "  The DO_DATA_RPT flag is used to set the validation and notification process\n";
   print "  to off/on.  The default is off.\n";
   print "  Return to accept default setting of 0 (off) or enter 1 to turn it on.\n";
   print "\n";
   print "  Default DO_DATA_RPT  $rpt \n";
   print "     ?\n";

   my $new_rpt = <>;
   $new_rpt =~ s/^\s+|\s+$//g;
   
   if( length($new_rpt ) > 0 ) {
      if( $new_rpt =~ '1' ) {
         $rpt = 1;
      }
   }
   my $my_rpt="export DO_DATA_RPT=\${DO_DATA_RPT:-$rpt}";
   print "my_rpt = $my_rpt\n";
   print "\n\n";
   sleep( 1 );

   # 
   #   Update the default account settings in the OznMon_user_settings script.
   #
   print "\n";
   print "Updating parm/OznMon_user_settings\n";

    my $account = "export ACCOUNT=\${ACCOUNT:-}";
   if( $machine eq "hera" ) {
      $account = "export ACCOUNT=\${ACCOUNT:-fv3-cpu}";
   } elsif( $machine eq "wcoss2" ){
      $account = "export ACCOUNT=\${ACCOUNT:-GFS-DEV}";
   }

   my $project = "export PROJECT=\${PROJECT:-GDAS-T2O}";
   if( $machine eq "wcoss2" ){
      $project = "export PROJECT=\${PROJECT:-GDAS-DEV}";
   } elsif( $machine ne "wcoss_c" && $machine ne "wcoss_d" ) {
      $project="export PROJECT=";
   }

   my $job_queue="export JOB_QUEUE=";
   if( $machine eq "wcoss_c" || $machine eq "wcoss2" ) {
      $job_queue="export JOB_QUEUE=\${JOB_QUEUE:-dev}";
   } elsif( $machine eq "wcoss" || $machine eq "wcoss_d" ){
      $job_queue = "export JOB_QUEUE=\${JOB_QUEUE:-dev_shared}";
   }


   my $uname = $ENV{ 'USER' };
   my $hpss_dir = "export HPSS_DIR=\${HPSS_DIR:-/NCEPDEV/hpssuser/g01/$uname/nbns/stats}";

   my $outfile = "tmp.file";
   open (OUT, ">", $outfile) || die "Cannot open file ".$outfile." for write";

   my $infile  = "./parm/OznMon_user_settings";
   open (IN, "<", $infile) || die "Cannot open file ".$infile." for read";  

   foreach $line (<IN>) {    
      chomp( $line );
      if ($line =~ m/export ACCOUNT/) {
         $line = $account;
      } 
      elsif( $line =~ m/export PROJECT/ ){
         $line = $project;
      }
      elsif( $line =~ m/export JOB_QUEUE/ ){
         $line = $job_queue;
      }
      elsif( $line =~ m/export HPSS_DIR/ ){
         $line = $hpss_dir;
      }
      elsif( $line =~ m/export DO_DATA_RPT/ ){
         $line = $my_rpt; 
      }
      print OUT "$line\n";    
   }    
   close OUT;    
   close IN;

   move $outfile, $infile;

   print "\n";
   print "Making all executables\n";

   `./build_OznMon_cmake.sh`;

 
   exit 0;

