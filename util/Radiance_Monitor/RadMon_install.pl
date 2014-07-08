#! /usr/bin/perl

#-------------------------------------------------------------------
#  RadMon_install.pl
#
#  This script makes sets all necessary configuration definitions
#  and calls the makeall.sh script to build all the necessary
#  executables.  This script works for ccs, zeus, and wcoss
#  machines.
#
#-------------------------------------------------------------------

   use IO::File;
   use File::Copy qw(move);

   my $machine = `/usr/bin/perl get_hostname.pl`;
   my $my_machine="export MY_MACHINE=$machine";

   if( $machine ne "ccs" && $machine ne "zeus" && $machine ne "wcoss" ) {
      die( "ERROR --- Unrecognized machine hostname, $machine.  Exiting now...\n" );
   }
   else {
      print "machine = $machine\n";
   }

   #
   #  zeus is the only little endian machine
   # 
   my $little_endian = "export LITTLE_ENDIAN=\${LITTLE_ENDIAN:-0}";
   if( $machine eq "zeus" ) {
      $little_endian = "export LITTLE_ENDIAN=\${LITTLE_ENDIAN:-0}";   
   }

   my $my_os = "linux";

   #
   #  Idenfity basedir location of package
   #
   print "\n";
   print "locating and saving RadMon package location\n"; 
   my $radmon;
   $radmon = `dirname $0`;
   $radmon =~ s/^\s+|\s+$//g;
 
   if( $radmon eq "." ) {
      $radmon = `pwd`;
      $radmon =~ s/^\s+|\s+$//g;
   }
   my $my_radmon = "export MY_RADMON=\${MY_RADMON:-$radmon}";
   print "my_radmon = $my_radmon \n";
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
   my $webdir = "/home/people/emc/www/htdocs/gmb/gdas/radiance/${webuser}";
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
   my $my_ptmp="export PTMP=\${PTMP:-/ptmpp1}";
   my $my_stmp="export STMP=\${STMP:-/stmpp1}";
   if( $machine eq "zeus" ) {
      $my_ptmp="export PTMP=\${PTMP:-/scratch2/portfolios/NCEPDEV/ptmp}";
      $my_stmp="export STMP=\${STMP:-/scratch2/portfolios/NCEPDEV/stmp}";
   } 

   print "my_ptmp = $my_ptmp\n";
   print "my_stmp = $my_stmp\n";

   my $radmon_config = "parm/RadMon_config";
   open my $in,  '<',  $radmon_config      or die "Can't read $radmon_config: $!";
   open my $out, '>', "$radmon_config.new" or die "Can't write $radmon_config.new: $!";

   while( <$in> ) {
      if( $_ =~ "MY_RADMON=" ) {
         print $out "$my_radmon\n";
      }
      elsif( $_ =~ "MY_TANKDIR=" ) {
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
      elsif( $_ =~ "PTMP=" ) {
         print $out "$my_ptmp\n";
      }
      elsif( $_ =~ "STMP=" ) {
         print $out "$my_stmp\n";
      }
      else {
         print $out $_;
      }
   } 
   close $out;
   close $in;
   move "$radmon_config.new", $radmon_config;


   # 
   #   Update the default account settings in the RadMon_user_settings script.
   #
   print "\n";
   print "Updating parm/RadMon_user_settings\n";

   my $account = "export ACCOUNT=\${ACCOUNT:-ada}";
   if( $machine ne "zeus" ) {
      $account = "export ACCOUNT=\${ACCOUNT:-}";
   }

   my $project = "export PROJECT=\${PROJECT:-GDAS-T20}";
   if( $machine ne "wcoss" ) {
      $project="export PROJECT=";
   } 

   my $job_queue = "export JOB_QUEUE=\${JOB_QUEUE:-dev}";
   if( $machine ne "wcoss" ) {
      $job_queue="export JOB_QUEUE=";
   } 


    my $uname = $ENV{ 'USER' };
    my $hpss_dir = "export HPSS_DIR=\${HPSS_DIR:-/NCEPDEV/hpssuser/g01/$uname/nbns/stats}";

    my $outfile = "tmp.file";
    open (OUT, ">", $outfile) || die "Cannot open file ".$outfile." for write";

    my $infile  = "./parm/RadMon_user_settings";
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
       print OUT "$line\n";    
    }    
    close OUT;    
    close IN;

    move $outfile, $infile;


    print "\n";
    print "Making all executables\n";

    `./makeall.sh clean`;
    `./makeall.sh`;
 
   exit 0;

