#! /usr/bin/perl

#-------------------------------------------------------------------
#  ConvMon_install.pl
#
#  This script makes sets all necessary configuration definitions
#  and calls the makeall.sh script to build all the necessary
#  executables.  This script works for ccs, zeus, and wcoss 
#  machines.  
#
#-------------------------------------------------------------------

   use IO::File;
   use File::Copy qw(move);

   my $machine = `/usr/bin/perl ./scripts/get_hostname.pl`;
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
   my $little_endian = "export LITTLE_ENDIAN=0";
   if( $machine eq "zeus" ) {      $little_endian = "export LITTLE_ENDIAN=1";
   }

   my $os = "linux";
   if( $machine eq "ccs" ) {
      my $_os = "aix";
   }
   my $my_os = "export MY_OS=$os";


   #
   #  Idenfity basedir location of package
   #
   print "\n";
   print "locating and saving ConvMon package location\n";
   my $convdir;
   $convdir = `dirname $0`;
   $convdir =~ s/^\s+|\s+$//g;

   if( $convdir eq "." ) {
      $convdir = `pwd`;
      $convdir =~ s/^\s+|\s+$//g;
   }
   my $my_convdir = "export CONVDIR=$convdir";
   print "my_convdir = $my_convdir \n";
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
   my $my_tankdir="export MY_TANKDIR=$tankdir";
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
   my $my_ptmp="export PTMP=/ptmp/${user_name}";
   my $my_stmp="export STMP=/stmp/${user_name}";

   if( $machine eq "zeus" ) {
      $my_ptmp="export PTMP=/scratch2/portfolios/NCEPDEV/ptmp";
      $my_stmp="export STMP=/scratch2/portfolios/NCEPDEV/stmp";
   }

   print "my_ptmp = $my_ptmp\n";
   print "my_stmp = $my_stmp\n";
   print "\n";

   #
   #  Update the conv_conf with the configuration information
   #
   my $conv_conf = "parm/conv_conf";
   open my $in,  '<',  $conv_conf      or die "Can't read $conv_conf: $!";
   open my $out, '>', "$conv_conf.new" or die "Can't write $conv_conf.new: $!";

   while( <$in> ) {
      if( $_ =~ "CONVDIR=" ) {
         print $out "$my_convdir\n";
      }
      elsif( $_ =~ "MY_TANKDIR=" ) {
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
   move "$conv_conf.new", $conv_conf;

   print "building executables\n"; 
   `./makeall.sh clean`;
   `./makeall.sh`;

   #     
   #   Update the default account settings in the data_map.xml file.
   #      
   print "updating defaults in data_map.xml \n";
   my $glbl_account = "GDAS-MTN";
   if( $machine eq "zeus" ) {
      $glbl_account = "ada"; 
   }
   elsif( $machine eq "wcoss" ) {
      $glbl_account = "dev";
   }

   `/usr/bin/perl ./scripts/update_data_map.pl ./parm/data_map.xml global_default account $glbl_account`;

exit 0;
