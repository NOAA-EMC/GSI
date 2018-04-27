#! /usr/bin/perl


#------------------------------------------------------------------------
#  get_typelist.pl
#
#  Given a conventional data type and a convinfo.txt
#    file return a list of types and subtypes in the order
#    of [type id]_[subtype].
#
#  Arguments:
#    --file   Required:  convinfo.txt formatted file
#    --type   Required:  conventional data type
#    --mon    Optional:  include monitored data (if not 
#    		specified only assimilated data types/subtypes 
#    		will be returned
#
#    
#------------------------------------------------------------------------

   use strict;
   use warnings;
   use Getopt::Long;


#   print "--> Begin get_typelist.pl\n";

   my $convfile  = '';
   my $type      = '';
   my $use_mon   = '0';

   GetOptions( 'file:s' => \$convfile,
               'type:s' => \$type,
               'mon!'   => \$use_mon);

#   print "Options: \n";
#   print "   convfile   = $convfile \n";
#   print "   type       = $type     \n";
#   print "   use_mon    = $use_mon  \n";


   my @results;

   open my $info, $convfile or die "Could not open $convfile: $!";


   #----------------------------------------------------------------
   #  for each line split on white space, then check for a match on 
   #  field 1 (type).  If that matches and $use_mon is set to 1 or 
   #  if field 3 (iuse flag) is 1 then format the subtype value and
   #  pack them together into a single string in the results array
   #----------------------------------------------------------------
   while( my $line = <$info>)  {   
      my @ln = split ' ', $line;

      if( $ln[0] eq $type ){

	 if( $use_mon == 1 || $ln[3] == 1 ){
            my $typenum    = $ln[1];
            my $subtypenum = $ln[2];
  
            if( length( $ln[2] ) < 2 ){
               $subtypenum = sprintf( "%02d", $ln[2] );          
            } 
            my $entry = "${type}${typenum}_${subtypenum}";
            push @results, $entry;
         }
      }
   }
   close $info;

   print "@results";

#   print "<-- End get_typelist.pl\n";
