#! /usr/bin/perl

#-------------------------------------------------------------------
#  update_data_map.pl
#
#  This script updates a requested field in the data_map.xml file.  If 
#  the requested field does not exist in but the parent node (suffix) 
#  is found then the requested field and value are added to the 
#  data_map.xml file.
#
#  Calling sequence:
#    >> update_data_map.pl ./path/to/data_map.xml suffix req_field new_val
#
#    1. data_map.xml file (full or relative path)
#    2. suffix identifying the data source (an element in the 
#       data_map.xml file)
#    3. requested field, one of the xml child elements of the 
#       suffix element.  
#    4. new value for the requested field
#
#  Return codes (sent to stdout):
#    0  update was successful
#    1  the suffix and/or field was not found.
#
#  Note:  Calling scripts generally assign a variable value to 
#         output from this script.  If diagnostic print messages 
#         are left uncommented then results will become undefined.
#-------------------------------------------------------------------
   use strict;
   use warnings;
   use XML::LibXML;

   my $dmfile = $ARGV[0];
   my $source = $ARGV[1];
   my $field  = $ARGV[2];
   my $value  = $ARGV[3];
   my $rc     = "1";

   my $parser = XML::LibXML->new();
   my $doc    = $parser->parse_file($dmfile);

   my $query  = "//$source/$field/text()";

   my($node)  = $doc->findnodes($query);

   if( $node ) {
      $node->setData("$value" );
      $doc->toFile( $dmfile );
      $rc = "0";
   }
   else {
      my $new_query = "//$source";
      my ($src_node) = $doc->findnodes($new_query);
      $src_node->appendTextChild( "$field", "$value" ); 
      $doc->toFile( $dmfile );
      $rc = "0";
   }
 
   print "$rc";
