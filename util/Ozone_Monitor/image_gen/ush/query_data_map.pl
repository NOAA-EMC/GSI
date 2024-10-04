#! /usr/bin/perl

#-------------------------------------------------------------------
#  query_data_map.pl
#
#  This script returns a requested field from the data_map.xml file.
#  It takes three items as input:
#    1. data_map.xml file name (full or relative path)
#    2. suffix identifying the data source (an element in the 
#       data_map.xml file)
#    3. requested field, one of the xml child elements of the 
#       suffix element.  
#
#  If the xml element associated with the suffix does not have the
#  requested field defined, then the default_global or 
#  default_regional element's corresponding field will be used.  The
#  default is default_global, but if the suffix contains an area of
#  "rgn" then the regional_default element will be used.
#
#  The contents of that field are echoed to stdout for the calling
#  script to access.  If the field is empty or missing nothing
#  will be returned.  The calling script should verify a value has
#  been returned before use.
#
#-------------------------------------------------------------------
    use strict;
    use warnings;
    use XML::LibXML;

    if( $#ARGV < 2 ) { 
       exit
    }

    my $dmfile = $ARGV[0];
    my $source = $ARGV[1];
    my $field  = $ARGV[2];
    my $default="global_default";
    use XML::LibXML;

    my $parser = XML::LibXML->new();
    my $doc    = $parser->parse_file($dmfile);

#   Print the contents of the field if it's found in source.
#   If the field is not found in source then use the default element
#   and output it's value for the requested field.  

    my @srcs = $doc->findnodes("/opt/$source");
    if( @srcs <= 0 ) {
       @srcs = $doc->findnodes("/opt/$default");
    }

    if ( @srcs > 0 ) {

       my $src = $srcs[0];
       my($answer) = $src->findnodes("./$field");
       my($area) = $src->findnodes("./area");
       my $src_area = $area->to_literal;

       if( $answer ) {
          print $answer->to_literal;
       }
       else {
          if( $src_area eq "rgn" ) {
             $default = "regional_default";
          }
          my($def_src) = $src->findnodes("/opt/$default");
          my($def_answer) = $def_src->findnodes("./$field");
          if( $def_answer ) {
             print $def_answer->to_literal;
          }
       }
    }
    
