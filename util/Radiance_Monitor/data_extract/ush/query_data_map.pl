#! /usr/bin/perl

#-------------------------------------------------------------------
#  query_data_map.pl
#
#  This script returns a requested field from the data_map.xml file.
#  It takes three items as input:
#    1. data_map.xml file (full or relative path)
#    2. suffix identifying the data source (an element in the 
#       data_map.xml file)
#    3. requested field, one of the xml child elements of the 
#       suffix element.  
#
#  If the xml element associated with the suffix does not have the
#  requested field defined, then the default_global or 
#  default_regional element's corresponding field will be used.
#
#  The contents of that field are echoed to stdout for the calling
#  script to access.  If the field is empty or missing nothing
#  will be returned.  The calling script should verify a value has
#  been returned before use.
#
#-------------------------------------------------------------------
    use Data::Dumper;
    use IO::File;

    require XML::Simple;
    my $xs = XML::Simple->new;

    my $dmfile = @ARGV[0];
    my $source = @ARGV[1];
    my $field  = @ARGV[2];

    $fh = IO::File->new( $dmfile );
    $config = $xs->XMLin($fh, KeyAttr => {source => name} );
#    print Dumper( $config->{$source} );

#   Print the contents of the field if it's found.
#   Note that the hash tree gets a little confused with "" 
#   contents of elements and returns "HASH[number]" instead
#   of "" if the element exists but has no contents.  Catch
#   that condition with the =~ m/HASH/ check
#
#   If the element is not found then get the default element
#   and output it's value for the requested field.

    if( exists( $config->{$source}->{$field} )) {
       if( $config->{$source}->{$field} =~ m/HASH/ ) {
          print "";
       }
       else {
          print "$config->{$source}->{$field}";
       }
    }
    else {
       if( $config->{$source}->{ 'area' } eq "glb" ) {
          $source = "global_default";
          print "$config->{$source}->{$field}";
       }
       else {
          $source = "regional_default";
          if( exists( $config->{$source}->{$field} )) {
             if( $config->{$source}->{$field} =~ m/HASH/ ) {
                print "";
             }
             else {
                print "$config->{$source}->{$field}";
             }
          }
       }
    }
