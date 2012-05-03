#! /usr/bin/perl

#-------------------------------------------------------------------
#  update_data_map.pl
#
#  This script updates a requested field in the data_map.xml file.
#  It takes four items as input:
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
    use Data::Dumper;
    use IO::File;

    require XML::Simple;
    my $xs = XML::Simple->new;

    my $dmfile = @ARGV[0];
    my $source = @ARGV[1];
    my $field  = @ARGV[2];
    my $value  = $ARGV[3];
    my $rc     = "1";

    $fh = IO::File->new( $dmfile );
    $config = $xs->XMLin($fh, KeyAttr => {source => name} );
    close( $fh );
#    print Dumper( $config->{$source} );

#
#   Test to see if the requested field exists in the global_default
#   element.  If it does not, then the requested field is not valid.
#
    if( exists( $config->{ 'global_default' }->{$field} )) { 

#      $field is valid.  Determine if $source exists.

       if( exists( $config->{$source} )) { 

#         Modify $field with $value and write the changes back to the xml file.

          $config->{$source}->{ $field } = $value;
          my $path = $dmfile; 
          open my $ofh, '>:encoding(iso-8859-1)', $path or die "open($path): $!";
          $xs->XMLout( $config, OutputFile => $ofh, XMLDecl => 1, NoAttr => 1 );
          close( $ofh );
          $rc = "0";
       }
    }

    print "$rc";
