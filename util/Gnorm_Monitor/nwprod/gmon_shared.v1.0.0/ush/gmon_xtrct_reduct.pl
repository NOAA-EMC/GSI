#! /usr/bin/perl
use strict;

#---------------------------------------------------------------------------
#  gmon_xtrct_reduct.pl
#
#  Extract the reduction stats for a GSI minimization run and store in
#  reduction.ieee_d files ready for GrADS use.
#---------------------------------------------------------------------------

if ($#ARGV != 3 ) {
	print "usage: extract_reduction.pl SUFFIX \n";
	exit;
}
my $suffix = $ARGV[0];
my $pdy    = $ARGV[1];
my $cyc    = $ARGV[2];
my $infile = $ARGV[3];

my $rc    = 0;
my $cdate = sprintf '%s%s', $pdy, $cyc;

if( (-e $infile) ) {

   my $reduct_target = "penalty and grad reduction";
   my $reduct_num = 12;

   open( INFILE, "<${infile}" ) or die "Can't open ${infile}: $!\n";

   my @reduct_array;

   while( my $line = <INFILE> ) {
      if( $line =~ /$reduct_target/ ) {
         my @reduct_ln  = split( / +/, $line ); 
         push( @reduct_array, $reduct_ln[$reduct_num] );
      }
   }

   close( INFILE );


   #################################
   #  write reduct_array to outfile
   #################################
   my $outfile = "${suffix}.reduction.${cdate}.ieee_d";
   open( OUTFILE, ">$outfile" ) or die "Can't open ${outfile}: $!\n";
   binmode OUTFILE;

   print OUTFILE pack( 'f*', @reduct_array);
   close( OUTFILE );

   #----------------------------
   #  copy outfile to $TANKverf
   #----------------------------
   my $tankdir = $ENV{"TANKverf"};
   if(! -d $tankdir) {
      system( "mkdir -p $tankdir" );
   }

   if( -e $outfile ) {
      my $newfile = "${tankdir}/${outfile}";
      system("cp -f $outfile $newfile");
   }

} else {				# $infile does not exist
   $rc = 5;
}

print "$rc \n"

