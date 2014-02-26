#!/usr/bin/perl 
#####################################################################################
# Program: byteswapper
#
# Reads list of files from specifed filelist and performs two, four, and eight
# byte swapping based on specified structure description file.
#
#  Initial Version:  28 Jun 2004
#  Initial Programmer: Paul Haggerty (STC)
#  Revision Log:
#  29 Jun 2004 PDH     Removed EBCDIC->ASCII conversion for character data.  This
#                      Step is already performed in the AMSU processing code
#  09 Jul 2004 PDH     Embedded code in loop so that more than one file can be
#                      read from the am[ab]file
#  27 Jul 2004 PDH     Re-coded introductory section to allow for a general
#                      structure file, and a general file_list file to be specified
#                      Changed name to reflect more general byte swapping actions
#####################################################################################

use English;
use Getopt::Std;
use File::Copy;
use Fcntl qw(:DEFAULT);

getopts('s:f:d:',\%opts);

if (!$opts{s} or !$opts{f} or !$opts{d}) 
	{ die "\n\nCorrect usage:\n  $PROGRAM_NAME -s Structure_file -f File-list_file -d input_directory_path\n"; }

unless ((-e "$opts{s}") and (-r "$opts{s}")) 
	{ die "\n\nA readable Structure Description file must be specified\n"; }

unless ((-e "$opts{f}") and (-r "$opts{f}")) 
	{ die "\n\nA readable file containg the list of raw files must be specified\n"; }

$structurefile=$opts{'s'};
$l1bnamefile=$opts{'f'};
$direct_in=$opts{'d'};

#Open the l1b Name file and extract the name of the file to be processed.
open NAMEFILE, "$l1bnamefile" or die "Can not open L1B Name file $l1bnamefile\n";
while($input_filename=<NAMEFILE>)
{
   chomp $input_filename;
   
   $output_filename=$input_filename . ".linux";
   
   # The EBCDIC to ASCII Conversion Table
   @conversion_table=(0,1,2,3,156,9,134,127,151,141,142,11,12,13,14,15,16,17,18,19,157,
   133,8,135,24,25,146,143,28,29,30,31,128,129,130,131,132,10,23,27,136,137,138,139,140,
   5,6,7,144,145,22,147,148,149,150,4,152,153,154,155,20,21,158,026,32,160,161,162,163,
   164,165,166,167,168,213,46,60,40,43,124,38,169,170,171,172,173,174,175,176,177,33,36,
   42,41,59,94,45,47,178,179,180,181,182,183,184,185,229,44,37,95,62,63,186,187,188,189,
   190,191,192,193,194,96,58,35,64,39,61,34,195,97,98,99,100,101,102,103,104,105,196,197,
   198,199,200,201,202,106,107,108,109,110,111,112,113,114,203,204,205,206,207,208,209,
   126,115,116,117,118,119,120,121,122,210,211,212,91,214,215,216,217,218,219,220,221,222,
   223,224,225,226,227,228,93,230,231,123,65,66,67,68,69,70,71,72,73,232,233,234,235,236,
   237,125,74,75,76,77,78,79,80,81,82,238,239,240,241,242,243,92,159,83,84,85,86,87,88,89,
   90,244,245,246,247,248,249,48,49,50,51,52,53,54,55,56,57,250,251,252,253,254,255);
   
   # Open the STRUCTURE File and read data
   open STRUCTURE, "$structurefile" or die "Can not open structure file $structurefile\n";
   $headerline=<STRUCTURE>;

   ($reclen)=$headerline=~/RECLEN=\s*(\d+)/;

   while (<STRUCTURE>)
   {
       ($element_name,$type,$byte_size,$position,$header_scanline,$elements,$dimensions,$xsize,$ysize)=split;
   #
   # Subtract 1 from the postion, so that the index starts at 0 instead of 1
   #
       $position--;
       $typevar="vartype$header_scanline";   $$typevar{$position}=$type;
       $typevar="bytesize$header_scanline";  $$typevar{$position}=$byte_size;
       $typevar="elements$header_scanline";  $$typevar{$position}=$elements;
   }
   close STRUCTURE;

foreach (sort numerically keys %vartype1) { 
	#print "Position: $_ Type: $vartype1{$_} Size: $bytesize1{$_} #Elem: $elements1{$_}\n"; 
}
foreach (sort numerically keys %vartype2) { 
	#print "Position: $_ Type: $vartype2{$_} Size: $bytesize2{$_} #Elem: $elements2{$_}\n"; 
}
 
   # Open the Input File 
   
   open (INPUT_FILE, "<:raw", "$direct_in/$input_filename") or die "Can not open Raw Data File $direct_in/$input_filename\n";
   open (OUTPUT_FILE, ">:raw", "$direct_in/$output_filename") or die "Can not open Output File $direct_in/$output_filename\n";
   #
   # Read the header
   #
   $bytesread=read(INPUT_FILE,$buffer,$reclen);

   foreach $position (sort numerically keys %vartype1)
   {
      if ($vartype1{$position} eq "C")
      {

# This section removed since processing programs already perform the EBCDIC to ASCII conversion
     	#for($i=0; $i<$elements1{$position}; $i++)
     	#  {
     	#    vec($buffer, $i+$position, 8) = $conversion_table[vec($buffer, $i+$position, 8)];
     	#  }
      }
      elsif ($bytesize1{$position} == 1) {next;}
      else
      {
          $start_pos=$position;
          for ($i=0; $i<$elements1{$position}; $i++)
          {
              for ($index=0; $index<(int $bytesize1{$position}/2); $index++)
              {
                   $temp=vec($buffer, ($start_pos+$index), 8);
                   vec($buffer, ($start_pos+$index), 8)=vec($buffer, $start_pos+($bytesize1{$position}-$index-1), 8);
                   vec($buffer, $start_pos+($bytesize1{$position}-$index-1), 8)=$temp;
              }
              $start_pos+=$bytesize1{$position};
          } 
      }

   }
   print OUTPUT_FILE $buffer;

   #
   # Read the data
   #
   while ($bytesread != 0)
   {
      $bytesread="";
      $bytesread=read(INPUT_FILE,$buffer,$reclen);

   # If this is the end of the file, drop out of the loop
      next, if ($bytesread==0);
   
      foreach $position (sort numerically keys %vartype2)
      {
         if ($vartype2{$position} eq "C")
         {
   # This section removed since processing programs already perform the EBCDIC to ASCII conversion
          #for($i=0; $i<$elements2{$position}; $i++)
          #   {
          #     vec($buffer, $i+$position, 8) = $conversion_table[vec($buffer, $i+$position, 8)];
          #    }
         }
         elsif ($bytesize2{$position} == 1) {next;}
         else
         {
             $start_pos=$position;
             for ($i=0; $i<$elements2{$position}; $i++)
             {
                 for ($index=0; $index<(int $bytesize2{$position}/2); $index++)
                 {
                      $temp=vec($buffer, ($start_pos+$index), 8);
                      vec($buffer, ($start_pos+$index), 8)=vec($buffer, $start_pos+($bytesize2{$position}-$index-1), 8);
                      vec($buffer, $start_pos+($bytesize2{$position}-$index-1), 8)=$temp;
                 }
                 $start_pos+=$bytesize2{$position};
             } 
         }
   
      }
      print OUTPUT_FILE $buffer;
   }
   
   close INPUT_FILE;
   close OUTPUT_FILE;
   
   # Replace the CEMSCS file with the newly converted version
   move("$direct_in/$output_filename","$direct_in/$input_filename");
}
close NAMEFILE;

sub numerically { $a <=> $b }
