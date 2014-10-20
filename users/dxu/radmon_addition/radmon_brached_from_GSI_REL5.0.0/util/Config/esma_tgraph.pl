#!/usr/bin/perl
#
# Parses output of esma_timer.sh and prints simple graph.
# 
#.......................................................................

use Getopt::Std;         # command line options

  getopts('bdes:t');

usage() if $#ARGV < 0;

$pat = '\+';

$tmax = 0;
$tmin = 9999999999;

while ( <> ) {

    @tokens = split;

    if ( /^[ ]*TIME/ ) {
        $name = $tokens[1];
        $name =~ s/_install//;
        $name =~ s/_GridComp/_/;
        $name =~ s/GridComp/_/;
        # print "Name: <$name>, what = <$tokens[2]>\n";
        $isdir = ( $name =~ /\+/ );
        if ( $isdir ) {
        }
        if ( $opt_d ) { $ok = $isdir; }
        else          { $ok = 1 - $isdir; } 
        if ( $ok ) {
	    if ( "$tokens[2]" eq "beg" ) {
		if ( $tokens[3] < $tmin ) { $tmin = $tokens[3]; } 
		$beg{$name} = $tokens[3];
	    }
	    if ( "$tokens[2]" eq "end" ) {
		if ( $tokens[3] > $tmax ) { $tmax = $tokens[3]; } 
		$end{$name} = $tokens[3];
	    }
	}
    }

}

$scale = 1 unless ( $scale = $opt_s );
foreach $name ( keys %beg ) { 
        $delta{$name} = $end{$name} - $beg{$name}; 
        $Del{$name} = int(0.5+($end{$name} - $beg{$name})/$scale); 
        $Beg{$name} = int(0.5+($beg{$name} - $tmin)/$scale);
        $End{$name} = int(0.5+($end{$name} - $tmin)/$scale);
}

if ( $opt_b ) {
     @Names =  sort bybeg keys %beg;
 } elsif ( $opt_e ) {
    print "by del\n";
     @Names =  sort byend keys %beg;
 } elsif ( $opt_t ) {
     @Names =  sort bydel keys %beg;
 } else {
     @Names =  sort byend keys %beg;
 }

foreach $name ( @Names ) {
    $del = $delta{$name};
    $off = $beg{$name} - $tmin;
    if ( $opt_s ) {
	$del = $del / $opt_s;
	$off = $off / $opt_s;
    }
    $pad="";
    for $i ( 1..$off ) { $pad = $pad." "; }
    for $i ( 1..$del ) { $pad = $pad.'*'; }
    if ( $del < 1 )   { $pad = $pad.'o';}
    printf("%4d | %20s |%s\n",$delta{$name}, substr($name,0,20), $pad)
       if ( $del>0 );
}

printf(">>> Elapsed time: %d minutes\n",($tmax-$tmin+0.0)/60.); 

#.......................................................................

sub bybeg {
    $Beg{$a} <=> $Beg{$b}
             or
    $delta{$b} <=> $delta{$a}
              or
    $end{$b} <=> $end{$a}
              or
    $a cmp $b;
}

sub byend {
    $End{$b} <=> $End{$a}
             or
    $delta{$b} <=> $delta{$a}
              or
    $end{$b} <=> $end{$a}
              or
    $beg{$a} <=> $beg{$b}
              or
    $a cmp $b;
}
 
sub bydel {
    $Del{$b} <=> $Del{$a}
              or
    $delta{$b} <=> $delta{$a}
              or
    $Beg{$a} <=> $Beg{$b}
              or
    $a cmp $b;
}
 
#.......................................................................

sub usage {

    print<<'EOF';

NAME
     esma_tgraph - Parses output of esma_timer.sh and prints simple graph
          
SYNOPSIS

     esma_tgraph.pl [-b] [-d] [-s dt] makelog_filename
          
DESCRIPTION

     Parses output of esma_timer.sh and prints simple graph, optionally
     sorting the names by begining or ending time.

OPTIONS

    -b     sort by begning time (default sorts by ending time)
    -d     prints times for directories, not individual files
    -s dt  time unit for display; default is dt=1 or 1 sec
    -t     sort by execution time

SEE ALSO

    esma_timer.sh   the basic timing script

EOF

  exit(1)

 }
