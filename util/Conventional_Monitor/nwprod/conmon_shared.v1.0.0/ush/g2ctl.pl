#!/usr/bin/perl -w
#   makes a GrADS control file for grib2 files
#
#   requires wgrib2 and Perl5
#
#   usage: g2ctl [options] [grib file] [optional index file] >[control file]
#
#   note: this script does not make the index file .. you have to run gribmap
#
#   Analyses: (using initial time)
#
#      $ g2ctl -0 example.grib >example.ctl
#      $ gribmap -0 -i example.ctl
#
#   Forecasts: (using verifiation time)
#
#      $ g2ctl example.grib >example.ctl
#      $ gribmap -i example.ctl
#
#   bugs:
#         many
#
# requires new wgrib2
# 
# documentation:  http://www.cpc.ncep.noaa.gov/products/wesley/g2ctl.html
#
#
# added output for rotated LatLon grids
#             Helmut P. Frank,  Helmut.Frank@dwd.de
#             Fri Sep 14 13:54:00 GMT 2001
# -ts, -lc options: Ag Stephens, BADC 3/2003
#
# requires grads 2.0a3+ - thinned grid update
#
$version="0.0.9";
use POSIX;
use Math::Trig qw(deg2rad rad2deg);


# ***** if wgrib2 is not on path, add it here
#$wgrib2='wgrib2';
$wgrib2='/nwprod/util/exec/wgrib2';
# $wgrib2='/export/cpc-lw-webisuzak/wd51we/bin.lnx64/wgrib2';
# $wgrib2='/export/cpc-lw-webisuzak/wd51we/grib2/wgrib2/wgrib2';

$wflag="";
$tflag="-VT";
$file="";
$index="";
$pdef="";
$prs="prs";
$calendar="";
$lc="";
$timestep="";
# $nearest_neighbor="";
$kludge="";
$no235z="";
$template="";
$pdef_nearest=1;
$raw="no";
$thinned_grid_msg=0;
$time_info=0;
$old_ens_name=0;

# set pdef_offset = 0 for old code, 1 for new code
$pdef_offset=0;
$use_minutes=1;

# vertical levels allowd, index = grib level number
$allow_profile[20] = '-tmp';
$profile_units[20] = 'temperature';
$allow_profile[100] = '-prs';
$profile_units[100] = 'hPa';
$allow_profile[102] = '+m';
$profile_units[102] = 'm above MSL';
$allow_profile[103] = '+m';
$profile_units[103] = 'm above ground';
$allow_profile[104] = '-';
$profile_units[104] = 'sigma';
$allow_profile[105] = '+hlev';
$profile_units[105] = 'hybrid';
$allow_profile[107] = '+iso';
$profile_units[107] = 'potential temperature';
$allow_profile[160] = '-dsl';
$profile_units[160] = 'm below sea level';

for ($i = 0; $i <= $#ARGV; $i++) {
   $_ = $ARGV[$i];
   SWITCH: {
      /^-verf/ && do { $tflag="-VT" ; last SWITCH; };
      /^-0/ && do { $tflag="-T" ; last SWITCH; };
      /^-b/ && do { $tflag ="-start_FT" ; last SWITCH };

      /^-no_prs/ && do { $prs="" ; last SWITCH; };
      /^-365/ && do { $calendar="365"; last SWITCH; };
      /^-no_min/ && do { $use_minutes = 0; last SWITCH; };
      /^-ts(\d+\w+)/ && do { $timestep=$1; last SWITCH; };
      /^-lc$/ && do { $lc="on"; last SWITCH; };
      /^-kludge/ && do { $kludge="on"; last SWITCH; };
      /^-no_235z/ && do { $no235z="on"; last SWITCH; };

      /^-iso_profile/ && do { undef @allow_profile; $allow_profile[235]='yes'; last SWITCH };
      /^-prs_profile/ && do { undef @allow_profile; $allow_profile[100]='yes'; last SWITCH };
      /^-m_profile/ && do { undef @allow_profile; $allow_profile[103]='yes'; 
                                  $allow_profile[105]='yes' ; last SWITCH };
      /^-no_profile/ && do { undef @allow_profile; last SWITCH };
      /^-time_info/ && do { $time_info=1 ; last SWITCH };

      /^-raw/ && do { $raw="yes" ; last SWITCH };
      /^-ens/ && do { @ens_names = split(',', $ARGV[++$i]); 
		$ens_match = $ARGV[$i];
		$ens_match =~ s/,/|/g;
		last SWITCH };
      /^-old_ens_name/ && do { $old_ens_name=1; last SWITCH };
      /^-pdef_linear/ && do { $pdef_nearest=0 ; last SWITCH };
      /^-/ && do { print STDERR "unknown option: $_\n"; exit 8; };
      if ($file eq "") {
         $file = $_;
      }
      elsif ($index eq "") {
         $index = $_;
      }
      else {
         $pdef = $_;
     }
   }
}

if ($file eq "") {
   if ($#ARGV >= 0) {
      print STDERR "*** missing grib file ***\n\n\n";
   }
   print STDERR "$0 $version  wesley ebisuzaki\n";
   print STDERR " makes a Grads control file for grib2 files\n";
   print STDERR " usage: $0 [options] [grib_file] [optional index file] [optional pdef file] >[ctl file]\n";
   print STDERR " -verf           .. use end of ave/acc period or fcst time (default), run gribmap\n";
   print STDERR " -0              .. use analysis or fcst_hour=0, run gribmap -0\n";
   print STDERR " -b              .. use use start of ave/acc period or fcst time, run gribmap -b\n";
   print STDERR " -365            .. 365 day calendar\n";
   print STDERR " -no_mins        .. ignore minutes\n";
   print STDERR " -ts[timestep]   .. set timestep for individual time files (e.g. -ts6hr)\n";
   print STDERR " -lc             .. set lowercase option for parameter names\n";
   print STDERR " -iso_profile    .. set z coordinate to ocean isotherms\n";
   print STDERR " -prs_profile    .. set z coordinate to pressure (mb)\n";
   print STDERR " -m_profile      .. set z coordinate to meters above ground/msl\n";
   print STDERR " -no_profile     .. no z coordinates\n";
   print STDERR " -time_info      .. timing info in comment section\n";
   print STDERR " -pdef_linear    .. linear interpolation for thinned grids\n";
   print STDERR " -raw            .. raw grid\n";
   print STDERR " -iso_profile    .. make profile using subsurface isoterms\n";
   print STDERR " -ens \"e1,..,en\" .. list of ensemble names for use with %e template ALPHA\n";
   print STDERR " -old_ens_name   .. use old ensemble names (versions before 8/2010)\n";
   print STDERR "\n";
   print STDERR "Note 1: the index file will be generated by the gribmap program, default: grib_file.idx\n";
   print STDERR "Note 2: the pdef file is only generated for thinned lat-lon grids, default: grib_file.pdef\n";
   print STDERR "Note 3: template options supported: %y4 %y2 %m2 %m1 %d2 %d1 %h2 %h1 %n2 %f2 %f3 %e %j3\n";
   print STDERR "                %iy4 %iy2 %im2 %im1 %id2 %id1 %ih2 %ih1 %in2 %fn2 %fhn %fdhn\n";
   exit 8;
}

$_ = $file;

if (/%y4/ || /%y2/ || /%m2/ || /%m1/ || /%d2/ || /%d1/ || /%h2/ || /%h1/ || /%n2/ || /%h1/
   || /%f2/ || /%j3/ || /%f3/ || /%e/ || /%iy2/ || /%iy4/ || /%im1/ || /%im2/ ||
   /%id1/ || /%id2/ || /%ih1/ || /%ih2/ || /%in2/ || /%fn2/ || /%fhn/ || /%fdhn/ )  { $template='on'; }

# if (-d "c:\\") {

# if (-d "c:\\") {
if ($^O eq 'MSWin32') {
   $ListA="$ENV{TEMP}\\h$$.tmp";
   $TmpFile="NUL";
   unlink $ListA;
   $sys="win";
}
else {
   $TmpFile="/dev/null";
   $j=0;
   while (-f "/tmp/g.$$.$j.tmp") {
      $j++;
   }
   $ListA="/tmp/g.$$.$j.tmp";
   $sys="unix";
}

# ctlfilename = name used by control file (different for template option(
# file = file name (of first matching file)
$ctlfilename=$file;


# inventory of All records
if ($template eq "on") {
   $gfile=$file;

   if ($sys eq 'win') {
      $gfile =~ s=\\=/=g;
   }
   $gfile =~ s/%y4/\\d{4}/g;
   $gfile =~ s/%iy4/\\d{4}/g;
   $gfile =~ s/%y2/\\d{2}/g;
   $gfile =~ s/%iy2/\\d{2}/g;
   $gfile =~ s/%m2/\\d{2}/g;
   $gfile =~ s/%im2/\\d{2}/g;
   $gfile =~ s/%m1/\\d{1,2}/g;
   $gfile =~ s/%im1/\\d{1,2}/g;
   $gfile =~ s/%d2/\\d{2}/g;
   $gfile =~ s/%id2/\\d{2}/g;
   $gfile =~ s/%d1/\\d{1,2}/g;
   $gfile =~ s/%id1/\\d{1,2}/g;
   $gfile =~ s/%h2/\\d{2}/g;
   $gfile =~ s/%ih2/\\d{2}/g;
   $gfile =~ s/%h1/\\d{1,2}/g;
   $gfile =~ s/%ih1/\\d{1,2}/g;
   $gfile =~ s/%n2/\\d{1,2}/g;
   $gfile =~ s/%in2/\\d{1,2}/g;
   $gfile =~ s/%h3/\\d{3}/g;
   $gfile =~ s/%f2/\\d{2,3}/g;
   $gfile =~ s/%f3/\\d{3}/g;
   $gfile =~ s/%j3/\\d{1,2,3}/g;
   $gfile =~ s/%fn2/\\d{2}/g;
   $gfile =~ s/%fhn/\\d{4}/g;
   $gfile =~ s/%fdhn/\\d{6}/g;


   $gfile_e = $gfile;
   $dir_e = $gfile;
   $gfile =~ s/%e/$ens_names[0]/;
   $dir=$gfile;
   $dir =~ s=(/*)[^/]*$=$1=;
   $dir_e =~ s=(/*)[^/]*$=$1=;
   $gfile =~ s/^$dir//;
   $gfile_e =~ s/^$dir_e//;

   $head=$gfile;
   $head =~ s=\\d\{.*==;
   $tail=$gfile;
   $tail =~ s=.*\\d\{.*\}==;
   if ($tail eq $head) { $tail=''; }

   if ($dir eq "") {
      opendir(DIR,'.');
   }
   else {
      opendir(DIR,$dir);
   }
   @allfiles = grep /^$gfile$/, readdir DIR;
   closedir DIR;

   if ($#allfiles <= -1 ) {
      print STDERR "\nError: could not find any files in directory: $dir\n";
      exit 8;
   }

# allfiles has the name of all the files
# need to find times: t0, t1, .. t-las

   for ($i = 0; $i <= $#allfiles; $i++) {
      $_ = $allfiles[$i];
      $_ =~ s/$head//;
      $_ =~ s/$tail//;
#     now $_ has the date code / forecast hour

      if ($i == 0) {
         $min_tval = $_;
         $min_t2val = $_;
         $max_tval = $_;
      }
      elsif ($min_tval eq $min_t2val && $min_tval eq $max_tval) {
          if ($_ > $min_tval) {
              $min_t2val = $_;
              $max_tval = $_;
          }
          else {
             $min_tval = $_;
          }
      }
      else {
         if ($_ > $max_tval) { $max_tval = $_; }
         elsif ($_ < $min_tval) {
            $min_t2val = $min_tval;
            $min_tval = $_;
         }
         elsif ($_ < $min_t2val && $_ > $min_tval) {
            $min_t2val = $_;
         }
      }
   }
   $file="$dir$head$min_tval$tail";
   if ($sys eq 'win') {
      $file =~ s=/=\\=g;
   }
#
#  make inventory of first two files and last file
#  need to get dt and last date

   system "$wgrib2 $wflag $tflag -lev -lev0 -ctl_ens -ftime -ctl_inv \"$dir$head$min_tval$tail\" >$ListA";
   if ($#allfiles >= 1) {
      system "$wgrib2 $wflag $tflag -lev -lev0 -ctl_ens -ftime -ctl_inv \"$dir$head$min_t2val$tail\" >>$ListA";
   }
   if ($#allfiles >= 2) {
      system "$wgrib2 $wflag $tflag -lev -lev0 -ctl_ens -ftime -ctl_inv \"$dir$head$max_tval$tail\" >>$ListA";
   }
}
else {
   system "$wgrib2 $wflag $tflag -lev -lev0 -ctl_ens -ftime -ctl_inv \"$file\" >$ListA";
}
if ( ! -s $ListA ) {
    print STDERR "Big problem:\n";
    print STDERR "  either $file is missing or not a grib file\n";
    print STDERR "  or wgrib2 is not on your path or wgrib2 is too old\n";
    print STDERR "  or can not write to $ListA (full disk or permissions).\n";
    unlink $ListA;
    exit 8;
}


# make table of dates and variables

scan_ListA();
unlink $ListA;


@sdates=sort keys(%dates);

# number of time 1 or greater
$ntime=$#sdates + 1;

$time=$sdates[0];

$year = substr($time,0,4);
$mo = substr($time,4,2);
$day = substr($time,6,2);
$hour = substr($time,8,2);
$minute = substr($time,10,2);
if ($use_minutes == 0) { $minute = 0; }
if ($mo < 0 || $mo > 12) {
   print "illegal date code $time\n";
   exit 8;
}

$month=substr("janfebmaraprmayjunjulaugsepoctnovdec",$mo*3-3,3);

if ($ntime > 1) {
    $year1 = substr($sdates[1],0,4);
    $mo1 = substr($sdates[1],4,2);
    $day1 = substr($sdates[1],6,2);
    $hour1 = substr($sdates[1],8,2);
    $minute1 = substr($sdates[1],10,2);
    if ($use_minutes == 0) { $minute1 = 0; }

    $year_last = substr($sdates[$#sdates],0,4);
    $mo_last = substr($sdates[$#sdates],4,2);
    $day_last = substr($sdates[$#sdates],6,2);
    $hour_last = substr($sdates[$#sdates],8,2);
    $minute_last = substr($sdates[$#sdates],10,2);
    if ($use_minutes == 0) { $minute_last = 0; }
}

# ---------------intro------------------------------------

if ("$index" eq "") {$index="$file.idx";}
if ("$pdef" eq "") { $pdef = "$file.pdef";}


if ($sys eq "unix") {
   $caret1 = (substr($file,0,1) eq "/") ? "" : '^';
   $caret2 = (substr($index,0,1) eq "/") ? "" : '^';
   $caret3 = (substr($pdef,0,1) eq "/") ? "" : '^';
}
else {
   $caret1 = (substr($file,1,1) eq ":") ? "" : '^';
   $caret2 = (substr($index,1,1) eq ":") ? "" : '^';
   $caret3 = (substr($pdef,1,1) eq ":") ? "" : '^';
}

print "dset $caret1$ctlfilename\nindex $caret2$index\n";

print "undef 9.999E+20\ntitle $file\n* produced by g2ctl v$version\n";
print "* command line options: @ARGV\n";


# ------------------- grid -----------------------
$griddef = `$wgrib2 $wflag \"$file\" -one_line -d 1 -nxny -grid -vector_dir`;
$_=$griddef;

$_ = $griddef;
$_ =~ s/^[^(]*//;
$_ =~ s/:.*//;

/\((\S*) x -*(\d*)\)/;
$nx=$1;
$ny=$2;

$_=$griddef;

$t = substr($griddef,0,240);
print "* griddef=$t\n";
print "dtype grib2\n";
if ($template eq "on") { print "options template\n"; }

if ($raw eq 'yes') {
   print "xdef $nx linear 0 0.1\n";
   print "ydef $ny linear 0 0.1\n";
}

elsif (/: Gaussian grid:/) {
   / lon (\S*) to (\S*)/;
   $lon0=$1;
   $lon1=$2;
   if ($lon1 <= $lon0) { $lon1 += 360.0; }
   $dlon = ($lon1 - $lon0) / ($nx - 1);

   / lat (\S*) to (\S*)/;
   if ($1 < $2) {
      $lat0=$1;
      $lat1=$2;
   }
   else {
      $lat0=$2;
      $lat1=$1;
   }
 
   /number of latitudes between pole-equator=(\S*)/;
   $nGauLats=2*$1;
   print "* ny=$ny nlat=$nGauLats lat0=$lat0 lat1=$lat1\n";
   print "xdef $nx linear $lon0 $dlon\n";
   print "ydef $ny levels\n";
   &print_range_gau_lats;
}

elsif (/ thinned global Gaussian grid:/) {

   / input (\S*)/;
   $scan = $1;
   if ($scan ne 'WE:NS' && $scan ne 'WE:SN') { print "\n* **** unsupported scan mode $scan\n"; }

   /#grid points by latitude: (.*)/;
   $list = $1;
   /#points=(\d*)/;
   $npnts=$1;
   $i = 1; $nx = 0;
   foreach $t (split(' ',$list)) {
       $t =~ s/:.*//;
       $nlon[$i++] = $t;
       if ($nx < $t) { $nx = $t; }
   }
   $dx = 360.0 / $nx;
   $t = 0;
   printf "xdef $nx linear 0 $dx\n";
   print "ydef $ny levels\n";
   &print_gau_lats;

#  now to create the pdef file

   open (PDEF, ">$pdef");
   binmode PDEF;

   $regional_thinned_grid=0;
   if ($pdef_nearest == 0) { 
      pdef_linear();
   } else {
      pdef_near();
   }
   $thinned_grid_msg=1;
   close(PDEF);
}
elsif (/ thinned (global|regional) lat-lon grid:/) {
   / lat *(\S*) to (\S*) by (\S*) /;
   $lat0 = $1;
   $lat1 = $2;
   $dy = $3;
   / lon (\S*) to (\S*) with variable spacing/;
   $lon0=$1;
   $lon1=$2;
   /\(-1 x (\d*)\)/;
   $ny = $1;
   / #points=(\d*) /;
   $npnts = $1;
   / input (\S*)/;
   $scan = $1;
   if ($scan ne 'WE:NS' && $scan ne 'WE:SN') { print "\n* **** unsupported scan mode $scan\n"; }

   if ($lat0 > $lat1) {
       $yrev = 1;
       print "ydef $ny linear $lat1 ", abs($dy), "\n"
   }
   else {
      $yrev = 0;
      print "ydef $ny linear $lat0 ", abs($dy), "\n"
   }

   $t=$_;
   $_ =~ s/^.*points by latitude: //;
   $_ =~ s/:.*//;
   $list=$_;
   $i = 1;
   foreach $t (split(' ',$list)) {
     $nlon[$i++] = $t;
   }
   if ($lon1 <= $lon0) { $lon1 += 360.0; }
#
# check if global
#
   $nx = $nlon[$ny];
   $dx = ($lon1 - $lon0) / ($nx - 1);
   $dx=$dx*$nx;
   $regional_thinned_grid = 1;
   if (abs($dx-360.0) < 0.01) {$regional_thinned_grid=0; }
#
# figure out dx and nx for the projection grid
#
   $nx=$nlon[1];
   if ($nx < $nlon[$ny]) { $nx = $nlon[$ny]; }
   $dx = ($lon1 - $lon0) / ($nx - 1);
   print "xdef $nx linear $lon0 $dx\n";


   open (PDEF, ">$pdef");
   binmode PDEF;

   $regional_thinned_grid=1;
   if ($pdef_nearest == 0) { 
      pdef_linear();
   } else {
      pdef_near();
   }

   close(PDEF);
   $thinned_grid_msg=1;
}


elsif (/: lat-lon grid:/) {
   / lat (\S*) to (\S*) by (\S*)/;
   $lat0=$1;
   $lat1=$2;
   $dlat=$3;

   / lon (\S*) to (\S*) by (\S*)/;
   $lon0=$1;
   # $lon1=$2;
   $dlon=$3;

   if ($lat0 > $lat1) {
      print "ydef $ny linear $lat1 ", abs($dlat), "\n"
   }
   else {
      print "ydef $ny linear $lat0 ", abs($dlat), "\n"
   }
   if ($lon0 + ($nx-1) * $dlon > 360.0) { $lon0 -= 360.0; }
   print "xdef $nx linear $lon0 $dlon\n";
}

elsif (/ Mercator grid: /) {

   # beta: mercator
   # scan modes .. assumes west to east
   /lat *(\S*) to (\S*) /;
   $lat1 = $1;
   $lat2 = $2;
   /lon (\S*) to (\S*) /;
   $lon1 = $1;
   $lon2 = $2;
   /grid: \((\d*) x (\d*)\) /;
   $nx = $1;
   $ny = $2;

   $dlon = $lon2 - $lon1;
   if ($dlon <= 0) {
      $dlon = $dlon + 360;
   }
   $dlon = $dlon / ($nx - 1);

   if ($lon1 + ($nx-1) * $dlon > 360.0) { $lon1 -= 360.0 }

   print "xdef $nx linear $lon1 $dlon\n";

   if ($lat1 > $lat2) {
#            print "options yrev\n";
            $t = $lat2;
            $lat2 = $lat1;
            $lat1 = $t;
   }
   print "ydef $ny levels\n";
   $i = 0;
   $n1 = log(tan((45+$lat1/2)*3.1415927/180));
   $n2 = log(tan((45+$lat2/2)*3.1415927/180));
   $dy = ($n2 - $n1) / ($ny - 1);

   while ($i < $ny) {
      $nn = $n1 + $dy * $i;
      $lat = (atan(exp($nn))*180/3.1415927-45)*2;
      printf ("%9.4f ", $lat);
      $i++;
      if ($i % 7 == 0) { print "\n"; }
   }
   if ($i % 7 != 0) { print "\n"; }


}

elsif (/ Lambert Conformal: /) {
   / Lat1 (\S*) Lon1 (\S*) LoV (\S*) LatD \S* Latin1 (\S*) Latin2 (\S*) /;
   $lat1 = $1;
   $lon1 = $2;
   $lov = $3;
   $latin1 = $4;
   $latin2 = $5;

   /Pole \((\S*) x (\S*)\) Dx (\S*) m Dy (\S*) m /;
   $nx = $1;
   $ny = $2;
   $dx = $3;
   $dy = $4;

   $t='lcc';
   if (/:winds\(grid\)/) { $t='lccr' ; }

   if ($dx == 0 || $dy == 0) {
      print STDERR "Problem: DX or DY is missing! Cannot make a control file\n";
      exit;
   }

   &domain;

# make sure than lon1 is within 180 degrees of lov
# lov, lon1 should be between 0 and 360
   if ($lon1 < $lov - 180.0) { $lon1 += 360.0; }
   if ($lon1 > $lov + 180.0) { $lon1 -= 360.0; }

# make sure that lov is within 180 degrees of west
# lov is between 0 and 360 and west is between -180 and 180

   if ($lov < ($west+$east)/2 - 180.0) { $lov += 360.0 ; $lon1 += 360.0; };
   if ($lov > ($west+$east)/2 + 180.0) { $lov -= 360.0 ; $lon1 -= 360.0; };

   print "pdef $nx $ny $t $lat1 $lon1 1 1 $latin1 $latin2 $lov $dx $dy\n";

#   if ($lon1 - $west >= 360.0) {
#      $lon1 -= 360.0;
#      $lov -= 360.0;
#   }
#   if ($lov < $lon1 - 180.0) { $lov += 360.0; }
#   if ($lov > $lon1 + 180.0) { $lov -= 360.0; }
#   print "pdef $nx $ny $t $lat1 $lon1 1 1 $latin1 $latin2 $lov $dx $dy\n";

   $dx = $dx / (110000.0 * cos($lat1*3.141592654/180.0));
   $dy = $dy / 110000.0;

   $tmp = int(($east - $west) / $dx + 1);
   if (($west == -180) && ($east == 180)) {
      $tmp -= 1;
   }
   print "xdef $tmp linear $west $dx\n";
   $tmp = int(($north - $south) / $dy + 1);
   print "ydef $tmp linear $south $dy\n";

}
elsif (/ polar stereographic grid: /) {
   / (\S*) pole lat1 (\S*) lon1 (\S*) latD (\S*) lonV (\S*) dx (\S*) m dy (\S*) m/;
   $pole = $1;
   $lat1 = $2;
   $lon1 = $3;
   $orient = $5;
   $dx = $6;
   $dy = $7;

   / \((\S*) x (\S*)\) input (\S*) /;
   $nx=$1;
   $ny=$2;
   $scan=64;
#   $scan=-1;
#   if ($3 eq 'WE:NS') { $scan = 0; }
#   if ($3 eq 'WE:SN') { $scan = 64; }
#   if ($scan == -1) { printf STDERR "HELP polar stereographic code needs to be extended!!\n"; }

   # probably only works for scan=64

   $dpr=3.14159265358979/180.0;
   $rearth=6.3712e6;

   $h=1;
   $proj="nps";
   if ($pole eq "South") {
      $h=-1;
      $orient -= 180.0;
      $proj="sps";
   }
   $hi=1;
   $hj=-1;
   if (($scan/128 % 2) == 1) {
      $hi=-1;
   }
   if (($scan/64 % 2) == 1) {
      $hj=1;
   }
   if ($dx == 0 || $dy == 0) {
      print STDERR "Problem: DX or DY is missing! Cannot make a control file\n";
      exit;
   }
   $dxs=$dx*$hi;
   $dys=$dy*$hj;
   $de=(1+sin(60*$dpr))*$rearth;
   $dr=$de*cos($lat1*$dpr)/(1+$h*sin($lat1*$dpr));
   $xp=1-$h*sin(($lon1-$orient)*$dpr)*$dr/$dxs;
   $yp=1+cos(($lon1-$orient)*$dpr)*$dr/$dys;
   $dx=$h*$dx/1000;

   printf "pdef $nx $ny $proj $xp $yp $orient $dx\n";

   &domain;

   if ( 1 <= $xp && $xp <= $nx && 1 <= $yp && $yp <= $ny) {
       # pole is in the grid!
       $dx = 1000.0 * abs($dx);
       $nx = int(2 * $rearth * 3.14 / $dx + 1);
       $dx = 360 /($nx-1);
       print "xdef $nx linear -180 $dx\n";
       if ($proj eq "nps") { $north=90; }
       else { $south = -90; }
       $dy = abs($dy);
       $ny = int($rearth * 3.14 * ($north-$south)/180.0 / $dy + 1);
       $dy = ($north-$south)/($ny-1);
       $tmp = int(($north - $south) / $dy + 1);
       print "ydef $tmp linear $south $dy\n";
   }
   else {
      $dx = 1000.0 * abs($dx);
      $nx = int(2 * $rearth * 3.14 * ($east-$west)/360.0 / $dx + 1);
      $dx = ($east-$west) /($nx-1);

#     make dx a faction of 360 so grads can id as a global field
      $nx = int(360.0 / $dx) + 1.0;
      $dx = 360.0 / $nx;
      $nx = int( ($east-$west)/$dx ) + 1;

      $dy = abs($dy);
      $ny = int($rearth * 3.14 * ($north-$south)/180.0 / $dy + 1);
      $dy = ($north-$south)/($ny-1);

      $tmp = int(($east - $west) / $dx + 1);
      if (($west == -180) && ($east == 180)) {
         $tmp -= 1;
      }
      print "xdef $tmp linear $west $dx\n";

      $tmp = int(($north - $south) / $dy + 1);
      print "ydef $tmp linear $south $dy\n";
   }
}
elsif ( /  thinned gaussian:/) {
        / lat *(\S*) to (\S*)/;
        $lat0 = $1;
        $lat1 = $2;

        / long (\S*) to (\S*), (\S*) grid pts   \(-1 x (\S*)\) /;
        $lon0=$1;
        $lon1=$2;
        $nxny = $3;
        $ny = $4;
        print "ydef $ny levels\n";
        $yrev = 0;
        if ($lat0 > $lat1) { $yrev = 1; }

        $eps = 3e-14;
        $m=int(($ny+1)/2);

        $i=1;
        while ($i <= $m) {
                $z=cos(3.141592654*($i-0.25)/($ny+0.5));
                do {
                        $p1 = 1;
                        $p2 = 0;
                        $j = 1;
                        while ($j <= $ny) {
                                $p3 = $p2;
                                $p2 = $p1;
                                $p1=((2*$j-1)*$z*$p2-($j-1)*$p3)/$j;
                                $j++;
                        }
                        $pp = $ny*($z*$p1-$p2)/($z*$z-1);

                        $z1 = $z;
                        $z = $z1 - $p1/$pp;
                } until abs($z-$z1) < $eps;
                $x[$i] = -atan2($z,sqrt(1-$z*$z))*180/3.141592654;
                $x[$ny+1-$i] = -$x[$i];
                $i++;
        }
        $i = 1;
        while ($i < $ny) {
                printf  " %7.3f", $x[$i];
                if (($i % 10) == 0) { print "\n"; }
                $i++;
        }
        printf " %7.3f\n", $x[$ny];

        $t=$_;

        s/\n//g;
        / bdsgrid \S* (.*) min/;
        $list=$1;
        $i = 1;
        $nx = 0;
        foreach $t (split(' ',$list)) {
           $nlon[$i++] = $t;
           if ($nx < $t) { $nx = $t; }
        }
        if ($lon1 <= $lon0) { $lon1 += 360.0; }
        $dx = ($lon1 - $lon0) / ($nx - 1);
        print "xdef $nx linear $lon0 $dx\n";

#  now to create the pdef file

        open (PDEF, ">$pdef");
        binmode PDEF;

        print "pdef $nxny 1 file 1 stream binary $caret3$pdef\n";

        if ($yrev == 0) {
           $offset = 0;
           for ($j = 1; $j <= $ny; $j++) {
              for ($i = 0; $i < $nx; $i++) {
                 $x = $i / ($nx - 1.0) * ($nlon[$j] - 1);
                 $x = floor($x + 0.5) + $offset + $pdef_offset;
                 print PDEF pack("L", $x);
              }
              $offset += $nlon[$j];
           }
        }
        else {
           $offset = $nxny;
           for ($j = $ny; $j >= 1; $j--) {
              $offset -= $nlon[$j];
              for ($i = 0; $i < $nx; $i++) {
                 $x = $i / ($nx - 1.0) * ($nlon[$j] - 1);
                 $x = floor($x + 0.5) + $offset + $pdef_offset;
                 print PDEF pack("L", $x);
              }
           }
        }

#      print weights
       $x = pack("f", 1.0);
       for ($i = 0; $i < $nx*$ny; $i++) {
          print PDEF $x;
       }
#  print wind rotation
       $x = pack("L", -999);
       for ($i = 0; $i < $nx*$ny; $i++) {
          print PDEF $x;
       }
       close(PDEF);
       $thinned_grid_msg=1;
}

#     rotated LatLon grid
elsif (/ rotated lat-lon grid/) {
      / lat (\S*) to (\S*) by (\S*)/;
      $lat0 = $1;
      $lat1 = $2;
      $dlat = $3;

      / lon (\S*) to (\S*) by (\S*)/;
      $lon0 = $1;
      $lon1 = $2;
      $dlon = $3;

      /rotated lat-lon grid:\((\S*) x (\S*)\)/;
      $nx = $1;
      $ny = $2;

      / south pole lat=(\S*) lon=(\S*) angle of rot=(\S*):/;
      $lat_sp = $1;
      $lon_sp = $2;
      $rot_angle = $3;

      print "* Rotated LatLon grid: South pole lat $lat_sp lon $lon_sp",
             "  rot angle $rot_angle\n";

      if ($dlon < $dlat) { $dlat = $dlon ;}
      $dlon = $dlat;

#      $dlon = ($lon1-$lon0)/($nx-1);
#      $dlat = ($lat1-$lat0)/($ny-1);

      $t0 = $lon_sp+180.0;
      if ($t0 > 360.0) { $t0 -= 360.0; }
      $t1 = -$lat_sp;
      $pdef='rotll';
      if (/:winds\(grid\)/) { $pdef='rotllr'; }
      print "pdef $nx $ny $pdef $t0 $t1 $dlon $dlat $lon0 $lat0\n";
      ($swlat, $swlon) = rr2ll($lat_sp,$lon_sp,$rot_angle,$lat1,$lon0);

      if ($lat0 > $lat1) {
         ($swlat, $swlon) = rr2ll($lat_sp,$lon_sp,$rot_angle,$lat1,$lon0);
         ($nwlat, $nwlon) = rr2ll($lat_sp,$lon_sp,$rot_angle,$lat0,$lon0);
         ($nelat, $nelon) = rr2ll($lat_sp,$lon_sp,$rot_angle,$lat0,$lon1);
         ($selat, $selon) = rr2ll($lat_sp,$lon_sp,$rot_angle,$lat1,$lon1);
         $northlat=($nwlat>$nelat)?$nwlat:$nelat;
         $southlat=($swlat<$selat)?$swlat:$selat;
         $goodny=($northlat-$southlat)/$dlat;
         $nlat=$northlat-fmod($northlat,abs($dlat));
         print "options yrev\n";
         print "ydef $goodny linear $nlat ", abs($dlat), "\n"
      }
      else {
         ($swlat, $swlon) = rr2ll($lat_sp,$lon_sp,$rot_angle,$lat0,$lon0);
         ($nwlat, $nwlon) = rr2ll($lat_sp,$lon_sp,$rot_angle,$lat1,$lon0);
         ($nelat, $nelon) = rr2ll($lat_sp,$lon_sp,$rot_angle,$lat1,$lon1);
         ($selat, $selon) = rr2ll($lat_sp,$lon_sp,$rot_angle,$lat0,$lon1);
         $northlat=($nwlat>$nelat)?$nwlat:$nelat;
         $southlat=($swlat<$selat)?$swlat:$selat;
         $goodny=floor(($northlat-$southlat)/$dlat)+1;
         $slat=$southlat-fmod($southlat,abs($dlat));
         print "ydef $goodny linear $slat ", abs($dlat), "\n"
      }

      $eastlon=($nelon>$selon)?$nelon:$selon;
      $westlon=($swlon<$nwlon)?$swlon:$nwlon;
      $goodnx=floor(($eastlon-$westlon)/$dlon)+1;
      $wlon=$westlon-fmod($westlon,abs($dlon));
      print "xdef $goodnx linear $wlon $dlon\n";
}
elsif (/:grid_template=204:/) {
   print "pdef 1200 1684 bilin sequential binary-big pdef_ncep_12.atl\n";
   print "xdef 1300  linear -105.04 0.1325\n";
   print "ydef 1500  linear -25.2   0.0678\n";
}
else {
   print STDERR "*** script needs to be modified ***\n";
   print STDERR "*** unknown grid ***\n";
   exit 8;
}


# make the tdef statement

&tdef;

# ------------------var-------------------------------------;

#  Find the vertical profile

$nlevelmax=0;
$levelsmax=0;
$profile_kpds6=-1;

foreach $fname (sort keys(%flevels)) {
   ($name, $kpds6) = split(/:/, $fname);
   if (defined($allow_profile[$kpds6])) {
      $_=$flevels{$fname};
      $nlev = (tr/ / /) - 1;
      if ($nlev > $nlevelmax) {
         $nlevelmax = $nlev;
         $levelsmax = $flevels{$fname};
         $profile_kpds6 = $kpds6;
      } 
   }
}

if ($nlevelmax <= 1) {
   print "zdef 1 linear 1 1\n";
}
else {
   print "* PROFILE $profile_units[$profile_kpds6]\n";
   print "zdef $nlevelmax levels ";
   $tail=substr($allow_profile[$profile_kpds6],1);
   ($_ = $levelsmax) =~ s/.//;
   chop($_);
   if (substr($allow_profile[$profile_kpds6],0,1) eq '+') {
      $levelsmax = join ' ', sort {$a <=> $b} split(/ /,$_);
   }
   else { 
      $levelsmax = join ' ', sort {$b <=> $a} split(/ /,$_);
   }
   print "$levelsmax\n";
   if ($profile_kpds6 == 100) { print "options pascals\n"; }
}
if ($nlevelmax == 1) { $profile_kpds6 = -1; }

$nvar = 0;

#
foreach $fname (sort keys(%fcomments)) {
   ($name, $kpds6) = split(/:/, $fname);
   ($kpds5, $comment) = split(/:/, $fcomments{$fname});

   # fix names to be grads compatible
   # eliminate dashes, underscores, blanks and put no in front of leading digits
   $_ = $name;
   $_ =~ tr/_\- //d;
   if ($lc) { $_ =~ s/(.*)/\L$1/gi; }
   if ( /^\d/ ) { $_ = "no$_"; }
   $name = $_;

   if (defined $flayers{$fname}) {
      $kpds7s = $flayers{$fname};
      foreach $ll (split(' ',$kpds7s)) {
         $lev_txt = $lev_def{"$kpds6,$ll"};
         $lev_0 = $lev0{"$kpds6,$ll"};

         $ll =~ s/,a/ a/;
         if ($ll eq ',') {
            $out[$nvar++] = "${name}$lev_0 0,$kpds6   $kpds5 ** $lev_txt $comment";
         }
         else {
            $out[$nvar++] = "${name}$lev_0 0,$kpds6,$ll   $kpds5 ** $lev_txt $comment";
         }
      }
   }
   if (defined $flevels{$fname}) {
      $_ = $flevels{$fname};
      $kpds7s = $flevels{$fname};
      $nlev = (tr/ / /) - 1;

      $is_profile = 0;
         if ($profile_kpds6 == $kpds6 && $nlev >= 3) {

         ($_ = $kpds7s) =~ s/.//;
         chop($_);
         if ($allow_profile[$profile_kpds6] eq '+') {
            $_ = join ' ', sort {$a <=> $b} split(/ /,$_);
         }
         else { 
            $_ = join ' ', sort {$b <=> $a} split(/ /,$_);
         }
        if (substr($levelsmax,0,length($_)) eq $_) { $is_profile = 1; }
        if ($is_profile == 0 && $nlev > 4) { $is_profile = 2; $nlev = $nlevelmax; }
      }

#      if ($is_profile == 1) {
#          $out[$nvar++] = "$name$tail   $nlev,$kpds6      $kpds5 ** (profile) $comment";
#      }
#      elsif ($is_profile == 2) {
      if ($is_profile == 1 || $is_profile == 2) {
          if ($kpds6 == 100) { $_ =~ s/00 / /g; $_ =~ s/00$//;}
          if (length($_) > 30) { $_ =~ s/( \d* \d* \d* \d*).* (\d* \d* \d* \d*) /$1.. $2 /; }
          $kpds6 =~ s/,a/ a/;
          $out[$nvar++] = "$name$tail   $nlev,$kpds6  $kpds5 ** ($_) $comment";
      }
      else {
         foreach $ll (split(' ',$kpds7s)) {
            $lev_txt = $lev_def{"$kpds6,$ll"};
            $lev_0 = $lev0{"$kpds6,$ll"};
            $ll =~ s/,a/ a/;
            $out[$nvar++] = "${name}$lev_0  0,$kpds6,$ll   $kpds5 ** $lev_txt $comment";
         }
      }
   }
}

# simplify the names
for ($i = 0; $i < $nvar; $i++) {
   $_ = $out[$i];
   chomp;
   $_ =~ s/ .*//;
   $_ =~ s/\/\/.*\/\///;
   $count{$_} += 1;
}

print "vars $nvar\n";
for ($i = 0; $i < $nvar; $i++) {
#   print "*old: $out[$i]";

   $_ = $out[$i];
   chomp;
   $_ =~ s/ .*$//;
   $_ =~ s/\/\/.*\/\///;

   $j = $count{$_};
   if ($j == 1) {
      $rest = $out[$i];
      $rest =~ s/^[^ ]*//;
      print "$_ $rest";
   } else {
      $_ = $out[$i];
      $_ =~ s/\/\/none\/\///g;
      $_ =~ s/\/\///g;
      print "$_";
   }
}
print "ENDVARS\n";

# ensemble code
# case 1: %e
# case 2: no %e

# print "ens_match=$ens_match\n";

if (defined($ens_match)) {
   $num_ens=$#ens_names + 1;
   print "EDEF $num_ens\n";
   foreach (@ens_names) {
      $file = "$gfile_e";
      $file =~ s/%e/$_/;
      $tdir=$dir_e;
      $tdir =~ s/%e/$_/;
      if ($tdir eq "") {
         opendir(DIR,'.');
      }
      else {
         opendir(DIR,$tdir);
      }
      @allfiles = sort grep /^$file$/, readdir DIR;
      closedir DIR;
      if ($#allfiles <= -1 ) {
         print STDERR "\nError: could not find any files in directory: $file\n";
         exit 8;
      }
      $result = `$wgrib2 -d 1 -VT -v2 -ctl_ens "$dir$allfiles[0]"`;
      @results = split(':',$result);
      $results[2] =~ s/_/:/;
      $results[3] =~ s/ens=//;
      print "$_ $tdef_n $results[2] $results[3]";
   }
   print "ENDEDEF\n";
}
else {
   $num_ens=keys(%ens_first);
   if ($num_ens > 0) {
      print "EDEF $num_ens\n";
      $i=1;
      foreach $fname (keys(%ens_first)) {
         $tmp = gdate($ens_first{$fname});
         $gname = $fname;
         $gname =~ s/0,/c/;
         $gname =~ s/1,/l/;
         $gname =~ s/2,/n/;
         $gname =~ s/3,/p/;
         $gname =~ s/4,/m/;
         $gname =~ s/,/_/;
         if ($old_ens_name eq 0) {
             print "E$gname $tdef_n $tmp $fname\n";
         }
         else {
             print "e$i $tdef_n $tmp $fname\n";
             $i++;
         }
      }
      print "ENDEDEF\n";
   }
}

if ($thinned_grid_msg == 1) {
   print STDERR "\n\n";
   print STDERR "****   Thinned grid used.  Requires grads 2.0a3 or later ***\n";
   print STDERR "\n\n";
}
exit 0;


#------------------ jday --------------------
# jday(year,mo,day) return the julian day relative to jan 0
# mo=1..12
#
sub jday {

   local($n);
   local($nleap);
   local($year1);
   $n=substr(" 000 031 059 090 120 151 181 212 243 273 304 334",($_[1]-1)*4,4);
   $n = $n + $_[2];
   $year1 = $_[0] - 1905;

   if ($calendar eq '365') {
       $n += $year1 * 365;
   }
   else {
      if ($_[1] > 2 && $_[0] % 4 == 0) {
         if ($_[0] % 400 == 0 || $_[0] % 100 != 0) {
            $n++;
         }
      }
      $nleap = int($year1 / 4);
      $n = $n + $nleap + $year1 * 365;
   }
   $n;
}

#------------------ write tdef statement ------------------
# still not great but better than before

sub tdef {

   local($tmp);
   local($n);
   $n=$ntime;

   if ($timestep) { $dt=$timestep }
   else { 
      if ($ntime == 1) {
         if ($timestep) { $dt=$timestep }
         else { $dt="1mo"; }
      }
      elsif ( ($minute != $minute1) || ($hour != $hour1)) {
         $tmp= (&jday($year1,$mo1,$day1) - &jday($year,$mo,$day)) * 24 * 60 + ($hour1 - $hour)*60 
		+ $minute1 - $minute;
	  if (($tmp % 60) == 0) { 
	      $tmp = $tmp / 60;
	      $dt="${tmp}hr";
              $n = (&jday($year_last,$mo_last,$day_last) - &jday($year,$mo,$day)) * 24 + $hour_last - $hour;
              $n = int($n / $tmp) + 1;
	  }
	  else {
	      $dt="${tmp}mn";
              $n= (&jday($year_last,$mo_last,$day_last) - &jday($year,$mo,$day)) * 24 * 60 
			+ ($hour_last - $hour)*60 + ($minute_last - $minute);
              $n = int($n / $tmp) + 1;
	  }
      } 
      elsif ($day != $day1) {
         $tmp = &jday($year1,$mo1,$day1) - &jday($year,$mo,$day);
         $dt="${tmp}dy";
         $n=int((&jday($year_last,$mo_last,$day_last) - &jday($year,$mo,$day))/$tmp)+1;
      }
      elsif ($mo != $mo1) {
         # assume that dt < 12 months
         $tmp = $year1*12+$mo1 - $year*12-$mo;
         $dt="${tmp}mo";
         $n = int(($year_last*12+$mo_last - $year*12 - $mo) / $tmp) + 1;
      }
      else {
         $tmp = $year1 - $year;
         $dt="${tmp}yr";
         $n = int(($year_last - $year) / $tmp) + 1;
      }
   }
   if ($calendar eq "365") {
       print "options 365_day_calendar\n";
   }
   if ($minute == 0) { print "tdef $n linear ${hour}Z$day$month$year $dt\n"; }
   else { print "tdef $n linear ${hour}:${minute}Z$day$month$year $dt\n"; }
   $tdef_n = $n;
}

#----------------- find size of domain for regular grids

sub domain {
      $_ = `$wgrib2 -d 1 "$file" -domain`;
      /:N=(\S*) S=(\S*) W=(\S*) E=(\S*)/;
      $north=$1;
      $south=$2;
      $west=$3;
      $east=$4;
}

#----------------- print Gaussian latitudes from lat1..lat2
sub print_range_gau_lats {
   local($i);
   local($eps);
   local($m);
   local($z);
   local($i);
   local($j);
   local($p1);
   local($p2);
   local($p3);
   local($z1);
   local($x);

   $eps = 3e-14;
   $m=int(($nGauLats+1)/2);
   $i=1;
   while ($i <= $m) {
      $z=cos(3.141592654*($i-0.25)/($nGauLats+0.5));
      do {
         $p1 = 1;
         $p2 = 0;
         $j = 1;
         while ($j <= $nGauLats) {
            $p3 = $p2;
            $p2 = $p1;
            $p1=((2*$j-1)*$z*$p2-($j-1)*$p3)/$j;
            $j++;
         }
         $pp = $nGauLats*($z*$p1-$p2)/($z*$z-1);

         $z1 = $z;
         $z = $z1 - $p1/$pp;
      } until abs($z-$z1) < $eps;
      $x[$i] = -atan2($z,sqrt(1-$z*$z))*180/3.141592654;
      $x[$nGauLats+1-$i] = -$x[$i];
      $i++;
   }
   $i = $j = 1;
   while ($i <= $nGauLats) {
      if ($x[$i] > $lat0 - 0.002 && $x[$i] < $lat1 + 0.002) {
         printf  " %7.3f", $x[$i];
         if (($j % 14) == 0) { print "\n"; }
         $j++;
      }
      $i++;
   }
   if ((($j-1) % 14) != 0) { print "\n"; }
}



#----------------- print Gaussian latitudes
sub print_gau_lats {
   local($i);
   local($eps);
   local($m);
   local($z);
   local($i);
   local($j);
   local($p1);
   local($p2);
   local($p3);
   local($z1);
   local($x);

   $eps = 3e-14;
   $m=int(($ny+1)/2);
   $i=1;
   while ($i <= $m) {
      $z=cos(3.141592654*($i-0.25)/($ny+0.5));
      do {
         $p1 = 1;
         $p2 = 0;
         $j = 1;
         while ($j <= $ny) {
            $p3 = $p2;
            $p2 = $p1;
            $p1=((2*$j-1)*$z*$p2-($j-1)*$p3)/$j;
            $j++;
         }
         $pp = $ny*($z*$p1-$p2)/($z*$z-1);

         $z1 = $z;
         $z = $z1 - $p1/$pp;
      } until abs($z-$z1) < $eps;
      $x[$i] = -atan2($z,sqrt(1-$z*$z))*180/3.141592654;
      $x[$ny+1-$i] = -$x[$i];
      $i++;
   }
   $i = 1;
   while ($i < $ny) {
      printf  " %7.3f", $x[$i];
      if (($i % 14) == 0) { print "\n"; }
      $i++;
   }
   printf " %7.3f\n", $x[$ny];
}


sub gau_pdf_near {
#
# writes pdef file for thinned gaussian grid
#
# nx, ny = dimension of output grid
#
   print "pdef $npnts 1 general 1 stream binary $caret3$pdef\n";
   if ($scan eq 'WE:NS') {
      $offset = $npnts;
      for ($j = $ny; $j >= 1; $j--) {
         $offset -= $nlon[$j];
         for ($i = 0; $i < $nx; $i++) {
            $x = $i / ($nx - $t) * ($nlon[$j] - $t);
            $x = floor($x + 0.5);
            if ($x >= $nlon[$j]) { $x -= $nlon[$j] };
            $x += $offset + $pdef_offset;
            print PDEF pack("L", $x);
         }
      }
   }
   else {
      $offset = 0;
      for ($j = 1; $j <= $ny; $j++) {
         for ($i = 0; $i < $nx; $i++) {
            $x = $i / ($nx - $t) * ($nlon[$j] - $t);
            $x = floor($x + 0.5);
            if ($x >= $nlon[$j]) { $x -= $nlon[$j]; }
            $x += $offset + $pdef_offset;
            print PDEF pack("L", $x);
         }
         $offset += $nlon[$j];
      }
   }
#  print weights
   $x = pack("f", 1.0);
   for ($i = 0; $i < $nx*$ny; $i++) {
      print PDEF $x;
   }

#  print wind rotation
   $x = pack("L", -999);
   for ($i = 0; $i < $nx*$ny; $i++) {
       print PDEF $x;
   }
}

sub gau_pdf_linear {
#
# not working
#  linear interpolation along latitude

   my @n1 = ();  # left locations
   my @n2 = ();  # right locations
   my @w1 = ();  # left weights
   my @w2 = ();  # right weights

   print "pdef $npnts 1 general 2 stream binary $caret3$pdef\n";

           $offset = 0;
           for ($j = 1; $j <= $ny; $j++) {
              for ($i = 0; $i < $nx; $i++) {
                 $x = $i / ($nx - $t) * ($nlon[$j] - $t);

#                grid points and weights
		 $nl = floor($x);
		 $wl = 1 - ($x - $nl);
		 $nr = $nl + 1;
		 $wr = $x - $nl;

                 if ($nl >= $nlon[$j]) { $nl -= $nlon[$j]; }
                 if ($nr >= $nlon[$j]) { $nr -= $nlon[$j]; }

                 $nr += $offset + $pdef_offset;
                 $nl += $offset + $pdef_offset;

		 push @n1, $nl;
		 push @w1, $wl;
		 push @n2, $nr;
		 push @w2, $wr;

              }
              $offset += $nlon[$j];
           }

# write locations and weights to PDEF file

   $rem =($nx*$ny) % 4;

   for ($i = 0; $i < $rem; $i++) {
       print PDEF pack("L", $n1[$i]);
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
       print PDEF pack("LLLL", $n1[$i], $n1[$i+1], $n1[$i+2], $n1[$i+3]);
   }

   for ($i = 0; $i < $rem; $i++) {
       print PDEF pack("f", $w1[$i]);
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
       print PDEF pack("ffff", $w1[$i], $w1[$i+1], $w1[$i+2], $w1[$i+3]);
   }


   for ($i = 0; $i < $rem; $i++) {
       print PDEF pack("L", $n2[$i]);
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
       print PDEF pack("LLLL", $n2[$i], $n2[$i+1], $n2[$i+2], $n2[$i+3]);
   }

   for ($i = 0; $i < $rem; $i++) {
       print PDEF pack("f", $w2[$i]);
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
       print PDEF pack("ffff", $w2[$i], $w2[$i+1], $w2[$i+2], $w2[$i+3]);
   }

#  wind rotation

   $x = pack("L", -999);
   for ($i = 0; $i < $rem; $i++) {
      print PDEF $x;
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
      print PDEF $x,$x,$x,$x;
   }
}

sub rr2ll{
  my $polelat=shift;
  my $polelon=shift;
  my $rotation=shift;
  my $rlat=shift;
  my $rlon=shift;
  my $a = deg2rad(90.0+$polelat);
  my $b = deg2rad($polelon);
  my $r = deg2rad($rotation);
  my $pr = deg2rad($rlat);
  my $gr = -deg2rad($rlon);
  my $pm = asin(cos($pr)*cos($gr));
  my $gm = atan2(cos($pr)*sin($gr),-sin($pr));
  my $lat = rad2deg(asin(sin($a)*sin($pm)-cos($a)*cos($pm)*cos($gm-$r)));
  my $lon = -rad2deg((-$b+atan2(cos($pm)*sin($gm-$r),
                         sin($a)*cos($pm)*cos($gm-$r)+cos($a)*sin($pm))));
  return ($lat, $lon);
}

#
# reads inventory
# picks out variables and levels
#
sub scan_ListA {
   my $ens;
   open (FileDate, "<$ListA");
   while (defined($_ = <FileDate>)) {
      ($a,$b,$d,$lev_txt,$lev_0,$ens,$ftime,$ctl_info) = split(/:/,$_);
      $d =~ s/.*=//;
      $ens =~ s/.*=//;
      $ftime =~ s/,missing=[0-9]*//;
# print "ens=($ens) ($ctl_info) $d \n";
      if ($ens ne '') {
         if (!defined($ens_first{$ens})) {
            $ens_first{$ens} = $d;
            $ens_last{$ens} = $d;
         }
         if ($ens_first{$ens} > $d) { $ens_first{$ens} = $d; }
         if ($ens_last{$ens} < $d) { $ens_last{$ens} = $d; }
      }

      $dates{$d}="";

      ($varname,$a,$kpds5,$sp,$comment) = split(' ',$ctl_info,5);
#     print "      ($varname--$a--$kpds5--$sp--$ftime -- $comment)\n";
      ($kpds6,$kpds7) = split(/,/,$a,2);
#     for sfc fields - wne
      if (! defined($kpds7)) { $kpds7 = ','; }
      $varname = "$varname//$sp//:$kpds6";
      if ($time_info eq 0) {
         if (! defined $fcomments{$varname}) { $fcomments{$varname} = "$kpds5:$comment"; }
      }
      else {
         if (! defined $fcomments{$varname}) { $fcomments{$varname} = "$kpds5:($ftime) $comment"; }
      }
      if ($kpds7 =~ /,/) {
         if (defined $flayers{$varname}) {
            if (!($flayers{$varname} =~ / $kpds7 /)) {
               $flayers{$varname} .= "$kpds7 ";
            }
         }
         else {
            $flayers{$varname} = " $kpds7 ";
         }
      }
      else {
         if (defined $flevels{$varname}) {
            if (!($flevels{$varname} =~ / $kpds7 /)) {
               $flevels{$varname} .= "$kpds7 ";
            }
         }
         else {
            $flevels{$varname} = " $kpds7 ";
         }
      }
      if (! defined $lev_def{"$kpds6,$kpds7"}) { $lev_def{"$kpds6,$kpds7"} = $lev_txt; }
      if (! defined $lev0{"$kpds6,$kpds7"}) { $lev0{"$kpds6,$kpds7"} = $lev_0; }
   }
   close (FileDate);

}
sub gdate{
   my ($year, $mo, $day, $hour, $month, $minute);
   $_ = $_[0];
   $year = substr($_,0,4);
   $mo = substr($_,4,2);
   $day = substr($_,6,2);
   $hour = substr($_,8,2);
   $minute = substr($_,10,2);

   if ($mo < 0 || $mo > 12) {
   print "illegal date code $time\n";
   exit 8;
   }

   $month=substr("janfebmaraprmayjunjulaugsepoctnovdec",$mo*3-3,3);
   if ($minute != 0) { return " ${hour}:${minute}Z$day$month$year"; }
   return " ${hour}Z$day$month$year";
}

sub pdef_near {

#  now to create the pdef file
#  regional means grid points are on left and right boundaries
#
# $regional_thinned_grid = 1
# 
   print "pdef $npnts 1 file 1 stream binary $caret3$pdef\n";

   $t=0;
   if ($regional_thinned_grid == 1) { $t = 1; }

   if ($scan eq 'WE:SN') {
      $offset = 0;
      for ($j = 1; $j <= $ny; $j++) {
         for ($i = 0; $i < $nx; $i++) {
            $x = $i / ($nx - $t) * ($nlon[$j] - $t);
            $x = floor($x + 0.5);
            if ($x >= $nlon[$j]) { $x -= $nlon[$j]; }
            $x += $offset + $pdef_offset;
            print PDEF pack("L", $x);
         }
         $offset += $nlon[$j];
      }
   }
   else {
      $offset = $npnts;
      for ($j = $ny; $j >= 1; $j--) {
         $offset -= $nlon[$j];
         for ($i = 0; $i < $nx; $i++) {
            $x = $i / ($nx - $t) * ($nlon[$j] - $t);
            $x = floor($x + 0.5);
            if ($x >= $nlon[$j]) { $x -= $nlon[$j] };
            $x += $offset + $pdef_offset;
            print PDEF pack("L", $x);
         }
      }
   }
#  print weights
   $x = pack("f", 1.0);
   for ($i = 0; $i < $nx*$ny; $i++) {
      print PDEF $x;
   }
#  print wind rotation
   $x = pack("L", -999);
   for ($i = 0; $i < $nx*$ny; $i++) {
      print PDEF $x;
   }
}


sub pdef_linear {

#  linear interpolation along latitude

   my @n1 = ();  # left locations
   my @n2 = ();  # right locations
   my @w1 = ();  # left weights
   my @w2 = ();  # right weights

   print "pdef $npnts 1 file 2 stream binary $caret3$pdef\n";

   $t=0;
   if ($regional_thinned_grid == 1) { $t = 1; }

   if ($scan eq 'WE:SN') {
      $offset = 0;
      for ($j = 1; $j <= $ny; $j++) {
         for ($i = 0; $i < $nx; $i++) {
            $x = $i / ($nx - $t) * ($nlon[$j] - $t);

#           grid points and weights
            $nl = floor($x);
	    $wl = 1 - ($x - $nl);
	    $nr = $nl + 1;
	    $wr = $x - $nl;

            if ($nl >= $nlon[$j]) { $nl -= $nlon[$j]; }
            if ($nr >= $nlon[$j]) { $nr -= $nlon[$j]; }

            $nr += $offset + $pdef_offset;
            $nl += $offset + $pdef_offset;

	    push @n1, $nl;
	    push @w1, $wl;
	    push @n2, $nr;
	    push @w2, $wr;

         }
         $offset += $nlon[$j];
     }
   }
   else {
      $offset = $npnts;
      for ($j = $ny; $j >= 1; $j--) {
          $offset -= $nlon[$j];
          for ($i = 0; $i < $nx; $i++) {
             $x = $i / ($nx - $t) * ($nlon[$j] - $t);

#            grid points and weights
	     $nl = floor($x);
	     $wl = 1.0 - ($x - $nl);
	     $nr = $nl + 1;
	     $wr = 1.0 - $wl;

             if ($nl >= $nlon[$j]) { $nl -= $nlon[$j]; }
             if ($nr >= $nlon[$j]) { $nr -= $nlon[$j]; }

             $nr+= $offset + $pdef_offset;
             $nl+= $offset + $pdef_offset;

	     push @n1, $nl;
	     push @w1, $wl;
	     push @n2, $nr;
             push @w2, $wr;
         }
      }
   }

# write locations and weights to PDEF file

   $rem =($nx*$ny) % 4;

   for ($i = 0; $i < $rem; $i++) {
       print PDEF pack("L", $n1[$i]);
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
       print PDEF pack("LLLL", $n1[$i], $n1[$i+1], $n1[$i+2], $n1[$i+3]);
   }

   for ($i = 0; $i < $rem; $i++) {
       print PDEF pack("f", $w1[$i]);
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
       print PDEF pack("ffff", $w1[$i], $w1[$i+1], $w1[$i+2], $w1[$i+3]);
   }


   for ($i = 0; $i < $rem; $i++) {
       print PDEF pack("L", $n2[$i]);
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
       print PDEF pack("LLLL", $n2[$i], $n2[$i+1], $n2[$i+2], $n2[$i+3]);
   }

   for ($i = 0; $i < $rem; $i++) {
       print PDEF pack("f", $w2[$i]);
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
       print PDEF pack("ffff", $w2[$i], $w2[$i+1], $w2[$i+2], $w2[$i+3]);
   }

#  wind rotation

   $x = pack("L", -999);
   for ($i = 0; $i < $rem; $i++) {
      print PDEF $x;
   }
   for ($i = $rem; $i < $nx*$ny; $i += 4) {
      print PDEF $x,$x,$x,$x;
   }
}

