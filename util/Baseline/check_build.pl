#!/usr/bin/perl

  $baselineDir = "$ENV{'NS'}/GSI-Baseline";
  chdir ($baselineDir);
  $hashcmd = 'git log --pretty=oneline | head -1 | awk \'{print $1}\'';
  $lasthash = `$hashcmd`;
  chop($lasthash);
  $updatecmd = "git remote update; git pull origin tmpmerg";
  system("$updatecmd");
  $newhash = `$hashcmd`;
  chop($newhash);

# if($newhash ne $lasthash) { 
  if($newhash eq $lasthash) { 
    # rebuild new master    
    # this is just for mars until it gets added to the master
#   $sedcmd = 'sed -i \'s/v\\[0-9/m\\[0-9/g\' CMakeLists.txt';
#   system($sedcmd);
    $buildcmd = "./ush/build_all_cmake.sh $baselineDir 1";
    system($buildcmd); 
    chdir ("$baselineDir/build");
    #get the platform name from regression_var.out
    @words = split(/\s+/,`cat regression_var.out`);
    $platform_name = @words[1];
    if($platform_name eq "WCOSS") {
      $platform="wcoss";
    } elsif($platform_name eq "WCOSS_C") {
      $platform="cray";
    } elsif($platform_name eq "THEIA") {
      $platform="theia";
    } elsif($platform_name eq "WCOSS_D") {
      $platform="wcoss_d";
    }
    system("module use $baselineDir/modulefiles; module load modulefile.ProdGSI.$platform; REND=2 ctest -j 15");
#   system("module load lsf; REND=2 ctest -V -I 1,1");
  }


