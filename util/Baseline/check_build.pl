#!/usr/bin/perl

  $baselineDir = "$ENV{'NS'}/GSI-Baseline";
  chdir ($baselineDir);
  $hashcmd = 'git log --pretty=oneline | head -1 | awk \'{print $1}\'';
  $lasthash = `$hashcmd`;
  chop($lasthash);
  $updatecmd = "git remote update; git pull origin cmake-refactor";
  system("$updatecmd");
  $newhash = `$hashcmd`;
  chop($newhash);

  if($newhash ne $lasthash) { 
    # rebuild new master    
    $buildcmd = "./ush/build_all_cmake.sh 0 $baselineDir";
    system($buildcmd); 
    chdir ("$baselineDir/build");
#   system("module load lsf; REND=2 ctest -V -I 1,1");
  }


