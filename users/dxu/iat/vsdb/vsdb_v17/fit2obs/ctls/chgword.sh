#!/bin/sh
set -ax

#old="options big_endian sequential template"
#new="options little_endian sequential template"
old="little_endian"
new="ENDIAN"

	filelist=`eval grep -l -s "$old" *.ctl `

	for files in $filelist
	do
	  name=`eval echo $files`
	  sed "s?$old?$new?g" $name >name.new  || exit 8
	  #diff $name name.new      
	  mv name.new $name
	done

exit
