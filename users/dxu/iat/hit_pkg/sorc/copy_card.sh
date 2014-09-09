#!/bin/sh
set -x

#for year in 2000 2001 2002 2003 2004 ; do
for year in 2014  ; do

sed "s?2010?$year?g" card2010_al.t >xt
mv xt card${year}_al.t

sed "s?2010?$year?g" card2010_al.i >xt
mv xt card${year}_al.i

sed "s?2010?$year?g" card2010_ep.t >xt
mv xt card${year}_ep.t

sed "s?2010?$year?g" card2010_ep.i >xt
mv xt card${year}_ep.i

sed "s?2010?$year?g" card2010_wp.t >xt
mv xt card${year}_wp.t

sed "s?2010?$year?g" card2010_wp.i >xt
mv xt card${year}_wp.i



done
