#!/bin/ksh

###################################################################
echo "------------------------------------------------"
echo "setup.sh - Utility to set PDY variables"
echo "           This utility will output a file PDY in"
echo "           the current working directory which can be" 
echo "           sourced in the parent script to set"
echo "           PDY PDYm1-9"
echo "------------------------------------------------"
echo "History: Jul 27, 1998 - Implement new script."                  
echo "------------------------------------------------"
###################################################################

# Check to be sure that cycle var is set
if test -z "$cycle"
then
   echo "cycle var not set in J-Job"
   exit
fi

# Set the PDY var if not already set
if test -z "$PDY"
then
   cp /com/date/$cycle ncepdate
   cp /com/date/$cycle NMCDATE
   PDY=`cut -c7-14 /com/date/$cycle`
else
   PDYbad=`cut -c7-14 /com/date/$cycle`
   sed s/$PDYbad/$PDY/g /com/date/$cycle > ncepdate
   cp ncepdate NMCDATE
fi

# Add prefix P to the date
PDATE=P$PDY
echo pdate="$PDATE"

# Use grep to search through date table to find production date-line
# date-line = PDATE & day minus 1, day minus 2, ..to.. day minus 7
export NWPROD=${NWPROD:-/nwprod}
DATElne_minus=`sh $NWPROD/util/ush/finddate.sh $PDY s-10`
DATElne_plus=`sh $NWPROD/util/ush/finddate.sh $PDY s+10`

# List date-line
echo DATElne_minus="$DATElne_minus"
echo DATElne_plus="$DATElne_plus"

# Cut date-line to form PDY variables
PDYm7=`echo "$DATElne_minus"|cut -c55-62`
PDYm6=`echo "$DATElne_minus"|cut -c46-53`
PDYm5=`echo "$DATElne_minus"|cut -c37-44`
PDYm4=`echo "$DATElne_minus"|cut -c28-35`
PDYm3=`echo "$DATElne_minus"|cut -c19-26`
PDYm2=`echo "$DATElne_minus"|cut -c10-17`
PDYm1=`echo "$DATElne_minus"|cut -c1-8`

PDYp1=`echo "$DATElne_plus"|cut -c1-8`
PDYp2=`echo "$DATElne_plus"|cut -c10-17`
PDYp3=`echo "$DATElne_plus"|cut -c19-26`
PDYp4=`echo "$DATElne_plus"|cut -c28-35`
PDYp5=`echo "$DATElne_plus"|cut -c37-44`
PDYp6=`echo "$DATElne_plus"|cut -c46-53`
PDYp7=`echo "$DATElne_plus"|cut -c55-62`

# LIST PDYs
echo "export PDYm7=$PDYm7" > PDY
echo "export PDYm6=$PDYm6" >> PDY
echo "export PDYm5=$PDYm5" >> PDY
echo "export PDYm4=$PDYm4" >> PDY
echo "export PDYm3=$PDYm3" >> PDY
echo "export PDYm2=$PDYm2" >> PDY
echo "export PDYm1=$PDYm1" >> PDY
echo "export PDY=$PDY" >> PDY
echo "export PDYp1=$PDYp1" >> PDY
echo "export PDYp2=$PDYp2" >> PDY
echo "export PDYp3=$PDYp3" >> PDY
echo "export PDYp4=$PDYp4" >> PDY
echo "export PDYp5=$PDYp5" >> PDY
echo "export PDYp6=$PDYp6" >> PDY
echo "export PDYp7=$PDYp7" >> PDY

chmod 775 PDY
