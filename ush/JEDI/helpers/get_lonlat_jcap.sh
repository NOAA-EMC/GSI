# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "1534" ]]; then
   export LONA=3072
   export LATA=1536
   export DELTIM=120
elif [[ "$JCAP" = "1148" ]]; then
   export LONA=2304
   export LATA=1152
   export DELTIM=120
elif [[ "$JCAP" = "878" ]]; then
   export LONA=1760
   export LATA=880
   export DELTIM=120
elif [[ "$JCAP" = "766" ]]; then
   export LONA=1536
   export LATA=768
   export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}
elif [[ "$JCAP" = "574" ]]; then
   export LONA=1152
   export LATA=576
   export DELTIM=450
elif [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
elif [[ "$JCAP" = "254" ]]; then
   export LONA=512
   export LATA=256
   export DELTIM=450
elif [[ "$JCAP" = "126" ]]; then
   export LONA=384
   export LATA=190
   export DELTIM=600
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLON=$LONA
export NLAT=$((${LATA}+2))
