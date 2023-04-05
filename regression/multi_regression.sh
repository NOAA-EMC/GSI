#!/bin/sh --login

regtests_all="global_3dvar
              global_4dvar
              global_4denvar
              netcdf_fv3_regional
              rrfs_3denvar_glbens
              hwrf_nmm_d2
              hwrf_nmm_d3
              rtma
              global_enkf"

regtests_debug="global_3dvar
                global_4dvar
                global_4denvar
                netcdf_fv3_regional
                rrfs_3denvar_glbens
                hwrf_nmm_d2
                hwrf_nmm_d3
                rtma
                global_enkf"

# Choose which regression test to run; by default, run all
regtests=${1:-$regtests_all}

for regtest in $regtests; do
    rm -f ${regtest}.out
    echo "Launching regression test: $regtest"
    /bin/sh regression_driver.sh $regtest >& ${regtest}.out &
    sleep 1
done

exit
