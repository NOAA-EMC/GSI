#!/bin/sh --login

regtests_all="global_3dvar
              global_4dvar
              global_4denvar
              arw_netcdf
              arw_binary
              nmm_binary
              nmm_netcdf
              nmmb_nems_4denvar
              netcdf_fv3_regional
              hwrf_nmm_d2
              hwrf_nmm_d3
              rtma
              global_enkf"

regtests_debug="global_3dvar
                global_4dvar
                global_4denvar
                arw_netcdf
                arw_binary
                nmm_binary
                nmm_netcdf
                nmmb_nems_4denvar
                netcdf_fv3_regional
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
