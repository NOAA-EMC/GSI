#!/bin/sh --login

regtests_all="global_T62
              global_T62_ozonly
              global_4dvar_T62
              global_4denvar_T126
              global_fv3_4denvar_T126
              global_fv3_4denvar_C192
              global_lanczos_T62
              arw_netcdf
              arw_binary
              nmm_binary
              nmm_netcdf
              nmmb_nems_4denvar
              netcdf_fv3_regional
              hwrf_nmm_d2
              hwrf_nmm_d3
              rtma
              global_enkf_T62
              global_C96_fv3aero
              global_C96_fv3aerorad"

regtests_debug="global_T62
                global_4dvar_T62
                global_4denvar_T126
                global_fv3_4denvar_T126
                global_fv3_4denvar_C192
                global_lanczos_T62
                arw_netcdf
                arw_binary
                nmm_binary
                nmm_netcdf
                nmmb_nems_4denvar
                netcdf_fv3_regional
                hwrf_nmm_d2
                hwrf_nmm_d3
                rtma
                global_enkf_T62
                global_C96_fv3aero
                global_C96_fv3aerorad"

# Choose which regression test to run; by default, run all
regtests=${1:-$regtests_all}

for regtest in $regtests; do
    rm -f ${regtest}.out
    echo "Launching regression test: $regtest"
    /bin/sh regression_driver.sh $regtest >& ${regtest}.out &
    sleep 1
done

exit
