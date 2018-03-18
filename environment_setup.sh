#!/bin/bash

# modules
module purge
module load comp/intel-16.0.3.210

# linking
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/discover/nobackup/projects/lis/libs/netcdf/4.3.3.1_intel-14.0.3.174_sp3/lib
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/discover/nobackup/projects/lis/libs/grib_api/1.15.0_intel-14.0.3.174_sp3/lib


