#!/bin/bash -x

#SBATCH --job-name=conv
#SBATCH --output=conv.report
#SBATCH --time=12:00:00
#SBATCH --ntasks=36
#SBATCH --account=s1688
#SBATCH --constraint=hasw

# memory
#ulimit -s unlimited

# modules
#source /usr/share/modules/init/bash
#module purge
#module load comp/intel-16.0.3.210
#module load tool/matlab-R2015b

# linking
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/discover/nobackup/projects/lis/libs/netcdf/4.3.3.1_intel-14.0.3.174_sp3/lib
envVar="{'LD_LIBRARY_PATH': '$LD_LIBRARY_PATH'}"

# location
cd /discover/nobackup/gnearing/NoahMP_Sensitivity/sensitivity_noah_type 

# pods
/usr/local/other/PoDS/PoDS/pods.py -e " $envVar " -x /discover/nobackup/gnearing/NoahMP_Sensitivity/sensitivity_noah_type/execfile_conv.pods -n 18

# done
exit 0




