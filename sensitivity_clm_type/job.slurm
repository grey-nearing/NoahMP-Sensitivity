#!/bin/bash -x

#SBATCH --job-name=clm1
#SBATCH --output=slurm.report
#SBATCH --time=12:00:00
#SBATCH --ntasks=56
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
cd /discover/nobackup/gnearing/projects/NoahMP-Sensitivity/sensitivity_clm_type

# pods
/usr/local/other/PoDS/PoDS/pods.py -e " $envVar " -x /discover/nobackup/gnearing/projects/NoahMP-Sensitivity/sensitivity_clm_type/execfile.pods -n 16

# done
exit 0




