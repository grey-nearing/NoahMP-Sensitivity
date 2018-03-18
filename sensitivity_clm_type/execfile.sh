#!/bin/bash -x

# modules
ulimit -s unlimited
source /usr/share/modules/init/bash
module purge
module load comp/intel-16.0.3.210
module load tool/matlab-R2015b

cd ./runtime_dirs/run_$1_$2
pwd

matlab -nodesktop -nodisplay -nosplash -nojvm -r "Sobol_Wrapper" > "report.matlab" #&

#sh run_matlab.sh Sobol_Wrapper

