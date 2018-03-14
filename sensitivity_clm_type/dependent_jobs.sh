#!/bin/bash -x 
JOB1=$(sbatch job.slurm | cut -f 4 -d' ')
echo $JOB1 
JOB2=$(sbatch -d afterany:$JOB1 job.slurm | cut -f 4 -d' ')
echo $JOB2 
JOB3=$(sbatch -d afterany:$JOB2 job.slurm | cut -f 4 -d' ')
echo $JOB3 
JOB4=$(sbatch -d afterany:$JOB3 job.slurm | cut -f 4 -d' ')
echo $JOB4 
JOB5=$(sbatch -d afterany:$JOB4 job.slurm | cut -f 4 -d' ')
echo $JOB5 
JOB6=$(sbatch -d afterany:$JOB5 job.slurm | cut -f 4 -d' ')
echo $JOB6 
JOB7=$(sbatch -d afterany:$JOB6 job.slurm | cut -f 4 -d' ')
echo $JOB7 
JOB8=$(sbatch -d afterany:$JOB7 job.slurm | cut -f 4 -d' ')
echo $JOB8 
JOB9=$(sbatch -d afterany:$JOB8 job.slurm | cut -f 4 -d' ')
echo $JOB9 
JOB10=$(sbatch -d afterany:$JOB9 job.slurm | cut -f 4 -d' ')
echo $JOB10 
JOB11=$(sbatch -d afterany:$JOB10 job.slurm | cut -f 4 -d' ')
echo $JOB11 
JOB12=$(sbatch -d afterany:$JOB11 job.slurm | cut -f 4 -d' ')
echo $JOB12 
JOB13=$(sbatch -d afterany:$JOB12 job.slurm | cut -f 4 -d' ')
echo $JOB13 
JOB14=$(sbatch -d afterany:$JOB13 job.slurm | cut -f 4 -d' ')
echo $JOB14 
JOB15=$(sbatch -d afterany:$JOB14 job.slurm | cut -f 4 -d' ')
echo $JOB15 

