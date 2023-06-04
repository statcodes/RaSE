#!/bin/sh

#SBATCH --job-name=ex
#SBATCH --time=18:00:00
#SBATCH --tasks-per-node=1
#SBATCH --mem=64g
#SBATCH --cpus-per-task=16
#SBATCH --array=1100-1200

export OMP_NUM_THREADS=1
module load r/gcc/4.1.2
R CMD BATCH --no-save --no-restore realdatafifa_2.R script_$SLURM_ARRAY_TASK_ID
