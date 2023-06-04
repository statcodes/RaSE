#!/bin/sh

#SBATCH --job-name=ex
#SBATCH --time=18:00:00
#SBATCH --tasks-per-node=1
#SBATCH --mem=64g
#SBATCH --cpus-per-task=12
#SBATCH --array=1201-1400

export OMP_NUM_THREADS=1
module load r/gcc/4.1.2
R CMD BATCH --no-save --no-restore realdatafifa_3.R script_$SLURM_ARRAY_TASK_ID
