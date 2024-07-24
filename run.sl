#!/bin/bash -e
#SBATCH --job-name=punishStrat    # job name (shows up in queue)
#SBATCH --time=00-00:30:00        # Walltime (DD-HH:MM:SS)
#SBATCH --mem=150G                # total memory
#SBATCH --cpus-per-task=20        # 4 CPUs
#SBATCH --account=uoa03415        # Project code

# load R
module load R

# load pandoc
module load pandoc/2.8.0.1

# run script
Rscript run.R
