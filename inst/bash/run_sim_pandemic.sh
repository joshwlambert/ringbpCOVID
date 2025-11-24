#!/bin/bash
#SBATCH --job-name=ringbpCOVID
#SBATCH --ntasks=16
#SBATCH --nodes=1
#SBATCH --mem=10GB
#SBATCH --time=05:00:00
#SBATCH --output=%x_%j.log
pwd; hostname; date

echo "Running COVID-19 {ringbp} analysis script..."

module load R/4.4.0

echo "Number of cores available: "

Rscript -e "future::availableCores()"

Rscript inst/scripts/run_sim_pandemic.R

date
