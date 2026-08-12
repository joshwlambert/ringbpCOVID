#!/bin/bash
#SBATCH --job-name=install_ringbpCOVID
#SBATCH --ntasks=1
#SBATCH --mem=5GB
#SBATCH --time=01:00:00
#SBATCH --output=install_ringbpCOVID.log
pwd; hostname; date

ml R/4.4.0
# dependencies = TRUE to installs suggested dependencies
# (for pandemic ringbp (v0.1.0)),
# upgrade = "never" to stops remotes upgrading packages on every run
Rscript -e "remotes::install_github('joshwlambert/ringbpCOVID', dependencies = TRUE, upgrade = 'never')"
