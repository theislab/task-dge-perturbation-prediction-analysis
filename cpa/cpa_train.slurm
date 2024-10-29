#!/bin/bash
#SBATCH -p gpu_p
#SBATCH -q gpu_normal
#SBATCH -t 48:00:00
#SBATCH --mem=160G
#SBATCH --gpus=1
#SBATCH --cpus-per-task=20
#SBATCH --nice=0

# Activate your conda environment
conda activate cpa

echo "Running cpa_train.py"

# Run the Python script
python cpa_train.py