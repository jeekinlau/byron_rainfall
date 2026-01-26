#!/bin/bash

# Navigate to path where script is located
cd ~/Desktop/rainfall_github 

# Always start clean and synced
git fetch origin
git reset --hard origin/main

# Run the R script
Rscript weekly_rainfall.R

# Add changes to git
git add .


# Commit changes with timestamp
git commit -m "Auto-update rainfall data $(date '+%Y-%m-%d %H:%M:%S')"

# Rebase on any new remote commits
git pull --rebase origin main

# Push to GitHub
git push origin main
