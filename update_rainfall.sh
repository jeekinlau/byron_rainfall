#!/bin/bash

# Navigate to path where script is located
cd ~/Desktop/rainfall_github 

# Pull any updates
git pull origin main

# Run the R script
Rscript weekly_rainfall.R

# Add changes to git
git add .


# Commit changes with timestamp
git commit -m "Auto-update rainfall data $(date '+%Y-%m-%d %H:%M:%S')"



# Push to GitHub
git push origin main --force
