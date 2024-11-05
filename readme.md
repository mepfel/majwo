# README
**Probabilistic Approaches to Forecasting the Time Path of Energy Peak Demand**

This repository contains the code and data for my master's thesis. The thesis explores probabilistic approaches for forecasting energy peak demand comparing models based on past peak data (DSA) with those incorporating additional load data (DSB).

## Folder Structure

- `analysis`: Scripts for energy load data and peak demand analysis
- `data`: Datasets for analysis (raw and processed)
- `evaluation`: Model evaluation results, metrics, and visualizations
- `models`: Forecasting models and post-processing methods
  - `DSA`: Point forecasting models (in the file `models_dsa.R`) and post-processing techniques (starting with the prefix `pp_`) for DSA approach
  - `DSB`: Point forecasting models (in the file `models_dsb.R`) and post-processing (starting with the prefix `pp_`) for DSB approach

## Usage

1. **Data Preparation**: Place datasets in the `data` or use the available data
2. **Running Models**: Execute forecasting scripts in the `models` folder.
3. **Evaluation**: Use `evaluation` scripts for model performance assessment.

## Requirements

All code is written in `R`.

The following packages are necessary:
```{r}
install.packages(c("tidyverse", "plotly", "scoringRules", "fable", "MASS", "forecast", "ggplot2", "quantreg", "randomForest"))
```