## 📁 Repository Structure
serum-magnesium-mortality-critically-ill-mimic/
│
├── sql/
│ └── 011_cohort_extraction.sql # MIMIC-IV cohort building (PostgreSQL)
│
├── r/
│ ├── 001_sensitivity_analysis.R # Main sensitivity analyses
│ ├── 002_forest_plots_sensitivity.R # Forest plots for sensitivity results
│ ├── 003_threshold_analysis.R # RCS + grid search for Mg threshold
│ ├── 004_forest_plots_subgroup.R # Forest plots for subgroup analyses
│ ├── 005_survival_curves.R # Kaplan–Meier survival curves
│ ├── 006_density_plots_psm.R # Density plots before/after PSM
│ ├── 007_baseline_table_psm.R # Table 1: baseline characteristics
│ ├── 008_loveplot_psm.R # Love plot for covariate balance
│ └── 009_secondary_outcomes.R # Secondary outcomes (90-day mortality)
│
├── python/
│ └── 010_ml_feature_selection.ipynb # ML-assisted feature selection
│
├── output/
│ ├── figures/ # All generated plots
│ └── tables/ # All generated tables
│
├── README.md
└── LICENSE
