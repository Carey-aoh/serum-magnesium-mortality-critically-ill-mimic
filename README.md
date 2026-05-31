# serum-magnesium-mortality-critically-ill-mimic

Official code for *"Optimal Serum Magnesium Level and Mortality in Critically Ill Patients: A Retrospective Cohort Study from MIMIC-IV"*.

This repository implements a complete analytical pipeline for a large-scale retrospective cohort study (N=42,419) investigating the nonlinear association between serum magnesium and short-term mortality in critically ill patients, with a focus on identifying an optimal serum magnesium threshold and evaluating the effectiveness of magnesium supplementation and early target attainment.

## 🔍 Key Findings

- **Optimal threshold identified**: Serum magnesium level of approximately **2.1 mg/dL** was associated with the lowest 28-day mortality
- **Supplementation benefit**: In patients with low-normal magnesium (1.7–2.1 mg/dL), supplementation was associated with lower 28-day mortality (HR **0.85**, 95% CI 0.74–0.97; *P* = 0.010)
- **Target attainment matters**: Early attainment of magnesium ≥2.1 mg/dL within 24 hours after the first dose was also associated with reduced mortality (HR **0.81**, 95% CI 0.65–0.99; *P* = 0.041)

## 📊 Study Overview

| Aspect | Description |
| :--- | :--- |
| **Study design** | Retrospective cohort study using de-identified ICU electronic health records |
| **Data source** | MIMIC-IV (Medical Information Mart for Intensive Care IV) |
| **Study population** | 42,419 adult ICU patients after applying eligibility and exclusion criteria |
| **Exposure** | Intravenous magnesium sulfate supplementation during ICU stay |
| **Target attainment** | Serum magnesium ≥2.1 mg/dL within 24 hours after the first magnesium dose |
| **Primary outcome** | 28-day all-cause mortality |
| **Core methods** | PostgreSQL data extraction; restricted cubic splines; segmented Cox models; ML-assisted feature selection; PSM; IPTW; landmark analysis; SHAP-based interpretability |

## 🧰 Requirements

**SQL (PostgreSQL)**
- MIMIC-IV database access required

**R (≥4.0)**
- tidyverse, survival, rms, MatchIt, tableone, ggplot2

**Python (≥3.8)**
- numpy, pandas, scikit-learn, shap

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
│ └── 010_ml_feature_selection.ipynb # ML-assisted feature selection + SHAP
│
├── output/
│ ├── figures/ # RCS curves, survival curves, forest plots, love plot, density plots
│ └── tables/ # Baseline table, sensitivity results, secondary outcomes
│
├── README.md
└── LICENSE
## 🚀 Workflow

### Step 1: Data Extraction (SQL)
Execute `sql/011_cohort_extraction.sql` on your PostgreSQL instance of MIMIC-IV to generate the base cohort.

### Step 2: Threshold Identification (R)
Run `r/003_threshold_analysis.R` to perform restricted cubic spline analysis (4 knots) and identify the optimal serum magnesium threshold via grid search maximizing model log-likelihood.

### Step 3: ML-Assisted Feature Selection (Python)
Run `python/010_ml_feature_selection.ipynb` to derive consensus covariate sets using:
- Random Forest, Logistic Regression, K-Nearest Neighbors
- Selection strategies: SelectKBest, SelectFromModel, Recursive Feature Elimination (RFE)
- SHAP for feature interpretability

### Step 4: Causal Effect Estimation (R)
Run `r/004_forest_plots_subgroup.R` and `r/009_secondary_outcomes.R` to estimate the association between magnesium supplementation and mortality using:
- Propensity score matching (1:1, caliper = 0.2 SD)
- Inverse probability of treatment weighting (IPTW)
- Multivariable Cox regression

### Step 5: Baseline Table & Covariate Balance (R)
Run `r/007_baseline_table_psm.R`, `r/006_density_plots_psm.R`, and `r/008_loveplot_psm.R` to generate Table 1 and assess covariate balance before/after matching.

### Step 6: Survival Curves & Sensitivity Analyses (R)
Run `r/005_survival_curves.R` for Kaplan–Meier plots, and `r/001_sensitivity_analysis.R` with `r/002_forest_plots_sensitivity.R` to validate findings across different model specifications and subgroups.
