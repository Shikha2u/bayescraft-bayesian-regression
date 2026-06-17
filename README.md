# Manufacturing Quality Prediction with Bayesian Regression — R, MCMC & Posterior Analysis

> **BayesCraft** · Bayesian linear regression and MCMC inference in R to predict material breakage angles from manufacturing data, with prior specification, hierarchical model comparison, and full uncertainty quantification.

## Impact

Manufacturing quality control often requires understanding not just point estimates but **uncertainty** in predictions. This project applies Bayesian linear regression to predict material breakage angles from production conditions — demonstrating statistical depth beyond standard frequentist ML.

## About the Data

External real-world manufacturing quality dataset. The target variable is **breakage angle** — a measure of material quality under controlled baking/recipe conditions.

## Key Results

- Bayesian linear regression with prior specification
- Posterior inference and credible intervals
- Model comparison across hierarchical specifications
- Exploratory analysis of recipe and replicate effects

## Tech Stack

R · Bayesian inference · MCMC · ggplot2 · dplyr

## Documentation

| Doc | Description |
|-----|-------------|
| [Problem Statement](docs/01-problem-statement.md) | Context and objectives |
| [Methodology](docs/02-methodology.md) | Priors, MCMC, model comparison |
| [Results Summary](docs/03-results-summary.md) | Key findings |
| [Final Report](reports/bayesian_analysis_report.pdf) | Complete analysis write-up |

## Project Structure

```
hierarchical-bayesian-regression/
├── R/
│   └── bayesian_regression_analysis.R
├── data/
│   └── manufacturing_quality_clean.csv
├── docs/
└── reports/
```

## Setup & Usage

```bash
# Install R packages
install.packages(c("mvtnorm", "ggplot2", "dplyr", "tidyr"))

# Run from project root
Rscript R/bayesian_regression_analysis.R
```

## Skills Demonstrated

- Bayesian linear regression
- Prior and posterior analysis
- Uncertainty quantification
- Real-world statistical modeling in R

## License

MIT
