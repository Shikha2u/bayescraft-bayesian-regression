# Methodology

## Data Preprocessing

- Load cleaned manufacturing dataset (`data/manufacturing_quality_clean.csv`)
- Create unique replicate identifiers combining recipe and batch
- Factor coding for recipe levels
- Exploratory summaries by recipe and replicate

## Models Considered

### 1. Pooled Model
Single regression treating all observations as one group — ignores recipe structure.

### 2. Separate Model
Independent regressions per recipe level — flexible but data-hungry.

### 3. Hierarchical / Bayesian Model
Partial pooling across recipes — shares strength across groups while allowing recipe-specific effects.

## Bayesian Framework

- **Likelihood:** Normal model for breakage angle given predictors
- **Priors:** Weakly informative priors on intercepts and slopes
- **Inference:** MCMC sampling to obtain posterior draws
- **Output:** Posterior means, credible intervals, predictive checks

## Model Comparison

Models compared using:
- Posterior predictive checks
- Deviance information criteria (DIC) or similar
- Residual and fit diagnostics via visualization

## Software

- **R** with `mvtnorm`, `ggplot2`, `dplyr`, `tidyr`
- Analysis script: `R/bayesian_regression_analysis.R`

## Reproducibility

Run from project root so relative paths resolve correctly:

```bash
Rscript R/bayesian_regression_analysis.R
```

See [Final Report](../reports/bayesian_analysis_report.pdf) for complete mathematical detail and results.
