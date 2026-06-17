# Problem Statement

## Context

In manufacturing and quality control, predicting material properties under varying production conditions is essential. Rather than relying solely on point estimates from ordinary least squares, Bayesian methods provide **full posterior distributions** — enabling credible intervals and principled uncertainty quantification.

## Objective

Model the relationship between production variables (recipe, temperature, etc.) and **breakage angle** — a quality metric — using Bayesian regression techniques.

## Target Variable

- **Breakage angle** (`angle`): continuous response measuring material breakage under controlled conditions

## Predictors

- **Recipe** (`recipe`): categorical factor (different production recipes)
- **Replicate** (`replicate`): batch replicate within recipe
- **Temperature** and other process variables in the cleaned dataset

## Why Bayesian?

| Frequentist | Bayesian |
|-------------|----------|
| Single point estimate | Full posterior distribution |
| Confidence intervals (repeated-sampling) | Credible intervals (direct probability statements) |
| Fixed effects only | Natural incorporation of prior knowledge |

## Success Criteria

- Exploratory analysis of manufacturing data structure
- Specify and fit Bayesian regression models
- Compare pooled, separate, and hierarchical specifications
- Report posterior summaries and interpret uncertainty
