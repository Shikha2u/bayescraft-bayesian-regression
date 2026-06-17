# Results Summary

> Full detail is in [bayesian_analysis_report.pdf](../reports/bayesian_analysis_report.pdf). This page highlights key takeaways for quick review.

## Exploratory Findings

- Multiple recipe levels with varying numbers of replicates per recipe
- Breakage angle varies substantially across recipes and temperature conditions
- Clear grouping structure motivates hierarchical modeling

## Model Results

- **Pooled model** provides a baseline but may underfit recipe-specific effects
- **Separate models** capture recipe differences but have higher uncertainty with small replicate counts
- **Hierarchical Bayesian model** balances flexibility and stability through partial pooling

## Uncertainty Quantification

Bayesian inference yields:
- Posterior mean estimates for regression coefficients
- 95% credible intervals for effects and predictions
- Direct probability statements about parameter values

## Conclusions

- Bayesian regression is well-suited for this structured manufacturing dataset
- Hierarchical specification recommended when groups (recipes) have unequal sample sizes
- Posterior intervals provide actionable uncertainty for quality control decisions

## Next Steps (optional extensions)

- Compare additional priors (informative vs weakly informative)
- Cross-validation or hold-out prediction evaluation
- Sensitivity analysis for prior choices
