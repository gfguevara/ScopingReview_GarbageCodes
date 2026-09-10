# Methodological Spectrum of Garbage Code Redistribution

This reference matrix categorizes and describes the quantitative and qualitative methodologies documented in the literature for the identification, cleaning, and redistribution of ill-defined and garbage-coded mortality data. Methods are structured in ascending order of mathematical and computational complexity.

| Method | Analytical Complexity | Minimum Data Requirements | Key Epistemological Assumption | Standard R Packages / Tools | Grounding Literature |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **1. Qualitative Coding Rule Reclassification** | Very Low (Deterministic Rules) | Individual-level multi-line death certificates | Causal chains violating clinical logic can be deterministically corrected. | `dplyr`, `stringr` (Regex-based ICD-10 diagnostic sequence checking) | `[@who2022]`, `[@zhang2025]` |
| **2. Proportional Redistribution (Pro Rata)** | Low (Empirical Fractions) | Aggregated cause-specific death counts by age-sex strata | Garbage coding is completely independent of the true underlying cause of death. | `dplyr`, `tidyr` (Pro-rata fractional grouping) | `[@johnson2021]`, `[@wengler2021]` |
| **3. Fixed Proportions (A Priori)** | Medium-Low (Predefined Weights) | Aggregated death counts and expert consensus target lists | Clinical redistribution fractions are globally static across time and space. | `dplyr` (Static weight multiplication matrices) | `[@naghavi2010]`, `[@kyu2021]` |
| **4. Subnational Regression (Ledermann)** | Medium (Spatial Correlation Models) | Aggregated subnational cause-specific counts over shared periods | Subnational spatial variation reveals stable, unobservable dissimulation coefficients. | `stats::lm` (Refined Ledermann ordinary least squares regression) | `[@grigoriev2024]`, `[@giraldo2017]` |
| **5. Database Record Linkage** | Medium-High (Probabilistic or Deterministic Matching) | Individual-level death certificates linked to clinical databases | Fuzzy or deterministic identifiers are sufficient to reconstruct historical clinical truth. | `fastLink`, `RecordLinkage`, `Rcapture` (Capture-recapture completeness estimation) | `[@bierrenbach2019]`, `[@costa2020]` |
| **6. Coarsened Exact Matching (CEM)** | High (Non-parametric Strata Matching) | Individual-level Multiple Cause of Death (MCoD) records | Garbage-coded deaths share identical clinical profiles as control certificates with contributing garbage codes. | `cem` (Coarsened Exact Matching algorithms) | `[@stevens2010]`, `[@fihel2021]`, `[@zhang2025]` |
| **7. Advanced Parametric Regression** | High (Parametric Probability Classification) | Individual MCoD records and multi-dimensional covariates | Logit-transformed probabilities of target causes are linear functions of clinical predictors. | `rstan`, `brms`, `glmnet` (Bayesian mixed-effects, LASSO penalization) | `[@foreman2016]`, `[@johnson2021]`, `[@ng2020]` |
| **8. Supervised Machine Learning Classifiers** | Very High (Non-linear Classifiers) | Individual MCoD records, clinical features, and demographics | Complex, non-linear clinical and environmental features predict the underlying cause. | `caret`, `rpart`, `xgboost`, `randomForest` (Decision trees, ensemble classifiers) | `[@desouza2025]`, `[@teixeira2020]`, `[@zimeomorais2023]` |

***

### How to use this matrix in your scoping review protocol:
This hierarchical categorization provides a rigorous conceptual framework to map your findings during the data synthesis stage. You can use it to:
1. **Analyze Methodological Frequency:** Characterize the frequency of each redistribution category across low- and middle-income countries.
2. **Evaluate Technical Transferability:** Map each category against the technical requirements of individual-level stochastic microsimulations, demonstrating that methods utilizing individual-level data (such as CEM, Bayesian regression, and machine learning) are natively more compatible with agent-based modeling parameterization.
