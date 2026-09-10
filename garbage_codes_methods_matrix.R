# R Script to generate the Garbage Codes Redistribution Methods Matrix
# This script creates a clean, publication-ready data frame and exports it 
# as a markdown table or CSV for integration into systematic scoping reviews.

# Install/Load required packages (safely)
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
library(knitr)

# Define the dataset
methods_matrix <- data.frame(
  Method = c(
    "1. Qualitative Coding Rule Reclassification",
    "2. Proportional Redistribution (Pro Rata)",
    "3. Fixed Proportions (A Priori)",
    "4. Subnational Regression (Ledermann)",
    "5. Database Record Linkage",
    "6. Coarsened Exact Matching (CEM)",
    "7. Advanced Parametric Regression",
    "8. Supervised Machine Learning Classifiers"
  ),
  Complexity = c(
    "Very Low (Deterministic Rules)",
    "Low (Empirical Fractions)",
    "Medium-Low (Predefined Weights)",
    "Medium (Spatial Correlation Models)",
    "Medium-High (Deterministic/Probabilistic Matching)",
    "High (Non-parametric Strata Matching)",
    "High (Parametric Probability Classification)",
    "Very High (Non-linear Classifiers)"
  ),
  Data_Requirements = c(
    "Individual-level multi-line death certificates",
    "Aggregated cause-specific death counts by age-sex strata",
    "Aggregated death counts and expert consensus target lists",
    "Aggregated subnational cause-specific counts over shared periods",
    "Individual-level death certificates linked to clinical databases",
    "Individual-level Multiple Cause of Death (MCoD) records",
    "Individual MCoD records and multi-dimensional covariates",
    "Individual MCoD records, clinical features, and demographics"
  ),
  Key_Assumptions = c(
    "Causal chains violating clinical logic can be deterministically corrected.",
    "Garbage coding is completely independent of the true underlying cause of death.",
    "Clinical redistribution fractions are globally static across time and space.",
    "Subnational spatial variation reveals stable, unobservable dissimulation coefficients.",
    "Fuzzy/deterministic identifiers are sufficient to reconstruct historical clinical truth.",
    "Garbage-coded deaths share identical clinical profiles as control certificates with contributing garbage codes.",
    "Logit-transformed probabilities of target causes are linear functions of clinical predictors.",
    "Complex, non-linear clinical and environmental features predict the underlying cause."
  ),
  R_Packages_Tools = c(
    "dplyr, stringr (Regex CIE parsing rules)",
    "dplyr, tidyr (Pro-rata fractional grouping)",
    "dplyr (Static weight multiplication)",
    "stats::lm (OLS regression pipeline)",
    "fastLink, RecordLinkage, Rcapture",
    "cem (Coarsened Exact Matching)",
    "rstan, brms, glmnet (Bayesian mixed-effects, LASSO)",
    "caret, rpart, xgboost, randomForest"
  ),
  Primary_Sources = c(
    "[@who2022], [@zhang2025]",
    "[@johnson2021], [@wengler2021]",
    "[@naghavi2010], [@kyu2021]",
    "[@grigoriev2024], [@giraldo2017]",
    "[@bierrenbach2019], [@costa2020]",
    "[@stevens2010], [@fihel2021], [@zhang2025]",
    "[@foreman2016], [@johnson2021], [@ng2020]",
    "[@desouza2025], [@teixeira2020], [@zimeomorais2023]"
  ),
  stringsAsFactors = FALSE
)

# Export options
# 1. Save as CSV
write.csv(methods_matrix, "garbage_codes_methods_matrix.csv", row.names = FALSE)
cat("CSV file successfully exported as 'garbage_codes_methods_matrix.csv'\n\n")

# 2. Print as Markdown Table
markdown_table <- kable(
  methods_matrix, 
  col.names = c("Method", "Analytical Complexity", "Minimum Data Requirements", "Key Epistemological Assumption", "Standard R Packages", "Grounding Literature"), 
  format = "markdown"
)
cat(markdown_table)
