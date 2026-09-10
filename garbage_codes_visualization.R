# ==============================================================================
# The Methodological Spectrum of Garbage Code Redistribution
# Conceptual Visualization Script using ggplot2
# ==============================================================================
# This script constructs a publication-quality 2D conceptual chart
# mapping the eight major garbage code redistribution methods against 
# their computational complexity and minimum input data requirements.
# ==============================================================================

# Install and load ggplot2 if not already present
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# 1. Construct the analytical database
methods_data <- data.frame(
  id = 1:8,
  method = c(
    "1. Qualitative Coding\nRule Reclassification",
    "2. Proportional\nRedistribution (Pro Rata)",
    "3. Fixed Proportions\n(A Priori Matrices)",
    "4. Subnational\nRegression (Ledermann)",
    "5. Database\nRecord Linkage",
    "6. Coarsened Exact\nMatching (CEM)",
    "7. Advanced Parametric\nRegression",
    "8. Supervised Machine\nLearning Classifiers"
  ),
  complexity = c(1, 2, 3, 4, 5, 6, 7, 8),
  data_req = c(3, 1, 2, 4, 7, 5, 6, 8),
  size = c(4, 4.5, 5, 6, 9, 7, 8, 11), # Represents computational resource footprint
  category = c(
    "Foundational Deterministic", "Foundational Deterministic", "Foundational Deterministic",
    "Data Integration & Correlation", "Data Integration & Correlation",
    "Advanced Stochastic Modeling", "Advanced Stochastic Modeling", "Advanced Stochastic Modeling"
  )
)

# 2. Set up coordinate labels
complexity_labels <- c(
  "Qualitative\nRules", "Simple\nProportions", "Fixed\nWeights", 
  "Spatial\nRegression", "Record\nLinkage", "Non-parametric\nCEM", 
  "Parametric\nLogit/MICE", "Supervised\nMachine Learning"
)

data_labels <- c(
  "Aggregate\nCounts", "Expert\nTarget Lists", "ICD Causal\nChains", 
  "Subnational\nAggregate", "Individual\nMCoD Records", "MCoD +\nCovariates", 
  "Linked Clinical\nDatabases", "Linked MCoD +\nSocioeconomic"
)

# 3. Build the visualization using ggplot2
p <- ggplot(methods_data, aes(x = complexity, y = data_req)) +
  # Highlight the Microsimulation Compatibility Zone (shaded rectangle)
  annotate("rect", xmin = 5.5, xmax = 8.7, ymin = 0.2, ymax = 8.8,
           fill = "#7b68ee", alpha = 0.08) +
  
  # Text block for the Microsimulation Zone
  annotate("label", x = 7.1, y = 3.0, 
           label = "Stochastic Microsimulation\nParameterization Zone\n(Preserves Individual Variance)",
           color = "#4b0082", fontface = "bold", size = 3, 
           fill = "white", label.padding = unit(0.4, "lines"),
           label.size = 0.5, alpha = 0.9) +
  
  # Gridlines and baseline formatting
  theme_minimal(base_family = "sans") +
  
  # Map variables to points
  geom_point(aes(size = size, fill = category), 
             color = "#333333", shape = 21, stroke = 1.1, alpha = 0.9, show.legend = FALSE) +
  
  # Text labels for the 8 methods with manually managed positions to avoid overlaps
  geom_label(aes(label = method, 
                 vjust = ifelse(id %in% c(2, 6, 8), 1.7, -0.7)),
             size = 2.8, fontface = "bold", color = "#222222",
             fill = "white", alpha = 0.95, label.padding = unit(0.2, "lines"),
             label.size = 0.1, lineheight = 0.95) +
  
  # Color scale styling (professional editorial palette)
  scale_fill_manual(values = c(
    "Foundational Deterministic" = "#2b5c8f",
    "Data Integration & Correlation" = "#3b7a57",
    "Advanced Stochastic Modeling" = "#8a2be2"
  )) +
  
  # Configure scale limits and customized labels
  scale_x_continuous(breaks = 1:8, labels = complexity_labels, limits = c(0.3, 8.7)) +
  scale_y_continuous(breaks = 1:8, labels = data_labels, limits = c(0.2, 8.8)) +
  scale_size_continuous(range = c(4, 12)) +
  
  # Titles and subtitles (free of em dashes)
  labs(
    title = "The Methodological Spectrum of Garbage Code Redistribution",
    subtitle = "Mapping epidemiological algorithms from deterministic rules to advanced stochastic models",
    x = "Analytical & Computational Complexity\n(Deterministic/Basic Rules -> Advanced Predictive/Stochastic Models)",
    y = "Minimum Data Requirements\n(Simple Aggregate Totals -> Multi-Database Individual Microdata)"
  ) +
  
  # Theme and typographic adjustments
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#111111", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, color = "#555555", hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", size = 10, color = "#222222", margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", size = 10, color = "#222222", margin = margin(r = 12)),
    axis.text.x = element_text(size = 7.5, face = "bold", color = "#333333"),
    axis.text.y = element_text(size = 7.5, face = "bold", color = "#333333"),
    panel.grid.major = element_line(color = "#e0e0e0", linetype = "dashed", size = 0.4),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#fcfcfc", color = NA),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

# 4. Render and export the plot
ggsave("garbage_codes_spectrum.png", plot = p, width = 11, height = 7.5, dpi = 150)
print("GGPlot2 visualization successfully prepared. Chart saved as 'garbage_codes_spectrum.png'.")
