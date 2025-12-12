###############################################
## 02_cognitive_FA_figures.R
## Figures 2–4 and supplementary figure
###############################################

## ---------------------------
## Packages
## ---------------------------
fig_pkgs <- c("ggplot2", "patchwork", "ggstatsplot", "MASS", "ggpubr")
for (p in fig_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

## If needed:
## source("01_cognitive_FA_analyses.R")

## =====================================================
## FIGURE 2 – PCA scree plot + loadings
## =====================================================

loadings    <- results_cognitive_fancy_PCA$pc$rotation
eigenvalues <- results_cognitive_fancy_PCA$pc$sdev^2

explained_variance  <- eigenvalues / sum(eigenvalues) * 100
cumulative_variance <- cumsum(explained_variance)

scree_data <- data.frame(
  PC                = 1:length(eigenvalues),
  ExplainedVariance = explained_variance,
  CumulativeVariance = cumulative_variance
)

scree_plot <- ggplot(scree_data, aes(x = PC)) +
  geom_bar(
    aes(y = ExplainedVariance),
    stat  = "identity",
    fill  = "grey",
    color = "black"
  ) +
  geom_line(
    aes(y = CumulativeVariance, group = 1),
    color = "red",
    size  = 1
  ) +
  geom_point(
    aes(y = CumulativeVariance),
    color = "red",
    size  = 2
  ) +
  labs(
    x     = "PCs",
    y     = "Explained Variance (%)",
    title = "Explained Variance by PCs"
  ) +
  scale_x_continuous(
    breaks = seq(1, length(scree_data$PC), by = 1),
    labels = as.character(seq(1, length(scree_data$PC), by = 1))
  ) +
  theme_minimal() +
  theme(
    panel.grid    = element_blank(),
    axis.line     = element_line(color = "black"),
    axis.ticks    = element_line(color = "black"),
    text          = element_text(size = 14),
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    axis.title.y  = element_text(margin = margin(r = 10))
  )

loadings_df <- as.data.frame(loadings)
colnames(loadings_df) <- paste0("PC", 1:ncol(loadings_df))
loadings_df$Variable <- rownames(loadings_df)

loadings_long <- tidyr::pivot_longer(
  loadings_df,
  cols      = dplyr::starts_with("PC"),
  names_to  = "PC",
  values_to = "Loading"
)

selected_pcs <- c("PC1", "PC2", "PC3")

loadings_long_filtered <- loadings_long %>%
  dplyr::filter(PC %in% selected_pcs)

rename_labels <- c(
  "Vocab_extent"                             = "Vocabulary Range",
  "Verbal_fluency_nouns_per_item"           = "Verbal Fluency Task",
  "Topographic_orientation_labyrinth_score"  = "Labyrinth Navigation",
  "Non_verbal_reasoning_Ravens_matrices"     = "Raven’s Matrices",
  "Mental_rotation"                          = "Mental Rotation Test",
  "Corsi_Block_test_visuospatial_span"       = "Corsi Block Test",
  "Complex_mental_calculation_score"         = "Complex Calculations",
  "Auditory_verbal_learning_recalled_words"  = "Delayed Recall (Words)",
  "Arithmetical_facts_score"                 = "Arithmetic Knowledge"
)

loadings_long_filtered <- loadings_long_filtered %>%
  dplyr::mutate(Variable = rename_labels[Variable])

desired_order <- c(
  "Vocabulary Range",
  "Verbal Fluency Task",
  "Delayed Recall (Words)",
  "Mental Rotation Test",
  "Corsi Block Test",
  "Labyrinth Navigation",
  "Raven’s Matrices",
  "Arithmetic Knowledge",
  "Complex Calculations"
)

loadings_long_filtered <- loadings_long_filtered %>%
  dplyr::mutate(Variable = factor(Variable, levels = rev(desired_order))) %>%
  dplyr::mutate(
    PC = factor(
      PC,
      levels = c("PC1", "PC2", "PC3"),
      labels = c(
        "PC1: Generall Cognitive Function",
        "PC2: Visuospatial Processing and Memory",
        "PC3: Mathematics"
      )
    )
  )

loadings_plot <- ggplot(
  loadings_long_filtered,
  aes(x = Loading, y = Variable, fill = Loading > 0)
) +
  geom_bar(stat = "identity", color = "black", width = 0.8) +
  facet_wrap(~PC, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "grey")) +
  labs(
    x     = "Loadings",
    y     = "",
    title = "Loadings for Selected Principal Components"
  ) +
  theme_minimal() +
  theme(
    panel.grid    = element_blank(),
    axis.line.y   = element_line(color = "black"),
    axis.line.x   = element_line(color = "black"),
    axis.ticks    = element_line(color = "black"),
    axis.text.y   = element_text(size = 10),
    axis.text.x   = element_text(size = 10),
    strip.text    = element_text(size = 14, face = "bold"),
    plot.title    = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )

combined_plot_F2 <- (scree_plot | loadings_plot) +
  patchwork::plot_layout(widths = c(0.75, 2)) +
  patchwork::plot_annotation(tag_levels = "A")

combined_plot_F2

## =====================================================
## FIGURE 4 – Cross-dominance (PC2, BCC_3)
## =====================================================

PC2_crossLat <- long_data_PC_CC_FA_small_parts %>%
  dplyr::filter(CC_part == "BCC_3", Principal_Component == "PC2")

lm_model <- lm(Value ~ Sexe + Edinburg.Score + age_IRM_Anat, data = PC2_crossLat)
PC2_crossLat$residuals <- lm_model$residuals

lm_crosslat <- lm(residuals ~ crossed_dominance, data = PC2_crossLat)
anova(lm_crosslat)

## Posthoc grouping variable for Figure 4B
PC2_crossLat$posthoc_cross <- ifelse(
  PC2_crossLat$crossed_dominance == "consistent",
  PC2_crossLat$laterality_frontal_0.1,
  ifelse(
    PC2_crossLat$crossed_dominance == "weak_lat" &
      PC2_crossLat$laterality_frontal_0.1 != "biLat",
    PC2_crossLat$laterality_frontal_0.1,
    ifelse(
      PC2_crossLat$crossed_dominance == "weak_lat" &
        PC2_crossLat$laterality_frontal_0.1 == "biLat",
      PC2_crossLat$laterality_temporal_0.1,
      ifelse(
        PC2_crossLat$crossed_dominance == "crossed_dominance",
        "crossed",
        NA
      )
    )
  )
)

lm_crosslat <- lm(residuals ~ posthoc_cross, data = PC2_crossLat)
anova(lm_crosslat)

filtered_data <- PC2_crossLat %>%
  dplyr::group_by(posthoc_cross) %>%
  dplyr::filter(dplyr::n() >= 3) %>%
  dplyr::ungroup()

plot_a <- ggstatsplot::ggbetweenstats(
  data                        = PC2_crossLat,
  x                           = grouped_dominance,
  y                           = residuals,
  pairwise.display            = "all",
  type                        = "parametric",
  p.adjust.method             = "fdr",
  pairwise.comparisons.label  = "p.signif",
  ggtheme                     = ggplot2::theme_classic()
) +
  labs(
    x     = NULL,
    y     = "Residuals",
    title = "Spatial Attention Skills by Cross Laterality Groups"
  ) +
  theme(
    text          = element_text(size = 14, face = "bold"),
    axis.title.x  = element_text(margin = margin(t = 10)),
    axis.title.y  = element_text(margin = margin(r = 10)),
    plot.title    = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(labels = c(
    "consistent"        = "Consistently lateralised",
    "crossed_dominance" = "Crossed dominance"
  )) +
  scale_fill_gradient() +
  annotate(
    "text",
    x      = 2.5,
    y      = max(PC2_crossLat$Value) * 0.95,
    label  = "ANCOVA pFDR = 0.013",
    size   = 5,
    fontface = "bold",
    color  = "red",
    hjust  = 1
  )

plot_b <- ggstatsplot::ggbetweenstats(
  data                        = filtered_data,
  x                           = posthoc_cross,
  y                           = residuals,
  pairwise.display            = "all",
  type                        = "parametric",
  p.adjust.method             = "fdr",
  pairwise.comparisons.label  = "p.signif",
  ggtheme                     = ggplot2::theme_classic()
) +
  labs(
    x     = NULL,
    y     = "Residuals",
    title = "Post-Hoc Analysis"
  ) +
  theme(
    text          = element_text(size = 14, face = "bold"),
    axis.title.x  = element_text(margin = margin(t = 10)),
    axis.title.y  = element_text(margin = margin(r = 10)),
    plot.title    = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(labels = c(
    "crossed"  = "Crossed dominance",
    "leftLat"  = "Left-leaning",
    "rightLat" = "Right-leaning"
  )) +
  scale_fill_gradient()

combined_plot_F4 <- (plot_a | plot_b) +
  patchwork::plot_annotation(tag_levels = "A") +
  theme(plot.tag = element_text(size = 14, face = "bold"))

combined_plot_F4

ggplot2::ggsave(
  filename    = "/Volumes/LaCie/iMac/Documents/Liverpool/Thesis/Official_Writing/Chapter_9_(cognitive)/Paper_draft2/Preprint/Figure4.tiff",
  plot        = combined_plot_F4,
  width       = 378,
  height      = 193,
  units       = "mm",
  dpi         = 600,
  compression = "lzw"
)

## =====================================================
## FIGURE 3 – Regression plots (Splenium PC1, Genu PC2)
## =====================================================

# Splenium: SCC_3, PC1, Strong-Atypical
Regression_SCC_PC1 <- long_data_PC_CC_FA_small_parts %>%
  dplyr::filter(
    CC_part             == "SCC_3",
    Principal_Component == "PC1",
    laterality          == "Strong-Atypical"
  )

lm_model_a <- lm(Value ~ Sexe + age_IRM_Anat, data = Regression_SCC_PC1)
Regression_SCC_PC1$residuals_a <- residuals(lm_model_a)

lm_model_b <- lm(Value ~ Sexe + age_IRM_Anat + Edinburg.Score, data = Regression_SCC_PC1)
Regression_SCC_PC1$residuals_b <- residuals(lm_model_b)

lm_model_c <- lm(Value ~ mean, data = Regression_SCC_PC1)

new_data <- data.frame(
  mean           = seq(min(Regression_SCC_PC1$mean), max(Regression_SCC_PC1$mean), length.out = nrow(Regression_SCC_PC1)),
  Sexe           = "F",
  age_IRM_Anat   = mean(Regression_SCC_PC1$age_IRM_Anat, na.rm = TRUE),
  Edinburg.Score = mean(Regression_SCC_PC1$Edinburg.Score, na.rm = TRUE)
)

predictions_a <- predict(lm_model_a, newdata = new_data, interval = "confidence")
new_data$predicted_a <- predictions_a[, "fit"]
new_data$lower_ci_a  <- predictions_a[, "lwr"]
new_data$upper_ci_a  <- predictions_a[, "upr"]

predictions_b <- predict(lm_model_b, newdata = new_data, interval = "confidence")
new_data$predicted_b <- predictions_b[, "fit"]
new_data$lower_ci_b  <- predictions_b[, "lwr"]
new_data$upper_ci_b  <- predictions_b[, "upr"]

plot_aS <- ggplot(Regression_SCC_PC1, aes(x = mean, y = residuals_a)) +
  geom_point(size = 3, colour = "darkblue") +
  stat_smooth(
    method   = "lm",
    formula  = y ~ x,
    se       = TRUE,
    colour   = "red",
    fill     = "pink",
    linetype = "dashed",
    size     = 1
  ) +
  labs(
    title = "Mean FA in Splenium vs General Cognitive Function\n(Adjusted for Age and Sex)",
    x     = "Mean FA (Splenium)",
    y     = "Working Memory PC Score"
  ) +
  annotate(
    "text",
    x      = 0.64,
    y      = -1,
    label  = paste0("R² = -0.93", "\nP = 0.002", "\npFDR = 0.035"),
    size   = 5,
    hjust  = 1,
    colour = "black"
  ) +
  theme_classic() +
  theme(
    text       = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_bS <- ggplot(Regression_SCC_PC1, aes(x = mean, y = residuals_b)) +
  geom_point(size = 3, colour = "darkblue") +
  stat_smooth(
    method   = "lm",
    formula  = y ~ x,
    se       = TRUE,
    colour   = "red",
    fill     = "pink",
    linetype = "dashed",
    size     = 1
  ) +
  labs(
    title = "Mean FA in Splenium vs General Cognitive Function\n(Adjusted for Age, Sex and Handedness)",
    x     = "Mean FA (Splenium)",
    y     = "Working Memory PC Score"
  ) +
  annotate(
    "text",
    x      = 0.64,
    y      = -1,
    label  = paste0("R² = -0.94", "\nP = 0.004", "\npFDR = 0.062"),
    size   = 5,
    hjust  = 1,
    colour = "black"
  ) +
  theme_classic() +
  theme(
    text       = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_cS <- ggplot(Regression_SCC_PC1, aes(x = mean, y = Value)) +
  geom_point(size = 3, colour = "darkblue") +
  stat_smooth(
    method   = "lm",
    formula  = y ~ x,
    se       = TRUE,
    colour   = "red",
    fill     = "pink",
    linetype = "dashed",
    size     = 1
  ) +
  labs(
    title = "Mean FA in Splenium vs General Cognitive Function\n(No Covariates)",
    x     = "Mean FA (Splenium)",
    y     = "Working Memory PC Score"
  ) +
  annotate(
    "text",
    x      = 0.64,
    y      = -1,
    label  = paste0("R² = -0.36", "\nP = 0.0529", "\npFDR = 0.69"),
    size   = 5,
    hjust  = 1,
    colour = "black"
  ) +
  theme_classic() +
  theme(
    text       = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Genu: GCC_3, PC2, Strong-Atypical
Regression_GCC_PC2 <- long_data_PC_CC_FA_small_parts %>%
  dplyr::filter(
    CC_part             == "GCC_3",
    Principal_Component == "PC2",
    laterality          == "Strong-Atypical"
  )

lm_model_a <- lm(Value ~ Sexe + age_IRM_Anat, data = Regression_GCC_PC2)
Regression_GCC_PC2$residuals_a <- residuals(lm_model_a)

lm_model_b <- lm(Value ~ Sexe + age_IRM_Anat + Edinburg.Score, data = Regression_GCC_PC2)
Regression_GCC_PC2$residuals_b <- residuals(lm_model_b)

lm_model_c <- lm(Value ~ mean, data = Regression_GCC_PC2)

new_data <- data.frame(
  mean           = seq(min(Regression_GCC_PC2$mean), max(Regression_GCC_PC2$mean), length.out = nrow(Regression_GCC_PC2)),
  Sexe           = "F",
  age_IRM_Anat   = mean(Regression_GCC_PC2$age_IRM_Anat, na.rm = TRUE),
  Edinburg.Score = mean(Regression_GCC_PC2$Edinburg.Score, na.rm = TRUE)
)

predictions_a <- predict(lm_model_a, newdata = new_data, interval = "confidence")
new_data$predicted_a <- predictions_a[, "fit"]
new_data$lower_ci_a  <- predictions_a[, "lwr"]
new_data$upper_ci_a  <- predictions_a[, "upr"]

predictions_b <- predict(lm_model_b, newdata = new_data, interval = "confidence")
new_data$predicted_b <- predictions_b[, "fit"]
new_data$lower_ci_b  <- predictions_b[, "lwr"]
new_data$upper_ci_b  <- predictions_b[, "upr"]

plot_aG <- ggplot(Regression_GCC_PC2, aes(x = mean, y = residuals_a)) +
  geom_point(size = 3, colour = "darkblue") +
  stat_smooth(
    method   = "lm",
    formula  = y ~ x,
    se       = TRUE,
    colour   = "red",
    fill     = "pink",
    linetype = "dashed",
    size     = 1
  ) +
  labs(
    title = "Mean FA in Genu vs Spatial Attention\n(Adjusted for Age and Sex)",
    x     = "Mean FA (Genu)",
    y     = "Spatial Attention PC Score"
  ) +
  annotate(
    "text",
    x      = max(Regression_GCC_PC2$mean) * 0.99,
    y      = -1,
    label  = paste0("R² = 0.87", "\nP = 0.0009", "\npFDR = 0.04"),
    size   = 5,
    hjust  = 1,
    colour = "black"
  ) +
  theme_classic() +
  theme(
    text       = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_bG <- ggplot(Regression_GCC_PC2, aes(x = mean, y = residuals_b)) +
  geom_point(size = 3, colour = "darkblue") +
  stat_smooth(
    method   = "lm",
    formula  = y ~ x,
    se       = TRUE,
    colour   = "red",
    fill     = "pink",
    linetype = "dashed",
    size     = 1
  ) +
  labs(
    title = "Mean FA in Genu vs Spatial Attention\n(Adjusted for Age, Sex, Handedness)",
    x     = "Mean FA (Genu)",
    y     = "Spatial Attention PC Score)"
  ) +
  annotate(
    "text",
    x      = max(Regression_GCC_PC2$mean) * 0.99,
    y      = -1,
    label  = paste0("R² = 0.86", "\nP = 0.006", "\npFDR = 0.06"),
    size   = 5,
    hjust  = 1,
    colour = "black"
  ) +
  theme_classic() +
  theme(
    text       = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_cG <- ggplot(Regression_GCC_PC2, aes(x = mean, y = Value)) +
  geom_point(size = 3, colour = "darkblue") +
  stat_smooth(
    method   = "lm",
    formula  = y ~ x,
    se       = TRUE,
    colour   = "red",
    fill     = "pink",
    linetype = "dashed",
    size     = 1
  ) +
  labs(
    title = "Mean FA in Genu vs Spatial Attention\n(No Covariates)",
    x     = "Mean FA (Genu)",
    y     = "Spatial Attention PC Score"
  ) +
  annotate(
    "text",
    x      = max(Regression_GCC_PC2$mean) * 0.99,
    y      = -1,
    label  = paste0("R² = 0.54", "\nP = 0.01", "\npFDR = 0.438"),
    size   = 5,
    hjust  = 1,
    colour = "black"
  ) +
  theme_classic() +
  theme(
    text       = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

combined_plot_F3 <- (plot_aS | plot_bS) /
  (plot_aG | plot_bG) +
  patchwork::plot_annotation(tag_levels = "A")

combined_plot_F3

## =====================================================
## SUPPLEMENTARY FIGURE – regression plots by laterality
## =====================================================

Regression_SCC_PC1_all <- long_data_PC_CC_FA_small_parts %>%
  dplyr::filter(CC_part == "SCC_3", Principal_Component == "PC1")

compute_residuals_by_group <- function(data, formula) {
  data %>%
    dplyr::group_by(laterality) %>%
    dplyr::mutate(residuals = residuals(lm(formula, data = dplyr::cur_data_all()))) %>%
    dplyr::ungroup()
}

data_age_sex <- compute_residuals_by_group(
  Regression_SCC_PC1_all,
  Value ~ Sexe + age_IRM_Anat
)

plot_a_PC1_lat <- ggplot(data_age_sex, aes(x = mean, y = residuals, colour = laterality)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(
    method = "lm",
    aes(fill = laterality),
    alpha = 0.2,
    se    = TRUE
  ) +
  scale_color_brewer(palette = "Set1", name = "Laterality Group") +
  scale_fill_brewer(palette = "Set1", name = "Laterality Group") +
  labs(
    title = "Working Memory vs Mean FA in Splenium\n (Age, Sex)",
    x     = "Mean FA (Splenium)",
    y     = "Residuals (Working Memory PC)"
  ) +
  theme_classic() +
  theme(
    text        = element_text(size = 14),
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

Regression_GCC_PC2_all <- long_data_PC_CC_FA_small_parts %>%
  dplyr::filter(CC_part == "GCC_3", Principal_Component == "PC2")

data_age_sex_GCC <- Regression_GCC_PC2_all %>%
  dplyr::group_by(laterality) %>%
  dplyr::mutate(residuals = residuals(lm(Value ~ Sexe + age_IRM_Anat, data = dplyr::cur_data_all()))) %>%
  dplyr::ungroup()

plot_a_PC2_lat <- ggplot(data_age_sex_GCC, aes(x = mean, y = residuals, colour = laterality)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(
    method = "lm",
    aes(fill = laterality),
    alpha = 0.2,
    se    = TRUE
  ) +
  scale_color_brewer(palette = "Set1", name = "Laterality Group") +
  scale_fill_brewer(palette = "Set1", name = "Laterality Group") +
  labs(
    title = "Spatial Attention vs Mean FA in Genu\n(Age, Sex)",
    x     = "Mean FA (Genu)",
    y     = "Residuals (Spatial Attention PC)"
  ) +
  theme_classic() +
  theme(
    text        = element_text(size = 14),
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

combined_plot_supp <- (plot_a_PC1_lat | plot_a_PC2_lat) +
  patchwork::plot_annotation(tag_levels = "A")

combined_plot_supp
