###############################################
## 01_cognitive_FA_analyses.R
## Data preparation, PCA, ANCOVA, regressions
###############################################

## ---------------------------
## Packages
## ---------------------------
pkgs <- c(
  "qdapRegex",
  "readxl",
  "readr",
  "dplyr",
  "ggplot2",
  "samr",
  "tidyr",
  "broom",
  "tibble"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

## ---------------------------
## Working directory
## ---------------------------
setwd("/Volumes/LaCie/iMac/Documents/Liverpool/Thesis/Official_Writing/Chapter_9_(cognitive)")

## ---------------------------
## Load data
## ---------------------------

# Cognitive tests
Cognitive_tests <- readr::read_csv("data_cog2_.csv") %>%
  dplyr::rename(subject = Sujet_list)

# CC FA small parts
CC_FA_small_parts <- readr::read_csv("CC_smaller_parts_fa_stats.csv") %>%
  dplyr::rename(subject = subj)

# Demographics / laterality
demog <- readxl::read_xlsx("HFLI.xlsx")

## ---------------------------
## Cognitive: select, clean, standardise
## ---------------------------

cols_to_keep <- c(
  "subject",
  "Sexe",
  "Edinburg Score",
  "Educational level (nb of school years)",
  "RSTS - reading, score",
  "LSTS",
  "voc - vocabulary or verbal IQ",
  "gen - generation, generate associated words to the noun",
  "reydiff (15 minutes after delayed) - long term memory",
  "PSM - phonological memory, pseudowords",
  "rimtot - phonological memory rhyme",
  "Mental rotation test (number correct out of 20)",
  "Corsi test number of correct response",
  "Labyrinthe Score",
  "Advanced Progressive Matrices IQ",
  "Arithmetic Facts (out of 36)",
  "Complex calculation (out of 8)"
)

Cognitive_tests_filtered <- Cognitive_tests[, cols_to_keep] %>%
  na.omit()

colnames(Cognitive_tests_filtered) <- c(
  "subject",
  "Sexe",
  "Edinburg.Score",
  "Educational.level..nb.of.school.years.",
  "Reading_test_working_memory_score",
  "Listening_test_working_memory_score",
  "Vocab_extent",
  "Verbal_fluency_nouns_per_item",
  "Auditory_verbal_learning_recalled_words",
  "Auditory_phonological_learning_recalled_pseudowords",
  "Rhyme_judgement",
  "Mental_rotation",
  "Corsi_Block_test_visuospatial_span",
  "Topographic_orientation_labyrinth_score",
  "Non_verbal_reasoning_Ravens_matrices",
  "Arithmetical_facts_score",
  "Complex_mental_calculation_score"
)

cols_to_test <- c(
  "Vocab_extent",
  "Verbal_fluency_nouns_per_item",
  "Auditory_verbal_learning_recalled_words",
  "Mental_rotation",
  "Corsi_Block_test_visuospatial_span",
  "Topographic_orientation_labyrinth_score",
  "Non_verbal_reasoning_Ravens_matrices",
  "Arithmetical_facts_score",
  "Complex_mental_calculation_score"
)

subset_df <- Cognitive_tests_filtered[, cols_to_test]

# Scale data
scaled_df <- as.data.frame(scale(subset_df))
colnames(scaled_df) <- colnames(subset_df)

# Quick distribution check (diagnostics)
par(mfrow = c(3, 4))
for (i in 1:ncol(scaled_df)) {
  hist(scaled_df[, i], main = colnames(scaled_df)[i], xlab = "Value")
}
par(mfrow = c(1, 1))

## ---------------------------
## PCA via PCA_function
## ---------------------------

results_cognitive_fancy_PCA <- PCA_function(
  scaled_df,
  groups    = FALSE,
  pcs       = c(1, 2),
  useLabels = TRUE,
  type      = "loadings"
)

# Add principal components to dataframe
Cognitive_tests_filtered[, paste0("PC", 1:3)] <-
  results_cognitive_fancy_PCA$pc$x[, 1:3]

# Redundant but kept for clarity/compatibility
Cognitive_tests_filtered$PC1 <- results_cognitive_fancy_PCA$pc$x[, 1] # General cognition
Cognitive_tests_filtered$PC2 <- results_cognitive_fancy_PCA$pc$x[, 2] # Visuospatial Processing and Memory
Cognitive_tests_filtered$PC3 <- results_cognitive_fancy_PCA$pc$x[, 3] # Mathematics

## ---------------------------
## Laterality / demographics
## ---------------------------

lateralities <- demog[, c(
  "N sujet",
  "Atyp PROD 3 classes",
  "HFLI_production:phrases-listes_nomot",
  "age_IRM_Anat"
)]
colnames(lateralities)[1] <- "subject"

## ---------------------------
## Merge CC FA, cognition, laterality
## ---------------------------

merged_df <- CC_FA_small_parts %>%
  inner_join(Cognitive_tests_filtered, by = "subject") %>%
  inner_join(lateralities, by = "subject") %>%
  as_tibble()

colnames(merged_df)[26] <- "laterality"  # Atyp PROD 3 classes

## ---------------------------
## Long-format CC PCs – mid-sagittal only (part 3)
## ---------------------------

long_data_PC_CC_FA_small_parts <- merged_df %>%
  pivot_longer(
    cols      = starts_with("PC"),
    names_to  = "Principal_Component",
    values_to = "Value"
  ) %>%
  mutate(laterality_factor = factor(laterality)) %>%
  filter(part == 3) %>%
  mutate(CC_part = paste(CC_part, part, sep = "_")) %>%
  select(-part)

unique_tracts       <- unique(long_data_PC_CC_FA_small_parts$CC_part)
unique_PCs          <- unique(long_data_PC_CC_FA_small_parts$Principal_Component)
results_PC          <- long_data_PC_CC_FA_small_parts
results_CC_small    <- merged_df

## ---------------------------
## AF FA data – full AF
## ---------------------------

AF_FAs <- read.csv("af_parts_fa_stats.csv", header = TRUE)
AF_FAs$tract <- paste(AF_FAs$side, AF_FAs$part, sep = "_")
colnames(AF_FAs)[colnames(AF_FAs) == "subj"] <- "subject"

AF_FAs_full <- AF_FAs[grepl("Full", AF_FAs$tract), ]

join_and_rename <- function(data, lateralities_data) {
  data <- inner_join(data, Cognitive_tests_filtered, by = "subject")
  data <- inner_join(data, lateralities_data, by = "subject")
  colnames(data)[which(colnames(data) == "Atyp PROD 3 classes")] <- "laterality"
  colnames(data)[which(colnames(data) == "HFLI_production:phrases-listes_nomot")] <- "laterality_numeric"
  return(data)
}

Cog_and_AF_FAs_full <- join_and_rename(AF_FAs_full, lateralities)

long_data_AF_full_PC <- Cog_and_AF_FAs_full %>%
  pivot_longer(
    cols      = starts_with("PC"),
    names_to  = "Principal_Component",
    values_to = "Value"
  )

## ---------------------------
## ANCOVA – laterality (three groups), CC & AF
## ---------------------------

ancova_results <- data.frame()

covariate_cases <- c("no_covariates", "age_sex", "age_sex_handedness")

build_formula <- function(covariate_case, group_col) {
  if (covariate_case == "no_covariates") {
    as.formula(paste("Value ~", group_col, "* mean"))
  } else if (covariate_case == "age_sex") {
    as.formula(paste("Value ~", group_col, "* mean + age_IRM_Anat + Sexe"))
  } else if (covariate_case == "age_sex_handedness") {
    as.formula(paste("Value ~", group_col, "* mean + age_IRM_Anat + Sexe + Edinburg.Score"))
  }
}

datasets <- list(
  CC = long_data_PC_CC_FA_small_parts,
  AF = long_data_AF_full_PC
)

for (dataset_name in names(datasets)) {
  
  dataset <- datasets[[dataset_name]]
  tract_column <- ifelse(dataset_name == "CC", "CC_part", "tract")
  
  for (tract in unique(dataset[[tract_column]])) {
    for (PC in unique(dataset$Principal_Component)) {
      
      current_data <- dataset[
        dataset[[tract_column]] == tract &
          dataset$Principal_Component == PC,
      ]
      
      for (group_col in c("laterality")) {
        for (covariate_case in covariate_cases) {
          
          formula <- build_formula(covariate_case, group_col)
          
          lm_model      <- lm(formula, data = current_data)
          summary_model <- anova(lm_model)
          
          p_values <- summary_model$`Pr(>F)`
          terms    <- rownames(summary_model)
          
          summary_data <- data.frame(
            Tract          = tract,
            PC             = PC,
            Dataset        = dataset_name,
            Group_Type     = "Laterality_3groups",
            Covariate_Case = covariate_case,
            Term           = terms,
            P_Value        = p_values
          )
          
          ancova_results <- rbind(ancova_results, summary_data)
        }
      }
    }
  }
}

ancova_results <- ancova_results %>%
  group_by(Covariate_Case, Term) %>%
  mutate(FDR_P_Value = p.adjust(P_Value, method = "fdr")) %>%
  ungroup()

write.csv(
  ancova_results,
  "ancova_results_laterality3groups_CC_AF.csv",
  row.names = FALSE
)

## ---------------------------
## Cross-dominance and grouped dominance indices
## ---------------------------

LIs <- read.csv("BILGIN_LIs.csv")

determine_dominance <- function(frontal, temporal) {
  if ((frontal == "leftLat" && temporal == "rightLat") ||
      (frontal == "rightLat" && temporal == "leftLat")) {
    return("crossed_dominance")
  } else if (frontal == temporal) {
    return("consistent")
  } else if ("biLat" %in% c(frontal, temporal)) {
    return("weak_lat")
  } else {
    return("unknown")
  }
}

LIs$crossed_dominance <- mapply(
  determine_dominance,
  LIs$laterality_frontal_0.1,
  LIs$laterality_temporal_0.1
)

# Two-group version for analysis: consistent vs crossed_dominance
LIs$grouped_dominance <- ifelse(
  LIs$crossed_dominance %in% c("consistent", "weak_lat"),
  "consistent",
  LIs$crossed_dominance
)

LIs$crossed_dominance_extent <- abs(abs(LIs$frontal_LI) - abs(LIs$Temporal_LI))

names(LIs)[names(LIs) == "Participants_list_ieva"] <- "subject"

long_data_PC_CC_FA_small_parts <- merge(
  long_data_PC_CC_FA_small_parts,
  LIs[, c(
    "subject",
    "crossed_dominance",
    "grouped_dominance",
    "laterality_frontal_0.1",
    "laterality_temporal_0.1",
    "crossed_dominance_extent"
  )],
  by = "subject",
  all.x = TRUE
)

long_data_AF_full_PC <- merge(
  long_data_AF_full_PC,
  LIs[, c(
    "subject",
    "crossed_dominance",
    "grouped_dominance",
    "laterality_frontal_0.1",
    "laterality_temporal_0.1",
    "crossed_dominance_extent"
  )],
  by = "subject",
  all.x = TRUE
)

## ---------------------------
## ANCOVA – crossed vs grouped dominance
## ---------------------------

ancova_results_crossed <- data.frame()
ancova_results_grouped <- data.frame()

datasets <- list(
  CC = long_data_PC_CC_FA_small_parts,
  AF = long_data_AF_full_PC
)

for (dataset_name in names(datasets)) {
  
  dataset      <- datasets[[dataset_name]]
  tract_column <- ifelse(dataset_name == "CC", "CC_part", "tract")
  
  for (tract in unique(dataset[[tract_column]])) {
    for (PC in unique(dataset$Principal_Component)) {
      
      current_data <- dataset[
        dataset[[tract_column]] == tract &
          dataset$Principal_Component == PC,
      ]
      
      for (group_col in c("crossed_dominance", "grouped_dominance")) {
        for (covariate_case in covariate_cases) {
          
          formula       <- build_formula(covariate_case, group_col)
          lm_model      <- lm(formula, data = current_data)
          summary_model <- anova(lm_model)
          
          p_values <- summary_model$`Pr(>F)`
          terms    <- rownames(summary_model)
          
          summary_data <- data.frame(
            Tract          = tract,
            PC             = PC,
            Dataset        = dataset_name,
            Group_Type     = ifelse(
              group_col == "crossed_dominance",
              "CrossedDominance_3groups",
              "CrossedDominance_2groups"
            ),
            Covariate_Case = covariate_case,
            Term           = terms,
            P_Value        = p_values
          )
          
          if (group_col == "crossed_dominance") {
            ancova_results_crossed <- rbind(ancova_results_crossed, summary_data)
          } else {
            ancova_results_grouped <- rbind(ancova_results_grouped, summary_data)
          }
        }
      }
    }
  }
}

ancova_results_crossed <- ancova_results_crossed %>%
  group_by(Covariate_Case, Term) %>%
  mutate(FDR_P_Value = p.adjust(P_Value, method = "fdr")) %>%
  ungroup()

ancova_results_grouped <- ancova_results_grouped %>%
  group_by(Covariate_Case, Term) %>%
  mutate(FDR_P_Value = p.adjust(P_Value, method = "fdr")) %>%
  ungroup()

write.csv(
  ancova_results_crossed,
  "ancova_results_CrossedDominance3groups_CC_AF.csv",
  row.names = FALSE
)

write.csv(
  ancova_results_grouped,
  "ancova_results_CrossedDominance2groups_CC_AF.csv",
  row.names = FALSE
)

## ---------------------------
## Regression within dominance subgroups (grouped_dominance)
## ---------------------------

regression_results_dominance <- data.frame()

build_regression_formula <- function(covariate_case) {
  if (covariate_case == "no_covariates") {
    Value ~ mean
  } else if (covariate_case == "age_sex") {
    Value ~ mean + age_IRM_Anat + Sexe
  } else if (covariate_case == "age_sex_handedness") {
    Value ~ mean + age_IRM_Anat + Sexe + Edinburg.Score
  }
}

datasets <- list(
  CC = long_data_PC_CC_FA_small_parts,
  AF = long_data_AF_full_PC
)

for (dataset_name in names(datasets)) {
  
  dataset      <- datasets[[dataset_name]]
  tract_column <- ifelse(dataset_name == "CC", "CC_part", "tract")
  
  dataset <- dataset %>%
    mutate(grouped_dominance = factor(grouped_dominance))
  
  for (dom_level in levels(dataset$grouped_dominance)) {
    for (tract in unique(dataset[[tract_column]])) {
      for (pc in unique(dataset$Principal_Component)) {
        
        current_data <- dataset %>%
          filter(
            grouped_dominance == dom_level,
            .data[[tract_column]] == tract,
            Principal_Component == pc
          )
        
        if (nrow(current_data) < 5) next
        
        for (cov_case in covariate_cases) {
          
          formula       <- build_regression_formula(cov_case)
          lm_model      <- lm(formula, data = current_data)
          model_summary <- summary(lm_model)
          
          coef_df <- broom::tidy(lm_model) %>%
            mutate(
              Dataset       = dataset_name,
              Tract         = tract,
              PC            = pc,
              Grouping      = "CrossedDominance_2groups",
              Group_Level   = dom_level,
              CovariateCase = cov_case,
              R2            = model_summary$r.squared
            )
          
          regression_results_dominance <- dplyr::bind_rows(
            regression_results_dominance,
            coef_df
          )
        }
      }
    }
  }
}

regression_results_dominance <- regression_results_dominance %>%
  dplyr::group_by(term, CovariateCase) %>%
  dplyr::mutate(p_FDR = p.adjust(p.value, method = "fdr")) %>%
  dplyr::ungroup()

write.csv(
  regression_results_dominance,
  "Regression_within_CrossedDominance2groups_CC_AF.csv",
  row.names = FALSE
)

## ---------------------------
## Regression – laterality (three groups only)
## ---------------------------

regression_results <- data.frame()

datasets <- list(
  CC = long_data_PC_CC_FA_small_parts,
  AF = long_data_AF_full_PC
)

for (dataset_name in names(datasets)) {
  
  dataset      <- datasets[[dataset_name]]
  tract_column <- ifelse(dataset_name == "CC", "CC_part", "tract")
  
  group_col <- "laterality"
  if (!(group_col %in% colnames(dataset))) next
  
  for (group_level in unique(dataset[[group_col]])) {
    for (tract in unique(dataset[[tract_column]])) {
      for (PC in unique(dataset$Principal_Component)) {
        for (covariate_case in covariate_cases) {
          
          current_data <- dataset[
            dataset[[group_col]]      == group_level &
              dataset[[tract_column]] == tract &
              dataset$Principal_Component == PC,
          ]
          
          if (nrow(current_data) < 3) next
          
          if (covariate_case == "no_covariates") {
            formula <- Value ~ mean
          } else if (covariate_case == "age_sex") {
            formula <- Value ~ mean + age_IRM_Anat + Sexe
          } else if (covariate_case == "age_sex_handedness") {
            formula <- Value ~ mean + age_IRM_Anat + Sexe + Edinburg.Score
          }
          
          lm_model      <- lm(formula, data = current_data)
          model_summary <- summary(lm_model)
          terms         <- rownames(coef(model_summary))
          coefficients  <- coef(model_summary)
          p_values      <- coefficients[, "Pr(>|t|)"]
          r_squared     <- model_summary$r.squared
          
          regression_results <- rbind(
            regression_results,
            data.frame(
              Dataset        = dataset_name,
              Group_Column   = "Laterality_3groups",
              Group_Level    = group_level,
              Tract          = tract,
              PC             = PC,
              Covariate_Case = covariate_case,
              Term           = terms,
              P_Value        = p_values,
              R              = r_squared
            )
          )
        }
      }
    }
  }
}

regression_results <- regression_results %>%
  dplyr::group_by(Group_Column, Covariate_Case, Term) %>%
  dplyr::mutate(FDR_P_Value = p.adjust(P_Value, method = "fdr")) %>%
  dplyr::ungroup()

write.csv(
  regression_results,
  "regression_results_Laterality3groups_CC_AF.csv",
  row.names = FALSE
)

