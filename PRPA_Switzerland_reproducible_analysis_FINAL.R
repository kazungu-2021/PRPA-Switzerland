# ==============================================================================
# Perceived Forest Restoration Priority Analysis (PRPA)
# Switzerland national online-panel survey
# Reproducible analysis script
#
# 
#Input:
#   03_clean_data/CH_SwissStudy_clean.rds
#You run this to locate the dataset
# Outputs:
#   04_outputs/tables/
#   04_outputs/figures/
#   04_outputs/diagnostics/
#
# Notes:
#   - This script starts from the cleaned survey dataset.
#   - It does not install packages. Install required packages once before running.
#   - "Don't know" and other non-substantive responses are assumed to have
#     already been recoded to NA in the cleaned dataset.
#   - F21 (years living in the local area) is harmonised here using the final
#     rule used in the manuscript analysis.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Packages and folders
# ------------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "haven",
  "janitor",
  "psych",
  "ggplot2",
  "car",
  "effectsize",
  "rstatix"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(haven)
library(janitor)
library(psych)
library(ggplot2)
library(car)
library(effectsize)
library(rstatix)

dir.create("04_outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("04_outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("04_outputs/diagnostics", recursive = TRUE, showWarnings = FALSE)

input_file <- "03_clean_data/CH_SwissStudy_clean.rds"

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

# Helper: remove Haven value labels while preserving numeric codes
to_numeric <- function(x) {
  as.numeric(haven::zap_labels(x))
}

format_p <- function(p) {
  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
}

# ------------------------------------------------------------------------------
# 1. Load cleaned data and create analysis dataset
# ------------------------------------------------------------------------------

CH_SwissStudy_clean <- readRDS(
  "03_clean_data/CH_SwissStudy_clean.rds"
)

# Final F21 harmonisation used in the analysis.
# Some respondents entered a calendar year rather than number of years.
# Values >100 or > respondent age + 1 are treated as invalid.
survey_year <- 2025L

CH_SwissStudy_analysis <- CH_SwissStudy_clean %>%
  mutate(
    F21_original = to_numeric(F21),
    F24 = to_numeric(F24),
    F21 = case_when(
      F21_original >= 1900 & F21_original <= survey_year ~
        survey_year - F21_original,
      F21_original > 100 ~ NA_real_,
      F21_original > F24 + 1 ~ NA_real_,
      TRUE ~ F21_original
    ),
    Gender = haven::as_factor(F25),
    Education = haven::as_factor(F26),
    Forest_visit_frequency = haven::as_factor(F1a),
    Travel_time_to_forest = haven::as_factor(F1c),
    Restoration_awareness = haven::as_factor(F3),
    Forest_owner = haven::as_factor(F22),
    Social_proximity_to_owner = haven::as_factor(F23)
  )

# Variables used in the PRPA analysis
ecosystem_vars <- paste0("F5r", 1:8)
implementation_vars <- paste0("F15r", 1:14)
support_vars <- paste0("F16br", 1:6)
outcome_vars_items <- paste0("F17r", 1:16)
nature_vars <- paste0("F18r", 1:7)

required_analysis_vars <- c(
  ecosystem_vars,
  implementation_vars,
  support_vars,
  outcome_vars_items,
  nature_vars,
  "F19r1", "F19r2", "F19r3", "F19r4",
  "F20r1", "F20r2", "F20r3", "F20r4",
  "F21", "F24",
  "Gender", "Education", "Forest_visit_frequency",
  "Travel_time_to_forest", "Restoration_awareness",
  "Forest_owner", "Social_proximity_to_owner"
)

missing_vars <- setdiff(required_analysis_vars, names(CH_SwissStudy_analysis))

if (length(missing_vars) > 0) {
  stop(
    "Required variables are missing from the dataset: ",
    paste(missing_vars, collapse = ", ")
  )
}

# Convert all multi-item PRPA scales to ordinary numeric values
CH_SwissStudy_analysis <- CH_SwissStudy_analysis %>%
  mutate(
    across(
      all_of(c(
        ecosystem_vars,
        implementation_vars,
        support_vars,
        outcome_vars_items,
        nature_vars
      )),
      to_numeric
    )
  )

# ------------------------------------------------------------------------------
# 2. Respondent characteristics (Table 1)
# ------------------------------------------------------------------------------

categorical_summary <- function(data, variable, characteristic) {
  x <- data[[variable]]

  if (inherits(x, "haven_labelled")) {
    x <- haven::as_factor(x)
  }

  frequency_table <- table(x, useNA = "no")

  data.frame(
    Characteristic = characteristic,
    Category = names(frequency_table),
    n = as.integer(frequency_table),
    Percent = round(
      100 * as.integer(frequency_table) / sum(frequency_table),
      1
    ),
    Value = NA_character_,
    stringsAsFactors = FALSE
  )
}

age_summary <- data.frame(
  Characteristic = "Age, years",
  Category = "Mean (SD)",
  n = sum(!is.na(CH_SwissStudy_analysis$F24)),
  Percent = NA_real_,
  Value = sprintf(
    "%.1f (%.1f)",
    mean(CH_SwissStudy_analysis$F24, na.rm = TRUE),
    sd(CH_SwissStudy_analysis$F24, na.rm = TRUE)
  )
)

age_median <- data.frame(
  Characteristic = "",
  Category = "Median (range)",
  n = NA_integer_,
  Percent = NA_real_,
  Value = sprintf(
    "%.1f (%g–%g)",
    median(CH_SwissStudy_analysis$F24, na.rm = TRUE),
    min(CH_SwissStudy_analysis$F24, na.rm = TRUE),
    max(CH_SwissStudy_analysis$F24, na.rm = TRUE)
  )
)

residence_summary <- data.frame(
  Characteristic = "Years living in the local area",
  Category = "Mean (SD)",
  n = sum(!is.na(CH_SwissStudy_analysis$F21)),
  Percent = NA_real_,
  Value = sprintf(
    "%.1f (%.1f)",
    mean(CH_SwissStudy_analysis$F21, na.rm = TRUE),
    sd(CH_SwissStudy_analysis$F21, na.rm = TRUE)
  )
)

residence_median <- data.frame(
  Characteristic = "",
  Category = "Median (range)",
  n = NA_integer_,
  Percent = NA_real_,
  Value = sprintf(
    "%.1f (%g–%g)",
    median(CH_SwissStudy_analysis$F21, na.rm = TRUE),
    min(CH_SwissStudy_analysis$F21, na.rm = TRUE),
    max(CH_SwissStudy_analysis$F21, na.rm = TRUE)
  )
)

categorical_tables <- list(
  categorical_summary(CH_SwissStudy_analysis, "Gender", "Gender"),
  categorical_summary(CH_SwissStudy_analysis, "Education", "Educational attainment"),
  categorical_summary(
    CH_SwissStudy_analysis,
    "Forest_visit_frequency",
    "Forest visitation frequency"
  ),
  categorical_summary(
    CH_SwissStudy_analysis,
    "Travel_time_to_forest",
    "Travel time to forest"
  ),
  categorical_summary(
    CH_SwissStudy_analysis,
    "Restoration_awareness",
    "Awareness of local restoration programmes"
  ),
  categorical_summary(CH_SwissStudy_analysis, "Forest_owner", "Forest ownership"),
  categorical_summary(
    CH_SwissStudy_analysis,
    "Social_proximity_to_owner",
    "Relative or close friend owns a forest"
  )
)

profession_summary <- data.frame(
  Characteristic = "Forest- or environment-related profession",
  Category = c(
    "Environment or nature protection",
    "Farming",
    "Forestry",
    "None of the above"
  ),
  n = c(
    sum(to_numeric(CH_SwissStudy_analysis$F19r1) == 1, na.rm = TRUE),
    sum(to_numeric(CH_SwissStudy_analysis$F19r2) == 1, na.rm = TRUE),
    sum(to_numeric(CH_SwissStudy_analysis$F19r3) == 1, na.rm = TRUE),
    sum(to_numeric(CH_SwissStudy_analysis$F19r4) == 1, na.rm = TRUE)
  )
) %>%
  mutate(
    Percent = round(100 * n / nrow(CH_SwissStudy_analysis), 1),
    Value = NA_character_
  )

organisation_summary <- data.frame(
  Characteristic = "Membership of a forest- or environment-related organisation",
  Category = c(
    "Environment or nature protection",
    "Farming",
    "Forestry",
    "None of the above"
  ),
  n = c(
    sum(to_numeric(CH_SwissStudy_analysis$F20r1) == 1, na.rm = TRUE),
    sum(to_numeric(CH_SwissStudy_analysis$F20r2) == 1, na.rm = TRUE),
    sum(to_numeric(CH_SwissStudy_analysis$F20r3) == 1, na.rm = TRUE),
    sum(to_numeric(CH_SwissStudy_analysis$F20r4) == 1, na.rm = TRUE)
  )
) %>%
  mutate(
    Percent = round(100 * n / nrow(CH_SwissStudy_analysis), 1),
    Value = NA_character_
  )

Table_1_Respondent_characteristics <- bind_rows(
  age_summary,
  age_median,
  categorical_tables[[1]],
  categorical_tables[[2]],
  categorical_tables[[3]],
  categorical_tables[[4]],
  residence_summary,
  residence_median,
  categorical_tables[[5]],
  categorical_tables[[6]],
  categorical_tables[[7]],
  profession_summary,
  organisation_summary
) %>%
  select(Characteristic, Category, n, Percent, Value)

readr::write_excel_csv(
  Table_1_Respondent_characteristics,
  "04_outputs/tables/Table_1_Respondent_characteristics.csv",
  na = ""
)

# ------------------------------------------------------------------------------
# 3. Reliability of the four PRPA dimensions (Table 2)
# ------------------------------------------------------------------------------

ecosystem_services <- CH_SwissStudy_analysis %>% select(all_of(ecosystem_vars))
restoration_practices <- CH_SwissStudy_analysis %>% select(all_of(implementation_vars))
public_support <- CH_SwissStudy_analysis %>% select(all_of(support_vars))
expected_outcomes <- CH_SwissStudy_analysis %>% select(all_of(outcome_vars_items))

alpha_ecosystem <- psych::alpha(ecosystem_services)
alpha_practices <- psych::alpha(restoration_practices)
alpha_support <- psych::alpha(public_support)
alpha_outcomes <- psych::alpha(expected_outcomes)

Table_2_Reliability_statistics <- data.frame(
  Dimension = c(
    "Forest ecosystem services",
    "Restoration implementation",
    "Expected restoration outcomes",
    "Public support"
  ),
  Items = c(
    ncol(ecosystem_services),
    ncol(restoration_practices),
    ncol(expected_outcomes),
    ncol(public_support)
  ),
  Cronbach_alpha = round(
    c(
      alpha_ecosystem$total$raw_alpha,
      alpha_practices$total$raw_alpha,
      alpha_outcomes$total$raw_alpha,
      alpha_support$total$raw_alpha
    ),
    3
  )
)

readr::write_excel_csv(
  Table_2_Reliability_statistics,
  "04_outputs/tables/Table_2_Reliability_statistics.csv",
  na = ""
)

saveRDS(alpha_ecosystem, "04_outputs/tables/alpha_ecosystem.rds")
saveRDS(alpha_practices, "04_outputs/tables/alpha_practices.rds")
saveRDS(alpha_support, "04_outputs/tables/alpha_support.rds")
saveRDS(alpha_outcomes, "04_outputs/tables/alpha_outcomes.rds")

# ------------------------------------------------------------------------------
# 4. Ranking tables (Tables 3–6)
# ------------------------------------------------------------------------------

rank_items <- function(data, variables, labels, include_n = TRUE) {
  out <- data.frame(
    Variable = variables,
    Item = labels,
    stringsAsFactors = FALSE
  ) %>%
    rowwise() %>%
    mutate(
      Valid_n = sum(!is.na(data[[Variable]])),
      Mean = mean(data[[Variable]], na.rm = TRUE),
      SD = sd(data[[Variable]], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(desc(Mean)) %>%
    mutate(
      Rank = row_number(),
      `Mean (SD)` = sprintf("%.2f (%.2f)", Mean, SD)
    )

  if (include_n) {
    out %>% select(Rank, Item, Valid_n, `Mean (SD)`)
  } else {
    out %>%
      mutate(
        Mean = round(Mean, 2),
        SD = round(SD, 2)
      ) %>%
      select(Rank, Item, Mean, SD)
  }
}

F5_labels <- c(
  "Cooling temperature",
  "Air purification",
  "Water purification",
  "Natural hazard protection",
  "Wood production",
  "Food, medicinal herbs and ornamental plants",
  "Leisure and recreation",
  "Habitat for biodiversity"
)

F15_labels <- c(
  "Reforestation of previously deforested areas",
  "Reforestation of fallow agricultural land",
  "Reforestation after natural disasters",
  "Re-introduction of native tree species",
  "Re-introduction of native animal species",
  "Removal of invasive species",
  "Introduction of fire breaks",
  "Retention of deadwood",
  "Replanting a mix of tree species",
  "Improving forest connectivity",
  "Improving forest access for recreation",
  "Restricting the harvesting of forest products",
  "Reducing fuel loads to minimise fire risk",
  "Changing the type of forest management"
)

F17_labels <- c(
  "Firewood",
  "Wood for timber",
  "Food for animals",
  "Ornamental plants",
  "Berries",
  "Other edible fruits",
  "Mushrooms",
  "Habitat for species",
  "Forest beauty and aesthetics",
  "Cooler temperature",
  "Fresh air",
  "Clean water",
  "Space for hunting and fishing",
  "Space for outdoor recreation",
  "Space for social activities",
  "Aesthetic views"
)

F16_labels <- c(
  "Volunteering for tree planting and maintenance",
  "Participating in awareness campaigns",
  "Advocating for forest restoration policies",
  "Engaging in community restoration initiatives",
  "Donating money to restoration projects",
  "Supporting the use of taxpayers' money for restoration"
)

Table_3_ecosystem_services <- rank_items(
  CH_SwissStudy_analysis,
  ecosystem_vars,
  F5_labels,
  include_n = FALSE
)

Table_4_restoration_implementation <- rank_items(
  CH_SwissStudy_analysis,
  implementation_vars,
  F15_labels,
  include_n = TRUE
)

Table_5_expected_outcomes <- rank_items(
  CH_SwissStudy_analysis,
  outcome_vars_items,
  F17_labels,
  include_n = TRUE
)

Table_6_public_support <- rank_items(
  CH_SwissStudy_analysis,
  support_vars,
  F16_labels,
  include_n = TRUE
)

readr::write_excel_csv(
  Table_3_ecosystem_services,
  "04_outputs/tables/Table_3_Ecosystem_services_ranking.csv",
  na = ""
)

readr::write_excel_csv(
  Table_4_restoration_implementation,
  "04_outputs/tables/Table_4_Restoration_implementation_ranking.csv",
  na = ""
)

readr::write_excel_csv(
  Table_5_expected_outcomes,
  "04_outputs/tables/Table_5_Expected_restoration_outcomes_ranking.csv",
  na = ""
)

readr::write_excel_csv(
  Table_6_public_support,
  "04_outputs/tables/Table_6_Public_support_ranking.csv",
  na = ""
)

# ------------------------------------------------------------------------------
# 5. Distribution figures for the four PRPA dimensions
#    Figure 1 is the conceptual PRPA framework in the manuscript; therefore,
#    empirical figures generated here are numbered Figures 2–5.
# ------------------------------------------------------------------------------

theme_prpa <- theme_minimal(base_size = 11) +
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    plot.margin = margin(t = 10, r = 18, b = 10, l = 10)
  )

make_distribution_figure <- function(
    data,
    label_map,
    category_function,
    category_levels,
    palette,
    output_stem,
    legend_rows = 1,
    text_size = 2.8
) {
  variables <- names(label_map)

  long_data <- data %>%
    select(all_of(variables)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Variable",
      values_to = "Score"
    ) %>%
    mutate(
      Item = dplyr::recode(Variable, !!!label_map),
      Response_category = category_function(Score),
      Response_category = factor(
        Response_category,
        levels = category_levels
      )
    ) %>%
    filter(!is.na(Response_category))

  summary_data <- long_data %>%
    count(Item, Response_category, name = "n") %>%
    complete(
      Item,
      Response_category,
      fill = list(n = 0)
    ) %>%
    group_by(Item) %>%
    mutate(
      Valid_n = sum(n),
      Percent = 100 * n / Valid_n
    ) %>%
    ungroup()

  item_order <- data %>%
    summarise(
      across(
        all_of(variables),
        ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Variable",
      values_to = "Mean"
    ) %>%
    mutate(
      Item = dplyr::recode(Variable, !!!label_map)
    ) %>%
    arrange(Mean) %>%
    pull(Item)

  summary_data <- summary_data %>%
    mutate(
      Item = factor(Item, levels = item_order)
    )

  figure <- ggplot(
    summary_data,
    aes(
      x = Item,
      y = Percent,
      fill = Response_category
    )
  ) +
    geom_col(
      width = 0.72,
      colour = "white",
      linewidth = 0.25
    ) +
    geom_text(
      aes(
        label = ifelse(
          Percent >= 5,
          sprintf("%.1f%%", Percent),
          ""
        )
      ),
      position = position_stack(vjust = 0.5),
      size = text_size
    ) +
    coord_flip() +
    scale_fill_manual(
      values = palette,
      drop = FALSE
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%"),
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = NULL,
      y = "Percentage of respondents",
      fill = NULL
    ) +
    theme_prpa +
    guides(
      fill = guide_legend(
        nrow = legend_rows,
        byrow = TRUE
      )
    )

  ggsave(
    filename = paste0("04_outputs/figures/", output_stem, ".pdf"),
    plot = figure,
    width = 8.5,
    height = 6.0,
    units = "in"
  )

  ggsave(
    filename = paste0("04_outputs/figures/", output_stem, ".tiff"),
    plot = figure,
    width = 8.5,
    height = 6.0,
    units = "in",
    dpi = 600,
    compression = "lzw"
  )

  readr::write_excel_csv(
    summary_data %>% arrange(Item, Response_category),
    paste0("04_outputs/tables/", output_stem, "_data.csv"),
    na = ""
  )

  list(
    plot = figure,
    summary = summary_data
  )
}

F5_label_map <- setNames(
  c(
    "Temperature regulation",
    "Air purification",
    "Water purification",
    "Protection against natural hazards",
    "Wood production",
    "Food, medicinal plants and ornamental products",
    "Leisure and recreation",
    "Habitat for biodiversity"
  ),
  ecosystem_vars
)

F15_label_map <- setNames(F15_labels, implementation_vars)

F17_label_map <- setNames(
  c(
    "Firewood",
    "Wood for timber",
    "Animal fodder",
    "Ornamental plants",
    "Berries",
    "Wild edible fruits",
    "Mushrooms",
    "Habitat for biodiversity",
    "Forest beauty and aesthetics",
    "Temperature regulation",
    "Fresh air",
    "Clean water",
    "Opportunities for hunting and fishing",
    "Outdoor recreation opportunities",
    "Social and family activities",
    "Aesthetic views"
  ),
  outcome_vars_items
)

F16_label_map <- setNames(
  c(
    "Volunteering for tree planting and maintenance",
    "Participating in awareness campaigns",
    "Advocating for forest restoration policies",
    "Participating in community restoration initiatives",
    "Donating money to restoration projects",
    "Supporting public funding for forest restoration"
  ),
  support_vars
)

Figure_2_result <- make_distribution_figure(
  data = CH_SwissStudy_analysis,
  label_map = F5_label_map,
  category_function = function(x) {
    case_when(
      x >= 0 & x <= 2 ~ "Very low",
      x >= 3 & x <= 4 ~ "Low",
      x >= 5 & x <= 6 ~ "Moderate",
      x >= 7 & x <= 8 ~ "High",
      x >= 9 & x <= 10 ~ "Very high",
      TRUE ~ NA_character_
    )
  },
  category_levels = c(
    "Very low", "Low", "Moderate", "High", "Very high"
  ),
  palette = c(
    "Very low" = "#B2182B",
    "Low" = "#EF8A62",
    "Moderate" = "#D9D9D9",
    "High" = "#A6D96A",
    "Very high" = "#1A9850"
  ),
  output_stem = "Figure_2_Ecosystem_service_importance",
  legend_rows = 1,
  text_size = 3.0
)

Figure_3_result <- make_distribution_figure(
  data = CH_SwissStudy_analysis,
  label_map = F15_label_map,
  category_function = function(x) {
    case_when(
      x == 1 ~ "Never",
      x == 2 ~ "Rarely",
      x == 3 ~ "Sometimes",
      x == 4 ~ "Often",
      x == 5 ~ "Always",
      TRUE ~ NA_character_
    )
  },
  category_levels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
  palette = c(
    "Never" = "#B2182B",
    "Rarely" = "#EF8A62",
    "Sometimes" = "#D9D9D9",
    "Often" = "#A6D96A",
    "Always" = "#1A9850"
  ),
  output_stem = "Figure_3_Restoration_implementation",
  legend_rows = 1
)

Figure_4_result <- make_distribution_figure(
  data = CH_SwissStudy_analysis,
  label_map = F17_label_map,
  category_function = function(x) {
    case_when(
      x == 1 ~ "Significantly reduces",
      x == 2 ~ "Moderately reduces",
      x == 3 ~ "No significant impact",
      x == 4 ~ "Moderately enhances",
      x == 5 ~ "Significantly enhances",
      TRUE ~ NA_character_
    )
  },
  category_levels = c(
    "Significantly reduces",
    "Moderately reduces",
    "No significant impact",
    "Moderately enhances",
    "Significantly enhances"
  ),
  palette = c(
    "Significantly reduces" = "#B2182B",
    "Moderately reduces" = "#EF8A62",
    "No significant impact" = "#D9D9D9",
    "Moderately enhances" = "#A6D96A",
    "Significantly enhances" = "#1A9850"
  ),
  output_stem = "Figure_4_Expected_restoration_outcomes",
  legend_rows = 2
)

Figure_5_result <- make_distribution_figure(
  data = CH_SwissStudy_analysis,
  label_map = F16_label_map,
  category_function = function(x) {
    case_when(
      x == 1 ~ "Completely disagree",
      x == 2 ~ "Slightly disagree",
      x == 3 ~ "Neither agree nor disagree",
      x == 4 ~ "Slightly agree",
      x == 5 ~ "Completely agree",
      TRUE ~ NA_character_
    )
  },
  category_levels = c(
    "Completely disagree",
    "Slightly disagree",
    "Neither agree nor disagree",
    "Slightly agree",
    "Completely agree"
  ),
  palette = c(
    "Completely disagree" = "#B2182B",
    "Slightly disagree" = "#EF8A62",
    "Neither agree nor disagree" = "#D9D9D9",
    "Slightly agree" = "#A6D96A",
    "Completely agree" = "#1A9850"
  ),
  output_stem = "Figure_5_Public_support",
  legend_rows = 2,
  text_size = 3.0
)

Figure_2 <- Figure_2_result$plot
Figure_3 <- Figure_3_result$plot
Figure_4 <- Figure_4_result$plot
Figure_5 <- Figure_5_result$plot

# ------------------------------------------------------------------------------
# 6. Composite indices for explanatory analyses
# ------------------------------------------------------------------------------

CH_SwissStudy_analysis <- CH_SwissStudy_analysis %>%
  mutate(
    Ecosystem_Services_valid_items =
      rowSums(!is.na(across(all_of(ecosystem_vars)))),

    Restoration_Implementation_valid_items =
      rowSums(!is.na(across(all_of(implementation_vars)))),

    Expected_Outcomes_valid_items =
      rowSums(!is.na(across(all_of(outcome_vars_items)))),

    Public_Support_valid_items =
      rowSums(!is.na(across(all_of(support_vars)))),

    Nature_Connectedness_valid_items =
      rowSums(!is.na(across(all_of(nature_vars)))),

    # Mean index calculated when at least half of items are valid
    Ecosystem_Services_Index = if_else(
      Ecosystem_Services_valid_items >= 4,
      rowMeans(across(all_of(ecosystem_vars)), na.rm = TRUE),
      NA_real_
    ),

    Restoration_Implementation_Index = if_else(
      Restoration_Implementation_valid_items >= 7,
      rowMeans(across(all_of(implementation_vars)), na.rm = TRUE),
      NA_real_
    ),

    Expected_Outcomes_Index = if_else(
      Expected_Outcomes_valid_items >= 8,
      rowMeans(across(all_of(outcome_vars_items)), na.rm = TRUE),
      NA_real_
    ),

    Public_Support_Index = if_else(
      Public_Support_valid_items >= 3,
      rowMeans(across(all_of(support_vars)), na.rm = TRUE),
      NA_real_
    ),

    Nature_Connectedness_Index = if_else(
      Nature_Connectedness_valid_items >= 4,
      rowMeans(across(all_of(nature_vars)), na.rm = TRUE),
      NA_real_
    )
  )

index_names <- c(
  "Ecosystem_Services_Index",
  "Restoration_Implementation_Index",
  "Expected_Outcomes_Index",
  "Public_Support_Index",
  "Nature_Connectedness_Index"
)

index_labels <- c(
  "Perceived importance of ecosystem services",
  "Perceived restoration implementation",
  "Expected restoration outcomes",
  "Public support for restoration",
  "Nature connectedness"
)

Composite_Index_Summary <- data.frame(
  Index = index_labels,
  Valid_n = sapply(
    CH_SwissStudy_analysis[index_names],
    function(x) sum(!is.na(x))
  ),
  Missing_n = sapply(
    CH_SwissStudy_analysis[index_names],
    function(x) sum(is.na(x))
  ),
  Mean = sapply(
    CH_SwissStudy_analysis[index_names],
    function(x) mean(x, na.rm = TRUE)
  ),
  SD = sapply(
    CH_SwissStudy_analysis[index_names],
    function(x) sd(x, na.rm = TRUE)
  ),
  Median = sapply(
    CH_SwissStudy_analysis[index_names],
    function(x) median(x, na.rm = TRUE)
  ),
  Minimum = sapply(
    CH_SwissStudy_analysis[index_names],
    function(x) min(x, na.rm = TRUE)
  ),
  Maximum = sapply(
    CH_SwissStudy_analysis[index_names],
    function(x) max(x, na.rm = TRUE)
  )
) %>%
  mutate(
    across(
      c(Mean, SD, Median, Minimum, Maximum),
      ~ round(.x, 2)
    ),
    `Mean (SD)` = sprintf("%.2f (%.2f)", Mean, SD)
  ) %>%
  select(
    Index,
    Valid_n,
    Missing_n,
    `Mean (SD)`,
    Median,
    Minimum,
    Maximum
  )

readr::write_excel_csv(
  Composite_Index_Summary,
  "04_outputs/tables/Composite_Index_Summary.csv",
  na = ""
)

# ------------------------------------------------------------------------------
# 7. Explanatory variables and regrouping
# ------------------------------------------------------------------------------

F19_any_related <- (
  to_numeric(CH_SwissStudy_analysis$F19r1) == 1 |
    to_numeric(CH_SwissStudy_analysis$F19r2) == 1 |
    to_numeric(CH_SwissStudy_analysis$F19r3) == 1
)

F20_any_related <- (
  to_numeric(CH_SwissStudy_analysis$F20r1) == 1 |
    to_numeric(CH_SwissStudy_analysis$F20r2) == 1 |
    to_numeric(CH_SwissStudy_analysis$F20r3) == 1
)

CH_SwissStudy_analysis <- CH_SwissStudy_analysis %>%
  mutate(
    Environmental_profession = factor(
      if_else(
        F19_any_related,
        "Related profession",
        "No related profession",
        missing = NA_character_
      ),
      levels = c("No related profession", "Related profession")
    ),

    Environmental_organisation = factor(
      if_else(
        F20_any_related,
        "Member",
        "Not a member",
        missing = NA_character_
      ),
      levels = c("Not a member", "Member")
    ),

    Education_group = case_when(
      as.character(Education) == "Primary school" ~
        "School-level or other education",
      grepl("^Secondary school", as.character(Education)) ~
        "School-level or other education",
      as.character(Education) == "Others" ~
        "School-level or other education",
      as.character(Education) == "Professional/vocational training" ~
        "Professional/vocational training",
      as.character(Education) == "University" ~
        "University",
      TRUE ~ NA_character_
    ),

    Education_group = factor(
      Education_group,
      levels = c(
        "School-level or other education",
        "Professional/vocational training",
        "University"
      )
    ),

    Forest_visit_group = case_when(
      as.character(Forest_visit_frequency) %in%
        c("Not at all", "Less than once a month") ~
        "Less than monthly",
      as.character(Forest_visit_frequency) == "1-3 times a month" ~
        "Monthly",
      grepl("week", as.character(Forest_visit_frequency), ignore.case = TRUE) ~
        "Weekly or more often",
      TRUE ~ NA_character_
    ),

    Forest_visit_group = factor(
      Forest_visit_group,
      levels = c(
        "Less than monthly",
        "Monthly",
        "Weekly or more often"
      )
    ),

    Travel_time_group = case_when(
      as.character(Travel_time_to_forest) == "Under 10 minutes" ~
        "Under 10 minutes",
      as.character(Travel_time_to_forest) == "10–30 minutes" ~
        "10–30 minutes",
      grepl("^30 minutes", as.character(Travel_time_to_forest)) ~
        "More than 30 minutes",
      as.character(Travel_time_to_forest) == "Longer than 1 hour" ~
        "More than 30 minutes",
      TRUE ~ NA_character_
    ),

    Travel_time_group = factor(
      Travel_time_group,
      levels = c(
        "Under 10 minutes",
        "10–30 minutes",
        "More than 30 minutes"
      )
    )
  )

explanatory_plan <- data.frame(
  Domain = c(
    rep("Demographic characteristics", 3),
    rep("Forest experience", 3),
    "Forest engagement",
    rep("Stakeholder relationship and environmental engagement", 4),
    "Environmental orientation"
  ),
  Explanatory_variable = c(
    "Age",
    "Gender",
    "Educational attainment",
    "Frequency of forest visits",
    "Travel time to forest",
    "Years living in the local area",
    "Awareness of local restoration programmes",
    "Forest ownership",
    "Social proximity to forest ownership",
    "Forest- or environment-related profession",
    "Forest- or environment-related organisation membership",
    "Nature connectedness"
  ),
  Dataset_variable = c(
    "F24",
    "Gender",
    "Education_group",
    "Forest_visit_group",
    "Travel_time_group",
    "F21",
    "Restoration_awareness",
    "Forest_owner",
    "Social_proximity_to_owner",
    "Environmental_profession",
    "Environmental_organisation",
    "Nature_Connectedness_Index"
  ),
  Variable_type = c(
    "Continuous",
    "Categorical",
    "Categorical",
    "Ordinal categorical",
    "Ordinal categorical",
    "Continuous",
    "Categorical",
    "Categorical",
    "Categorical",
    "Categorical",
    "Categorical",
    "Continuous"
  ),
  Proposed_analysis = c(
    "Pearson correlation",
    "Two-group comparison",
    "Multi-group comparison",
    "Multi-group comparison",
    "Multi-group comparison",
    "Pearson correlation",
    "Two-group comparison",
    "Two-group comparison",
    "Multi-group comparison",
    "Two-group comparison",
    "Two-group comparison",
    "Pearson correlation"
  ),
  stringsAsFactors = FALSE
)

readr::write_excel_csv(
  explanatory_plan,
  "04_outputs/tables/Explanatory_Analysis_Plan.csv",
  na = ""
)

regrouped_variable_summary <- bind_rows(
  CH_SwissStudy_analysis %>%
    count(Category = Education_group, name = "n") %>%
    mutate(Variable = "Educational attainment"),

  CH_SwissStudy_analysis %>%
    count(Category = Forest_visit_group, name = "n") %>%
    mutate(Variable = "Frequency of forest visits"),

  CH_SwissStudy_analysis %>%
    count(Category = Travel_time_group, name = "n") %>%
    mutate(Variable = "Travel time to forest")
) %>%
  select(Variable, Category, n)

readr::write_excel_csv(
  regrouped_variable_summary,
  "04_outputs/tables/Regrouped_Explanatory_Variables.csv",
  na = ""
)

categorical_explanatory_vars <- c(
  "Gender",
  "Education_group",
  "Forest_visit_group",
  "Travel_time_group",
  "Restoration_awareness",
  "Forest_owner",
  "Social_proximity_to_owner",
  "Environmental_profession",
  "Environmental_organisation"
)

group_size_summary <- purrr::map_dfr(
  categorical_explanatory_vars,
  function(variable_name) {
    data.frame(
      Explanatory_variable = variable_name,
      Category = as.character(CH_SwissStudy_analysis[[variable_name]]),
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(Category)) %>%
      count(
        Explanatory_variable,
        Category,
        name = "n"
      )
  }
)

readr::write_excel_csv(
  group_size_summary,
  "04_outputs/tables/Explanatory_Group_Sizes.csv",
  na = ""
)

# Save analysis-ready dataset locally.
# Do not upload individual-level data to a public repository unless permitted.
saveRDS(
  CH_SwissStudy_analysis,
  "03_clean_data/CH_SwissStudy_analysis.rds"
)

# ------------------------------------------------------------------------------
# 8. Distribution diagnostics for composite outcome indices
# ------------------------------------------------------------------------------

diagnostic_outcomes <- c(
  "Ecosystem_Services_Index",
  "Restoration_Implementation_Index",
  "Expected_Outcomes_Index",
  "Public_Support_Index"
)

diagnostic_labels <- c(
  Ecosystem_Services_Index =
    "Perceived importance of ecosystem services",
  Restoration_Implementation_Index =
    "Perceived restoration implementation",
  Expected_Outcomes_Index =
    "Expected restoration outcomes",
  Public_Support_Index =
    "Public support for restoration"
)

diagnostic_long <- CH_SwissStudy_analysis %>%
  select(all_of(diagnostic_outcomes)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Outcome_variable",
    values_to = "Score"
  ) %>%
  mutate(
    Outcome = dplyr::recode(Outcome_variable, !!!diagnostic_labels)
  ) %>%
  filter(!is.na(Score))

Distribution_Diagnostics <- diagnostic_long %>%
  group_by(Outcome_variable, Outcome) %>%
  summarise(
    Valid_n = n(),
    Mean = mean(Score),
    SD = sd(Score),
    Median = median(Score),
    Minimum = min(Score),
    Maximum = max(Score),
    Skewness = psych::skew(Score),
    Kurtosis = psych::kurtosi(Score),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        Mean, SD, Median, Minimum,
        Maximum, Skewness, Kurtosis
      ),
      ~ round(.x, 3)
    ),
    `Mean (SD)` = sprintf("%.2f (%.2f)", Mean, SD)
  ) %>%
  select(
    Outcome,
    Valid_n,
    `Mean (SD)`,
    Median,
    Minimum,
    Maximum,
    Skewness,
    Kurtosis
  )

readr::write_excel_csv(
  Distribution_Diagnostics,
  "04_outputs/tables/Composite_Index_Distribution_Diagnostics.csv",
  na = ""
)

Shapiro_Results <- purrr::map_dfr(
  diagnostic_outcomes,
  function(outcome_name) {
    x <- CH_SwissStudy_analysis[[outcome_name]]
    x <- x[!is.na(x)]
    test_result <- shapiro.test(x)

    data.frame(
      Outcome = unname(diagnostic_labels[outcome_name]),
      Valid_n = length(x),
      W = unname(test_result$statistic),
      p_value = test_result$p.value,
      stringsAsFactors = FALSE
    )
  }
) %>%
  mutate(
    W = round(W, 4),
    p_value_display = format_p(p_value)
  )

readr::write_excel_csv(
  Shapiro_Results,
  "04_outputs/tables/Composite_Index_Shapiro_Wilk_Tests.csv",
  na = ""
)

Histogram_Diagnostics <- ggplot(
  diagnostic_long,
  aes(x = Score)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 25,
    fill = "grey75",
    colour = "white",
    linewidth = 0.25
  ) +
  geom_density(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ Outcome, scales = "free", ncol = 2) +
  labs(
    x = "Composite index score",
    y = "Density"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.text = element_text(colour = "black"),
    panel.grid.minor = element_blank()
  )

QQ_Diagnostics <- ggplot(
  diagnostic_long,
  aes(sample = Score)
) +
  stat_qq(size = 1.1, alpha = 0.55) +
  stat_qq_line(linewidth = 0.7) +
  facet_wrap(~ Outcome, scales = "free", ncol = 2) +
  labs(
    x = "Theoretical quantiles",
    y = "Observed quantiles"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.text = element_text(colour = "black"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "04_outputs/diagnostics/Composite_Index_Histogram_Diagnostics.pdf",
  Histogram_Diagnostics,
  width = 8.5,
  height = 6.5,
  units = "in"
)

ggsave(
  "04_outputs/diagnostics/Composite_Index_Histogram_Diagnostics.tiff",
  Histogram_Diagnostics,
  width = 8.5,
  height = 6.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "04_outputs/diagnostics/Composite_Index_QQ_Diagnostics.pdf",
  QQ_Diagnostics,
  width = 8.5,
  height = 6.5,
  units = "in"
)

ggsave(
  "04_outputs/diagnostics/Composite_Index_QQ_Diagnostics.tiff",
  QQ_Diagnostics,
  width = 8.5,
  height = 6.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

readr::write_excel_csv(
  diagnostic_long,
  "04_outputs/tables/Composite_Index_Diagnostic_Data.csv",
  na = ""
)

# ------------------------------------------------------------------------------
# 9. Categorical explanatory analyses
# ------------------------------------------------------------------------------

outcome_plan <- data.frame(
  Outcome_variable = c(
    "Ecosystem_Services_Index",
    "Restoration_Implementation_Index",
    "Expected_Outcomes_Index",
    "Public_Support_Index"
  ),
  Outcome_label = c(
    "Perceived importance of ecosystem services",
    "Perceived restoration implementation",
    "Expected restoration outcomes",
    "Public support for restoration"
  ),
  stringsAsFactors = FALSE
)

predictor_plan <- data.frame(
  Predictor_variable = categorical_explanatory_vars,
  Predictor_label = c(
    "Gender",
    "Educational attainment",
    "Frequency of forest visits",
    "Travel time to forest",
    "Awareness of local restoration programmes",
    "Forest ownership",
    "Social proximity to forest ownership",
    "Forest- or environment-related profession",
    "Forest- or environment-related organisation membership"
  ),
  stringsAsFactors = FALSE
)

run_categorical_analysis <- function(
    data,
    outcome,
    predictor,
    outcome_label = outcome,
    predictor_label = predictor,
    alpha = 0.05
) {
  analysis_data <- data %>%
    transmute(
      Outcome = .data[[outcome]],
      Group = factor(.data[[predictor]])
    ) %>%
    filter(!is.na(Outcome), !is.na(Group)) %>%
    droplevels()

  number_groups <- nlevels(analysis_data$Group)

  if (number_groups < 2) {
    stop("Predictor has fewer than two valid groups: ", predictor)
  }

  descriptives <- analysis_data %>%
    group_by(Group) %>%
    summarise(
      n = n(),
      Mean = mean(Outcome),
      SD = sd(Outcome),
      SE = SD / sqrt(n),
      CI_lower = Mean - qt(0.975, df = n - 1) * SE,
      CI_upper = Mean + qt(0.975, df = n - 1) * SE,
      .groups = "drop"
    ) %>%
    mutate(
      Outcome = outcome_label,
      Explanatory_variable = predictor_label,
      `Mean (SD)` = sprintf("%.2f (%.2f)", Mean, SD),
      `95% CI` = sprintf("%.2f to %.2f", CI_lower, CI_upper)
    ) %>%
    select(
      Outcome,
      Explanatory_variable,
      Group,
      n,
      `Mean (SD)`,
      `95% CI`,
      Mean,
      SD,
      SE,
      CI_lower,
      CI_upper
    )

  levene_result <- car::leveneTest(
    Outcome ~ Group,
    data = analysis_data,
    center = median
  )

  levene_f <- unname(levene_result[1, "F value"])
  levene_p <- unname(levene_result[1, "Pr(>F)"])
  equal_variances <- levene_p >= alpha

  if (number_groups == 2) {
    test_result <- t.test(
      Outcome ~ Group,
      data = analysis_data,
      var.equal = equal_variances,
      conf.level = 0.95
    )

    test_name <- ifelse(
      equal_variances,
      "Independent-samples t-test",
      "Welch t-test"
    )

    effect_result <- effectsize::cohens_d(
      Outcome ~ Group,
      data = analysis_data,
      pooled_sd = equal_variances,
      ci = 0.95
    )

    omnibus <- data.frame(
      Outcome = outcome_label,
      Explanatory_variable = predictor_label,
      Valid_n = nrow(analysis_data),
      Groups = number_groups,
      Levene_F = levene_f,
      Levene_p = levene_p,
      Variance_assumption = ifelse(
        equal_variances,
        "Homogeneous",
        "Unequal"
      ),
      Test = test_name,
      Statistic = unname(test_result$statistic),
      df1 = unname(test_result$parameter),
      df2 = NA_real_,
      p_value = test_result$p.value,
      Effect_size = "Cohen's d",
      Effect_value = effect_result$Cohens_d[1],
      Effect_CI_lower = effect_result$CI_low[1],
      Effect_CI_upper = effect_result$CI_high[1],
      stringsAsFactors = FALSE
    )

    posthoc <- data.frame()

  } else if (equal_variances) {
    model <- aov(
      Outcome ~ Group,
      data = analysis_data
    )

    model_summary <- summary(model)[[1]]

    statistic <- model_summary["Group", "F value"]
    df1 <- model_summary["Group", "Df"]
    df2 <- model_summary["Residuals", "Df"]
    p_value <- model_summary["Group", "Pr(>F)"]

    effect_result <- effectsize::eta_squared(
      model,
      partial = FALSE,
      ci = 0.95
    )

    omnibus <- data.frame(
      Outcome = outcome_label,
      Explanatory_variable = predictor_label,
      Valid_n = nrow(analysis_data),
      Groups = number_groups,
      Levene_F = levene_f,
      Levene_p = levene_p,
      Variance_assumption = "Homogeneous",
      Test = "One-way ANOVA",
      Statistic = statistic,
      df1 = df1,
      df2 = df2,
      p_value = p_value,
      Effect_size = "Eta squared",
      Effect_value = effect_result$Eta2[1],
      Effect_CI_lower = effect_result$CI_low[1],
      Effect_CI_upper = effect_result$CI_high[1],
      stringsAsFactors = FALSE
    )

    if (p_value < alpha) {
      tukey_result <- TukeyHSD(model, "Group")$Group

      posthoc <- data.frame(
        Comparison = rownames(tukey_result),
        Difference = tukey_result[, "diff"],
        CI_lower = tukey_result[, "lwr"],
        CI_upper = tukey_result[, "upr"],
        p_adjusted = tukey_result[, "p adj"],
        stringsAsFactors = FALSE,
        row.names = NULL
      ) %>%
        mutate(
          Outcome = outcome_label,
          Explanatory_variable = predictor_label,
          Posthoc_test = "Tukey HSD"
        ) %>%
        select(
          Outcome,
          Explanatory_variable,
          Posthoc_test,
          everything()
        )
    } else {
      posthoc <- data.frame()
    }

  } else {
    test_result <- oneway.test(
      Outcome ~ Group,
      data = analysis_data,
      var.equal = FALSE
    )

    statistic <- unname(test_result$statistic)
    df1 <- unname(test_result$parameter[1])
    df2 <- unname(test_result$parameter[2])
    p_value <- test_result$p.value

    # Point estimate retained for comparability with the original analysis.
    effect_result <- effectsize::eta_squared(
      lm(Outcome ~ Group, data = analysis_data),
      partial = FALSE,
      ci = 0.95
    )

    omnibus <- data.frame(
      Outcome = outcome_label,
      Explanatory_variable = predictor_label,
      Valid_n = nrow(analysis_data),
      Groups = number_groups,
      Levene_F = levene_f,
      Levene_p = levene_p,
      Variance_assumption = "Unequal",
      Test = "Welch ANOVA",
      Statistic = statistic,
      df1 = df1,
      df2 = df2,
      p_value = p_value,
      Effect_size = "Eta squared",
      Effect_value = effect_result$Eta2[1],
      Effect_CI_lower = effect_result$CI_low[1],
      Effect_CI_upper = effect_result$CI_high[1],
      stringsAsFactors = FALSE
    )

    if (p_value < alpha) {
      posthoc <- analysis_data %>%
        rstatix::games_howell_test(Outcome ~ Group) %>%
        transmute(
          Outcome = outcome_label,
          Explanatory_variable = predictor_label,
          Posthoc_test = "Games-Howell",
          Comparison = paste(group1, "vs", group2),
          Difference = estimate,
          CI_lower = conf.low,
          CI_upper = conf.high,
          p_adjusted = p.adj
        )
    } else {
      posthoc <- data.frame()
    }
  }

  omnibus <- omnibus %>%
    mutate(
      across(
        c(
          Levene_F,
          Statistic,
          df1,
          df2,
          Effect_value,
          Effect_CI_lower,
          Effect_CI_upper
        ),
        ~ round(.x, 3)
      ),
      Levene_p_display = format_p(Levene_p),
      p_value_display = format_p(p_value)
    )

  list(
    descriptives = descriptives,
    omnibus = omnibus,
    posthoc = posthoc
  )
}

categorical_analysis_plan <- tidyr::crossing(
  outcome_plan,
  predictor_plan
)

readr::write_excel_csv(
  categorical_analysis_plan,
  "04_outputs/tables/Categorical_Analysis_Plan.csv",
  na = ""
)

categorical_results <- purrr::pmap(
  categorical_analysis_plan,
  function(
      Outcome_variable,
      Outcome_label,
      Predictor_variable,
      Predictor_label
  ) {
    run_categorical_analysis(
      data = CH_SwissStudy_analysis,
      outcome = Outcome_variable,
      predictor = Predictor_variable,
      outcome_label = Outcome_label,
      predictor_label = Predictor_label
    )
  }
)

names(categorical_results) <- paste(
  categorical_analysis_plan$Outcome_variable,
  categorical_analysis_plan$Predictor_variable,
  sep = "__"
)

Categorical_Descriptives_Full <- purrr::map_dfr(
  categorical_results,
  "descriptives"
)

Categorical_Descriptives_Publication <- Categorical_Descriptives_Full %>%
  select(
    Outcome,
    Explanatory_variable,
    Group,
    n,
    `Mean (SD)`,
    `95% CI`
  )

Categorical_Omnibus_Full <- purrr::map_dfr(
  categorical_results,
  "omnibus"
)

# Benjamini-Hochberg correction across the 36 categorical omnibus tests
Categorical_Omnibus_Adjusted <- Categorical_Omnibus_Full %>%
  mutate(
    p_FDR = p.adjust(p_value, method = "BH"),
    p_FDR_display = format_p(p_FDR),
    Significant_raw = if_else(p_value < 0.05, "Yes", "No"),
    Significant_FDR = if_else(p_FDR < 0.05, "Yes", "No")
  )

Categorical_Inferential_Summary <- Categorical_Omnibus_Adjusted %>%
  mutate(
    Statistic_label = case_when(
      grepl("t-test", Test, ignore.case = TRUE) ~ "t",
      grepl("ANOVA", Test, ignore.case = TRUE) ~ "F",
      TRUE ~ "Statistic"
    ),

    df_display = case_when(
      Groups == 2 ~ sprintf("%.2f", df1),
      Test == "One-way ANOVA" ~ sprintf("%.0f, %.0f", df1, df2),
      Test == "Welch ANOVA" ~ sprintf("%.2f, %.2f", df1, df2),
      TRUE ~ NA_character_
    ),

    Test_result = paste0(
      Statistic_label,
      "(",
      df_display,
      ") = ",
      sprintf("%.2f", Statistic)
    ),

    Effect_display = case_when(
      Effect_size == "Cohen's d" ~ paste0(
        "d = ",
        sprintf("%.3f", Effect_value),
        " [",
        sprintf("%.3f", Effect_CI_lower),
        ", ",
        sprintf("%.3f", Effect_CI_upper),
        "]"
      ),
      Effect_size == "Eta squared" ~ paste0(
        "\u03b7\u00b2 = ",
        sprintf("%.3f", Effect_value)
      ),
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    Outcome,
    Explanatory_variable,
    Valid_n,
    Test,
    Test_result,
    p_value_display,
    p_FDR_display,
    Effect_display,
    Significant_raw,
    Significant_FDR
  )

non_empty_posthoc <- purrr::keep(
  categorical_results,
  ~ is.data.frame(.x$posthoc) && nrow(.x$posthoc) > 0
)

if (length(non_empty_posthoc) > 0) {
  Categorical_Posthoc_Full <- purrr::map_dfr(
    non_empty_posthoc,
    "posthoc"
  )
} else {
  Categorical_Posthoc_Full <- data.frame(
    Outcome = character(),
    Explanatory_variable = character(),
    Posthoc_test = character(),
    Comparison = character(),
    Difference = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    p_adjusted = numeric(),
    stringsAsFactors = FALSE
  )
}

Categorical_Posthoc_Publication <- Categorical_Posthoc_Full %>%
  mutate(
    Difference = round(Difference, 3),
    `95% CI` = if_else(
      !is.na(CI_lower) & !is.na(CI_upper),
      sprintf("%.3f to %.3f", CI_lower, CI_upper),
      NA_character_
    ),
    p_adjusted_display = format_p(p_adjusted)
  ) %>%
  select(
    Outcome,
    Explanatory_variable,
    Posthoc_test,
    Comparison,
    Difference,
    `95% CI`,
    p_adjusted_display
  )

readr::write_excel_csv(
  Categorical_Descriptives_Full,
  "04_outputs/tables/Categorical_Group_Descriptives_Full.csv",
  na = ""
)

readr::write_excel_csv(
  Categorical_Descriptives_Publication,
  "04_outputs/tables/Categorical_Group_Descriptives_Publication.csv",
  na = ""
)

readr::write_excel_csv(
  Categorical_Omnibus_Full,
  "04_outputs/tables/Categorical_Omnibus_Results_Full.csv",
  na = ""
)

readr::write_excel_csv(
  Categorical_Omnibus_Adjusted,
  "04_outputs/tables/Categorical_Omnibus_Results_FDR_Adjusted.csv",
  na = ""
)

readr::write_excel_csv(
  Categorical_Inferential_Summary,
  "04_outputs/tables/Categorical_Inferential_Summary_Publication.csv",
  na = ""
)

readr::write_excel_csv(
  Categorical_Posthoc_Full,
  "04_outputs/tables/Categorical_Posthoc_Results_Full.csv",
  na = ""
)

readr::write_excel_csv(
  Categorical_Posthoc_Publication,
  "04_outputs/tables/Categorical_Posthoc_Results_Publication.csv",
  na = ""
)

saveRDS(
  categorical_results,
  "04_outputs/Categorical_Analysis_Results_All.rds"
)

# ------------------------------------------------------------------------------
# 10. Non-parametric sensitivity analysis for Ecosystem Services Index
# ------------------------------------------------------------------------------

ecosystem_predictors <- predictor_plan

Sensitivity_Results <- purrr::pmap_dfr(
  ecosystem_predictors,
  function(Predictor_variable, Predictor_label) {
    analysis_data <- CH_SwissStudy_analysis %>%
      transmute(
        Outcome = Ecosystem_Services_Index,
        Group = factor(.data[[Predictor_variable]])
      ) %>%
      filter(!is.na(Outcome), !is.na(Group)) %>%
      droplevels()

    number_groups <- nlevels(analysis_data$Group)

    if (number_groups == 2) {
      test <- wilcox.test(
        Outcome ~ Group,
        data = analysis_data,
        exact = FALSE
      )

      data.frame(
        Outcome = "Perceived importance of ecosystem services",
        Explanatory_variable = Predictor_label,
        Test = "Wilcoxon rank-sum",
        Groups = number_groups,
        Statistic = unname(test$statistic),
        df = NA_real_,
        p_value = test$p.value,
        stringsAsFactors = FALSE
      )
    } else {
      test <- kruskal.test(
        Outcome ~ Group,
        data = analysis_data
      )

      data.frame(
        Outcome = "Perceived importance of ecosystem services",
        Explanatory_variable = Predictor_label,
        Test = "Kruskal–Wallis",
        Groups = number_groups,
        Statistic = unname(test$statistic),
        df = unname(test$parameter),
        p_value = test$p.value,
        stringsAsFactors = FALSE
      )
    }
  }
) %>%
  mutate(
    Statistic = round(Statistic, 2),
    p_value_display = format_p(p_value)
  )

Parametric_Ecosystem <- Categorical_Inferential_Summary %>%
  filter(
    Outcome == "Perceived importance of ecosystem services"
  )

Comparison_Table <- Parametric_Ecosystem %>%
  left_join(
    Sensitivity_Results,
    by = "Explanatory_variable",
    suffix = c("_Parametric", "_Sensitivity")
  )

Sensitivity_Summary <- Comparison_Table %>%
  transmute(
    Explanatory_variable,
    
    Parametric_test = Test_Parametric,
    Parametric_p = p_value_display_Parametric,
    FDR = p_FDR_display,
    
    Sensitivity_test = Test_Sensitivity,
    Sensitivity_p = p_value_display_Sensitivity,
    
    Robust_conclusion = case_when(
      Significant_FDR == "Yes" &
        p_value < 0.05 ~
        "Robustly significant",
      
      Significant_FDR == "No" &
        p_value >= 0.05 ~
        "Consistently non-significant",
      
      TRUE ~ "Borderline"
    )
  )

readr::write_excel_csv(
  Sensitivity_Results,
  "04_outputs/tables/Ecosystem_Sensitivity_Analysis.csv",
  na = ""
)

readr::write_excel_csv(
  Comparison_Table,
  "04_outputs/tables/Ecosystem_Parametric_vs_Sensitivity.csv",
  na = ""
)

readr::write_excel_csv(
  Sensitivity_Summary,
  "04_outputs/tables/Sensitivity_Analysis_Summary.csv",
  na = ""
)

# ------------------------------------------------------------------------------
# 11. Continuous explanatory analyses
#     Pearson correlations are primary; Spearman correlations are robustness checks.
# ------------------------------------------------------------------------------

continuous_predictor_plan <- data.frame(
  Predictor_variable = c(
    "F24",
    "F21",
    "Nature_Connectedness_Index"
  ),
  Predictor_label = c(
    "Age",
    "Years living in the local area",
    "Nature connectedness"
  ),
  stringsAsFactors = FALSE
)

continuous_outcome_plan <- outcome_plan %>%
  select(Outcome_variable, Outcome_label)

continuous_plan <- tidyr::crossing(
  continuous_predictor_plan,
  continuous_outcome_plan
)

run_continuous_analysis <- function(
    data,
    predictor,
    predictor_label,
    outcome,
    outcome_label
) {
  analysis_data <- data %>%
    transmute(
      Predictor = as.numeric(.data[[predictor]]),
      Outcome = as.numeric(.data[[outcome]])
    ) %>%
    filter(!is.na(Predictor), !is.na(Outcome))

  if (nrow(analysis_data) < 3) {
    stop(
      "Insufficient complete observations for ",
      predictor_label,
      " and ",
      outcome_label
    )
  }

  pearson_result <- cor.test(
    analysis_data$Predictor,
    analysis_data$Outcome,
    method = "pearson",
    conf.level = 0.95
  )

  spearman_result <- suppressWarnings(
    cor.test(
      analysis_data$Predictor,
      analysis_data$Outcome,
      method = "spearman",
      exact = FALSE
    )
  )

  data.frame(
    Predictor_variable = predictor,
    Predictor = predictor_label,
    Outcome_variable = outcome,
    Outcome = outcome_label,
    Valid_n = nrow(analysis_data),
    Pearson_r = unname(pearson_result$estimate),
    Pearson_CI_lower = unname(pearson_result$conf.int[1]),
    Pearson_CI_upper = unname(pearson_result$conf.int[2]),
    Pearson_p = pearson_result$p.value,
    Spearman_rho = unname(spearman_result$estimate),
    Spearman_p = spearman_result$p.value,
    stringsAsFactors = FALSE
  )
}

Continuous_Correlations <- purrr::pmap_dfr(
  continuous_plan,
  function(
      Predictor_variable,
      Predictor_label,
      Outcome_variable,
      Outcome_label
  ) {
    run_continuous_analysis(
      data = CH_SwissStudy_analysis,
      predictor = Predictor_variable,
      predictor_label = Predictor_label,
      outcome = Outcome_variable,
      outcome_label = Outcome_label
    )
  }
)

Continuous_Correlations_Publication <- Continuous_Correlations %>%
  mutate(
    `Pearson's r (95% CI)` = sprintf(
      "%.3f (%.3f to %.3f)",
      Pearson_r,
      Pearson_CI_lower,
      Pearson_CI_upper
    ),
    `p-value` = format_p(Pearson_p)
  ) %>%
  select(
    Predictor,
    Outcome,
    Valid_n,
    `Pearson's r (95% CI)`,
    `p-value`
  )

Continuous_Correlations_Robustness <- Continuous_Correlations %>%
  mutate(
    Spearman_rho = round(Spearman_rho, 3),
    Spearman_p_display = format_p(Spearman_p)
  ) %>%
  select(
    Predictor,
    Outcome,
    Valid_n,
    Spearman_rho,
    Spearman_p_display
  )

readr::write_excel_csv(
  continuous_plan,
  "04_outputs/tables/Continuous_Analysis_Plan.csv",
  na = ""
)

readr::write_excel_csv(
  Continuous_Correlations,
  "04_outputs/tables/Continuous_Correlations_Full.csv",
  na = ""
)

readr::write_excel_csv(
  Continuous_Correlations_Publication,
  "04_outputs/tables/Continuous_Correlations_Publication.csv",
  na = ""
)

readr::write_excel_csv(
  Continuous_Correlations_Robustness,
  "04_outputs/tables/Continuous_Correlations_Spearman_Robustness.csv",
  na = ""
)

# ------------------------------------------------------------------------------
# 12. Final validation and reproducibility information
# ------------------------------------------------------------------------------

stopifnot(
  nrow(categorical_analysis_plan) == 36,
  nrow(continuous_plan) == 12,
  nrow(Continuous_Correlations) == 12,
  all(
    c(
      "Ecosystem_Services_Index",
      "Restoration_Implementation_Index",
      "Expected_Outcomes_Index",
      "Public_Support_Index",
      "Nature_Connectedness_Index"
    ) %in% names(CH_SwissStudy_analysis)
  )
)

capture.output(
  sessionInfo(),
  file = "04_outputs/sessionInfo.txt"
)

message("PRPA analysis completed successfully.")

# ==============================================================================
# 13. CREATE PUBLIC AGGREGATED DATA
# ==============================================================================
#
# Creates anonymised item-level summary datasets for public sharing.
# No respondent-level observations or identifying information are exported.
# ==============================================================================

dir.create("data_public", showWarnings = FALSE)

# 13.1 Perceived importance of forest ecosystem services
...

# 13.2 Perceived restoration implementation
...

# 13.3 Expected restoration outcomes
...

# 13.4 Public support for forest restoration


# ==============================================================================
# PRPA Switzerland
# Create public aggregated data
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)

# Load final analysis-ready dataset
CH_SwissStudy_analysis <- readRDS(
  "03_clean_data/CH_SwissStudy_analysis.rds"
)

# Create output directory
dir.create(
  "data_public",
  showWarnings = FALSE
)
# ==============================================================================
# PUBLIC AGGREGATED DATA FOR GITHUB
# ==============================================================================

# ------------------------------------------------------------------------------
# 13.1 Perceived importance of forest ecosystem services
# ------------------------------------------------------------------------------

ecosystem_vars <- paste0("F5r", 1:8)

ecosystem_labels <- c(
  "Temperature regulation",
  "Air purification",
  "Water purification",
  "Protection against natural hazards",
  "Wood production",
  "Food, medicinal plants and ornamental products",
  "Leisure and recreation",
  "Habitat for biodiversity"
)

names(ecosystem_labels) <- ecosystem_vars

PRPA_ecosystem_services_aggregated <-
  CH_SwissStudy_analysis %>%
  select(all_of(ecosystem_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Score"
  ) %>%
  mutate(
    Item = dplyr::recode(
      Variable,
      !!!ecosystem_labels
    ),
    Response_category = case_when(
      Score >= 0 & Score <= 2 ~ "Very low",
      Score >= 3 & Score <= 4 ~ "Low",
      Score >= 5 & Score <= 6 ~ "Moderate",
      Score >= 7 & Score <= 8 ~ "High",
      Score >= 9 & Score <= 10 ~ "Very high",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Score)) %>%
  group_by(Variable, Item) %>%
  summarise(
    Valid_n = n(),
    Mean = mean(Score),
    SD = sd(Score),
    
    Very_low_percent =
      mean(Response_category == "Very low") * 100,
    
    Low_percent =
      mean(Response_category == "Low") * 100,
    
    Moderate_percent =
      mean(Response_category == "Moderate") * 100,
    
    High_percent =
      mean(Response_category == "High") * 100,
    
    Very_high_percent =
      mean(Response_category == "Very high") * 100,
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

write_csv(
  PRPA_ecosystem_services_aggregated,
  "data_public/PRPA_ecosystem_services_aggregated.csv"
)
# ------------------------------------------------------------------------------
# 13.2 Perceived restoration implementation
# ------------------------------------------------------------------------------

implementation_vars <- paste0("F15r", 1:14)

implementation_labels <- c(
  "Reforestation of previously deforested areas",
  "Reforestation of fallow agricultural land",
  "Reforestation after natural disasters",
  "Re-introduction of native tree species",
  "Re-introduction of native animal species",
  "Removal of invasive species",
  "Introduction of fire breaks",
  "Retention of deadwood",
  "Replanting a mix of tree species",
  "Improving forest connectivity",
  "Improving forest access for recreation",
  "Restricting the harvesting of forest products",
  "Reducing fuel loads to minimise fire risk",
  "Changing the type of forest management"
)

names(implementation_labels) <- implementation_vars

PRPA_restoration_implementation_aggregated <-
  CH_SwissStudy_analysis %>%
  select(all_of(implementation_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Score"
  ) %>%
  mutate(
    Item = dplyr::recode(
      Variable,
      !!!implementation_labels
    )
  ) %>%
  filter(!is.na(Score)) %>%
  group_by(Variable, Item) %>%
  summarise(
    Valid_n = n(),
    Mean = mean(Score),
    SD = sd(Score),
    
    Never_percent = mean(Score == 1) * 100,
    Rarely_percent = mean(Score == 2) * 100,
    Sometimes_percent = mean(Score == 3) * 100,
    Often_percent = mean(Score == 4) * 100,
    Always_percent = mean(Score == 5) * 100,
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

write_csv(
  PRPA_restoration_implementation_aggregated,
  "data_public/PRPA_restoration_implementation_aggregated.csv"
)

# ------------------------------------------------------------------------------
#13.3 Expected restoration outcomes
# ------------------------------------------------------------------------------

outcome_vars <- paste0("F17r", 1:16)

outcome_labels <- c(
  "Firewood",
  "Wood for timber",
  "Animal fodder",
  "Ornamental plants",
  "Berries",
  "Wild edible fruits",
  "Mushrooms",
  "Habitat for biodiversity",
  "Forest beauty and aesthetics",
  "Temperature regulation",
  "Fresh air",
  "Clean water",
  "Opportunities for hunting and fishing",
  "Outdoor recreation opportunities",
  "Social and family activities",
  "Aesthetic views"
)

names(outcome_labels) <- outcome_vars

PRPA_expected_outcomes_aggregated <-
  CH_SwissStudy_analysis %>%
  select(all_of(outcome_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Score"
  ) %>%
  mutate(
    Item = dplyr::recode(
      Variable,
      !!!outcome_labels
    )
  ) %>%
  filter(!is.na(Score)) %>%
  group_by(Variable, Item) %>%
  summarise(
    Valid_n = n(),
    Mean = mean(Score),
    SD = sd(Score),
    
    Significantly_reduces_percent =
      mean(Score == 1) * 100,
    
    Moderately_reduces_percent =
      mean(Score == 2) * 100,
    
    No_significant_impact_percent =
      mean(Score == 3) * 100,
    
    Moderately_enhances_percent =
      mean(Score == 4) * 100,
    
    Significantly_enhances_percent =
      mean(Score == 5) * 100,
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

write_csv(
  PRPA_expected_outcomes_aggregated,
  "data_public/PRPA_expected_outcomes_aggregated.csv"
)

# ------------------------------------------------------------------------------
# 13.4 Public support for forest restoration
# ------------------------------------------------------------------------------

support_vars <- paste0("F16br", 1:6)

support_labels <- c(
  "Volunteering for tree planting and maintenance",
  "Participating in awareness campaigns",
  "Advocating for forest restoration policies",
  "Participating in community restoration initiatives",
  "Donating money to restoration projects",
  "Supporting public funding for forest restoration"
)

names(support_labels) <- support_vars

PRPA_public_support_aggregated <-
  CH_SwissStudy_analysis %>%
  select(all_of(support_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Score"
  ) %>%
  mutate(
    Item = dplyr::recode(
      Variable,
      !!!support_labels
    )
  ) %>%
  filter(!is.na(Score)) %>%
  group_by(Variable, Item) %>%
  summarise(
    Valid_n = n(),
    Mean = mean(Score),
    SD = sd(Score),
    
    Completely_disagree_percent =
      mean(Score == 1) * 100,
    
    Slightly_disagree_percent =
      mean(Score == 2) * 100,
    
    Neither_agree_nor_disagree_percent =
      mean(Score == 3) * 100,
    
    Slightly_agree_percent =
      mean(Score == 4) * 100,
    
    Completely_agree_percent =
      mean(Score == 5) * 100,
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

write_csv(
  PRPA_public_support_aggregated,
  "data_public/PRPA_public_support_aggregated.csv"
)


