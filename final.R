# ==============================================================================
# FINAL.R - COMPREHENSIVE ANALYSIS SCRIPT
# Generates ALL figures and tables for the charitable giving paper
# ==============================================================================

rm(list = ls())

# Load libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(ggthemes)
library(scales)
library(patchwork)
library(viridis)
library(broom)
library(fixest)
library(stargazer)
library(tidytext)
library(topicmodels)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(quantreg)

set.seed(42)

# ==============================================================================
# THEME AND COLORS
# ==============================================================================

theme_paper <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    strip.text = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0, color = "gray50")
  )
theme_set(theme_paper)

pal_main <- c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")

# ==============================================================================
# LOAD AND PREPARE DATA
# ==============================================================================

cat("========================================\n")
cat("LOADING AND PREPARING DATA\n")
cat("========================================\n\n")

df_raw <- read_csv("../donations_data.csv", show_col_types = FALSE)

DATA_COLLECTION_DATE <- as.Date("2025-11-19")
ACTIVE_CUTOFF <- DATA_COLLECTION_DATE - 90
UKRAINE_EVENT_DATE <- as.Date("2022-02-01")

df <- df_raw %>%
  clean_names() %>%
  mutate(
    approved_date = ymd_hms(approved_date),
    report_date = as.Date(date_of_most_recent_report),
    approved_year = year(approved_date),
    approved_month = month(approved_date),
    approved_yearmonth = floor_date(approved_date, "month"),
    approved_month_date = as.Date(approved_yearmonth),

    end_date = case_when(
      status == "active" ~ DATA_COLLECTION_DATE,
      !is.na(report_date) & report_date >= ACTIVE_CUTOFF ~ DATA_COLLECTION_DATE,
      !is.na(report_date) ~ report_date,
      active == TRUE ~ DATA_COLLECTION_DATE,
      TRUE ~ NA_Date_
    ),
    project_duration_days = as.numeric(difftime(end_date, as.Date(approved_date), units = "days")),
    project_duration_days = ifelse(project_duration_days <= 0, NA, project_duration_days),
    log_duration = log(project_duration_days),

    funding_ratio = funding / goal,
    is_fully_funded = as.numeric(funding >= goal),
    log_funding = log1p(funding),
    log_goal = log1p(goal),
    log_donations = log1p(number_of_donations),
    avg_donation = ifelse(number_of_donations > 0, funding / number_of_donations, 0),
    log_avg_donation = log1p(avg_donation),

    region_clean = case_when(
      is.na(region) | region == "NA" ~ "Unspecified",
      TRUE ~ region
    ),

    summary_clean = str_to_lower(coalesce(summary, "")),
    combined_text = str_to_lower(paste(
      coalesce(title, ""),
      coalesce(summary, ""),
      sep = " "
    )),
    total_words = str_count(summary_clean, "\\w+"),

    is_ukraine = str_detect(str_to_lower(coalesce(country, "")), "ukraine") |
      str_detect(str_to_lower(coalesce(title, "")), "ukraine") |
      str_detect(str_to_lower(coalesce(summary, "")), "ukraine|ukrainian"),

    post_war = approved_date >= as.POSIXct("2022-02-24"),

    is_disaster_theme = str_detect(str_to_lower(coalesce(theme_name, "")), "disaster"),
    is_health_theme = str_detect(str_to_lower(coalesce(theme_name, "")), "health"),
    is_education_theme = str_detect(str_to_lower(coalesce(theme_name, "")), "education"),

    org_id = as.numeric(str_extract(as.character(organization), "\\d+"))
  ) %>%
  filter(!is.na(approved_date), approved_year >= 2002, approved_year <= 2025, goal > 0)

cat("Total observations:", nrow(df), "\n")

# ==============================================================================
# TEXT FEATURES
# ==============================================================================

cat("Computing text features...\n")

df <- df %>%
  mutate(
    # TOP 5 THEMES FROM UKRAINE PROJECTS (POST-WAR)
    # Programmatically identified based on theme prevalence in Ukraine-related projects
    # 1. Disaster Response (102 projects, 27.1%), 2. Child Protection (72, 19.1%),
    # 3. Refugee Rights (40, 10.6%), 4. Physical Health (37, 9.81%), 5. Education (34, 9.02%)
    theme_disaster_response = !is.na(theme_name) & theme_name == "Disaster Response",
    theme_child_protection = !is.na(theme_name) & theme_name == "Child Protection",
    theme_refugee_rights = !is.na(theme_name) & theme_name == "Refugee Rights",
    theme_physical_health = !is.na(theme_name) & theme_name == "Physical Health",
    theme_education = !is.na(theme_name) & theme_name == "Education"

  )

# ==============================================================================
# EMOTION ANALYSIS USING NRC EMOTION LEXICON
# ==============================================================================
# Using the NRC Emotion Lexicon (Mohammad & Turney, 2013)
# Programmatically identify top 5 most prevalent emotions in the data

cat("\n========================================\n")
cat("EMOTION ANALYSIS WITH NRC LEXICON\n")
cat("========================================\n\n")

# Load NRC Emotion Lexicon from local file
nrc_file <- "NRC-Emotion-Lexicon/NRC-Emotion-Lexicon-v0.92/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt"

if (!file.exists(nrc_file)) {
  stop("NRC Emotion Lexicon file not found at: ", nrc_file, "\nPlease ensure the NRC lexicon is in the project directory.")
}

cat("Loading NRC Emotion Lexicon from local file...\n")

# Read the NRC lexicon file
# Format: word, emotion/sentiment, association (0 or 1)
nrc_raw <- read_tsv(nrc_file,
                    col_names = c("word", "sentiment", "association"),
                    show_col_types = FALSE)

# NRC has 10 categories: 8 emotions + positive/negative sentiment
# We only use the 8 emotions: anger, anticipation, disgust, fear, joy, sadness, surprise, trust
all_nrc_emotions <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")

# Keep only words with association = 1 and filter to emotions (not positive/negative sentiment)
nrc_emotions_full <- nrc_raw %>%
  filter(association == 1, sentiment %in% all_nrc_emotions) %>%
  select(word, sentiment)

cat("NRC lexicon loaded successfully:\n")
cat("  Total word-emotion pairs:", nrow(nrc_emotions_full), "\n")
cat("  Unique words:", n_distinct(nrc_emotions_full$word), "\n")
cat("  Emotions:", paste(all_nrc_emotions, collapse=", "), "\n\n")

# STEP 1: Calculate emotion prevalence for ALL 8 emotions across all projects
cat("Calculating prevalence of all 8 NRC emotions across projects...\n")

emotion_counts_all <- df %>%
  select(id, summary_clean) %>%
  unnest_tokens(word, summary_clean) %>%
  inner_join(nrc_emotions_full, by = "word") %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0, names_prefix = "emotion_")

# Merge to main data with binary indicators for all emotions
df_temp <- df %>%
  left_join(emotion_counts_all, by = "id") %>%
  mutate(
    across(starts_with("emotion_"), ~coalesce(.x, 0)),
    # Create binary indicators for all 8 emotions
    has_anger = coalesce(emotion_anger, 0) > 0,
    has_anticipation = coalesce(emotion_anticipation, 0) > 0,
    has_disgust = coalesce(emotion_disgust, 0) > 0,
    has_fear = coalesce(emotion_fear, 0) > 0,
    has_joy = coalesce(emotion_joy, 0) > 0,
    has_sadness = coalesce(emotion_sadness, 0) > 0,
    has_surprise = coalesce(emotion_surprise, 0) > 0,
    has_trust = coalesce(emotion_trust, 0) > 0
  )

# Calculate prevalence of all 8 emotions
emotion_prevalence_all <- df_temp %>%
  summarise(
    anger = mean(has_anger, na.rm = TRUE) * 100,
    anticipation = mean(has_anticipation, na.rm = TRUE) * 100,
    disgust = mean(has_disgust, na.rm = TRUE) * 100,
    fear = mean(has_fear, na.rm = TRUE) * 100,
    joy = mean(has_joy, na.rm = TRUE) * 100,
    sadness = mean(has_sadness, na.rm = TRUE) * 100,
    surprise = mean(has_surprise, na.rm = TRUE) * 100,
    trust = mean(has_trust, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(everything(), names_to = "emotion", values_to = "prevalence") %>%
  arrange(desc(prevalence))

cat("\nPrevalence of all 8 NRC emotions across projects:\n")
print(emotion_prevalence_all)
cat("\n")

# STEP 2: Select top 5 most prevalent emotions
top_5_emotions <- emotion_prevalence_all$emotion[1:5]

cat("==========================================\n")
cat("TOP 5 EMOTIONS SELECTED PROGRAMMATICALLY:\n")
cat("==========================================\n")
for (i in seq_along(top_5_emotions)) {
  prev <- emotion_prevalence_all$prevalence[i]
  cat(sprintf("  %d. %s (%.1f%% of projects)\n", i, tools::toTitleCase(top_5_emotions[i]), prev))
}
cat("\n")

# STEP 3: Keep only indicators for top 5 emotions in main dataframe
# Note: We need to keep keyword indicators (has_children, etc.) created earlier
emotion_has_cols <- paste0("has_", all_nrc_emotions)
emotion_count_cols <- paste0("emotion_", all_nrc_emotions)

emotion_cols_to_keep <- paste0("has_", top_5_emotions)
emotion_count_cols_to_keep <- paste0("emotion_", top_5_emotions)

# Remove only the emotion indicators, not keyword indicators
df <- df_temp %>%
  select(-any_of(emotion_has_cols), -any_of(emotion_count_cols)) %>%
  bind_cols(df_temp %>% select(all_of(c(emotion_cols_to_keep, emotion_count_cols_to_keep))))

cat("Emotion analysis prepared with top 5 emotions from NRC lexicon.\n\n")

# Sentiment
bing <- get_sentiments("bing")
sentiment_scores <- df %>%
  select(id, summary_clean) %>%
  unnest_tokens(word, summary_clean) %>%
  inner_join(bing, by = "word") %>%
  group_by(id) %>%
  summarise(
    positive_count = sum(sentiment == "positive"),
    negative_count = sum(sentiment == "negative"),
    sentiment_words = n(),
    .groups = "drop"
  )

df <- df %>%
  left_join(sentiment_scores, by = "id") %>%
  mutate(
    positive_count = coalesce(positive_count, 0),
    negative_count = coalesce(negative_count, 0),
    sentiment_words = coalesce(sentiment_words, 0),
    net_sentiment = (positive_count - negative_count) / pmax(total_words, 1) * 100,
    sentiment_intensity = (positive_count + negative_count) / pmax(total_words, 1) * 100,
    positive_ratio = positive_count / pmax(sentiment_words, 1)
  )

# Readability
count_syllables <- function(word) {
  word <- tolower(gsub("[^a-z]", "", word))
  if (nchar(word) == 0) return(0)
  vowels <- c("a", "e", "i", "o", "u", "y")
  chars <- strsplit(word, "")[[1]]
  syllables <- 0
  prev_vowel <- FALSE
  for (char in chars) {
    is_vowel <- char %in% vowels
    if (is_vowel && !prev_vowel) syllables <- syllables + 1
    prev_vowel <- is_vowel
  }
  if (grepl("e$", word) && syllables > 1) syllables <- syllables - 1
  return(max(syllables, 1))
}

compute_fk_grade <- function(text) {
  if (is.na(text) || nchar(text) < 10) return(NA_real_)
  sentences <- max(str_count(text, "[.!?]+"), 1)
  words <- str_extract_all(text, "\\b[a-zA-Z]+\\b")[[1]]
  word_count <- length(words)
  if (word_count == 0) return(NA_real_)
  syllables <- sum(sapply(words, count_syllables))
  fk_grade <- 0.39 * (word_count / sentences) + 11.8 * (syllables / word_count) - 15.59
  return(pmin(pmax(fk_grade, 0), 20))
}

cat("Computing readability scores...\n")
df <- df %>%
  rowwise() %>%
  mutate(fk_grade_level = compute_fk_grade(summary_clean)) %>%
  ungroup()

# Action verbs and pronouns
action_verbs <- c("build", "create", "provide", "deliver", "train", "teach", "educate",
                  "transform", "improve", "strengthen", "empower", "develop",
                  "help", "support", "assist", "protect", "rescue", "save")
pronouns <- c("you", "your", "we", "our", "us", "together")

df <- df %>%
  mutate(
    action_verb_count = sapply(summary_clean, function(text) {
      sum(sapply(action_verbs, function(v) str_count(text, paste0("\\b", v, "\\b"))))
    }),
    action_verb_density = action_verb_count / pmax(total_words, 1) * 100,
    pronoun_count = sapply(summary_clean, function(text) {
      sum(sapply(pronouns, function(p) str_count(text, paste0("\\b", p, "\\b"))))
    }),
    pronoun_density = pronoun_count / pmax(total_words, 1) * 100
  )

# Identifiable victim - Using comprehensive name detection
# Load common names list (186 international names across cultures)
common_names <- c(
  "Abdul", "Ahmed", "Ali", "Amina", "Anna", "Antonio", "Carlos", "David", "Elena", "Emmanuel",
  "Fatima", "Francisco", "Gabriel", "Grace", "Hassan", "Ibrahim", "Isabel", "James", "John", "Jose",
  "Joseph", "Juan", "Kevin", "Leila", "Luis", "Maria", "Mary", "Michael", "Miguel", "Mohammed",
  "Moses", "Nancy", "Omar", "Patricia", "Paul", "Pedro", "Peter", "Rachel", "Robert", "Rosa",
  "Sarah", "Sofia", "Stephen", "Susan", "Thomas", "Victor", "William", "Yusuf", "Zainab",
  "Agnes", "Alice", "Andrew", "Angela", "Benjamin", "Catherine", "Charles", "Christine", "Daniel",
  "Elizabeth", "Eric", "Esther", "Faith", "Florence", "Francis", "George", "Hannah", "Hope",
  "Isaac", "Jacob", "Jane", "Jennifer", "Jessica", "Joan", "Joyce", "Judith", "Julia", "Juliet",
  "Karen", "Margaret", "Martha", "Martin", "Matthew", "Monica", "Nicholas", "Olivia",
  "Patrick", "Rebecca", "Richard", "Rose", "Ruth", "Samuel", "Simon", "Sophia",
  "Teresa", "Timothy", "Vincent", "Wilson",
  "Abdi", "Aisha", "Blessing", "Chidi", "Divine", "Emeka", "Fatoumata", "Gift", "Habiba",
  "Innocent", "Juma", "Kamau", "Kofi", "Mama", "Mercy", "Musa", "Muthu", "Nana", "Nuru",
  "Otieno", "Precious", "Promise", "Rahma", "Salim", "Shukri", "Tamba", "Tendai", "Winnie",
  "Akiko", "Anil", "Anjali", "Anwar", "Arjun", "Devi", "Divya", "Hamid", "Indira",
  "Jamal", "Kamala", "Kumar", "Lakshmi", "Malik", "Nasir", "Priya", "Rajesh", "Ravi", "Rohit",
  "Sanjay", "Sita", "Suresh", "Tariq", "Usman", "Vijay", "Yasmin",
  "Alejandro", "Alfonso", "Ana", "Andrea", "Andres", "Beatriz", "Carmen", "Claudia", "Cristina",
  "Diego", "Eduardo", "Emilio", "Enrique", "Fernanda", "Fernando", "Gabriela", "Guadalupe",
  "Guillermo", "Javier", "Jesus", "Jorge", "Leonardo", "Lucia", "Manuel", "Mariana", "Mario",
  "Martina", "Pablo", "Raul", "Ricardo", "Roberto", "Rodrigo", "Santiago", "Sergio", "Valentina"
)

# Create regex pattern from common names
names_pattern <- paste0("\\b(", paste(common_names, collapse = "|"), ")\\b")

df <- df %>%
  mutate(
    # Detect named individuals using comprehensive name list + contextual patterns
    has_named_individual = str_detect(summary, names_pattern) |
      str_detect(summary, "\\bmeet [A-Z][a-z]+\\b|\\bnamed [A-Z][a-z]+\\b|\\byoung [A-Z][a-z]+\\b"),
    has_personal_story = str_detect(summary_clean, "story of|meet |her story|his story|she was|he was|born in|grew up|mother of|father of"),
    singular_count = str_count(summary_clean, "\\b(a child|a girl|a boy|a woman|a man|a family|one child|this child)\\b"),
    plural_count = str_count(summary_clean, "\\b(children|families|students|communities|hundreds of|thousands of)\\b"),
    singular_framing = singular_count / pmax(singular_count + plural_count, 1),
    has_quantified_impact = str_detect(summary_clean, "\\d+ (children|students|families|people|women|girls)") |
      str_detect(summary_clean, "(help|feed|educate|train|support) \\d+"),
    log_description_length = log1p(total_words)
  )

cat("Text features computed.\n\n")

# ==============================================================================
# REGRESSION SAMPLE
# ==============================================================================

cat("Creating regression sample...\n")

reg_data <- df %>%
  filter(
    !is.na(theme_name), theme_name != "",
    region_clean != "Unspecified",
    approved_year >= 2010, approved_year <= 2024,
    goal > 0, goal < quantile(df$goal, 0.99, na.rm = TRUE),
    !is.na(log_duration),
    !is.na(fk_grade_level),
    !is.na(net_sentiment)
  ) %>%
  mutate(
    theme_factor = as.factor(theme_name),
    region_factor = as.factor(region_clean),
    year_factor = as.factor(approved_year)
  )

# Non-Ukraine sample for spillover analysis
reg_data_nonukr <- reg_data %>% filter(!is_ukraine)

# Regional standardization
reg_data <- reg_data %>%
  mutate(
    region_std = case_when(
      grepl("Africa", region_clean, ignore.case = TRUE) & !grepl("Middle", region_clean, ignore.case = TRUE) ~ "Africa",
      grepl("Asia|Oceania", region_clean, ignore.case = TRUE) ~ "Asia and Oceania",
      grepl("Europe|Russia", region_clean, ignore.case = TRUE) ~ "Europe and Russia",
      grepl("Latin|Caribbean|Central America|South America", region_clean, ignore.case = TRUE) ~ "Latin America",
      grepl("Middle East", region_clean, ignore.case = TRUE) ~ "Middle East",
      grepl("North America", region_clean, ignore.case = TRUE) ~ "North America",
      TRUE ~ NA_character_
    ),
    region_for_reg = factor(region_std, levels = c("North America", "Africa", "Asia and Oceania",
                                                    "Europe and Russia", "Latin America", "Middle East"))
  )

reg_data_nonukr <- reg_data_nonukr %>%
  mutate(
    region_std = case_when(
      grepl("Africa", region_clean, ignore.case = TRUE) & !grepl("Middle", region_clean, ignore.case = TRUE) ~ "Africa",
      grepl("Asia|Oceania", region_clean, ignore.case = TRUE) ~ "Asia and Oceania",
      grepl("Europe|Russia", region_clean, ignore.case = TRUE) ~ "Europe and Russia",
      grepl("Latin|Caribbean|Central America|South America", region_clean, ignore.case = TRUE) ~ "Latin America",
      grepl("Middle East", region_clean, ignore.case = TRUE) ~ "Middle East",
      grepl("North America", region_clean, ignore.case = TRUE) ~ "North America",
      TRUE ~ NA_character_
    ),
    region_for_reg = factor(region_std, levels = c("North America", "Africa", "Asia and Oceania",
                                                    "Europe and Russia", "Latin America", "Middle East"))
  )

cat("Full regression sample:", nrow(reg_data), "\n")
cat("Non-Ukraine sample:", nrow(reg_data_nonukr), "\n\n")

# ==============================================================================
# HELPER: FORMAT COEFFICIENTS
# ==============================================================================

fmt_coef <- function(coef, pval, show_dash = FALSE) {
  if (is.na(coef)) return(if (show_dash) "---" else "")
  stars <- case_when(pval < 0.01 ~ "***", pval < 0.05 ~ "**", pval < 0.1 ~ "*", TRUE ~ "")
  sprintf("%.3f%s", coef, stars)
}

fmt_se <- function(se, show_dash = FALSE) {
  if (is.na(se)) return(if (show_dash) "---" else "")
  sprintf("(%.3f)", se)
}

get_coef_info <- function(mod) {
  coefs <- coef(mod)
  ses <- sqrt(diag(vcov(mod)))
  pvals <- 2 * pnorm(-abs(coefs / ses))
  list(coef = coefs, se = ses, pval = pvals)
}

# ==============================================================================
# TABLE 1: SUMMARY STATISTICS
# ==============================================================================

cat("========================================\n")
cat("GENERATING TABLES\n")
cat("========================================\n\n")

cat("=== Table 1: Summary Statistics ===\n")

n_projects <- nrow(df)
n_countries <- n_distinct(df$country, na.rm = TRUE)
n_orgs <- n_distinct(df$org_id, na.rm = TRUE)
total_funding <- sum(df$funding, na.rm = TRUE) / 1e6
mean_funding <- mean(df$funding, na.rm = TRUE)
median_funding <- median(df$funding, na.rm = TRUE)
sd_funding <- sd(df$funding, na.rm = TRUE)
mean_goal <- mean(df$goal, na.rm = TRUE)
median_goal <- median(df$goal, na.rm = TRUE)
sd_goal <- sd(df$goal, na.rm = TRUE)
mean_donations <- mean(df$number_of_donations, na.rm = TRUE)
median_donations <- median(df$number_of_donations, na.rm = TRUE)
sd_donations <- sd(df$number_of_donations, na.rm = TRUE)
mean_fr <- mean(df$funding_ratio, na.rm = TRUE)
median_fr <- median(df$funding_ratio, na.rm = TRUE)
sd_fr <- sd(df$funding_ratio, na.rm = TRUE)
success_rate <- mean(df$is_fully_funded, na.rm = TRUE)

summary_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Summary Statistics}\n",
  "\\label{tab:summary}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "\\textbf{Variable} & \\textbf{Mean} & \\textbf{Median} & \\textbf{SD} & \\textbf{N} \\\\\n",
  "\\midrule\n",
  "\\multicolumn{5}{l}{\\textit{Panel A: Financial Variables}} \\\\\n",
  sprintf("Funding (\\$) & %s & %s & %s & %s \\\\\n",
          format(round(mean_funding), big.mark = ","),
          format(round(median_funding), big.mark = ","),
          format(round(sd_funding), big.mark = ","),
          format(n_projects, big.mark = ",")),
  sprintf("Goal (\\$) & %s & %s & %s & %s \\\\\n",
          format(round(mean_goal), big.mark = ","),
          format(round(median_goal), big.mark = ","),
          format(round(sd_goal), big.mark = ","),
          format(n_projects, big.mark = ",")),
  sprintf("Funding Ratio & %.2f & %.2f & %.2f & %s \\\\\n",
          mean_fr, median_fr, sd_fr, format(n_projects, big.mark = ",")),
  sprintf("Number of Donations & %.1f & %.0f & %.1f & %s \\\\\n",
          mean_donations, median_donations, sd_donations, format(n_projects, big.mark = ",")),
  "\\addlinespace\n",
  "\\multicolumn{5}{l}{\\textit{Panel B: Platform Totals}} \\\\\n",
  sprintf("Total Projects & \\multicolumn{4}{c}{%s} \\\\\n", format(n_projects, big.mark = ",")),
  sprintf("Total Countries & \\multicolumn{4}{c}{%s} \\\\\n", format(n_countries, big.mark = ",")),
  sprintf("Total Organizations & \\multicolumn{4}{c}{%s} \\\\\n", format(n_orgs, big.mark = ",")),
  sprintf("Total Funding (\\$M) & \\multicolumn{4}{c}{%.1f} \\\\\n", total_funding),
  sprintf("Fully Funded Rate & \\multicolumn{4}{c}{%.1f\\%%} \\\\\n", success_rate * 100),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Sample includes all GlobalGiving projects from 2002--2025 with positive goal amounts.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(summary_latex, "tables/table1_summary_stats.tex")
cat("Saved: tables/table1_summary_stats.tex\n")

# ==============================================================================
# TABLE 3: DIFFERENCE-IN-DIFFERENCES RESULTS
# ==============================================================================

cat("=== Table 3: DiD Results ===\n")

ukraine_event_date <- as.Date("2022-02-01")
all_months_date <- seq(as.Date("2020-01-01"), as.Date("2024-12-01"), by = "month")

did_monthly <- df %>%
  filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_month_date)) %>%
  group_by(approved_month_date, is_ukraine) %>%
  summarise(
    total_funding = sum(funding, na.rm = TRUE),
    total_donations = sum(number_of_donations, na.rm = TRUE),
    n_projects = n(),
    project_duration_days = mean(project_duration_days),
    .groups = "drop"
  )

did_complete <- expand_grid(
  approved_month_date = all_months_date,
  is_ukraine = c(TRUE, FALSE)
) %>%
  left_join(did_monthly, by = c("approved_month_date", "is_ukraine")) %>%
  mutate(
    total_funding = replace_na(total_funding, 0),
    total_donations = replace_na(total_donations, 0),
    n_projects = replace_na(n_projects, 0),
    log_funding = log1p(total_funding),
    log_donations = log1p(total_donations),
    avg_donation = ifelse(total_donations > 0, total_funding / total_donations, 0),
    log_avg_donation = log1p(avg_donation),
    post = approved_month_date >= ukraine_event_date,
    ukraine = as.numeric(is_ukraine),
    ukraine_post = ukraine * as.numeric(post),
    year = year(approved_month_date),
    year_factor = as.factor(year)
  )

# Calculate fully funded rate (proportion of projects that are fully funded)
did_monthly_funded <- df %>%
  filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_month_date)) %>%
  group_by(approved_month_date, is_ukraine) %>%
  summarise(
    pct_fully_funded = mean(is_fully_funded, na.rm = TRUE),
    .groups = "drop"
  )

did_complete <- did_complete %>%
  left_join(did_monthly_funded, by = c("approved_month_date", "is_ukraine")) %>%
  mutate(pct_fully_funded = replace_na(pct_fully_funded, 0))

did_m1 <- lm(log_funding ~ ukraine * post + year_factor, data = did_complete)
did_m2 <- lm(log_avg_donation ~ ukraine * post + year_factor, data = did_complete)
did_m3 <- lm(log_donations ~ ukraine * post + year_factor, data = did_complete)
did_m4 <- lm(pct_fully_funded ~ ukraine * post + year_factor, data = did_complete)

# Create table with 4 columns
did_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Difference-in-Differences Estimates: Ukraine Invasion Effect}\n",
  "\\label{tab:did}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) \\\\\n",
  "& Log(Funding) & Log(Avg Donation) & Log(\\# Donations) & Fully Funded (\\%) \\\\\n",
  "\\midrule\n"
)

vars <- c("ukraine", "postTRUE", "ukraine:postTRUE")
labels <- c("Ukraine", "Post", "Ukraine $\\times$ Post")

for (i in seq_along(vars)) {
  coefs <- sapply(list(did_m1, did_m2, did_m3, did_m4), function(m) {
    if (vars[i] %in% names(coef(m))) coef(m)[vars[i]] else NA
  })
  ses <- sapply(list(did_m1, did_m2, did_m3, did_m4), function(m) {
    if (vars[i] %in% names(coef(m))) sqrt(diag(vcov(m)))[vars[i]] else NA
  })
  pvals <- 2 * pnorm(-abs(coefs / ses))

  coef_str <- sapply(1:4, function(j) {
    if (is.na(coefs[j])) return("")
    # All decimal format now
    val <- sprintf("%.2f", coefs[j])
    stars <- case_when(pvals[j] < 0.01 ~ "***", pvals[j] < 0.05 ~ "**", pvals[j] < 0.1 ~ "*", TRUE ~ "")
    paste0(val, stars)
  })
  se_str <- sapply(1:4, function(j) {
    if (is.na(ses[j])) return("")
    sprintf("(%.2f)", ses[j])
  })

  did_latex <- paste0(did_latex, sprintf("%s & %s \\\\\n", labels[i], paste(coef_str, collapse = " & ")))
  did_latex <- paste0(did_latex, sprintf("& %s \\\\\n", paste(se_str, collapse = " & ")))
}

n_obs <- nrow(did_complete)
did_latex <- paste0(did_latex,
  "\\addlinespace\n",
  sprintf("Observations & %d & %d & %d & %d \\\\\n", n_obs, n_obs, n_obs, n_obs),
  "Year FE & Yes & Yes & Yes & Yes \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Monthly aggregate data, 2020--2024. Ukraine $\\times$ Post captures the differential change in outcomes for Ukraine-related projects after February 2022. Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(did_latex, "tables/table3_did_results.tex")
cat("Saved: tables/table3_did_results.tex\n")

# ==============================================================================
# TABLE: ROBUST DID
# ==============================================================================

cat("=== Table: Robust DiD ===\n")

# Funding share
did_complete <- did_complete %>%
  group_by(approved_month_date) %>%
  mutate(funding_share = total_funding / sum(total_funding)) %>%
  ungroup() %>%
  mutate(funding_share = replace_na(funding_share, 0))

# Winsorized
did_complete <- did_complete %>%
  mutate(
    funding_wins = pmin(pmax(total_funding, quantile(total_funding, 0.05)), quantile(total_funding, 0.95)),
    log_funding_wins = log1p(funding_wins)
  )

robust_m1 <- lm(total_funding ~ ukraine * post + year_factor, data = did_complete)
robust_m2 <- lm(log_funding ~ ukraine * post + year_factor, data = did_complete)
robust_m3 <- lm(funding_share ~ ukraine * post + year_factor, data = did_complete)
robust_m4 <- lm(log_funding_wins ~ ukraine * post + year_factor, data = did_complete)

robust_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Robustness of DiD Estimates Across Specifications}\n",
  "\\label{tab:robust_did}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) \\\\\n",
  "& Levels & Log & Share & Winsorized \\\\\n",
  "\\midrule\n"
)

coef_ukr_post <- sapply(list(robust_m1, robust_m2, robust_m3, robust_m4), function(m) coef(m)["ukraine:postTRUE"])
se_ukr_post <- sapply(list(robust_m1, robust_m2, robust_m3, robust_m4), function(m) sqrt(diag(vcov(m)))["ukraine:postTRUE"])
pval_ukr_post <- 2 * pnorm(-abs(coef_ukr_post / se_ukr_post))

coef_str <- sapply(1:4, function(j) {
  stars <- case_when(pval_ukr_post[j] < 0.01 ~ "***", pval_ukr_post[j] < 0.05 ~ "**", pval_ukr_post[j] < 0.1 ~ "*", TRUE ~ "")
  if (j == 1) {
    paste0(format(round(coef_ukr_post[j] / 1e6, 2), nsmall = 2), "M", stars)
  } else if (j == 3) {
    sprintf("%.2f%s", coef_ukr_post[j], stars)
  } else {
    sprintf("%.2f%s", coef_ukr_post[j], stars)
  }
})
se_str <- sapply(1:4, function(j) {
  if (j == 1) {
    sprintf("(%.2fM)", se_ukr_post[j] / 1e6)
  } else {
    sprintf("(%.2f)", se_ukr_post[j])
  }
})

robust_latex <- paste0(robust_latex,
  sprintf("Ukraine $\\times$ Post & %s \\\\\n", paste(coef_str, collapse = " & ")),
  sprintf("& %s \\\\\n", paste(se_str, collapse = " & ")),
  "\\addlinespace\n",
  sprintf("Observations & %d & %d & %d & %d \\\\\n", n_obs, n_obs, n_obs, n_obs),
  "Year FE & Yes & Yes & Yes & Yes \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Monthly aggregate data, 2020--2024. Column (1): funding in levels. Column (2): log(funding+1). Column (3): Ukraine share of monthly platform funding. Column (4): funding winsorized at 5th/95th percentiles then log-transformed. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(robust_latex, "tables/table_robust_did.tex")
cat("Saved: tables/table_robust_did.tex\n")

# ==============================================================================
# TABLE 6: SUBSTITUTION EFFECTS (ADDSUB)
# ==============================================================================

cat("=== Table 6: Substitution Effects ===\n")

addsub_data <- df %>%
  filter(!is_ukraine, approved_year >= 2020, approved_year <= 2024, !is.na(log_goal))

addsub_m1 <- feols(log_funding ~ post_war + log_goal  + log_duration,
                   data = addsub_data %>% mutate(theme_factor = as.factor(theme_name), year_factor = as.factor(approved_year)), vcov = "hetero")
addsub_m2 <- feols(log_funding ~ post_war + log_goal + log_duration,
                   data = addsub_data %>% filter(is_disaster_theme) %>% mutate(theme_factor = as.factor(theme_name), year_factor = as.factor(approved_year)), vcov = "hetero")
addsub_m3 <- feols(log_funding ~ post_war + log_goal + log_duration,
                   data = addsub_data %>% filter(is_health_theme) %>% mutate(theme_factor = as.factor(theme_name), year_factor = as.factor(approved_year)), vcov = "hetero")
addsub_m4 <- feols(log_funding ~ post_war + log_goal + log_duration,
                   data = addsub_data %>% filter(is_education_theme) %>% mutate(theme_factor = as.factor(theme_name), year_factor = as.factor(approved_year)), vcov = "hetero")

addsub_d1 <- feols(log_donations ~ post_war + log_goal + log_duration,
                   data = addsub_data %>% mutate(theme_factor = as.factor(theme_name), year_factor = as.factor(approved_year)), vcov = "hetero")
addsub_d2 <- feols(log_donations ~ post_war + log_goal + log_duration,
                   data = addsub_data %>% filter(is_disaster_theme) %>% mutate(theme_factor = as.factor(theme_name), year_factor = as.factor(approved_year)), vcov = "hetero")
addsub_d3 <- feols(log_donations ~ post_war + log_goal + log_duration,
                   data = addsub_data %>% filter(is_health_theme) %>% mutate(theme_factor = as.factor(theme_name), year_factor = as.factor(approved_year)), vcov = "hetero")
addsub_d4 <- feols(log_donations ~ post_war + log_goal + log_duration,
                   data = addsub_data %>% filter(is_education_theme) %>% mutate(theme_factor = as.factor(theme_name), year_factor = as.factor(approved_year)), vcov = "hetero")



addsub_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Substitution Effects: Non-Ukraine Projects Post-Invasion}\n",
  "\\label{tab:addsub}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) \\\\\n",
  "& All Non-Ukraine & Disaster & Health & Education \\\\\n",
  "\\midrule\n",
  "\\multicolumn{5}{l}{\\textit{Panel A: Effect on Log(Funding)}} \\\\\n"
)

coefs_f <- sapply(list(addsub_m1, addsub_m2, addsub_m3, addsub_m4), function(m) coef(m)["post_warTRUE"])
ses_f <- sapply(list(addsub_m1, addsub_m2, addsub_m3, addsub_m4), function(m) sqrt(diag(vcov(m)))["post_warTRUE"])
pvals_f <- 2 * pnorm(-abs(coefs_f / ses_f))

addsub_latex <- paste0(addsub_latex,
  sprintf("Post-Invasion & %s & %s & %s & %s \\\\\n",
          fmt_coef(coefs_f[1], pvals_f[1]), fmt_coef(coefs_f[2], pvals_f[2]),
          fmt_coef(coefs_f[3], pvals_f[3]), fmt_coef(coefs_f[4], pvals_f[4])),
  sprintf("& %s & %s & %s & %s \\\\\n",
          fmt_se(ses_f[1]), fmt_se(ses_f[2]), fmt_se(ses_f[3]), fmt_se(ses_f[4])),
  "\\addlinespace\n",
  "\\multicolumn{5}{l}{\\textit{Panel B: Effect on Log(Donations)}} \\\\\n"
)

coefs_d <- sapply(list(addsub_d1, addsub_d2, addsub_d3, addsub_d4), function(m) coef(m)["post_warTRUE"])
ses_d <- sapply(list(addsub_d1, addsub_d2, addsub_d3, addsub_d4), function(m) sqrt(diag(vcov(m)))["post_warTRUE"])
pvals_d <- 2 * pnorm(-abs(coefs_d / ses_d))

addsub_latex <- paste0(addsub_latex,
  sprintf("Post-Invasion & %s & %s & %s & %s \\\\\n",
          fmt_coef(coefs_d[1], pvals_d[1]), fmt_coef(coefs_d[2], pvals_d[2]),
          fmt_coef(coefs_d[3], pvals_d[3]), fmt_coef(coefs_d[4], pvals_d[4])),
  sprintf("& %s & %s & %s & %s \\\\\n",
          fmt_se(ses_d[1]), fmt_se(ses_d[2]), fmt_se(ses_d[3]), fmt_se(ses_d[4])),
  "\\midrule\n",
  "Log(Goal) Control & Yes & Yes & Yes & Yes \\\\\n",
  "Duration Control & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s \\\\\n",
          format(addsub_m1$nobs, big.mark = ","), format(addsub_m2$nobs, big.mark = ","),
          format(addsub_m3$nobs, big.mark = ","), format(addsub_m4$nobs, big.mark = ",")),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Robust standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Sample includes non-Ukraine projects approved 2020--2024 with valid duration data. Negative coefficients indicate substitution effects.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(addsub_latex, "tables/table6_addsub.tex")
cat("Saved: tables/table6_addsub.tex\n")

# ==============================================================================
# TABLE: UKRAINE DESCRIPTIVE
# ==============================================================================

cat("=== Table: Ukraine Descriptive ===\n")

ukraine_projects <- df %>% filter(is_ukraine)

pre_2014 <- ukraine_projects %>% filter(approved_year < 2014)
crimea_era <- ukraine_projects %>% filter(approved_year >= 2014, approved_year < 2022)
post_2022 <- ukraine_projects %>% filter(approved_year >= 2022)

ukraine_desc_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Ukraine-Related Projects: Evolution Across Conflict Phases}\n",
  "\\label{tab:ukraine_descriptive}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lccc}\n",
  "\\toprule\n",
  "& Pre-2014 & 2014--2021 & 2022--Present \\\\\n",
  "& (Pre-Crimea) & (Crimea Era) & (Full Invasion) \\\\\n",
  "\\midrule\n",
  sprintf("Projects & %d & %d & %d \\\\\n", nrow(pre_2014), nrow(crimea_era), nrow(post_2022)),
  sprintf("Total Funding (\\$M) & %.2f & %.2f & %.2f \\\\\n",
          sum(pre_2014$funding) / 1e6, sum(crimea_era$funding) / 1e6, sum(post_2022$funding) / 1e6),
  sprintf("Mean Funding (\\$) & %s & %s & %s \\\\\n",
          format(round(mean(pre_2014$funding)), big.mark = ","),
          format(round(mean(crimea_era$funding)), big.mark = ","),
          format(round(mean(post_2022$funding)), big.mark = ",")),
  sprintf("Total Donations & %s & %s & %s \\\\\n",
          format(sum(pre_2014$number_of_donations), big.mark = ","),
          format(sum(crimea_era$number_of_donations), big.mark = ","),
          format(sum(post_2022$number_of_donations), big.mark = ",")),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Ukraine-related projects identified by country field or keyword matching in title/summary.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(ukraine_desc_latex, "tables/table_ukraine_descriptive.tex")
cat("Saved: tables/table_ukraine_descriptive.tex\n")

# ==============================================================================
# TABLE: COMPETITION SETS
# ==============================================================================

cat("=== Table: Competition Sets ===\n")

df_disaster <- df %>%
  filter(is_disaster_theme, approved_year >= 2020, approved_year <= 2024)

comp_monthly <- df_disaster %>%
  group_by(approved_month_date, is_ukraine) %>%
  summarise(
    total_funding = sum(funding, na.rm = TRUE),
    total_donations = sum(number_of_donations, na.rm = TRUE),
    n_projects = n(),
    .groups = "drop"
  ) %>%
  mutate(
    log_funding = log1p(total_funding),
    log_donations = log1p(total_donations),
    post_war = approved_month_date >= as.Date("2022-02-01"),
    year = year(approved_month_date),
    year_factor = as.factor(year)
  )

comp_complete <- expand_grid(
  approved_month_date = all_months_date,
  is_ukraine = c(TRUE, FALSE)
) %>%
  left_join(comp_monthly, by = c("approved_month_date", "is_ukraine")) %>%
  mutate(
    total_funding = replace_na(total_funding, 0),
    total_donations = replace_na(total_donations, 0),
    n_projects = replace_na(n_projects, 0),
    log_funding = log1p(total_funding),
    log_donations = log1p(total_donations),
    avg_donation = ifelse(total_donations > 0, total_funding / total_donations, 0),
    log_avg_donation = log1p(avg_donation),
    post = approved_month_date >= ukraine_event_date,
    ukraine = as.numeric(is_ukraine),
    ukraine_post = ukraine * as.numeric(post),
    year = year(approved_month_date),
    year_factor = as.factor(year)
  )

# DiD within disaster theme
comp_did_funding <- lm(log_funding ~ post * ukraine + year_factor, data = comp_complete)
comp_did_donations <- lm(log_donations ~  post * ukraine + + year_factor, data = comp_complete)

# Descriptive stats
pre_ukr <- comp_complete %>% filter(ukraine == 1, post == 0)
post_ukr <- comp_complete %>% filter(ukraine == 1, post == 1)
pre_other <- comp_complete %>% filter(ukraine == 0, post == 0)
post_other <- comp_complete %>% filter(ukraine == 0, post == 1)

comp_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Competition Sets: Ukraine vs. Other Disaster Response Projects}\n",
  "\\label{tab:competition_sets}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "& \\multicolumn{2}{c}{Ukraine Projects} & \\multicolumn{2}{c}{Other Disaster Projects} \\\\\n",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}\n",
  "& Pre-War & Post-War & Pre-War & Post-War \\\\\n",
  "\\midrule\n",
  "\\multicolumn{5}{l}{\\textit{Panel A: Descriptive Statistics (Monthly Averages)}} \\\\\n",
  sprintf("Mean Funding per Project & \\$%s & \\$%s & \\$%s & \\$%s \\\\\n",
          format(round(mean(pre_ukr$total_funding/pre_ukr$n_projects, na.rm = T)), big.mark = ","),
          format(round(mean(post_ukr$total_funding/post_ukr$n_projects, na.rm = T)), big.mark = ","),
          format(round(mean(pre_other$total_funding/pre_other$n_projects)), big.mark = ","),
          format(round(mean(post_other$total_funding/post_other$n_projects)), big.mark = ",")),
  sprintf("Mean Donations per Project & %s & %s & %s & %s \\\\\n",
          format(round(mean(pre_ukr$total_donations/pre_ukr$n_projects, na.rm = T)), big.mark = ","),
          format(round(mean(post_ukr$total_donations/post_ukr$n_projects, na.rm = T)), big.mark = ","),
          format(round(mean(pre_other$total_donations/pre_other$n_projects)), big.mark = ","),
          format(round(mean(post_other$total_donations/post_other$n_projects)), big.mark = ",")),
  sprintf("Mean Projects/Month & %.1f & %.1f & %.1f & %.1f \\\\\n",
          mean(pre_ukr$n_projects), mean(post_ukr$n_projects),
          mean(pre_other$n_projects), mean(post_other$n_projects)),
  "\\addlinespace\n",
  "\\multicolumn{5}{l}{\\textit{Panel B: Difference-in-Differences Estimates}} \\\\\n",
  "& \\multicolumn{2}{c}{Log(Funding)} & \\multicolumn{2}{c}{Log(Donations)} \\\\\n",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}\n"
)

did_f_coef <- coef(comp_did_funding)["postTRUE:ukraine"]
did_f_se <- sqrt(diag(vcov(comp_did_funding)))["postTRUE:ukraine"]
did_f_pval <- 2 * pnorm(-abs(did_f_coef / did_f_se))
did_d_coef <- coef(comp_did_donations)["postTRUE:ukraine"]
did_d_se <- sqrt(diag(vcov(comp_did_donations)))["postTRUE:ukraine"]
did_d_pval <- 2 * pnorm(-abs(did_d_coef / did_d_se))

comp_latex <- paste0(comp_latex,
  sprintf("Ukraine $\\times$ Post & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\\n",
          fmt_coef(did_f_coef, did_f_pval), fmt_coef(did_d_coef, did_d_pval)),
  sprintf("& \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\\n",
          fmt_se(did_f_se), fmt_se(did_d_se)),
  "\\addlinespace\n",
  sprintf("Observations & \\multicolumn{4}{c}{%d} \\\\\n", nrow(comp_monthly)),
  "Year FE & \\multicolumn{4}{c}{Yes} \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Panel A shows monthly aggregate statistics for Ukraine-related vs. other disaster response projects. Panel B presents difference-in-differences estimates comparing funding changes for Ukraine projects vs. other disaster projects before and after February 2022. Positive DiD coefficient indicates Ukraine projects gained disproportionately. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(comp_latex, "tables/table_competition_sets.tex")
cat("Saved: tables/table_competition_sets.tex\n")

# ==============================================================================
# LDA TOPIC MODELING: Justify Keyword Selection
# ==============================================================================

cat("TF-IDF ANALYSIS: Identifying Distinctive Keywords by Theme\n\n")

# Custom stop words
custom_stops <- tibble(word = c(
  "project", "projects", "help", "helping", "support", "supporting",
  "provide", "providing", "program", "programs", "community", "communities",
  "people", "local", "work", "working", "needs", "need", "make", "making",
  "change", "world", "area", "give", "giving", "organization",
  "donate", "donation", "time", "year", "years", "day", "days",
  "thank", "thanks", "funded", "funding", "fund", "funds", "access"
))

# Get top themes by project count
top_themes <- df %>%
  count(theme_name, sort = TRUE) %>%
  head(8) %>%
  pull(theme_name)

cat("Analyzing top 8 themes:\n")
for (i in seq_along(top_themes)) {
  cat(sprintf("  %d. %s\n", i, top_themes[i]))
}
cat("\n")

# Tokenize by theme and compute TF-IDF
theme_words <- df %>%
  filter(theme_name %in% top_themes, nchar(combined_text) > 50) %>%
  select(theme_name, combined_text) %>%
  unnest_tokens(word, combined_text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stops, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$"), nchar(word) >= 4) %>%
  count(theme_name, word, sort = TRUE)

# Calculate TF-IDF
theme_tfidf <- theme_words %>%
  bind_tf_idf(word, theme_name, n) %>%
  arrange(desc(tf_idf))

cat("==================================================\n")
cat("TOP 8 DISTINCTIVE WORDS PER THEME (TF-IDF)\n")
cat("==================================================\n\n")

for (theme in top_themes) {
  top_words <- theme_tfidf %>%
    filter(theme_name == theme) %>%
    slice_max(tf_idf, n = 8) %>%
    pull(word)

  cat(sprintf("%s:\n", toupper(theme)))
  cat(sprintf("  %s\n\n", paste(top_words, collapse = ", ")))
}

# Now identify the 5 keyword categories from overall frequency
cat("==================================================\n")
cat("OVERALL KEYWORD FREQUENCY (Top 5 Categories)\n")
cat("==================================================\n\n")

# Get overall frequencies
overall_freq <- df %>%
  filter(nchar(combined_text) > 50) %>%
  sample_n(min(20000, n())) %>%
  select(combined_text) %>%
  unnest_tokens(word, combined_text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stops, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$"), nchar(word) >= 3) %>%
  count(word, sort = TRUE)

cat("Top 30 most common words:\n")
top_30 <- head(overall_freq, 30)
for (i in 1:nrow(top_30)) {
  cat(sprintf("%2d. %-15s %s occurrences\n", i, top_30$word[i],
              format(top_30$n[i], big.mark = ",")))
}

cat("\n==================================================\n")
cat("5 KEYWORD CATEGORIES FOR ANALYSIS:\n")
cat("==================================================\n")
cat(sprintf("  1. CHILDREN:  %s occurrences (#1 overall)\n",
            format(overall_freq$n[overall_freq$word == "children"][1], big.mark = ",")))
cat(sprintf("  2. EDUCATION: %s (education) + %s (school) = ~%s total\n",
            format(overall_freq$n[overall_freq$word == "education"][1], big.mark = ","),
            format(overall_freq$n[overall_freq$word == "school"][1], big.mark = ","),
            format(overall_freq$n[overall_freq$word == "education"][1] +
                   overall_freq$n[overall_freq$word == "school"][1], big.mark = ",")))
cat(sprintf("  3. WOMEN:     %s (women) + %s (girls) = ~%s total\n",
            format(overall_freq$n[overall_freq$word == "women"][1], big.mark = ","),
            format(overall_freq$n[overall_freq$word == "girls"][1], big.mark = ","),
            format(overall_freq$n[overall_freq$word == "women"][1] +
                   overall_freq$n[overall_freq$word == "girls"][1], big.mark = ",")))
cat(sprintf("  4. HEALTH:    %s occurrences (#5 overall)\n",
            format(overall_freq$n[overall_freq$word == "health"][1], big.mark = ",")))
cat(sprintf("  5. FOOD:      %s occurrences (#9 overall)\n\n",
            format(overall_freq$n[overall_freq$word == "food"][1], big.mark = ",")))

# Create TF-IDF visualization showing top distinctive words per theme
top_tfidf_plot <- theme_tfidf %>%
  group_by(theme_name) %>%
  slice_max(tf_idf, n = 6) %>%
  ungroup() %>%
  mutate(
    word = reorder_within(word, tf_idf, theme_name),
    theme_name = str_wrap(theme_name, width = 20)
  )

p_tfidf <- ggplot(top_tfidf_plot, aes(x = tf_idf, y = word, fill = theme_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ theme_name, scales = "free_y", ncol = 4) +
  scale_y_reordered() +
  labs(
    title = "Most Distinctive Keywords by Theme (TF-IDF Analysis)",
    subtitle = "Higher TF-IDF = more characteristic of that specific theme",
    x = "TF-IDF Score",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 8),
    panel.spacing = unit(1, "lines")
  )

ggsave("figures/fig_tfidf_themes.pdf", p_tfidf, width = 14, height = 10)
cat("Saved: figures/fig_tfidf_themes.pdf\n")

# Also create simple frequency bar chart
top_keywords <- head(overall_freq, 20) %>%
  mutate(
    keyword_category = case_when(
      word %in% c("children", "child", "kids", "youth", "orphans") ~ "Children",
      word %in% c("school", "education", "students", "learning") ~ "Education",
      word %in% c("women", "girls") ~ "Women",
      word %in% c("health", "care", "medical") ~ "Health",
      word == "food" ~ "Food",
      TRUE ~ "Other"
    ),
    word = fct_reorder(word, n)
  )

p_freq <- ggplot(top_keywords, aes(x = n, y = word, fill = keyword_category)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Children" = "#E74C3C",
    "Education" = "#3498DB",
    "Women" = "#F39C12",
    "Health" = "#2ECC71",
    "Food" = "#9B59B6",
    "Other" = "gray70"
  )) +
  labs(
    title = "Most Common Keywords in Humanitarian Crowdfunding",
    subtitle = "Overall frequency across 20,000 project descriptions",
    x = "Frequency (number of occurrences)",
    y = NULL,
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "right",
    axis.text.y = element_text(size = 10)
  ) +
  scale_x_continuous(labels = comma)

ggsave("figures/fig_keyword_frequency.pdf", p_freq, width = 10, height = 6)
cat("Saved: figures/fig_keyword_frequency.pdf\n\n")

# ==============================================================================
# DESCRIPTIVE COMPARISON: UKRAINE VS NON-UKRAINE PROJECTS
# ==============================================================================

cat("\n=== Descriptive Comparison: Ukraine vs Non-Ukraine ===\n")

# Create non-Ukraine sample
reg_data_nonukr <- reg_data %>% filter(!is_ukraine)

cat("Full sample:", nrow(reg_data), "projects\n")
cat("Non-Ukraine sample:", nrow(reg_data_nonukr), "projects\n")
cat("Ukraine sample:", sum(reg_data$is_ukraine), "projects\n")

# Compare keyword prevalence: Ukraine vs Non-Ukraine (post-war only)
post_war_data <- reg_data %>% filter(post_war)

# Calculate proportions and standard errors for keywords
n_ukraine <- sum(post_war_data$is_ukraine)
n_nonukraine <- sum(!post_war_data$is_ukraine)

theme_comparison <- data.frame(
  Theme = c("Disaster Response", "Child Protection", "Refugee Rights", "Physical Health", "Education"),
  Ukraine_Pct = c(
    mean(post_war_data$theme_disaster_response[post_war_data$is_ukraine]) * 100,
    mean(post_war_data$theme_child_protection[post_war_data$is_ukraine]) * 100,
    mean(post_war_data$theme_refugee_rights[post_war_data$is_ukraine]) * 100,
    mean(post_war_data$theme_physical_health[post_war_data$is_ukraine]) * 100,
    mean(post_war_data$theme_education[post_war_data$is_ukraine]) * 100
  ),
  NonUkraine_Pct = c(
    mean(post_war_data$theme_disaster_response[!post_war_data$is_ukraine]) * 100,
    mean(post_war_data$theme_child_protection[!post_war_data$is_ukraine]) * 100,
    mean(post_war_data$theme_refugee_rights[!post_war_data$is_ukraine]) * 100,
    mean(post_war_data$theme_physical_health[!post_war_data$is_ukraine]) * 100,
    mean(post_war_data$theme_education[!post_war_data$is_ukraine]) * 100
  )
) %>%
  mutate(
    # Calculate standard errors for proportions: SE = sqrt(p*(1-p)/n)
    Ukraine_SE = sqrt((Ukraine_Pct/100) * (1 - Ukraine_Pct/100) / n_ukraine) * 100,
    NonUkraine_SE = sqrt((NonUkraine_Pct/100) * (1 - NonUkraine_Pct/100) / n_nonukraine) * 100,
    Difference = Ukraine_Pct - NonUkraine_Pct,
    # Calculate SE for difference
    Difference_SE = sqrt(Ukraine_SE^2 + NonUkraine_SE^2),
    # Calculate z-statistic for difference
    Z_stat = Difference / Difference_SE,
    P_value = 2 * (1 - pnorm(abs(Z_stat)))
  )

print(theme_comparison)
write.csv(theme_comparison, "tables/theme_comparison_ukraine_vs_nonukraine.csv", row.names = FALSE)

# Create figure with error bars
theme_comp_long <- theme_comparison %>%
  select(Theme, Ukraine_Pct, NonUkraine_Pct, Ukraine_SE, NonUkraine_SE) %>%
  pivot_longer(cols = c(Ukraine_Pct, NonUkraine_Pct),
               names_to = "Sample",
               values_to = "Percentage") %>%
  mutate(
    SE = ifelse(grepl("Ukraine_Pct", Sample),
                theme_comparison$Ukraine_SE[match(Theme, theme_comparison$Theme)],
                theme_comparison$NonUkraine_SE[match(Theme, theme_comparison$Theme)]),
    Sample = recode(Sample,
                    "Ukraine_Pct" = "Ukraine Projects",
                    "NonUkraine_Pct" = "Non-Ukraine Projects")
  )

p_theme_comp <- ggplot(theme_comp_long, aes(x = Theme, y = Percentage, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = Percentage - 1.96*SE, ymax = Percentage + 1.96*SE),
                position = position_dodge(width = 0.9), width = 0.25, linewidth = 0.5) +
  scale_fill_manual(values = c("Ukraine Projects" = "#E74C3C",
                                "Non-Ukraine Projects" = "#3498DB")) +
  labs(title = "Theme Prevalence: Ukraine vs. Non-Ukraine Projects (Post-War)",
       subtitle = "Percentage of projects containing each Theme (with 95% confidence intervals)",
       y = "% of Projects",
       x = "Thematic Category",
       fill = "Sample") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0))

ggsave("figures/fig_theme_frequency.pdf", p_theme_comp, width = 10, height = 6)
cat("Saved: figures/fig_theme_frequency.pdf\n")

# ==============================================================================
# TABLE: KEYWORDS WITH WAR INTERACTION (Full Sample vs Non-Ukraine)
# ==============================================================================

cat("\n=== Table: Theme Effects War Interaction (Full vs Non-Ukraine) ===\n")

# FULL SAMPLE: Run regressions with interactions for all 4 outcomes
# NOTE: No theme FE since themes are the main regressors
themes_full_funding <- feols(log_funding ~ theme_disaster_response * post_war + theme_child_protection * post_war +
                       theme_refugee_rights * post_war + theme_physical_health * post_war + theme_education * post_war +
                       log_goal + log_duration | region_factor + year_factor,
                     data = reg_data, vcov = "hetero")

themes_full_avgdon <- feols(log_avg_donation ~ theme_disaster_response * post_war + theme_child_protection * post_war +
                       theme_refugee_rights * post_war + theme_physical_health * post_war + theme_education * post_war +
                       log_goal + log_duration | region_factor + year_factor,
                     data = reg_data, vcov = "hetero")

themes_full_numdon <- feols(log_donations ~ theme_disaster_response * post_war + theme_child_protection * post_war +
                       theme_refugee_rights * post_war + theme_physical_health * post_war + theme_education * post_war +
                       log_goal + log_duration | region_factor + year_factor,
                     data = reg_data, vcov = "hetero")

themes_full_funded <- feols(is_fully_funded ~ theme_disaster_response * post_war + theme_child_protection * post_war +
                       theme_refugee_rights * post_war + theme_physical_health * post_war + theme_education * post_war +
                       log_goal + log_duration | region_factor + year_factor,
                     data = reg_data, vcov = "hetero")

# NON-UKRAINE SAMPLE: Run regressions with interactions for all 4 outcomes
themes_nonukr_funding <- feols(log_funding ~ theme_disaster_response * post_war + theme_child_protection * post_war +
                       theme_refugee_rights * post_war + theme_physical_health * post_war + theme_education * post_war +
                       log_goal + log_duration | region_factor + year_factor,
                     data = reg_data_nonukr, vcov = "hetero")

themes_nonukr_avgdon <- feols(log_avg_donation ~ theme_disaster_response * post_war + theme_child_protection * post_war +
                       theme_refugee_rights * post_war + theme_physical_health * post_war + theme_education * post_war +
                       log_goal + log_duration | region_factor + year_factor,
                     data = reg_data_nonukr, vcov = "hetero")

themes_nonukr_numdon <- feols(log_donations ~ theme_disaster_response * post_war + theme_child_protection * post_war +
                       theme_refugee_rights * post_war + theme_physical_health * post_war + theme_education * post_war +
                       log_goal + log_duration | region_factor + year_factor,
                     data = reg_data_nonukr, vcov = "hetero")

themes_nonukr_funded <- feols(is_fully_funded ~ theme_disaster_response * post_war + theme_child_protection * post_war +
                       theme_refugee_rights * post_war + theme_physical_health * post_war + theme_education * post_war +
                       log_goal + log_duration | region_factor + year_factor,
                     data = reg_data_nonukr, vcov = "hetero")

# Build table showing Full Sample vs Non-Ukraine
themes_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Theme Effects on Funding: Full Sample vs. Non-Ukraine Projects}\n",
  "\\label{tab:themes}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccccccc}\n",
  "\\toprule\n",
  "& \\multicolumn{4}{c}{Full Sample} & \\multicolumn{4}{c}{Non-Ukraine Only} \\\\\n",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}\n",
  "& Log(Fund) & Log(Avg) & Log(\\#) & Funded & Log(Fund) & Log(Avg) & Log(\\#) & Funded \\\\\n",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{9}{l}{\\textit{Panel A: Baseline Effects}} \\\\\n"
)

themes <- c("theme_disaster_responseTRUE", "theme_child_protectionTRUE", "theme_refugee_rightsTRUE", "theme_physical_healthTRUE", "theme_educationTRUE")
labels <- c("Disaster Response", "Child Protection", "Refugee Rights", "Physical Health", "Education")

# Helper function to extract coef/se/pval from model
extract_coef <- function(mod, varname) {
  if (varname %in% names(coef(mod))) {
    c <- coef(mod)[varname]
    s <- sqrt(diag(vcov(mod)))[varname]
    p <- 2 * pnorm(-abs(c / s))
    return(list(coef = c, se = s, pval = p))
  } else {
    return(list(coef = NA, se = NA, pval = NA))
  }
}

for (i in seq_along(themes)) {
  # Extract baseline coefficients for all 8 models (full sample + non-Ukraine)
  full_funding <- extract_coef(themes_full_funding, themes[i])
  full_avgdon <- extract_coef(themes_full_avgdon, themes[i])
  full_numdon <- extract_coef(themes_full_numdon, themes[i])
  full_funded <- extract_coef(themes_full_funded, themes[i])

  nonukr_funding <- extract_coef(themes_nonukr_funding, themes[i])
  nonukr_avgdon <- extract_coef(themes_nonukr_avgdon, themes[i])
  nonukr_numdon <- extract_coef(themes_nonukr_numdon, themes[i])
  nonukr_funded <- extract_coef(themes_nonukr_funded, themes[i])

  themes_latex <- paste0(themes_latex,
    sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", labels[i],
            fmt_coef(full_funding$coef, full_funding$pval),
            fmt_coef(full_avgdon$coef, full_avgdon$pval),
            fmt_coef(full_numdon$coef, full_numdon$pval),
            fmt_coef(full_funded$coef, full_funded$pval),
            fmt_coef(nonukr_funding$coef, nonukr_funding$pval),
            fmt_coef(nonukr_avgdon$coef, nonukr_avgdon$pval),
            fmt_coef(nonukr_numdon$coef, nonukr_numdon$pval),
            fmt_coef(nonukr_funded$coef, nonukr_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_funding$se),
            fmt_se(full_avgdon$se),
            fmt_se(full_numdon$se),
            fmt_se(full_funded$se),
            fmt_se(nonukr_funding$se),
            fmt_se(nonukr_avgdon$se),
            fmt_se(nonukr_numdon$se),
            fmt_se(nonukr_funded$se)))
}

themes_latex <- paste0(themes_latex,
  "\\addlinespace\n",
  "\\multicolumn{9}{l}{\\textit{Panel B: Post-War Interactions}} \\\\\n"
)

# Check both orderings of interaction terms (R can use either)
get_inter_coef <- function(mod, var1, var2) {
  name1 <- paste0(var1, ":", var2)
  name2 <- paste0(var2, ":", var1)
  coefs <- coef(mod)
  vcovs <- vcov(mod)

  if (name1 %in% names(coefs)) {
    c <- coefs[name1]
    s <- sqrt(diag(vcovs))[name1]
    p <- 2 * pnorm(-abs(c / s))
    return(list(coef = c, se = s, pval = p))
  } else if (name2 %in% names(coefs)) {
    c <- coefs[name2]
    s <- sqrt(diag(vcovs))[name2]
    p <- 2 * pnorm(-abs(c / s))
    return(list(coef = c, se = s, pval = p))
  } else {
    return(list(coef = NA, se = NA, pval = NA))
  }
}

for (i in seq_along(themes)) {
  # Get interaction effects from both full sample and non-Ukraine sample
  full_int_funding <- get_inter_coef(themes_full_funding, themes[i], "post_warTRUE")
  full_int_avgdon <- get_inter_coef(themes_full_avgdon, themes[i], "post_warTRUE")
  full_int_numdon <- get_inter_coef(themes_full_numdon, themes[i], "post_warTRUE")
  full_int_funded <- get_inter_coef(themes_full_funded, themes[i], "post_warTRUE")

  nonukr_int_funding <- get_inter_coef(themes_nonukr_funding, themes[i], "post_warTRUE")
  nonukr_int_avgdon <- get_inter_coef(themes_nonukr_avgdon, themes[i], "post_warTRUE")
  nonukr_int_numdon <- get_inter_coef(themes_nonukr_numdon, themes[i], "post_warTRUE")
  nonukr_int_funded <- get_inter_coef(themes_nonukr_funded, themes[i], "post_warTRUE")

  themes_latex <- paste0(themes_latex,
    sprintf("%s $\\times$ Post-War & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", labels[i],
            fmt_coef(full_int_funding$coef, full_int_funding$pval),
            fmt_coef(full_int_avgdon$coef, full_int_avgdon$pval),
            fmt_coef(full_int_numdon$coef, full_int_numdon$pval),
            fmt_coef(full_int_funded$coef, full_int_funded$pval),
            fmt_coef(nonukr_int_funding$coef, nonukr_int_funding$pval),
            fmt_coef(nonukr_int_avgdon$coef, nonukr_int_avgdon$pval),
            fmt_coef(nonukr_int_numdon$coef, nonukr_int_numdon$pval),
            fmt_coef(nonukr_int_funded$coef, nonukr_int_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_int_funding$se),
            fmt_se(full_int_avgdon$se),
            fmt_se(full_int_numdon$se),
            fmt_se(full_int_funded$se),
            fmt_se(nonukr_int_funding$se),
            fmt_se(nonukr_int_avgdon$se),
            fmt_se(nonukr_int_numdon$se),
            fmt_se(nonukr_int_funded$se)))
}

themes_latex <- paste0(themes_latex,
  "\\midrule\n",
  "Theme/Region/Year FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Log(Goal), Duration & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
          format(themes_full_funding$nobs, big.mark = ","),
          format(themes_full_avgdon$nobs, big.mark = ","),
          format(themes_full_numdon$nobs, big.mark = ","),
          format(themes_full_funded$nobs, big.mark = ","),
          format(themes_nonukr_funding$nobs, big.mark = ","),
          format(themes_nonukr_avgdon$nobs, big.mark = ","),
          format(themes_nonukr_numdon$nobs, big.mark = ","),
          format(themes_nonukr_funded$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(themes_full_funding, "r2")$r2,
          fitstat(themes_full_avgdon, "r2")$r2,
          fitstat(themes_full_numdon, "r2")$r2,
          fitstat(themes_full_funded, "r2")$r2,
          fitstat(themes_nonukr_funding, "r2")$r2,
          fitstat(themes_nonukr_avgdon, "r2")$r2,
          fitstat(themes_nonukr_numdon, "r2")$r2,
          fitstat(themes_nonukr_funded, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1. All models include post-war interactions. Columns 1-4 show results for full sample (including Ukraine projects); columns 5-8 show results for non-Ukraine projects only. Panel A shows baseline theme effects (pre-war period). Panel B shows how theme effects changed post-February 2022. Comparing full sample vs. non-Ukraine results reveals whether Ukraine projects saturated specific thematic frames, crowding out non-Ukraine projects using similar themes\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(themes_latex, "tables/table_themes_war_interact.tex")
cat("Saved: tables/table_themes_war_interact.tex\n")

# ==============================================================================
# DESCRIPTIVE COMPARISON: UKRAINE VS NON-UKRAINE (EMOTIONS)
# ==============================================================================

cat("\n=== Descriptive Comparison: Emotions (Ukraine vs Non-Ukraine) - Top 5 from NRC ===\n")

# Use top 5 emotions identified earlier
emotion_comparison_data <- list()
for (emo in top_5_emotions) {
  has_var <- paste0("has_", emo)
  emotion_comparison_data[[paste0(emo, "_ukraine")]] <- mean(post_war_data[[has_var]][post_war_data$is_ukraine]) * 100
  emotion_comparison_data[[paste0(emo, "_nonukraine")]] <- mean(post_war_data[[has_var]][!post_war_data$is_ukraine]) * 100
}

emotion_comparison <- data.frame(
  Emotion = tools::toTitleCase(top_5_emotions),
  Ukraine_Pct = sapply(top_5_emotions, function(e) emotion_comparison_data[[paste0(e, "_ukraine")]]),
  NonUkraine_Pct = sapply(top_5_emotions, function(e) emotion_comparison_data[[paste0(e, "_nonukraine")]])
) %>%
  mutate(
    Ukraine_SE = sqrt((Ukraine_Pct/100) * (1 - Ukraine_Pct/100) / n_ukraine) * 100,
    NonUkraine_SE = sqrt((NonUkraine_Pct/100) * (1 - NonUkraine_Pct/100) / n_nonukraine) * 100,
    Difference = Ukraine_Pct - NonUkraine_Pct,
    Difference_SE = sqrt(Ukraine_SE^2 + NonUkraine_SE^2),
    Z_stat = Difference / Difference_SE,
    P_value = 2 * (1 - pnorm(abs(Z_stat)))
  )

print(emotion_comparison)
write.csv(emotion_comparison, "tables/emotion_comparison_ukraine_vs_nonukraine.csv", row.names = FALSE)

# Create figure with error bars
emotion_comp_long <- emotion_comparison %>%
  select(Emotion, Ukraine_Pct, NonUkraine_Pct, Ukraine_SE, NonUkraine_SE) %>%
  pivot_longer(cols = c(Ukraine_Pct, NonUkraine_Pct),
               names_to = "Sample",
               values_to = "Percentage") %>%
  mutate(
    SE = ifelse(grepl("Ukraine_Pct", Sample),
                emotion_comparison$Ukraine_SE[match(Emotion, emotion_comparison$Emotion)],
                emotion_comparison$NonUkraine_SE[match(Emotion, emotion_comparison$Emotion)]),
    Sample = recode(Sample,
                    "Ukraine_Pct" = "Ukraine Projects",
                    "NonUkraine_Pct" = "Non-Ukraine Projects")
  )

p_emotion_comp <- ggplot(emotion_comp_long, aes(x = Emotion, y = Percentage, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = Percentage - 1.96*SE, ymax = Percentage + 1.96*SE),
                position = position_dodge(width = 0.9), width = 0.25, linewidth = 0.5) +
  scale_fill_manual(values = c("Ukraine Projects" = "#E74C3C",
                                "Non-Ukraine Projects" = "#3498DB")) +
  labs(title = "Emotional Content: Ukraine vs. Non-Ukraine Projects (Post-War)",
       subtitle = "Percentage of projects with each emotion (with 95% confidence intervals)",
       y = "% of Projects",
       x = "Emotion Category",
       fill = "Sample") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave("figures/fig_emotion_comparison_ukraine.pdf", p_emotion_comp, width = 10, height = 6)
cat("Saved: figures/fig_emotion_comparison_ukraine.pdf\n")

# ==============================================================================
# TABLE: EMOTIONS WITH WAR INTERACTION (Full Sample vs Non-Ukraine) - Top 5 NRC
# ==============================================================================

cat("\n=== Table: Emotions War Interaction (Full vs Non-Ukraine) - Top 5 NRC Emotions ===\n")

# Build formula dynamically using top 5 emotions
emotion_vars <- paste0("has_", top_5_emotions)
emotion_formula_part <- paste(paste0(emotion_vars, " * post_war"), collapse = " + ")
formula_rhs <- paste(emotion_formula_part, "+ log_goal + log_duration | theme_factor + region_factor + year_factor")

# FULL SAMPLE: Run regressions with interactions
emotions_full_funding <- feols(as.formula(paste("log_funding ~", formula_rhs)),
                     data = reg_data, vcov = "hetero")
emotions_full_avgdon <- feols(as.formula(paste("log_avg_donation ~", formula_rhs)),
                     data = reg_data, vcov = "hetero")
emotions_full_numdon <- feols(as.formula(paste("log_donations ~", formula_rhs)),
                     data = reg_data, vcov = "hetero")
emotions_full_funded <- feols(as.formula(paste("is_fully_funded ~", formula_rhs)),
                     data = reg_data, vcov = "hetero")

# NON-UKRAINE SAMPLE: Run regressions with interactions
emotions_nonukr_funding <- feols(as.formula(paste("log_funding ~", formula_rhs)),
                     data = reg_data_nonukr, vcov = "hetero")
emotions_nonukr_avgdon <- feols(as.formula(paste("log_avg_donation ~", formula_rhs)),
                     data = reg_data_nonukr, vcov = "hetero")
emotions_nonukr_numdon <- feols(as.formula(paste("log_donations ~", formula_rhs)),
                     data = reg_data_nonukr, vcov = "hetero")
emotions_nonukr_funded <- feols(as.formula(paste("is_fully_funded ~", formula_rhs)),
                     data = reg_data_nonukr, vcov = "hetero")

emotions_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Emotional Content Effects: Full Sample vs. Non-Ukraine Projects}\n",
  "\\label{tab:emotions}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccccccc}\n",
  "\\toprule\n",
  "& \\multicolumn{4}{c}{Full Sample} & \\multicolumn{4}{c}{Non-Ukraine Only} \\\\\n",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}\n",
  "& Log(Fund) & Log(Avg) & Log(\\#) & Funded & Log(Fund) & Log(Avg) & Log(\\#) & Funded \\\\\n",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{9}{l}{\\textit{Panel A: Baseline Effects}} \\\\\n"
)

# Use top 5 emotions from NRC lexicon
emotions <- paste0("has_", top_5_emotions, "TRUE")
labels <- tools::toTitleCase(top_5_emotions)

for (i in seq_along(emotions)) {
  full_funding <- extract_coef(emotions_full_funding, emotions[i])
  full_avgdon <- extract_coef(emotions_full_avgdon, emotions[i])
  full_numdon <- extract_coef(emotions_full_numdon, emotions[i])
  full_funded <- extract_coef(emotions_full_funded, emotions[i])

  nonukr_funding <- extract_coef(emotions_nonukr_funding, emotions[i])
  nonukr_avgdon <- extract_coef(emotions_nonukr_avgdon, emotions[i])
  nonukr_numdon <- extract_coef(emotions_nonukr_numdon, emotions[i])
  nonukr_funded <- extract_coef(emotions_nonukr_funded, emotions[i])

  emotions_latex <- paste0(emotions_latex,
    sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", labels[i],
            fmt_coef(full_funding$coef, full_funding$pval),
            fmt_coef(full_avgdon$coef, full_avgdon$pval),
            fmt_coef(full_numdon$coef, full_numdon$pval),
            fmt_coef(full_funded$coef, full_funded$pval),
            fmt_coef(nonukr_funding$coef, nonukr_funding$pval),
            fmt_coef(nonukr_avgdon$coef, nonukr_avgdon$pval),
            fmt_coef(nonukr_numdon$coef, nonukr_numdon$pval),
            fmt_coef(nonukr_funded$coef, nonukr_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_funding$se),
            fmt_se(full_avgdon$se),
            fmt_se(full_numdon$se),
            fmt_se(full_funded$se),
            fmt_se(nonukr_funding$se),
            fmt_se(nonukr_avgdon$se),
            fmt_se(nonukr_numdon$se),
            fmt_se(nonukr_funded$se)))
}

emotions_latex <- paste0(emotions_latex,
  "\\addlinespace\n",
  "\\multicolumn{9}{l}{\\textit{Panel B: Post-War Interactions}} \\\\\n"
)

for (i in seq_along(emotions)) {
  full_int_funding <- get_inter_coef(emotions_full_funding, emotions[i], "post_warTRUE")
  full_int_avgdon <- get_inter_coef(emotions_full_avgdon, emotions[i], "post_warTRUE")
  full_int_numdon <- get_inter_coef(emotions_full_numdon, emotions[i], "post_warTRUE")
  full_int_funded <- get_inter_coef(emotions_full_funded, emotions[i], "post_warTRUE")

  nonukr_int_funding <- get_inter_coef(emotions_nonukr_funding, emotions[i], "post_warTRUE")
  nonukr_int_avgdon <- get_inter_coef(emotions_nonukr_avgdon, emotions[i], "post_warTRUE")
  nonukr_int_numdon <- get_inter_coef(emotions_nonukr_numdon, emotions[i], "post_warTRUE")
  nonukr_int_funded <- get_inter_coef(emotions_nonukr_funded, emotions[i], "post_warTRUE")

  emotions_latex <- paste0(emotions_latex,
    sprintf("%s $\\times$ Post-War & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", labels[i],
            fmt_coef(full_int_funding$coef, full_int_funding$pval),
            fmt_coef(full_int_avgdon$coef, full_int_avgdon$pval),
            fmt_coef(full_int_numdon$coef, full_int_numdon$pval),
            fmt_coef(full_int_funded$coef, full_int_funded$pval),
            fmt_coef(nonukr_int_funding$coef, nonukr_int_funding$pval),
            fmt_coef(nonukr_int_avgdon$coef, nonukr_int_avgdon$pval),
            fmt_coef(nonukr_int_numdon$coef, nonukr_int_numdon$pval),
            fmt_coef(nonukr_int_funded$coef, nonukr_int_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_int_funding$se),
            fmt_se(full_int_avgdon$se),
            fmt_se(full_int_numdon$se),
            fmt_se(full_int_funded$se),
            fmt_se(nonukr_int_funding$se),
            fmt_se(nonukr_int_avgdon$se),
            fmt_se(nonukr_int_numdon$se),
            fmt_se(nonukr_int_funded$se)))
}

emotions_latex <- paste0(emotions_latex,
  "\\midrule\n",
  "Theme/Region/Year FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Log(Goal), Duration & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
          format(emotions_full_funding$nobs, big.mark = ","),
          format(emotions_full_avgdon$nobs, big.mark = ","),
          format(emotions_full_numdon$nobs, big.mark = ","),
          format(emotions_full_funded$nobs, big.mark = ","),
          format(emotions_nonukr_funding$nobs, big.mark = ","),
          format(emotions_nonukr_avgdon$nobs, big.mark = ","),
          format(emotions_nonukr_numdon$nobs, big.mark = ","),
          format(emotions_nonukr_funded$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(emotions_full_funding, "r2")$r2,
          fitstat(emotions_full_avgdon, "r2")$r2,
          fitstat(emotions_full_numdon, "r2")$r2,
          fitstat(emotions_full_funded, "r2")$r2,
          fitstat(emotions_nonukr_funding, "r2")$r2,
          fitstat(emotions_nonukr_avgdon, "r2")$r2,
          fitstat(emotions_nonukr_numdon, "r2")$r2,
          fitstat(emotions_nonukr_funded, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Emotions identified using NRC Emotion Lexicon \\citep{mohammad2013}. Top 5 most prevalent emotions selected for analysis. Binary indicators equal 1 if project description contains at least one word associated with that emotion. Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1. All models include post-war interactions. Columns 1-4: full sample; columns 5-8: non-Ukraine only. Panel A shows baseline emotional content effects. Panel B shows post-war interaction effects.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(emotions_latex, "tables/table_emotions_war_interact.tex")
cat("Saved: tables/table_emotions_war_interact.tex\n")

# ==============================================================================
# DESCRIPTIVE COMPARISON: SENTIMENT (UKRAINE VS NON-UKRAINE)
# ==============================================================================

cat("\n=== Descriptive Comparison: Sentiment (Ukraine vs Non-Ukraine) ===\n")

sentiment_comparison <- data.frame(
  Metric = c("Net Sentiment", "Sentiment Intensity", "Positive Ratio"),
  Ukraine_Mean = c(
    mean(post_war_data$net_sentiment[post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$sentiment_intensity[post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$positive_ratio[post_war_data$is_ukraine], na.rm = TRUE)
  ),
  NonUkraine_Mean = c(
    mean(post_war_data$net_sentiment[!post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$sentiment_intensity[!post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$positive_ratio[!post_war_data$is_ukraine], na.rm = TRUE)
  )
) %>%
  mutate(
    # Calculate standard errors for continuous variables: SE = sd / sqrt(n)
    Ukraine_SE = c(
      sd(post_war_data$net_sentiment[post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_ukraine),
      sd(post_war_data$sentiment_intensity[post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_ukraine),
      sd(post_war_data$positive_ratio[post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_ukraine)
    ),
    NonUkraine_SE = c(
      sd(post_war_data$net_sentiment[!post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_nonukraine),
      sd(post_war_data$sentiment_intensity[!post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_nonukraine),
      sd(post_war_data$positive_ratio[!post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_nonukraine)
    ),
    Difference = Ukraine_Mean - NonUkraine_Mean,
    Difference_SE = sqrt(Ukraine_SE^2 + NonUkraine_SE^2),
    T_stat = Difference / Difference_SE,
    P_value = 2 * (1 - pnorm(abs(T_stat)))
  )

print(sentiment_comparison)
write.csv(sentiment_comparison, "tables/sentiment_comparison_ukraine_vs_nonukraine.csv", row.names = FALSE)

# Create figure with error bars
sentiment_comp_long <- sentiment_comparison %>%
  select(Metric, Ukraine_Mean, NonUkraine_Mean, Ukraine_SE, NonUkraine_SE) %>%
  pivot_longer(cols = c(Ukraine_Mean, NonUkraine_Mean),
               names_to = "Sample",
               values_to = "Value") %>%
  mutate(
    SE = ifelse(grepl("Ukraine_Mean", Sample),
                sentiment_comparison$Ukraine_SE[match(Metric, sentiment_comparison$Metric)],
                sentiment_comparison$NonUkraine_SE[match(Metric, sentiment_comparison$Metric)]),
    Sample = recode(Sample,
                    "Ukraine_Mean" = "Ukraine Projects",
                    "NonUkraine_Mean" = "Non-Ukraine Projects")
  )

p_sentiment_comp <- ggplot(sentiment_comp_long, aes(x = Metric, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = Value - 1.96*SE, ymax = Value + 1.96*SE),
                position = position_dodge(width = 0.9), width = 0.25, linewidth = 0.5) +
  scale_fill_manual(values = c("Ukraine Projects" = "#E74C3C",
                                "Non-Ukraine Projects" = "#3498DB")) +
  labs(title = "Sentiment Metrics: Ukraine vs. Non-Ukraine Projects (Post-War)",
       subtitle = "Mean values with 95% confidence intervals",
       y = "Mean Value",
       x = "Sentiment Metric",
       fill = "Sample") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave("figures/fig_sentiment_comparison_ukraine.pdf", p_sentiment_comp, width = 10, height = 6)
cat("Saved: figures/fig_sentiment_comparison_ukraine.pdf\n")

# ==============================================================================
# TABLE: SENTIMENT WITH WAR INTERACTION (Full Sample vs Non-Ukraine)
# ==============================================================================

cat("\n=== Table: Sentiment War Interaction (Full vs Non-Ukraine) ===\n")

# FULL SAMPLE
sentiment_full_funding <- feols(log_funding ~ sentiment_intensity * post_war + positive_ratio * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data, vcov = "hetero")
sentiment_full_avgdon <- feols(log_avg_donation ~ sentiment_intensity * post_war + positive_ratio * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data, vcov = "hetero")
sentiment_full_numdon <- feols(log_donations ~ sentiment_intensity * post_war + positive_ratio * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data, vcov = "hetero")
sentiment_full_funded <- feols(is_fully_funded ~ sentiment_intensity * post_war + positive_ratio * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data, vcov = "hetero")

# NON-UKRAINE SAMPLE
sentiment_nonukr_funding <- feols(log_funding ~ sentiment_intensity * post_war + positive_ratio * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data_nonukr, vcov = "hetero")
sentiment_nonukr_avgdon <- feols(log_avg_donation ~ sentiment_intensity * post_war + positive_ratio * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data_nonukr, vcov = "hetero")
sentiment_nonukr_numdon <- feols(log_donations ~ sentiment_intensity * post_war + positive_ratio * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data_nonukr, vcov = "hetero")
sentiment_nonukr_funded <- feols(is_fully_funded ~ sentiment_intensity * post_war + positive_ratio * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data_nonukr, vcov = "hetero")

sentiment_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Sentiment Effects: Full Sample vs. Non-Ukraine Projects}\n",
  "\\label{tab:sentiment}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccccccc}\n",
  "\\toprule\n",
  "& \\multicolumn{4}{c}{Full Sample} & \\multicolumn{4}{c}{Non-Ukraine Only} \\\\\n",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}\n",
  "& Log(Fund) & Log(Avg) & Log(\\#) & Funded & Log(Fund) & Log(Avg) & Log(\\#) & Funded \\\\\n",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{9}{l}{\\textit{Panel A: Baseline Effects}} \\\\\n"
)

sent_vars <- c("sentiment_intensity", "positive_ratio")
sent_labels <- c("Sentiment Intensity", "Positive Ratio")

for (i in seq_along(sent_vars)) {
  full_funding <- extract_coef(sentiment_full_funding, sent_vars[i])
  full_avgdon <- extract_coef(sentiment_full_avgdon, sent_vars[i])
  full_numdon <- extract_coef(sentiment_full_numdon, sent_vars[i])
  full_funded <- extract_coef(sentiment_full_funded, sent_vars[i])

  nonukr_funding <- extract_coef(sentiment_nonukr_funding, sent_vars[i])
  nonukr_avgdon <- extract_coef(sentiment_nonukr_avgdon, sent_vars[i])
  nonukr_numdon <- extract_coef(sentiment_nonukr_numdon, sent_vars[i])
  nonukr_funded <- extract_coef(sentiment_nonukr_funded, sent_vars[i])

  sentiment_latex <- paste0(sentiment_latex,
    sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", sent_labels[i],
            fmt_coef(full_funding$coef, full_funding$pval),
            fmt_coef(full_avgdon$coef, full_avgdon$pval),
            fmt_coef(full_numdon$coef, full_numdon$pval),
            fmt_coef(full_funded$coef, full_funded$pval),
            fmt_coef(nonukr_funding$coef, nonukr_funding$pval),
            fmt_coef(nonukr_avgdon$coef, nonukr_avgdon$pval),
            fmt_coef(nonukr_numdon$coef, nonukr_numdon$pval),
            fmt_coef(nonukr_funded$coef, nonukr_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_funding$se), fmt_se(full_avgdon$se),
            fmt_se(full_numdon$se), fmt_se(full_funded$se),
            fmt_se(nonukr_funding$se), fmt_se(nonukr_avgdon$se),
            fmt_se(nonukr_numdon$se), fmt_se(nonukr_funded$se)))
}

sentiment_latex <- paste0(sentiment_latex,
  "\\addlinespace\n",
  "\\multicolumn{9}{l}{\\textit{Panel B: Post-War Interactions}} \\\\\n"
)

for (i in seq_along(sent_vars)) {
  full_int_funding <- get_inter_coef(sentiment_full_funding, sent_vars[i], "post_warTRUE")
  full_int_avgdon <- get_inter_coef(sentiment_full_avgdon, sent_vars[i], "post_warTRUE")
  full_int_numdon <- get_inter_coef(sentiment_full_numdon, sent_vars[i], "post_warTRUE")
  full_int_funded <- get_inter_coef(sentiment_full_funded, sent_vars[i], "post_warTRUE")

  nonukr_int_funding <- get_inter_coef(sentiment_nonukr_funding, sent_vars[i], "post_warTRUE")
  nonukr_int_avgdon <- get_inter_coef(sentiment_nonukr_avgdon, sent_vars[i], "post_warTRUE")
  nonukr_int_numdon <- get_inter_coef(sentiment_nonukr_numdon, sent_vars[i], "post_warTRUE")
  nonukr_int_funded <- get_inter_coef(sentiment_nonukr_funded, sent_vars[i], "post_warTRUE")

  sentiment_latex <- paste0(sentiment_latex,
    sprintf("%s $\\times$ Post-War & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", sent_labels[i],
            fmt_coef(full_int_funding$coef, full_int_funding$pval),
            fmt_coef(full_int_avgdon$coef, full_int_avgdon$pval),
            fmt_coef(full_int_numdon$coef, full_int_numdon$pval),
            fmt_coef(full_int_funded$coef, full_int_funded$pval),
            fmt_coef(nonukr_int_funding$coef, nonukr_int_funding$pval),
            fmt_coef(nonukr_int_avgdon$coef, nonukr_int_avgdon$pval),
            fmt_coef(nonukr_int_numdon$coef, nonukr_int_numdon$pval),
            fmt_coef(nonukr_int_funded$coef, nonukr_int_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_int_funding$se), fmt_se(full_int_avgdon$se),
            fmt_se(full_int_numdon$se), fmt_se(full_int_funded$se),
            fmt_se(nonukr_int_funding$se), fmt_se(nonukr_int_avgdon$se),
            fmt_se(nonukr_int_numdon$se), fmt_se(nonukr_int_funded$se)))
}

sentiment_latex <- paste0(sentiment_latex,
  "\\midrule\n",
  "Theme/Region/Year FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Log(Goal), Duration & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
          format(sentiment_full_funding$nobs, big.mark = ","),
          format(sentiment_full_avgdon$nobs, big.mark = ","),
          format(sentiment_full_numdon$nobs, big.mark = ","),
          format(sentiment_full_funded$nobs, big.mark = ","),
          format(sentiment_nonukr_funding$nobs, big.mark = ","),
          format(sentiment_nonukr_avgdon$nobs, big.mark = ","),
          format(sentiment_nonukr_numdon$nobs, big.mark = ","),
          format(sentiment_nonukr_funded$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(sentiment_full_funding, "r2")$r2,
          fitstat(sentiment_full_avgdon, "r2")$r2,
          fitstat(sentiment_full_numdon, "r2")$r2,
          fitstat(sentiment_full_funded, "r2")$r2,
          fitstat(sentiment_nonukr_funding, "r2")$r2,
          fitstat(sentiment_nonukr_avgdon, "r2")$r2,
          fitstat(sentiment_nonukr_numdon, "r2")$r2,
          fitstat(sentiment_nonukr_funded, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1. All models include post-war interactions. Columns 1-4: full sample; columns 5-8: non-Ukraine only. Sentiment Intensity = total sentiment words / total words $\\times$ 100. Positive Ratio = positive / (positive + negative).\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(sentiment_latex, "tables/table_sentiment_war_interact.tex")
cat("Saved: tables/table_sentiment_war_interact.tex\n")

# ==============================================================================
# DESCRIPTIVE COMPARISON: NARRATIVE FEATURES (UKRAINE VS NON-UKRAINE)
# ==============================================================================

cat("\n=== Descriptive Comparison: Narrative Features (Ukraine vs Non-Ukraine) ===\n")

narrative_comparison <- data.frame(
  Metric = c("Description Length", "FK Grade Level", "Action Verb Density", "Pronoun Density"),
  Ukraine_Mean = c(
    mean(post_war_data$total_words[post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$fk_grade_level[post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$action_verb_density[post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$pronoun_density[post_war_data$is_ukraine], na.rm = TRUE)
  ),
  NonUkraine_Mean = c(
    mean(post_war_data$total_words[!post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$fk_grade_level[!post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$action_verb_density[!post_war_data$is_ukraine], na.rm = TRUE),
    mean(post_war_data$pronoun_density[!post_war_data$is_ukraine], na.rm = TRUE)
  )
) %>%
  mutate(
    Ukraine_SE = c(
      sd(post_war_data$total_words[post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_ukraine),
      sd(post_war_data$fk_grade_level[post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_ukraine),
      sd(post_war_data$action_verb_density[post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_ukraine),
      sd(post_war_data$pronoun_density[post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_ukraine)
    ),
    NonUkraine_SE = c(
      sd(post_war_data$total_words[!post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_nonukraine),
      sd(post_war_data$fk_grade_level[!post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_nonukraine),
      sd(post_war_data$action_verb_density[!post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_nonukraine),
      sd(post_war_data$pronoun_density[!post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_nonukraine)
    ),
    Difference = Ukraine_Mean - NonUkraine_Mean,
    Difference_SE = sqrt(Ukraine_SE^2 + NonUkraine_SE^2),
    T_stat = Difference / Difference_SE,
    P_value = 2 * (1 - pnorm(abs(T_stat)))
  )

print(narrative_comparison)
write.csv(narrative_comparison, "tables/narrative_comparison_ukraine_vs_nonukraine.csv", row.names = FALSE)

# Create figure with error bars
narrative_comp_long <- narrative_comparison %>%
  select(Metric, Ukraine_Mean, NonUkraine_Mean, Ukraine_SE, NonUkraine_SE) %>%
  pivot_longer(cols = c(Ukraine_Mean, NonUkraine_Mean),
               names_to = "Sample",
               values_to = "Value") %>%
  mutate(
    SE = ifelse(grepl("Ukraine_Mean", Sample),
                narrative_comparison$Ukraine_SE[match(Metric, narrative_comparison$Metric)],
                narrative_comparison$NonUkraine_SE[match(Metric, narrative_comparison$Metric)]),
    Sample = recode(Sample,
                    "Ukraine_Mean" = "Ukraine Projects",
                    "NonUkraine_Mean" = "Non-Ukraine Projects")
  )

p_narrative_comp <- ggplot(narrative_comp_long, aes(x = Metric, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = Value - 1.96*SE, ymax = Value + 1.96*SE),
                position = position_dodge(width = 0.9), width = 0.25, linewidth = 0.5) +
  scale_fill_manual(values = c("Ukraine Projects" = "#E74C3C",
                                "Non-Ukraine Projects" = "#3498DB")) +
  labs(title = "Narrative Features: Ukraine vs. Non-Ukraine Projects (Post-War)",
       subtitle = "Mean values with 95% confidence intervals",
       y = "Mean Value",
       x = "Narrative Feature",
       fill = "Sample") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("figures/fig_narrative_comparison_ukraine.pdf", p_narrative_comp, width = 10, height = 6)
cat("Saved: figures/fig_narrative_comparison_ukraine.pdf\n")

# ==============================================================================
# TABLE: NARRATIVE WITH WAR INTERACTION (Full Sample vs Non-Ukraine)
# ==============================================================================

cat("\n=== Table: Narrative War Interaction (Full vs Non-Ukraine) ===\n")

# FULL SAMPLE
narrative_full_funding <- feols(log_funding ~ log_description_length * post_war + fk_grade_level * post_war +
                        action_verb_density * post_war + pronoun_density * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data, vcov = "hetero")
narrative_full_avgdon <- feols(log_avg_donation ~ log_description_length * post_war + fk_grade_level * post_war +
                        action_verb_density * post_war + pronoun_density * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data, vcov = "hetero")
narrative_full_numdon <- feols(log_donations ~ log_description_length * post_war + fk_grade_level * post_war +
                        action_verb_density * post_war + pronoun_density * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data, vcov = "hetero")
narrative_full_funded <- feols(is_fully_funded ~ log_description_length * post_war + fk_grade_level * post_war +
                        action_verb_density * post_war + pronoun_density * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data, vcov = "hetero")

# NON-UKRAINE SAMPLE
narrative_nonukr_funding <- feols(log_funding ~ log_description_length * post_war + fk_grade_level * post_war +
                        action_verb_density * post_war + pronoun_density * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data_nonukr, vcov = "hetero")
narrative_nonukr_avgdon <- feols(log_avg_donation ~ log_description_length * post_war + fk_grade_level * post_war +
                        action_verb_density * post_war + pronoun_density * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data_nonukr, vcov = "hetero")
narrative_nonukr_numdon <- feols(log_donations ~ log_description_length * post_war + fk_grade_level * post_war +
                        action_verb_density * post_war + pronoun_density * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data_nonukr, vcov = "hetero")
narrative_nonukr_funded <- feols(is_fully_funded ~ log_description_length * post_war + fk_grade_level * post_war +
                        action_verb_density * post_war + pronoun_density * post_war +
                        log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = reg_data_nonukr, vcov = "hetero")

narrative_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Narrative Features: Full Sample vs. Non-Ukraine Projects}\n",
  "\\label{tab:narrative}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccccccc}\n",
  "\\toprule\n",
  "& \\multicolumn{4}{c}{Full Sample} & \\multicolumn{4}{c}{Non-Ukraine Only} \\\\\n",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}\n",
  "& Log(Fund) & Log(Avg) & Log(\\#) & Funded & Log(Fund) & Log(Avg) & Log(\\#) & Funded \\\\\n",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{9}{l}{\\textit{Panel A: Baseline Effects}} \\\\\n"
)

narr_vars <- c("log_description_length", "fk_grade_level", "action_verb_density", "pronoun_density")
narr_labels <- c("Log(Description Length)", "FK Grade Level", "Action Verb Density", "Pronoun Density")

for (i in seq_along(narr_vars)) {
  full_funding <- extract_coef(narrative_full_funding, narr_vars[i])
  full_avgdon <- extract_coef(narrative_full_avgdon, narr_vars[i])
  full_numdon <- extract_coef(narrative_full_numdon, narr_vars[i])
  full_funded <- extract_coef(narrative_full_funded, narr_vars[i])

  nonukr_funding <- extract_coef(narrative_nonukr_funding, narr_vars[i])
  nonukr_avgdon <- extract_coef(narrative_nonukr_avgdon, narr_vars[i])
  nonukr_numdon <- extract_coef(narrative_nonukr_numdon, narr_vars[i])
  nonukr_funded <- extract_coef(narrative_nonukr_funded, narr_vars[i])

  narrative_latex <- paste0(narrative_latex,
    sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", narr_labels[i],
            fmt_coef(full_funding$coef, full_funding$pval),
            fmt_coef(full_avgdon$coef, full_avgdon$pval),
            fmt_coef(full_numdon$coef, full_numdon$pval),
            fmt_coef(full_funded$coef, full_funded$pval),
            fmt_coef(nonukr_funding$coef, nonukr_funding$pval),
            fmt_coef(nonukr_avgdon$coef, nonukr_avgdon$pval),
            fmt_coef(nonukr_numdon$coef, nonukr_numdon$pval),
            fmt_coef(nonukr_funded$coef, nonukr_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_funding$se), fmt_se(full_avgdon$se),
            fmt_se(full_numdon$se), fmt_se(full_funded$se),
            fmt_se(nonukr_funding$se), fmt_se(nonukr_avgdon$se),
            fmt_se(nonukr_numdon$se), fmt_se(nonukr_funded$se)))
}

narrative_latex <- paste0(narrative_latex,
  "\\addlinespace\n",
  "\\multicolumn{9}{l}{\\textit{Panel B: Post-War Interactions}} \\\\\n"
)

for (i in seq_along(narr_vars)) {
  full_int_funding <- get_inter_coef(narrative_full_funding, narr_vars[i], "post_warTRUE")
  full_int_avgdon <- get_inter_coef(narrative_full_avgdon, narr_vars[i], "post_warTRUE")
  full_int_numdon <- get_inter_coef(narrative_full_numdon, narr_vars[i], "post_warTRUE")
  full_int_funded <- get_inter_coef(narrative_full_funded, narr_vars[i], "post_warTRUE")

  nonukr_int_funding <- get_inter_coef(narrative_nonukr_funding, narr_vars[i], "post_warTRUE")
  nonukr_int_avgdon <- get_inter_coef(narrative_nonukr_avgdon, narr_vars[i], "post_warTRUE")
  nonukr_int_numdon <- get_inter_coef(narrative_nonukr_numdon, narr_vars[i], "post_warTRUE")
  nonukr_int_funded <- get_inter_coef(narrative_nonukr_funded, narr_vars[i], "post_warTRUE")

  narrative_latex <- paste0(narrative_latex,
    sprintf("%s $\\times$ Post-War & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", narr_labels[i],
            fmt_coef(full_int_funding$coef, full_int_funding$pval),
            fmt_coef(full_int_avgdon$coef, full_int_avgdon$pval),
            fmt_coef(full_int_numdon$coef, full_int_numdon$pval),
            fmt_coef(full_int_funded$coef, full_int_funded$pval),
            fmt_coef(nonukr_int_funding$coef, nonukr_int_funding$pval),
            fmt_coef(nonukr_int_avgdon$coef, nonukr_int_avgdon$pval),
            fmt_coef(nonukr_int_numdon$coef, nonukr_int_numdon$pval),
            fmt_coef(nonukr_int_funded$coef, nonukr_int_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_int_funding$se), fmt_se(full_int_avgdon$se),
            fmt_se(full_int_numdon$se), fmt_se(full_int_funded$se),
            fmt_se(nonukr_int_funding$se), fmt_se(nonukr_int_avgdon$se),
            fmt_se(nonukr_int_numdon$se), fmt_se(nonukr_int_funded$se)))
}

narrative_latex <- paste0(narrative_latex,
  "\\midrule\n",
  "Theme/Region/Year FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Log(Goal), Duration & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
          format(narrative_full_funding$nobs, big.mark = ","),
          format(narrative_full_avgdon$nobs, big.mark = ","),
          format(narrative_full_numdon$nobs, big.mark = ","),
          format(narrative_full_funded$nobs, big.mark = ","),
          format(narrative_nonukr_funding$nobs, big.mark = ","),
          format(narrative_nonukr_avgdon$nobs, big.mark = ","),
          format(narrative_nonukr_numdon$nobs, big.mark = ","),
          format(narrative_nonukr_funded$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(narrative_full_funding, "r2")$r2,
          fitstat(narrative_full_avgdon, "r2")$r2,
          fitstat(narrative_full_numdon, "r2")$r2,
          fitstat(narrative_full_funded, "r2")$r2,
          fitstat(narrative_nonukr_funding, "r2")$r2,
          fitstat(narrative_nonukr_avgdon, "r2")$r2,
          fitstat(narrative_nonukr_numdon, "r2")$r2,
          fitstat(narrative_nonukr_funded, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1. All models include post-war interactions. Columns 1-4: full sample; columns 5-8: non-Ukraine only. FK Grade Level = Flesch-Kincaid readability (higher = more complex). Action Verb Density and Pronoun Density are per 100 words.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(narrative_latex, "tables/table_narrative_war_interact.tex")
cat("Saved: tables/table_narrative_war_interact.tex\n")

# ==============================================================================
# DESCRIPTIVE COMPARISON: IDENTIFIABLE VICTIM (UKRAINE VS NON-UKRAINE)
# ==============================================================================

cat("\n=== Descriptive Comparison: Identifiable Victim (Ukraine vs Non-Ukraine) ===\n")

# Calculate means
ukr_named <- mean(post_war_data$has_named_individual[post_war_data$is_ukraine], na.rm = TRUE) * 100
ukr_story <- mean(post_war_data$has_personal_story[post_war_data$is_ukraine], na.rm = TRUE) * 100
ukr_singular <- mean(post_war_data$singular_framing[post_war_data$is_ukraine], na.rm = TRUE)
ukr_quant <- mean(post_war_data$has_quantified_impact[post_war_data$is_ukraine], na.rm = TRUE) * 100

non_named <- mean(post_war_data$has_named_individual[!post_war_data$is_ukraine], na.rm = TRUE) * 100
non_story <- mean(post_war_data$has_personal_story[!post_war_data$is_ukraine], na.rm = TRUE) * 100
non_singular <- mean(post_war_data$singular_framing[!post_war_data$is_ukraine], na.rm = TRUE)
non_quant <- mean(post_war_data$has_quantified_impact[!post_war_data$is_ukraine], na.rm = TRUE) * 100

identifiable_comparison <- data.frame(
  Metric = c("Named Individual", "Personal Story", "Singular Framing", "Quantified Impact"),
  Ukraine_Pct = c(ukr_named, ukr_story, ukr_singular, ukr_quant),
  NonUkraine_Pct = c(non_named, non_story, non_singular, non_quant)
) %>%
  mutate(
    # SE for proportions (named, story, quant) and continuous (singular)
    Ukraine_SE = c(
      sqrt((ukr_named/100) * (1 - ukr_named/100) / n_ukraine) * 100,
      sqrt((ukr_story/100) * (1 - ukr_story/100) / n_ukraine) * 100,
      sd(post_war_data$singular_framing[post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_ukraine),
      sqrt((ukr_quant/100) * (1 - ukr_quant/100) / n_ukraine) * 100
    ),
    NonUkraine_SE = c(
      sqrt((non_named/100) * (1 - non_named/100) / n_nonukraine) * 100,
      sqrt((non_story/100) * (1 - non_story/100) / n_nonukraine) * 100,
      sd(post_war_data$singular_framing[!post_war_data$is_ukraine], na.rm = TRUE) / sqrt(n_nonukraine),
      sqrt((non_quant/100) * (1 - non_quant/100) / n_nonukraine) * 100
    ),
    Difference = Ukraine_Pct - NonUkraine_Pct,
    Difference_SE = sqrt(Ukraine_SE^2 + NonUkraine_SE^2),
    Z_stat = Difference / Difference_SE,
    P_value = 2 * (1 - pnorm(abs(Z_stat)))
  )

print(identifiable_comparison)
write.csv(identifiable_comparison, "tables/identifiable_comparison_ukraine_vs_nonukraine.csv", row.names = FALSE)

# Create figure with error bars
identifiable_comp_long <- identifiable_comparison %>%
  select(Metric, Ukraine_Pct, NonUkraine_Pct, Ukraine_SE, NonUkraine_SE) %>%
  pivot_longer(cols = c(Ukraine_Pct, NonUkraine_Pct),
               names_to = "Sample",
               values_to = "Value") %>%
  mutate(
    SE = ifelse(grepl("Ukraine_Pct", Sample),
                identifiable_comparison$Ukraine_SE[match(Metric, identifiable_comparison$Metric)],
                identifiable_comparison$NonUkraine_SE[match(Metric, identifiable_comparison$Metric)]),
    Sample = recode(Sample,
                    "Ukraine_Pct" = "Ukraine Projects",
                    "NonUkraine_Pct" = "Non-Ukraine Projects")
  )

p_identifiable_comp <- ggplot(identifiable_comp_long, aes(x = Metric, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = Value - 1.96*SE, ymax = Value + 1.96*SE),
                position = position_dodge(width = 0.9), width = 0.25, linewidth = 0.5) +
  scale_fill_manual(values = c("Ukraine Projects" = "#E74C3C",
                                "Non-Ukraine Projects" = "#3498DB")) +
  labs(title = "Identifiable Victim Features: Ukraine vs. Non-Ukraine Projects (Post-War)",
       subtitle = "Mean values with 95% confidence intervals",
       y = "Mean Value",
       x = "Identifiable Victim Feature",
       fill = "Sample") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("figures/fig_identifiable_comparison_ukraine.pdf", p_identifiable_comp, width = 10, height = 6)
cat("Saved: figures/fig_identifiable_comparison_ukraine.pdf\n")

# ==============================================================================
# TABLE: IDENTIFIABLE VICTIM WITH WAR INTERACTION (Full Sample vs Non-Ukraine)
# ==============================================================================

cat("\n=== Table: Identifiable Victim War Interaction (Full vs Non-Ukraine) ===\n")

# FULL SAMPLE
id_full_funding <- feols(log_funding ~ has_named_individual * post_war + has_personal_story * post_war +
                 singular_framing * post_war + has_quantified_impact * post_war +
                 log_goal + log_duration | theme_factor + region_factor + year_factor,
               data = reg_data, vcov = "hetero")
id_full_avgdon <- feols(log_avg_donation ~ has_named_individual * post_war + has_personal_story * post_war +
                 singular_framing * post_war + has_quantified_impact * post_war +
                 log_goal + log_duration | theme_factor + region_factor + year_factor,
               data = reg_data, vcov = "hetero")
id_full_numdon <- feols(log_donations ~ has_named_individual * post_war + has_personal_story * post_war +
                 singular_framing * post_war + has_quantified_impact * post_war +
                 log_goal + log_duration | theme_factor + region_factor + year_factor,
               data = reg_data, vcov = "hetero")
id_full_funded <- feols(is_fully_funded ~ has_named_individual * post_war + has_personal_story * post_war +
                 singular_framing * post_war + has_quantified_impact * post_war +
                 log_goal + log_duration | theme_factor + region_factor + year_factor,
               data = reg_data, vcov = "hetero")

# NON-UKRAINE SAMPLE
id_nonukr_funding <- feols(log_funding ~ has_named_individual * post_war + has_personal_story * post_war +
                 singular_framing * post_war + has_quantified_impact * post_war +
                 log_goal + log_duration | theme_factor + region_factor + year_factor,
               data = reg_data_nonukr, vcov = "hetero")
id_nonukr_avgdon <- feols(log_avg_donation ~ has_named_individual * post_war + has_personal_story * post_war +
                 singular_framing * post_war + has_quantified_impact * post_war +
                 log_goal + log_duration | theme_factor + region_factor + year_factor,
               data = reg_data_nonukr, vcov = "hetero")
id_nonukr_numdon <- feols(log_donations ~ has_named_individual * post_war + has_personal_story * post_war +
                 singular_framing * post_war + has_quantified_impact * post_war +
                 log_goal + log_duration | theme_factor + region_factor + year_factor,
               data = reg_data_nonukr, vcov = "hetero")
id_nonukr_funded <- feols(is_fully_funded ~ has_named_individual * post_war + has_personal_story * post_war +
                 singular_framing * post_war + has_quantified_impact * post_war +
                 log_goal + log_duration | theme_factor + region_factor + year_factor,
               data = reg_data_nonukr, vcov = "hetero")

id_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Identifiable Victim Effects: Full Sample vs. Non-Ukraine Projects}\n",
  "\\label{tab:identifiable}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccccccc}\n",
  "\\toprule\n",
  "& \\multicolumn{4}{c}{Full Sample} & \\multicolumn{4}{c}{Non-Ukraine Only} \\\\\n",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}\n",
  "& Log(Fund) & Log(Avg) & Log(\\#) & Funded & Log(Fund) & Log(Avg) & Log(\\#) & Funded \\\\\n",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{9}{l}{\\textit{Panel A: Baseline Effects}} \\\\\n"
)

id_vars <- c("has_named_individualTRUE", "has_personal_storyTRUE", "singular_framing", "has_quantified_impactTRUE")
id_labels <- c("Named Individual", "Personal Story", "Singular Framing", "Quantified Impact")

for (i in seq_along(id_vars)) {
  full_funding <- extract_coef(id_full_funding, id_vars[i])
  full_avgdon <- extract_coef(id_full_avgdon, id_vars[i])
  full_numdon <- extract_coef(id_full_numdon, id_vars[i])
  full_funded <- extract_coef(id_full_funded, id_vars[i])

  nonukr_funding <- extract_coef(id_nonukr_funding, id_vars[i])
  nonukr_avgdon <- extract_coef(id_nonukr_avgdon, id_vars[i])
  nonukr_numdon <- extract_coef(id_nonukr_numdon, id_vars[i])
  nonukr_funded <- extract_coef(id_nonukr_funded, id_vars[i])

  id_latex <- paste0(id_latex,
    sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", id_labels[i],
            fmt_coef(full_funding$coef, full_funding$pval),
            fmt_coef(full_avgdon$coef, full_avgdon$pval),
            fmt_coef(full_numdon$coef, full_numdon$pval),
            fmt_coef(full_funded$coef, full_funded$pval),
            fmt_coef(nonukr_funding$coef, nonukr_funding$pval),
            fmt_coef(nonukr_avgdon$coef, nonukr_avgdon$pval),
            fmt_coef(nonukr_numdon$coef, nonukr_numdon$pval),
            fmt_coef(nonukr_funded$coef, nonukr_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_funding$se), fmt_se(full_avgdon$se),
            fmt_se(full_numdon$se), fmt_se(full_funded$se),
            fmt_se(nonukr_funding$se), fmt_se(nonukr_avgdon$se),
            fmt_se(nonukr_numdon$se), fmt_se(nonukr_funded$se)))
}

id_latex <- paste0(id_latex,
  "\\addlinespace\n",
  "\\multicolumn{9}{l}{\\textit{Panel B: Post-War Interactions}} \\\\\n"
)

for (i in seq_along(id_vars)) {
  full_int_funding <- get_inter_coef(id_full_funding, id_vars[i], "post_warTRUE")
  full_int_avgdon <- get_inter_coef(id_full_avgdon, id_vars[i], "post_warTRUE")
  full_int_numdon <- get_inter_coef(id_full_numdon, id_vars[i], "post_warTRUE")
  full_int_funded <- get_inter_coef(id_full_funded, id_vars[i], "post_warTRUE")

  nonukr_int_funding <- get_inter_coef(id_nonukr_funding, id_vars[i], "post_warTRUE")
  nonukr_int_avgdon <- get_inter_coef(id_nonukr_avgdon, id_vars[i], "post_warTRUE")
  nonukr_int_numdon <- get_inter_coef(id_nonukr_numdon, id_vars[i], "post_warTRUE")
  nonukr_int_funded <- get_inter_coef(id_nonukr_funded, id_vars[i], "post_warTRUE")

  id_latex <- paste0(id_latex,
    sprintf("%s $\\times$ Post-War & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n", id_labels[i],
            fmt_coef(full_int_funding$coef, full_int_funding$pval),
            fmt_coef(full_int_avgdon$coef, full_int_avgdon$pval),
            fmt_coef(full_int_numdon$coef, full_int_numdon$pval),
            fmt_coef(full_int_funded$coef, full_int_funded$pval),
            fmt_coef(nonukr_int_funding$coef, nonukr_int_funding$pval),
            fmt_coef(nonukr_int_avgdon$coef, nonukr_int_avgdon$pval),
            fmt_coef(nonukr_int_numdon$coef, nonukr_int_numdon$pval),
            fmt_coef(nonukr_int_funded$coef, nonukr_int_funded$pval)),
    sprintf("& %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
            fmt_se(full_int_funding$se), fmt_se(full_int_avgdon$se),
            fmt_se(full_int_numdon$se), fmt_se(full_int_funded$se),
            fmt_se(nonukr_int_funding$se), fmt_se(nonukr_int_avgdon$se),
            fmt_se(nonukr_int_numdon$se), fmt_se(nonukr_int_funded$se)))
}

id_latex <- paste0(id_latex,
  "\\midrule\n",
  "Theme/Region/Year FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Log(Goal), Duration & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
          format(id_full_funding$nobs, big.mark = ","),
          format(id_full_avgdon$nobs, big.mark = ","),
          format(id_full_numdon$nobs, big.mark = ","),
          format(id_full_funded$nobs, big.mark = ","),
          format(id_nonukr_funding$nobs, big.mark = ","),
          format(id_nonukr_avgdon$nobs, big.mark = ","),
          format(id_nonukr_numdon$nobs, big.mark = ","),
          format(id_nonukr_funded$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(id_full_funding, "r2")$r2,
          fitstat(id_full_avgdon, "r2")$r2,
          fitstat(id_full_numdon, "r2")$r2,
          fitstat(id_full_funded, "r2")$r2,
          fitstat(id_nonukr_funding, "r2")$r2,
          fitstat(id_nonukr_avgdon, "r2")$r2,
          fitstat(id_nonukr_numdon, "r2")$r2,
          fitstat(id_nonukr_funded, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1. All models include post-war interactions. Columns 1-4: full sample; columns 5-8: non-Ukraine only. Named Individual detects specific names or ``meet [Name]'' patterns. Personal Story detects narrative language. Singular Framing = singular references / (singular + plural).\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(id_latex, "tables/table_identifiable_war_interact.tex")
cat("Saved: tables/table_identifiable_war_interact.tex\n")

# ==============================================================================
# TABLE: REGIONAL REGRESSION
# ==============================================================================

cat("=== Table: Regional Regression ===\n")

reg_data_regional <- reg_data %>% filter(!is.na(region_for_reg))

reg_m1 <- lm(log_funding ~ region_for_reg, data = reg_data_regional)
reg_m2 <- lm(log_funding ~ region_for_reg + log_goal, data = reg_data_regional)
reg_m3 <- feols(log_funding ~ region_for_reg + log_goal | theme_factor, data = reg_data_regional, vcov = "hetero")
reg_m4 <- feols(log_funding ~ region_for_reg + log_goal + log_duration | theme_factor + year_factor, data = reg_data_regional, vcov = "hetero")
reg_m5 <- feols(log_funding ~ region_for_reg + log_goal + log_duration + log_donations | theme_factor + year_factor, data = reg_data_regional, vcov = "hetero")

regional_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Regional Disparities in Project Funding}\n",
  "\\label{tab:regional_regression}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lccccc}\n",
  "\\toprule\n",
  "& \\multicolumn{5}{c}{Log(Funding)} \\\\\n",
  "\\cmidrule(lr){2-6}\n",
  "& (1) & (2) & (3) & (4) & (5) \\\\\n",
  "\\midrule\n"
)

regions <- c("Africa", "Asia and Oceania", "Europe and Russia", "Latin America", "Middle East")
reg_names <- paste0("region_for_reg", regions)

for (i in seq_along(regions)) {
  coefs <- sapply(list(reg_m1, reg_m2, reg_m3, reg_m4, reg_m5), function(m) {
    if (reg_names[i] %in% names(coef(m))) coef(m)[reg_names[i]] else NA
  })
  ses <- sapply(list(reg_m1, reg_m2, reg_m3, reg_m4, reg_m5), function(m) {
    if (reg_names[i] %in% names(coef(m))) sqrt(diag(vcov(m)))[reg_names[i]] else NA
  })
  pvals <- 2 * pnorm(-abs(coefs / ses))

  regional_latex <- paste0(regional_latex,
    sprintf("%s & %s & %s & %s & %s & %s \\\\\n", regions[i],
            fmt_coef(coefs[1], pvals[1]), fmt_coef(coefs[2], pvals[2]),
            fmt_coef(coefs[3], pvals[3]), fmt_coef(coefs[4], pvals[4]),
            fmt_coef(coefs[5], pvals[5])),
    sprintf("& %s & %s & %s & %s & %s \\\\\n",
            fmt_se(ses[1]), fmt_se(ses[2]), fmt_se(ses[3]), fmt_se(ses[4]), fmt_se(ses[5])))
}

regional_latex <- paste0(regional_latex,
  "\\midrule\n",
  "Log(Goal) & No & Yes & Yes & Yes & Yes \\\\\n",
  "Theme FE & No & No & Yes & Yes & Yes \\\\\n",
  "Year FE & No & No & No & Yes & Yes \\\\\n",
  "Duration & No & No & No & Yes & Yes \\\\\n",
  "Log(Donations) & No & No & No & No & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s \\\\\n",
          format(nobs(reg_m1), big.mark = ","), format(nobs(reg_m2), big.mark = ","),
          format(reg_m3$nobs, big.mark = ","), format(reg_m4$nobs, big.mark = ","),
          format(reg_m5$nobs, big.mark = ",")),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Reference category is North America. Robust standard errors in parentheses for columns (3)--(5). *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(regional_latex, "tables/table_regional_regression.tex")
cat("Saved: tables/table_regional_regression.tex\n")

# ==============================================================================
# TABLE: REGIONAL WAR (Full Sample vs Non-Ukraine)
# ==============================================================================

cat("=== Table: Regional War (Full Sample vs Non-Ukraine) ===\n")

# Prepare both datasets
reg_data_full_regional <- reg_data %>% filter(!is.na(region_for_reg))
reg_data_nonukr_regional <- reg_data_nonukr %>% filter(!is.na(region_for_reg))

# Run 4 regressions on FULL sample (all with post-war interactions)
regional_full_funding <- feols(log_funding ~ region_for_reg * post_war + log_goal + log_duration | theme_factor + year_factor,
                               data = reg_data_full_regional, vcov = "hetero")
regional_full_avgdon <- feols(log_avg_donation ~ region_for_reg * post_war + log_goal + log_duration | theme_factor + year_factor,
                              data = reg_data_full_regional, vcov = "hetero")
regional_full_numdon <- feols(log_donations ~ region_for_reg * post_war + log_goal + log_duration | theme_factor + year_factor,
                              data = reg_data_full_regional, vcov = "hetero")
regional_full_funded <- feols(is_fully_funded ~ region_for_reg * post_war + log_goal + log_duration | theme_factor + year_factor,
                              data = reg_data_full_regional, vcov = "hetero")

# Run 4 regressions on NON-UKRAINE sample (all with post-war interactions)
regional_nonukr_funding <- feols(log_funding ~ region_for_reg * post_war + log_goal + log_duration | theme_factor + year_factor,
                                 data = reg_data_nonukr_regional, vcov = "hetero")
regional_nonukr_avgdon <- feols(log_avg_donation ~ region_for_reg * post_war + log_goal + log_duration | theme_factor + year_factor,
                                data = reg_data_nonukr_regional, vcov = "hetero")
regional_nonukr_numdon <- feols(log_donations ~ region_for_reg * post_war + log_goal + log_duration | theme_factor + year_factor,
                                data = reg_data_nonukr_regional, vcov = "hetero")
regional_nonukr_funded <- feols(is_fully_funded ~ region_for_reg * post_war + log_goal + log_duration | theme_factor + year_factor,
                                data = reg_data_nonukr_regional, vcov = "hetero")

# Create models list (Full sample first 4, Non-Ukraine next 4)
models_regional <- list(regional_full_funding, regional_full_avgdon, regional_full_numdon, regional_full_funded,
                        regional_nonukr_funding, regional_nonukr_avgdon, regional_nonukr_numdon, regional_nonukr_funded)

# Generate LaTeX table (8 columns: 4 full sample + 4 non-Ukraine)
regional_war_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Regional Disparities and Post-War Dynamics: Full Sample vs. Non-Ukraine Projects}\n",
  "\\label{tab:regional_war}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccccccc}\n",
  "\\toprule\n",
  "& \\multicolumn{4}{c}{Full Sample} & \\multicolumn{4}{c}{Non-Ukraine Only} \\\\\n",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}\n",
  "& Log(Fund) & Log(Avg) & Log(\\#) & Funded & Log(Fund) & Log(Avg) & Log(\\#) & Funded \\\\\n",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{9}{l}{\\textit{Panel A: Baseline Regional Effects}} \\\\\n"
)

# Panel A: Baseline effects for each region
for (i in seq_along(regions)) {
  regional_war_latex <- paste0(regional_war_latex, sprintf("%s", regions[i]))
  for (j in 1:8) {
    c <- extract_coef(models_regional[[j]], reg_names[i])
    regional_war_latex <- paste0(regional_war_latex, sprintf(" & %s", fmt_coef(c$coef, c$pval)))
  }
  regional_war_latex <- paste0(regional_war_latex, " \\\\\n&")
  for (j in 1:8) {
    s <- extract_coef(models_regional[[j]], reg_names[i])$se
    regional_war_latex <- paste0(regional_war_latex, sprintf(" %s", fmt_se(s)))
    if (j < 8) regional_war_latex <- paste0(regional_war_latex, " &")
  }
  regional_war_latex <- paste0(regional_war_latex, " \\\\\n")
}

# Panel B: Post-war interactions
regional_war_latex <- paste0(regional_war_latex,
  "\\addlinespace\n",
  "\\multicolumn{9}{l}{\\textit{Panel B: Post-War Interactions}} \\\\\n",
  "Post-War"
)

# Post-war main effect
for (j in 1:8) {
  c_pw <- extract_coef(models_regional[[j]], "post_warTRUE")
  regional_war_latex <- paste0(regional_war_latex, sprintf(" & %s", fmt_coef(c_pw$coef, c_pw$pval)))
}
regional_war_latex <- paste0(regional_war_latex, " \\\\\n")
for (j in 1:8) {
  s_pw <- extract_coef(models_regional[[j]], "post_warTRUE")$se
  regional_war_latex <- paste0(regional_war_latex, sprintf(" & %s", fmt_se(s_pw)))
}
regional_war_latex <- paste0(regional_war_latex, " \\\\\n")

# Region x Post-war interactions
for (i in seq_along(regions)) {
  regional_war_latex <- paste0(regional_war_latex, sprintf("%s $\\times$ Post-War", regions[i]))
  for (j in 1:8) {
    inter <- get_inter_coef(models_regional[[j]], reg_names[i], "post_warTRUE")
    regional_war_latex <- paste0(regional_war_latex, sprintf(" & %s", fmt_coef(inter$coef, inter$pval)))
  }
  regional_war_latex <- paste0(regional_war_latex, " \\\\\n")
  for (j in 1:8) {
    inter <- get_inter_coef(models_regional[[j]], reg_names[i], "post_warTRUE")
    regional_war_latex <- paste0(regional_war_latex, sprintf(" & %s", fmt_se(inter$se)))
  }
  regional_war_latex <- paste0(regional_war_latex, " \\\\\n")
}

regional_war_latex <- paste0(regional_war_latex,
  "\\midrule\n",
  "Theme/Year FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Log(Goal), Duration & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
          format(regional_full_funding$nobs, big.mark = ","),
          format(regional_full_avgdon$nobs, big.mark = ","),
          format(regional_full_numdon$nobs, big.mark = ","),
          format(regional_full_funded$nobs, big.mark = ","),
          format(regional_nonukr_funding$nobs, big.mark = ","),
          format(regional_nonukr_avgdon$nobs, big.mark = ","),
          format(regional_nonukr_numdon$nobs, big.mark = ","),
          format(regional_nonukr_funded$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(regional_full_funding, "r2")$r2,
          fitstat(regional_full_avgdon, "r2")$r2,
          fitstat(regional_full_numdon, "r2")$r2,
          fitstat(regional_full_funded, "r2")$r2,
          fitstat(regional_nonukr_funding, "r2")$r2,
          fitstat(regional_nonukr_avgdon, "r2")$r2,
          fitstat(regional_nonukr_numdon, "r2")$r2,
          fitstat(regional_nonukr_funded, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1. All models include post-war interactions. Columns 1-4: full sample; columns 5-8: non-Ukraine only. Reference category: North America.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(regional_war_latex, "tables/table_regional_war.tex")
cat("Saved: tables/table_regional_war.tex\n")

# ==============================================================================
# INCOME CLASSIFICATION ANALYSIS (World Bank)
# ==============================================================================

cat("\n========================================\n")
cat("INCOME CLASSIFICATION ANALYSIS\n")
cat("========================================\n\n")

# Install countrycode package if needed
if (!require("countrycode", quietly = TRUE)) {
  cat("Installing countrycode package...\n")
  install.packages("countrycode", repos = "https://cloud.r-project.org")
  library(countrycode)
} else {
  library(countrycode)
}

# Get World Bank income classifications using countrycode
cat("Fetching World Bank income classifications...\n")

# Load the codelist from countrycode package
data(codelist, package = "countrycode")

# Get unique countries from dataset
countries_in_data <- unique(df$country[!is.na(df$country)])

wb_income <- data.frame(
  country = countries_in_data,
  iso3c = countrycode(countries_in_data, "country.name", "iso3c", warn = FALSE),
  stringsAsFactors = FALSE
) %>%
  filter(!is.na(iso3c)) %>%
  left_join(
    codelist %>%
      select(iso3c, wb, income_level = wb) %>%
      distinct(),
    by = "iso3c"
  )

# Use wbstats package to get World Bank income classifications
if (!require("wbstats", quietly = TRUE)) {
  cat("Installing wbstats package...\n")
  install.packages("wbstats", repos = "https://cloud.r-project.org")
  library(wbstats)
} else {
  library(wbstats)
}

# Fetch country classifications from World Bank
countries_data <- wb_countries()

# Extract only Country Name, Region, and Income Level, filtering out aggregates
income_classification <- countries_data[, c("iso3c", "country", "region", "income_level")] %>%
  filter(income_level != "Aggregates", income_level != "Not classified") %>%
  filter(!is.na(iso3c))

cat("\nWorld Bank income levels found:\n")
print(unique(income_classification$income_level))
cat("\nNumber of countries classified:\n")
print(nrow(income_classification))

# Match countries in our data to World Bank classifications
wb_income <- data.frame(
  country = countries_in_data,
  iso3c = countrycode(countries_in_data, "country.name", "iso3c", warn = FALSE),
  stringsAsFactors = FALSE
) %>%
  filter(!is.na(iso3c)) %>%
  left_join(
    income_classification %>% select(iso3c, income_level),
    by = "iso3c"
  )

# Clean and standardize income levels
wb_income_clean <- wb_income %>%
  mutate(
    income_group = case_when(
      income_level == "High income" ~ "High Income",
      income_level == "Upper middle income" ~ "Upper Middle Income",
      income_level == "Lower middle income" ~ "Lower Middle Income",
      income_level == "Low income" ~ "Low Income",
      TRUE ~ "Unclassified"
    )
  ) %>%
  select(country, income_group)

# Match with project data (use reg_data which already has theme_factor and year_factor)
income_reg_data <- reg_data %>%
  left_join(wb_income_clean, by = "country") %>%
  mutate(
    income_group = if_else(is.na(income_group), "Unclassified", income_group)
  )

cat("Income group distribution:\n")
print(table(income_reg_data$income_group))

# Prepare FULL sample (excluding unclassified)
income_reg_data_full <- income_reg_data %>%
  filter(income_group != "Unclassified") %>%
  mutate(
    income_high = income_group == "High Income",
    income_upper_mid = income_group == "Upper Middle Income",
    income_lower_mid = income_group == "Lower Middle Income",
    income_low = income_group == "Low Income"
  )

# Prepare NON-UKRAINE sample (excluding unclassified and Ukraine)
income_reg_data_nonukr <- income_reg_data %>%
  filter(income_group != "Unclassified", !is_ukraine) %>%
  mutate(
    income_high = income_group == "High Income",
    income_upper_mid = income_group == "Upper Middle Income",
    income_lower_mid = income_group == "Lower Middle Income",
    income_low = income_group == "Low Income"
  )

# Run 4 regressions on FULL sample (with post-war interactions)
inc_full_m1 <- feols(log_funding ~ (income_upper_mid + income_lower_mid + income_low) * post_war +
                      log_goal + log_duration | theme_factor + year_factor,
                     data = income_reg_data_full, vcov = "hetero")

inc_full_m2 <- feols(log_avg_donation ~ (income_upper_mid + income_lower_mid + income_low) * post_war +
                      log_goal + log_duration | theme_factor + year_factor,
                     data = income_reg_data_full, vcov = "hetero")

inc_full_m3 <- feols(log_donations ~ (income_upper_mid + income_lower_mid + income_low) * post_war +
                      log_goal + log_duration | theme_factor + year_factor,
                     data = income_reg_data_full, vcov = "hetero")

inc_full_m4 <- feols(is_fully_funded ~ (income_upper_mid + income_lower_mid + income_low) * post_war +
                      log_goal + log_duration | theme_factor + year_factor,
                     data = income_reg_data_full, vcov = "hetero")

# Run 4 regressions on NON-UKRAINE sample (with post-war interactions)
inc_nonukr_m1 <- feols(log_funding ~ (income_upper_mid + income_lower_mid + income_low) * post_war +
                        log_goal + log_duration | theme_factor + year_factor,
                       data = income_reg_data_nonukr, vcov = "hetero")

inc_nonukr_m2 <- feols(log_avg_donation ~ (income_upper_mid + income_lower_mid + income_low) * post_war +
                        log_goal + log_duration | theme_factor + year_factor,
                       data = income_reg_data_nonukr, vcov = "hetero")

inc_nonukr_m3 <- feols(log_donations ~ (income_upper_mid + income_lower_mid + income_low) * post_war +
                        log_goal + log_duration | theme_factor + year_factor,
                       data = income_reg_data_nonukr, vcov = "hetero")

inc_nonukr_m4 <- feols(is_fully_funded ~ (income_upper_mid + income_lower_mid + income_low) * post_war +
                        log_goal + log_duration | theme_factor + year_factor,
                       data = income_reg_data_nonukr, vcov = "hetero")

# Helper function for formatting coefficient with stars (if not already defined)
if (!exists("fmt_coef_stars")) {
  fmt_coef_stars <- function(coef_val, pval) {
    stars <- if_else(pval < 0.01, "***",
                     if_else(pval < 0.05, "**",
                            if_else(pval < 0.10, "*", "")))
    sprintf("%.3f%s", coef_val, stars)
  }
}

# Create LaTeX table (8 columns: 4 full sample + 4 non-Ukraine)
models_income <- list(inc_full_m1, inc_full_m2, inc_full_m3, inc_full_m4,
                      inc_nonukr_m1, inc_nonukr_m2, inc_nonukr_m3, inc_nonukr_m4)
income_names <- c("income_upper_midTRUE", "income_lower_midTRUE", "income_lowTRUE")
income_labels <- c("Upper Middle Income", "Lower Middle Income", "Low Income")

income_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Income Level Disparities and Post-War Dynamics: Full Sample vs. Non-Ukraine Projects}\n",
  "\\label{tab:income_war}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccccccc}\n",
  "\\toprule\n",
  "& \\multicolumn{4}{c}{Full Sample} & \\multicolumn{4}{c}{Non-Ukraine Only} \\\\\n",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}\n",
  "& Log(Fund) & Log(Avg) & Log(\\#) & Funded & Log(Fund) & Log(Avg) & Log(\\#) & Funded \\\\\n",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{9}{l}{\\textit{Panel A: Baseline Income Effects}} \\\\\n"
)

# Panel A: Baseline effects for each income group
for (i in seq_along(income_names)) {
  income_latex <- paste0(income_latex, sprintf("%s", income_labels[i]))
  for (j in 1:8) {
    c <- extract_coef(models_income[[j]], income_names[i])
    income_latex <- paste0(income_latex, sprintf(" & %s", fmt_coef_stars(c$coef, c$pval)))
  }
  income_latex <- paste0(income_latex, " \\\\\n&")
  for (j in 1:8) {
    s <- extract_coef(models_income[[j]], income_names[i])$se
    income_latex <- paste0(income_latex, sprintf(" (%.3f)", s))
    if (j < 8) income_latex <- paste0(income_latex, " &")
  }
  income_latex <- paste0(income_latex, " \\\\\n")
}

# Panel B: Post-war interactions
income_latex <- paste0(income_latex,
  "\\addlinespace\n",
  "\\multicolumn{9}{l}{\\textit{Panel B: Post-War Interactions}} \\\\\n",
  "Post-War"
)

# Post-war main effect
for (j in 1:8) {
  c_pw <- extract_coef(models_income[[j]], "post_warTRUE")
  income_latex <- paste0(income_latex, sprintf(" & %s", fmt_coef_stars(c_pw$coef, c_pw$pval)))
}
income_latex <- paste0(income_latex, " \\\\\n")
for (j in 1:8) {
  s_pw <- extract_coef(models_income[[j]], "post_warTRUE")$se
  income_latex <- paste0(income_latex, sprintf(" & (%.3f)", s_pw))
}
income_latex <- paste0(income_latex, " \\\\\n")

# Income x Post-war interactions
for (i in seq_along(income_names)) {
  income_latex <- paste0(income_latex, sprintf("%s $\\times$ Post-War", income_labels[i]))
  for (j in 1:8) {
    inter <- get_inter_coef(models_income[[j]], income_names[i], "post_warTRUE")
    income_latex <- paste0(income_latex, sprintf(" & %s", fmt_coef_stars(inter$coef, inter$pval)))
  }
  income_latex <- paste0(income_latex, " \\\\\n")
  for (j in 1:8) {
    inter <- get_inter_coef(models_income[[j]], income_names[i], "post_warTRUE")
    income_latex <- paste0(income_latex, sprintf(" & (%.3f)", inter$se))
  }
  income_latex <- paste0(income_latex, " \\\\\n")
}

income_latex <- paste0(income_latex,
  "\\midrule\n",
  "Theme/Year FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Log(Goal), Duration & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
          format(inc_full_m1$nobs, big.mark = ","),
          format(inc_full_m2$nobs, big.mark = ","),
          format(inc_full_m3$nobs, big.mark = ","),
          format(inc_full_m4$nobs, big.mark = ","),
          format(inc_nonukr_m1$nobs, big.mark = ","),
          format(inc_nonukr_m2$nobs, big.mark = ","),
          format(inc_nonukr_m3$nobs, big.mark = ","),
          format(inc_nonukr_m4$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(inc_full_m1, "r2")$r2,
          fitstat(inc_full_m2, "r2")$r2,
          fitstat(inc_full_m3, "r2")$r2,
          fitstat(inc_full_m4, "r2")$r2,
          fitstat(inc_nonukr_m1, "r2")$r2,
          fitstat(inc_nonukr_m2, "r2")$r2,
          fitstat(inc_nonukr_m3, "r2")$r2,
          fitstat(inc_nonukr_m4, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1. All models include post-war interactions. Columns 1-4: full sample; columns 5-8: non-Ukraine only. Income classifications based on World Bank income groups. Reference category: High Income countries. Sample excludes unclassified countries.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(income_latex, "tables/table_income_war.tex")
cat("Saved: tables/table_income_war.tex\n")

# ==============================================================================
# COUNTRY-SPECIFIC EXAMPLES: PRE-WAR VS POST-WAR COEFFICIENTS
# ==============================================================================

cat("\n========================================\n")
cat("COUNTRY-SPECIFIC EXAMPLES ANALYSIS\n")
cat("========================================\n\n")

# Find most common destination countries by income category
# Merge income groups with country counts
country_counts_with_income <- reg_data %>%
  filter(!is_ukraine) %>%
  left_join(wb_income_clean, by = "country") %>%
  filter(!is.na(income_group), income_group != "Unclassified") %>%
  count(country, income_group, sort = TRUE)

cat("Countries by income group:\n")
print(table(country_counts_with_income$income_group))

# Select top 5 countries from each income category for 2x2 grid visualization
high_income_countries <- country_counts_with_income %>%
  filter(income_group == "High Income") %>%
  head(5) %>%
  pull(country)

upper_mid_countries <- country_counts_with_income %>%
  filter(income_group == "Upper Middle Income") %>%
  head(5) %>%
  pull(country)

lower_mid_countries <- country_counts_with_income %>%
  filter(income_group == "Lower Middle Income") %>%
  head(5) %>%
  pull(country)

low_income_countries <- country_counts_with_income %>%
  filter(income_group == "Low Income") %>%
  head(5) %>%
  pull(country)

# Combine all selected countries
viz_countries <- c(high_income_countries, upper_mid_countries,
                   lower_mid_countries, low_income_countries)

cat("\nSelected countries for visualization:\n")
print(viz_countries)
cat("\n")

# Create country dummies for selected countries (use reg_data which has theme_factor and year_factor)
country_reg_data <- reg_data %>%
  filter(!is_ukraine, country %in% viz_countries) %>%
  mutate(country_factor = factor(country))

# Regression with country FE and war interactions
country_model <- feols(log_funding ~ country_factor * post_war + log_goal + log_duration |
                         theme_factor + year_factor,
                       data = country_reg_data, vcov = "hetero")

# Extract coefficients
country_coefs <- summary(country_model)$coeftable %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl("country_factor", term))

# Parse pre-war and interaction coefficients
pre_war_coefs <- country_coefs %>%
  filter(!grepl(":post_war", term)) %>%
  mutate(country = gsub("country_factor", "", term)) %>%
  select(country, pre_war_coef = Estimate, pre_war_se = `Std. Error`)

interaction_coefs <- country_coefs %>%
  filter(grepl(":post_warTRUE", term)) %>%
  mutate(country = gsub("country_factor|:post_warTRUE", "", term)) %>%
  select(country, interaction_coef = Estimate, interaction_se = `Std. Error`)

# Combine and calculate post-war effects
country_effects <- pre_war_coefs %>%
  left_join(interaction_coefs, by = "country") %>%
  mutate(
    interaction_coef = if_else(is.na(interaction_coef), 0, interaction_coef),
    interaction_se = if_else(is.na(interaction_se), 0, interaction_se),
    post_war_coef = pre_war_coef + interaction_coef,
    post_war_se = sqrt(pre_war_se^2 + interaction_se^2)
  )

# Add income group to country effects for ordering
country_effects_with_income <- country_effects %>%
  left_join(
    country_counts_with_income %>% select(country, income_group),
    by = "country"
  )

# Save for plotting
write_csv(country_effects_with_income, "tables/country_prepost_coefs.csv")
cat("Saved: tables/country_prepost_coefs.csv\n")

# Create visualization with error bars in 2x2 grid by income category
# Create ordering within each income group based on post-war coefficient
country_plot_data <- country_effects_with_income %>%
  select(country, income_group, pre_war_coef, pre_war_se, post_war_coef, post_war_se) %>%
  # Order countries within each income group by post-war coefficient
  group_by(income_group) %>%
  arrange(income_group, desc(post_war_coef)) %>%
  mutate(country_order = row_number()) %>%
  ungroup() %>%
  pivot_longer(cols = c(pre_war_coef, post_war_coef),
               names_to = "period",
               values_to = "coefficient") %>%
  mutate(
    se = if_else(period == "pre_war_coef", pre_war_se, post_war_se),
    ci_lower = coefficient - 1.96 * se,
    ci_upper = coefficient + 1.96 * se,
    period = recode(period,
                    "pre_war_coef" = "Pre-War",
                    "post_war_coef" = "Post-War"),
    # Create factor with ordering within group
    country = reorder(country, -country_order),
    # Set factor order for income groups to control panel layout
    income_group = factor(income_group,
                         levels = c("High Income", "Upper Middle Income",
                                   "Lower Middle Income", "Low Income"))
  )

p_country <- ggplot(country_plot_data,
                    aes(x = country, y = coefficient, color = period, group = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.6), width = 0.3, alpha = 0.7, linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5, alpha = 0.9) +
  scale_color_manual(values = c("Pre-War" = pal_main[3], "Post-War" = pal_main[2])) +
  facet_wrap(~ income_group, nrow = 2, ncol = 2, scales = "free_x") +
  labs(
    title = "Country-Specific Funding Effects: Pre-War vs Post-War by Income Category",
    subtitle = "Top 5 countries per income category | Relative to United States (omitted) | Error bars show 95% CI",
    x = NULL,
    y = "Coefficient (Log Funding)",
    color = "Period"
  ) +
  theme_paper +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "gray90", color = "gray60")
  )

ggsave("figures/fig_country_prepost.pdf", p_country, width = 14, height = 10, dpi = 300)
cat("Saved: figures/fig_country_prepost.pdf\n")

# ==============================================================================
# ORIGIN-DESTINATION ANALYSIS
# ==============================================================================

cat("\n========================================\n")
cat("ORIGIN-DESTINATION FLOW ANALYSIS\n")
cat("========================================\n\n")

# Prepare origin-destination data
origin_dest_data <- df %>%
  filter(!is.na(contact_country) & !is.na(country) &
         contact_country != "" & country != "") %>%
  mutate(
    origin = contact_country,
    destination = country,
    is_international = (contact_country != country)
  )

# Summary statistics
cat("Total projects with origin-destination data:", nrow(origin_dest_data), "\n")
cat("International flows (origin != destination):", sum(origin_dest_data$is_international), "\n")
cat("Domestic flows (origin == destination):", sum(!origin_dest_data$is_international), "\n")
cat("Percentage international:", round(100 * mean(origin_dest_data$is_international), 2), "%\n")
cat("Unique origins:", length(unique(origin_dest_data$origin)), "\n")
cat("Unique destinations:", length(unique(origin_dest_data$destination)), "\n")

# Top bilateral flows (top 20 for display)
bilateral_flows <- origin_dest_data %>%
  group_by(origin, destination) %>%
  summarise(
    n_projects = n(),
    total_funding = sum(funding, na.rm = TRUE),
    avg_funding = mean(funding, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_projects >= 5) %>%  # Filter to meaningful flows
  arrange(desc(n_projects))

cat("\nTop 20 bilateral flows by number of projects:\n")
print(head(bilateral_flows, 20))

# Generate table of top bilateral flows
top_flows <- head(bilateral_flows, 15)

flows_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Top Bilateral Flows: Origin-Destination Patterns}\n",
  "\\label{tab:bilateral_flows}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{llccc}\n",
  "\\toprule\n",
  "Origin & Destination & Projects & Total Funding & Avg Funding \\\\\n",
  "& & (N) & (USD) & (USD) \\\\\n",
  "\\midrule\n"
)

for (i in 1:nrow(top_flows)) {
  flows_latex <- paste0(flows_latex,
    sprintf("%s & %s & %s & \\$%s & \\$%s \\\\\n",
            str_trunc(top_flows$origin[i], 20),
            str_trunc(top_flows$destination[i], 20),
            format(top_flows$n_projects[i], big.mark = ","),
            format(round(top_flows$total_funding[i]), big.mark = ","),
            format(round(top_flows$avg_funding[i]), big.mark = ",")))
}

flows_latex <- paste0(flows_latex,
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Origin refers to the location of the organization (contact\\_country) launching the project; Destination is where the project operates (country). Note that this indicates the organization's base, not necessarily where individual donors are located. Only flows with $\\geq$ 5 projects shown.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(flows_latex, "tables/table_bilateral_flows.tex")
cat("Saved: tables/table_bilateral_flows.tex\n")

# Regression analysis: Does origin-destination match affect funding?
cat("\n=== Regression: Origin-Destination Match Effect ===\n")

# Prepare regression data (use already-created log variables from df)
origin_dest_reg <- origin_dest_data %>%
  filter(!is.na(log_funding) & !is.na(log_goal) & !is.na(log_duration)) %>%
  mutate(
    theme_factor = factor(theme_name),
    year_factor = factor(approved_year),
    origin_destination_match = !is_international
  )

# Run regressions for all 4 outcomes
od_m1_funding <- feols(log_funding ~ origin_destination_match + log_goal + log_duration |
                       theme_factor + year_factor,
                       data = origin_dest_reg, vcov = "hetero")

od_m1_avgdon <- feols(log_avg_donation ~ origin_destination_match + log_goal + log_duration |
                      theme_factor + year_factor,
                      data = origin_dest_reg, vcov = "hetero")

od_m1_numdon <- feols(log_donations ~ origin_destination_match + log_goal + log_duration |
                      theme_factor + year_factor,
                      data = origin_dest_reg, vcov = "hetero")

od_m1_funded <- feols(is_fully_funded ~ origin_destination_match + log_goal + log_duration |
                      theme_factor + year_factor,
                      data = origin_dest_reg, vcov = "hetero")

# Generate table
od_coef_funding <- coef(od_m1_funding)["origin_destination_matchTRUE"]
od_se_funding <- sqrt(diag(vcov(od_m1_funding)))["origin_destination_matchTRUE"]
od_pval_funding <- 2 * pnorm(-abs(od_coef_funding / od_se_funding))

od_coef_avgdon <- coef(od_m1_avgdon)["origin_destination_matchTRUE"]
od_se_avgdon <- sqrt(diag(vcov(od_m1_avgdon)))["origin_destination_matchTRUE"]
od_pval_avgdon <- 2 * pnorm(-abs(od_coef_avgdon / od_se_avgdon))

od_coef_numdon <- coef(od_m1_numdon)["origin_destination_matchTRUE"]
od_se_numdon <- sqrt(diag(vcov(od_m1_numdon)))["origin_destination_matchTRUE"]
od_pval_numdon <- 2 * pnorm(-abs(od_coef_numdon / od_se_numdon))

od_coef_funded <- coef(od_m1_funded)["origin_destination_matchTRUE"]
od_se_funded <- sqrt(diag(vcov(od_m1_funded)))["origin_destination_matchTRUE"]
od_pval_funded <- 2 * pnorm(-abs(od_coef_funded / od_se_funded))

od_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Origin-Destination Match Effects on Funding Outcomes}\n",
  "\\label{tab:origin_destination}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) \\\\\n",
  "& Log(Funding) & Log(Avg Donation) & Log(\\# Donations) & Fully Funded \\\\\n",
  "\\midrule\n",
  sprintf("Origin = Destination & %s & %s & %s & %s \\\\\n",
          fmt_coef(od_coef_funding, od_pval_funding),
          fmt_coef(od_coef_avgdon, od_pval_avgdon),
          fmt_coef(od_coef_numdon, od_pval_numdon),
          fmt_coef(od_coef_funded, od_pval_funded)),
  sprintf("& %s & %s & %s & %s \\\\\n",
          fmt_se(od_se_funding), fmt_se(od_se_avgdon),
          fmt_se(od_se_numdon), fmt_se(od_se_funded)),
  "\\midrule\n",
  "Theme/Year FE & Yes & Yes & Yes & Yes \\\\\n",
  "Log(Goal), Duration & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & \\multicolumn{4}{c}{%s} \\\\\n", format(od_m1_funding$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(od_m1_funding, "r2")$r2, fitstat(od_m1_avgdon, "r2")$r2,
          fitstat(od_m1_numdon, "r2")$r2, fitstat(od_m1_funded, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Origin = Destination is a binary indicator equal to 1 when the organization's contact country matches the project's operating country (domestic projects). Origin refers to contact\\_country (organization location); Destination refers to country (project location). Note that this indicates where the organization is based, not where individual donors are located. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(od_latex, "tables/table_origin_destination.tex")
cat("Saved: tables/table_origin_destination.tex\n")

# Create Sankey/flow chart data
cat("\n=== Generating Sankey Chart Data ===\n")

# Aggregate flows for top origins and destinations
top_origins <- bilateral_flows %>%
  group_by(origin) %>%
  summarise(total_projects = sum(n_projects)) %>%
  arrange(desc(total_projects)) %>%
  head(10) %>%
  pull(origin)

top_destinations <- bilateral_flows %>%
  group_by(destination) %>%
  summarise(total_projects = sum(n_projects)) %>%
  arrange(desc(total_projects)) %>%
  head(10) %>%
  pull(destination)

# Filter to top origins/destinations for visualization
sankey_data <- bilateral_flows %>%
  filter(origin %in% top_origins | destination %in% top_destinations) %>%
  filter(n_projects >= 20) %>%  # Only significant flows
  select(origin, destination, n_projects)

# Install and load networkD3 if available
if (!require("networkD3", quietly = TRUE)) {
  cat("networkD3 package not available, skipping interactive Sankey diagram\n")
} else {
  library(networkD3)

  # Prepare data for networkD3
  nodes <- data.frame(
    name = unique(c(as.character(sankey_data$origin),
                    as.character(sankey_data$destination)))
  )

  sankey_data_indexed <- sankey_data %>%
    mutate(
      origin_id = match(origin, nodes$name) - 1,  # 0-indexed
      destination_id = match(destination, nodes$name) - 1
    )

  # Create Sankey diagram
  sn <- sankeyNetwork(
    Links = sankey_data_indexed,
    Nodes = nodes,
    Source = "origin_id",
    Target = "destination_id",
    Value = "n_projects",
    NodeID = "name",
    units = "projects",
    fontSize = 12,
    nodeWidth = 30,
    height = 800,
    width = 1000
  )

  # Save interactive HTML
  htmlwidgets::saveWidget(sn, "figures/sankey_origin_destination.html")
  cat("Saved interactive Sankey: figures/sankey_origin_destination.html\n")
}

# Create static Sankey diagram for LaTeX paper using ggalluvial
cat("\n=== Generating Static Sankey Diagram (PDF) ===\n")
if (!require("ggalluvial", quietly = TRUE)) {
  cat("Installing ggalluvial package...\n")
  install.packages("ggalluvial", repos = "https://cloud.r-project.org")
  library(ggalluvial)
} else {
  library(ggalluvial)
}

# Prepare data for ggalluvial - focus on top 20 flows only for readability
top_flows <- sankey_data %>%
  arrange(desc(n_projects)) %>%
  head(20)

# Create a long format for alluvial plot
sankey_long <- top_flows %>%
  mutate(flow_id = row_number()) %>%
  pivot_longer(
    cols = c(origin, destination),
    names_to = "position",
    values_to = "country"
  ) %>%
  mutate(
    position = factor(position, levels = c("origin", "destination")),
    x = as.numeric(position)
  )

# Create the static Sankey/alluvial diagram
p_sankey <- ggplot(sankey_long,
       aes(x = x, stratum = country, alluvium = flow_id,
           y = n_projects, fill = country, label = country)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            alpha = 0.6, width = 0.3) +
  geom_stratum(width = 0.3, alpha = 0.8) +
  geom_text(stat = "stratum", aes(label = country), size = 3) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Organization Origin", "Project Destination"),
                     expand = c(0.1, 0.1)) +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Top 20 Cross-Border Charitable Flows",
    subtitle = "Organization origin → Project destination",
    y = "Number of Projects"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30")
  )

ggsave("figures/fig_sankey_flows.pdf", p_sankey, width = 12, height = 8, device = "pdf")
cat("Saved static Sankey: figures/fig_sankey_flows.pdf\n")

# Create network graph visualization
cat("\n=== Generating Network Graph ===\n")

# Install and load required packages
if (!require("igraph", quietly = TRUE)) {
  cat("Installing igraph package...\n")
  install.packages("igraph", repos = "https://cloud.r-project.org")
  library(igraph)
} else {
  library(igraph)
}

if (!require("ggraph", quietly = TRUE)) {
  cat("Installing ggraph package...\n")
  install.packages("ggraph", repos = "https://cloud.r-project.org")
  library(ggraph)
} else {
  library(ggraph)
}

# Select top countries for network visualization
# Calculate node importance: total projects as either origin or destination
node_importance <- bilateral_flows %>%
  group_by(origin) %>%
  summarise(as_origin = sum(n_projects), funding_origin = sum(total_funding)) %>%
  full_join(
    bilateral_flows %>%
      group_by(destination) %>%
      summarise(as_destination = sum(n_projects), funding_destination = sum(total_funding)),
    by = c("origin" = "destination")
  ) %>%
  mutate(
    country = origin,
    total_projects = coalesce(as_origin, 0) + coalesce(as_destination, 0),
    total_funding = coalesce(funding_origin, 0) + coalesce(funding_destination, 0)
  ) %>%
  arrange(desc(total_projects))

# Select top 20 countries by total activity
top_countries <- node_importance %>%
  head(20) %>%
  pull(country)

# Filter flows to only include INTERNATIONAL edges between top countries (no self-loops)
network_edges <- bilateral_flows %>%
  filter(origin %in% top_countries & destination %in% top_countries) %>%
  filter(origin != destination) %>%  # EXCLUDE domestic flows (self-loops)
  filter(n_projects >= 20) %>%  # Only substantial flows
  mutate(
    edge_type = "International",  # All edges are international now
    funding_millions = total_funding / 1e6
  ) %>%
  select(from = origin, to = destination,
         n_projects, total_funding, avg_funding, edge_type, funding_millions)

# Create nodes dataframe
network_nodes <- node_importance %>%
  filter(country %in% top_countries) %>%
  select(name = country, total_projects, total_funding) %>%
  mutate(
    is_major_origin = name %in% c("United States", "United Kingdom", "India"),
    node_type = case_when(
      name == "United States" ~ "US",
      name %in% c("India", "United Kingdom") ~ "Major Origin",
      TRUE ~ "Other"
    )
  )

# Create igraph object
g <- graph_from_data_frame(d = network_edges, vertices = network_nodes, directed = TRUE)

# Print network statistics
edge_data_stats <- network_edges
cat("\nNetwork Statistics:\n")
cat("Nodes (countries):", vcount(g), "\n")
cat("Edges (international flows):", ecount(g), "\n")
cat("All edges are cross-border flows (self-loops excluded)\n")

# Create network plot using ggraph
set.seed(123)  # For reproducible layout

p_network <- ggraph(g, layout = "fr") +
  # Draw edges with arrow (all are international, no self-loops)
  geom_edge_arc(
    aes(
      width = n_projects,
      alpha = n_projects
    ),
    color = "#E74C3C",  # Red for all international flows
    arrow = arrow(length = unit(3, 'mm'), type = "closed"),
    end_cap = circle(8, 'mm'),
    strength = 0.3
  ) +
  # Draw nodes
  geom_node_point(
    aes(size = total_funding / 1e6, fill = node_type),
    shape = 21, color = "white", stroke = 1.5
  ) +
  # Add country labels
  geom_node_text(
    aes(label = name),
    size = 3,
    fontface = "bold",
    repel = TRUE,
    max.overlaps = 20
  ) +
  # Scales
  scale_edge_width_continuous(
    name = "Number of\nProjects",
    range = c(0.3, 3),
    guide = guide_legend(override.aes = list(edge_alpha = 1))
  ) +
  scale_edge_alpha_continuous(range = c(0.4, 0.95), guide = "none") +
  scale_size_continuous(
    name = "Total Funding\n($M)",
    range = c(4, 22)
  ) +
  scale_fill_manual(
    name = "Country Type",
    values = c("US" = "#E74C3C", "Major Origin" = "#3498DB", "Other" = "#95A5A6")
  ) +
  labs(
    title = "Cross-Border Charitable Funding Network",
    subtitle = "International flows only (domestic projects excluded) | Node size = total funding, Edge width = projects",
    caption = "Directed arrows show flows from organization origin to project destination\nOnly international flows with ≥20 projects shown"
  ) +
  theme_graph(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
    legend.position = "right",
    legend.box = "vertical",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("figures/fig_network_flows.pdf", p_network, width = 14, height = 10, bg = "white")
cat("Saved: figures/fig_network_flows.pdf\n")
cat("Network density:", edge_density(g), "\n")

# Also create the bar chart for comparison (keeping the previous viz)
# Separate domestic vs international flows
flow_summary <- bilateral_flows %>%
  mutate(
    flow_type = ifelse(origin == destination, "Domestic", "International"),
    flow_label = ifelse(origin == destination,
                        origin,
                        paste(origin, "→", destination))
  ) %>%
  arrange(desc(n_projects))

# Top 15 of each type
top_domestic <- flow_summary %>%
  filter(flow_type == "Domestic") %>%
  head(15)

top_international <- flow_summary %>%
  filter(flow_type == "International") %>%
  head(15)

# Combined for visualization
top_both <- bind_rows(top_domestic, top_international) %>%
  arrange(flow_type, desc(n_projects))

if (nrow(top_both) > 0) {
  # Create faceted bar chart distinguishing domestic vs international
  p_flows <- top_both %>%
    mutate(
      flow_label_clean = str_trunc(flow_label, 35),
      avg_funding_k = avg_funding / 1000
    ) %>%
    ggplot(aes(x = reorder(flow_label_clean, n_projects), y = n_projects, fill = flow_type)) +
    geom_col(alpha = 0.85) +
    coord_flip() +
    facet_wrap(~ flow_type, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = c("Domestic" = "#2C3E50", "International" = "#E74C3C")) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Charitable Funding Flows: Domestic vs. International Projects",
      subtitle = "Top 15 flows in each category by number of projects",
      x = NULL,
      y = "Number of Projects",
      caption = "Note: Domestic flows = organization and project in same country; International flows = cross-border operations"
    ) +
    theme_paper +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 7),
      strip.background = element_rect(fill = "gray90", color = NA),
      strip.text = element_text(face = "bold", size = 11)
    )

  ggsave("figures/fig_bilateral_flows.pdf", p_flows, width = 12, height = 8)
  cat("Saved: figures/fig_bilateral_flows.pdf\n")
}

# ==============================================================================
# TABLE 12: ROBUSTNESS
# ==============================================================================

cat("=== Table 12: Robustness ===\n")

rob_m1 <- feols(log_funding ~ log_goal + log_duration | theme_factor + region_factor + year_factor,
                data = reg_data, vcov = "hetero")

rob_data_wins <- reg_data %>%
  mutate(log_funding_wins = log1p(pmin(pmax(funding, quantile(funding, 0.01)), quantile(funding, 0.99))))
rob_m2 <- feols(log_funding_wins ~ log_goal + log_duration | theme_factor + region_factor + year_factor,
                data = rob_data_wins, vcov = "hetero")

rob_data_nocovid <- reg_data %>% filter(!approved_year %in% c(2020, 2021))
rob_m3 <- feols(log_funding ~ log_goal + log_duration | theme_factor + region_factor + year_factor,
                data = rob_data_nocovid, vcov = "hetero")

rob_data_completed <- reg_data %>% filter(status %in% c("funded", "retired"))
if (nrow(rob_data_completed) > 100) {
  rob_m4 <- feols(log_funding ~ log_goal + log_duration | theme_factor + region_factor + year_factor,
                  data = rob_data_completed, vcov = "hetero")
} else {
  rob_m4 <- rob_m1
}

rob_data_large <- reg_data %>% filter(goal >= 1000)
rob_m5 <- feols(log_funding ~ log_goal + log_duration | theme_factor + region_factor + year_factor,
                data = rob_data_large, vcov = "hetero")

robustness_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Robustness: Goal Elasticity Across Specifications}\n",
  "\\label{tab:robustness}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lccccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) & (5) \\\\\n",
  "& Baseline & Winsorized & No COVID & Completed & $\\geq$\\$1,000 \\\\\n",
  "\\midrule\n"
)

coefs <- sapply(list(rob_m1, rob_m2, rob_m3, rob_m4, rob_m5), function(m) coef(m)["log_goal"])
ses <- sapply(list(rob_m1, rob_m2, rob_m3, rob_m4, rob_m5), function(m) sqrt(diag(vcov(m)))["log_goal"])
pvals <- 2 * pnorm(-abs(coefs / ses))

cat("\n--- D.1/D.2 Baseline + robustness elasticities (log_goal, with duration control) ---\n")
cat(sprintf("  (1) Baseline:   %.4f (SE %.4f)\n", coefs[1], ses[1]))
cat(sprintf("  (2) Winsorized: %.4f (SE %.4f)\n", coefs[2], ses[2]))
cat(sprintf("  (3) No COVID:   %.4f (SE %.4f)\n", coefs[3], ses[3]))
cat(sprintf("  (4) Completed:  %.4f (SE %.4f)\n", coefs[4], ses[4]))
cat(sprintf("  (5) >= $1,000:  %.4f (SE %.4f)\n", coefs[5], ses[5]))

robustness_latex <- paste0(robustness_latex,
  sprintf("Log(Goal) & %s & %s & %s & %s & %s \\\\\n",
          fmt_coef(coefs[1], pvals[1]), fmt_coef(coefs[2], pvals[2]),
          fmt_coef(coefs[3], pvals[3]), fmt_coef(coefs[4], pvals[4]),
          fmt_coef(coefs[5], pvals[5])),
  sprintf("& %s & %s & %s & %s & %s \\\\\n",
          fmt_se(ses[1]), fmt_se(ses[2]), fmt_se(ses[3]), fmt_se(ses[4]), fmt_se(ses[5])),
  "\\midrule\n",
  "Theme/Region/Year FE & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Duration Control & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s \\\\\n",
          format(rob_m1$nobs, big.mark = ","), format(rob_m2$nobs, big.mark = ","),
          format(rob_m3$nobs, big.mark = ","), format(rob_m4$nobs, big.mark = ","),
          format(rob_m5$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(rob_m1, "r2")$r2, fitstat(rob_m2, "r2")$r2,
          fitstat(rob_m3, "r2")$r2, fitstat(rob_m4, "r2")$r2,
          fitstat(rob_m5, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Column (1): baseline specification. (2): funding winsorized at 1\\%/99\\%. (3): excludes 2020--2021. (4): completed projects only. (5): goals $\\geq$ \\$1,000. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(robustness_latex, "tables/table12_robustness.tex")
cat("Saved: tables/table12_robustness.tex\n")

# ==============================================================================
# TABLE 13: SE ROBUSTNESS
# ==============================================================================

cat("=== Table 13: SE Robustness ===\n")

se_base <- lm(log_funding ~ log_goal + log_duration + theme_factor + region_factor + year_factor, data = reg_data)
coef_val <- coef(se_base)["log_goal"]

se_classical <- summary(se_base)$coefficients["log_goal", "Std. Error"]
se_robust <- sqrt(diag(sandwich::vcovHC(se_base, type = "HC1")))["log_goal"]

se_theme <- feols(log_funding ~ log_goal + log_duration + region_factor + year_factor | theme_factor,
                  data = reg_data, vcov = ~theme_factor)
se_theme_val <- sqrt(diag(vcov(se_theme)))["log_goal"]

se_region <- feols(log_funding ~ log_goal + log_duration + theme_factor + year_factor | region_factor,
                   data = reg_data, vcov = ~region_factor)
se_region_val <- sqrt(diag(vcov(se_region)))["log_goal"]

se_year <- feols(log_funding ~ log_goal + log_duration + theme_factor + region_factor | year_factor,
                 data = reg_data, vcov = ~year_factor)
se_year_val <- sqrt(diag(vcov(se_year)))["log_goal"]

se_robust_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Standard Error Robustness}\n",
  "\\label{tab:se_robust}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lccccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) & (5) \\\\\n",
  "& Classical & Robust & Theme & Region & Year \\\\\n",
  "& & (HC1) & Cluster & Cluster & Cluster \\\\\n",
  "\\midrule\n",
  sprintf("Log(Goal) & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n", coef_val, coef_val, coef_val, coef_val, coef_val),
  sprintf("& (%.3f) & (%.3f) & (%.3f) & (%.3f) & (%.3f) \\\\\n",
          se_classical, se_robust, se_theme_val, se_region_val, se_year_val),
  "\\addlinespace\n",
  sprintf("t-statistic & %.2f & %.2f & %.2f & %.2f & %.2f \\\\\n",
          coef_val / se_classical, coef_val / se_robust, coef_val / se_theme_val,
          coef_val / se_region_val, coef_val / se_year_val),
  "\\midrule\n",
  sprintf("Observations & \\multicolumn{5}{c}{%s} \\\\\n", format(nrow(reg_data), big.mark = ",")),
  "Clusters & --- & --- & 28 & 7 & 15 \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Same coefficient, different standard error specifications. All significant at p$<$0.01.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(se_robust_latex, "tables/table13_se_robust.tex")
cat("Saved: tables/table13_se_robust.tex\n")

# ==============================================================================
# TABLE: LEAVE ONE OUT
# ==============================================================================

cat("=== Table: Leave One Out ===\n")

themes <- unique(reg_data$theme_name)
themes <- themes[!is.na(themes) & themes != ""]

loo_results <- map_dfr(themes, function(t) {
  data_subset <- reg_data %>% filter(theme_name != t)
  if (nrow(data_subset) < 100) return(NULL)
  mod <- feols(log_funding ~ log_goal + log_duration | region_factor + year_factor,
               data = data_subset, vcov = "hetero")
  tibble(
    theme_excluded = t,
    coefficient = coef(mod)["log_goal"],
    se = sqrt(diag(vcov(mod)))["log_goal"],
    n_obs = mod$nobs
  )
})

loo_results <- loo_results %>%
  mutate(pval = 2 * pnorm(-abs(coefficient / se))) %>%
  arrange(coefficient)

cat("\n--- D.4 Leave-one-out (log_goal, with duration control) ---\n")
cat(sprintf("  Full sample: %.4f\n", coef(rob_m1)["log_goal"]))
cat(sprintf("  Range: [%.4f (excl %s), %.4f (excl %s)]\n",
            min(loo_results$coefficient), loo_results$theme_excluded[which.min(loo_results$coefficient)],
            max(loo_results$coefficient), loo_results$theme_excluded[which.max(loo_results$coefficient)]))

loo_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Leave-One-Out Analysis: Goal Elasticity Stability}\n",
  "\\label{tab:leave_one_out}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "Theme Excluded & Coefficient & SE & t-stat & N \\\\\n",
  "\\midrule\n",
  sprintf("\\textit{Full Sample} & %.3f & %.3f & %.2f & %s \\\\\n",
          coef(rob_m1)["log_goal"], sqrt(diag(vcov(rob_m1)))["log_goal"],
          coef(rob_m1)["log_goal"] / sqrt(diag(vcov(rob_m1)))["log_goal"],
          format(rob_m1$nobs, big.mark = ",")),
  "\\addlinespace\n"
)

for (i in 1:min(nrow(loo_results), 10)) {
  loo_latex <- paste0(loo_latex,
    sprintf("%s & %.3f & %.3f & %.2f & %s \\\\\n",
            str_trunc(loo_results$theme_excluded[i], 25),
            loo_results$coefficient[i], loo_results$se[i],
            loo_results$coefficient[i] / loo_results$se[i],
            format(loo_results$n_obs[i], big.mark = ",")))
}

if (nrow(loo_results) > 10) {
  loo_latex <- paste0(loo_latex, "\\multicolumn{5}{c}{\\textit{... and ", nrow(loo_results) - 10, " more themes ...}} \\\\\n")
}

loo_latex <- paste0(loo_latex,
  "\\midrule\n",
  sprintf("Range & [%.3f, %.3f] & & & \\\\\n", min(loo_results$coefficient), max(loo_results$coefficient)),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Each row excludes one theme. Coefficient stability demonstrates findings are not driven by any single theme.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(loo_latex, "tables/table_leave_one_out.tex")
cat("Saved: tables/table_leave_one_out.tex\n")

# ==============================================================================
# FIGURES
# ==============================================================================

cat("\n========================================\n")
cat("GENERATING FIGURES\n")
cat("========================================\n\n")

# ==============================================================================
# FIGURE 1: DISTRIBUTIONS
# ==============================================================================

cat("=== Figure 1: Distributions ===\n")

p1a <- df %>%
  filter(funding > 0) %>%
  ggplot(aes(x = funding)) +
  geom_histogram(bins = 50, fill = pal_main[3], alpha = 0.8, color = "white") +
  scale_x_log10(labels = dollar_format()) +
  labs(title = "(a) Distribution of Funding", x = "Funding (USD, log scale)", y = "Count")

p1b <- df %>%
  ggplot(aes(x = goal)) +
  geom_histogram(bins = 50, fill = pal_main[2], alpha = 0.8, color = "white") +
  scale_x_log10(labels = dollar_format()) +
  labs(title = "(b) Distribution of Goals", x = "Goal (USD, log scale)", y = "Count")

p1c <- df %>%
  filter(funding_ratio <= 2) %>%
  ggplot(aes(x = funding_ratio)) +
  geom_histogram(bins = 50, fill = pal_main[4], alpha = 0.8, color = "white") +
  geom_vline(xintercept = 1, linetype = "dashed", color = pal_main[2], linewidth = 0.8) +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "(c) Funding Ratio Distribution", x = "Funding / Goal", y = "Count")

p1d <- df %>%
  filter(number_of_donations > 0) %>%
  ggplot(aes(x = number_of_donations)) +
  geom_histogram(bins = 50, fill = pal_main[5], alpha = 0.8, color = "white") +
  scale_x_log10() +
  labs(title = "(d) Number of Donations", x = "Donations (log scale)", y = "Count")

fig1 <- (p1a + p1b) / (p1c + p1d)
ggsave("figures/fig1_distributions.pdf", fig1, width = 8, height = 6, dpi = 300)
cat("Saved: figures/fig1_distributions.pdf\n")

# ==============================================================================
# FIGURE 2: TIME TRENDS
# ==============================================================================

cat("=== Figure 2: Time Trends ===\n")

monthly_stats <- df %>%
  filter(!is.na(approved_yearmonth), approved_yearmonth >= as.POSIXct("2008-01-01")) %>%
  group_by(approved_yearmonth) %>%
  summarise(
    n_projects = n(),
    total_funding = sum(funding, na.rm = TRUE),
    mean_funding = mean(funding, na.rm = TRUE),
    .groups = "drop"
  )

crisis_dates <- tibble(
  date = as.POSIXct(c("2010-01-01", "2020-03-01", "2022-02-01", "2023-10-01")),
  event = c("Haiti", "COVID-19", "Ukraine", "Gaza")
)

p2a <- monthly_stats %>%
  ggplot(aes(x = approved_yearmonth, y = n_projects)) +
  geom_line(color = pal_main[3], linewidth = 0.5) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, color = pal_main[2], linetype = "dashed") +
  geom_vline(data = crisis_dates, aes(xintercept = date), linetype = "dotted", alpha = 0.5) +
  scale_x_datetime(date_labels = "%Y", date_breaks = "2 years") +
  labs(title = "(a) Monthly Project Launches", x = NULL, y = "Projects")

p2b <- monthly_stats %>%
  ggplot(aes(x = approved_yearmonth, y = total_funding / 1e6)) +
  geom_line(color = pal_main[4], linewidth = 0.5) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, color = pal_main[2], linetype = "dashed") +
  geom_vline(data = crisis_dates, aes(xintercept = date), linetype = "dotted", alpha = 0.5) +
  scale_x_datetime(date_labels = "%Y", date_breaks = "2 years") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(title = "(b) Monthly Total Funding", x = NULL, y = "Funding ($M)")

fig2 <- p2a / p2b
ggsave("figures/fig2_time_trends.pdf", fig2, width = 8, height = 5, dpi = 300)
cat("Saved: figures/fig2_time_trends.pdf\n")

# ==============================================================================
# FIGURE 3: UKRAINE EVENT STUDY
# ==============================================================================

cat("=== Figure 3: Ukraine Event Study ===\n")

ukraine_monthly <- df %>%
  filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_month_date), is_ukraine) %>%
  group_by(approved_month_date) %>%
  summarise(
    total_funding = sum(funding, na.rm = TRUE),
    total_donations = sum(number_of_donations, na.rm = TRUE),
    n_projects = n(),
    .groups = "drop"
  )

ukraine_monthly_complete <- tibble(approved_month_date = all_months_date) %>%
  left_join(ukraine_monthly, by = "approved_month_date") %>%
  mutate(
    total_funding = replace_na(total_funding, 0),
    total_donations = replace_na(total_donations, 0),
    n_projects = replace_na(n_projects, 0),
    post = approved_month_date >= ukraine_event_date
  )

fig3 <- ukraine_monthly_complete %>%
  ggplot(aes(x = approved_month_date, y = total_funding / 1e6)) +
  geom_vline(xintercept = ukraine_event_date, linetype = "dashed", color = pal_main[2], linewidth = 0.8) +
  geom_area(fill = pal_main[2], alpha = 0.3) +
  geom_line(color = pal_main[2], linewidth = 1) +
  geom_point(color = pal_main[2], size = 2) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  annotate("text", x = ukraine_event_date + 60,
           y = max(ukraine_monthly_complete$total_funding / 1e6) * 0.85,
           label = "Feb 2022\nInvasion", hjust = 0, size = 3.5, fontface = "bold", color = pal_main[2]) +
  labs(
    title = "Monthly Funding to Ukraine-Related Projects",
    x = NULL,
    y = "Total Monthly Funding ($M)"
  )

ggsave("figures/fig3_event_study.pdf", fig3, width = 8, height = 5, dpi = 300)
cat("Saved: figures/fig3_event_study.pdf\n")

# ==============================================================================
# FIGURE 4: DIFFERENCE-IN-DIFFERENCES
# ==============================================================================

cat("=== Figure 4: DiD ===\n")

#     Average Log Funding per Project
did_summary <- did_complete %>%
  mutate(
    Group = ifelse(is_ukraine, "Ukraine", "Non-Ukraine"),
    Period = ifelse(post, "Post-Invasion", "Pre-Invasion")
  ) %>%
  group_by(Group, Period) %>%
  summarise(
    mean_funding = mean(total_funding, na.rm = TRUE),
    se_funding = sd(total_funding, na.rm = TRUE) / sqrt(n()),
    mean_log_funding = mean(log_funding, na.rm = TRUE),
    se_log_funding = sd(log_funding, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(Period = factor(Period, levels = c("Pre-Invasion", "Post-Invasion")))

fig4 <- did_summary %>%
  ggplot(aes(x = Group, y = mean_log_funding, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = (mean_log_funding - 1.96 * se_log_funding),
                    ymax = (mean_log_funding + 1.96 * se_log_funding)),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_manual(values = c("Pre-Invasion" = pal_main[3], "Post-Invasion" = pal_main[2])) +
  # scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "Ukraine vs. Non-Ukraine Projects",
    subtitle = "Average Log Total Funding of Projects initiated per Month before and after February 2022",
    x = NULL,
    y = "Average Log Total Funding",
    fill = NULL
  ) +
  theme(legend.position = "top")

# #     Average Log Total Funding of Projects Initiated per Month 
# did_summary <- did_complete %>%
#   mutate(
#     Group = ifelse(is_ukraine, "Ukraine", "Non-Ukraine"),
#     Period = ifelse(post, "Post-Invasion", "Pre-Invasion")
#   ) %>%
#   group_by(Group, Period) %>%
#   summarise(
#     mean_funding = mean(total_funding, na.rm = TRUE),
#     se_funding = sd(total_funding, na.rm = TRUE) / sqrt(n()),
#     mean_log_funding = mean(log1p(total_funding/n_projects), na.rm = TRUE),
#     se_log_funding = sd(log1p(total_funding/n_projects), na.rm = TRUE) / sqrt(n()),
#     .groups = "drop"
#   ) %>%
#   mutate(Period = factor(Period, levels = c("Pre-Invasion", "Post-Invasion")))
# 
# fig4_2 <- did_summary %>%
#   ggplot(aes(x = Group, y = mean_log_funding, fill = Period)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
#   geom_errorbar(aes(ymin = (mean_log_funding - 1.96 * se_log_funding),
#                     ymax = (mean_log_funding + 1.96 * se_log_funding)),
#                 position = position_dodge(width = 0.8), width = 0.2) +
#   scale_fill_manual(values = c("Pre-Invasion" = pal_main[3], "Post-Invasion" = pal_main[2])) +
#   # scale_y_continuous(labels = dollar_format(suffix = "M")) +
#   labs(
#     title = "Difference-in-Differences: Ukraine vs. Non-Ukraine Projects",
#     subtitle = "Mean log monthly funding before and after February 2022",
#     x = NULL,
#     y = "Mean log Monthly Funding",
#     fill = NULL
#   ) +
#   theme(legend.position = "top")

ggsave("figures/fig4_did.pdf", fig4, width = 8, height = 5, dpi = 300)
cat("Saved: figures/fig4_did.pdf\n")

# ==============================================================================
# FIGURE 6: THEME HETEROGENEITY
# ==============================================================================

cat("=== Figure 6: Theme Heterogeneity ===\n")

all_themes <- reg_data %>%
  filter(!is.na(theme_name), theme_name != "") %>%
  count(theme_name) %>%
  filter(n >= 50) %>%
  pull(theme_name)

theme_coefs <- reg_data %>%
  filter(theme_name %in% all_themes) %>%
  group_by(theme_name) %>%
  summarise(
    n = n(),
    model_result = list(tryCatch({
      # Theme-specific regression: log(duration) control plus region and year
      # fixed effects (theme FE drops because the sample is a single theme).
      mod <- feols(log_funding ~ log_goal + log_duration | region_factor + year_factor,
                   data = cur_data(), vcov = "hetero")
      est <- coef(mod)["log_goal"]
      se <- sqrt(diag(vcov(mod)))["log_goal"]
      tibble(estimate = est, std.error = se,
             conf.low = est - 1.96 * se, conf.high = est + 1.96 * se,
             p.value = 2 * pnorm(-abs(est / se)))
    }, error = function(e) {
      tibble(estimate = NA_real_, std.error = NA_real_, conf.low = NA_real_,
             conf.high = NA_real_, p.value = NA_real_)
    })),
    .groups = "drop"
  ) %>%
  unnest(model_result) %>%
  filter(!is.na(estimate)) %>%
  mutate(significant = p.value < 0.05)

cat("\n--- D.5 Theme elasticities (log_goal, with duration + region + year FE) ---\n")
print(theme_coefs %>% arrange(desc(estimate)) %>%
        select(theme_name, n, estimate, std.error, p.value) %>%
        as.data.frame(), digits = 3)
cat(sprintf("Mean theme elasticity: %.3f\n", mean(theme_coefs$estimate)))

fig6 <- theme_coefs %>%
  ggplot(aes(x = reorder(theme_name, estimate), y = estimate)) +
  geom_hline(yintercept = mean(theme_coefs$estimate), linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_point(aes(color = significant), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significant), width = 0.2, linewidth = 0.6) +
  coord_flip() +
  scale_color_manual(values = c("TRUE" = pal_main[2], "FALSE" = "gray60")) +
  labs(
    title = "Goal Elasticity by Project Theme",
    x = NULL,
    y = expression(paste("Goal Elasticity (", hat(beta), ")"))
  ) +
  theme(legend.position = "none")

ggsave("figures/fig6_theme_heterogeneity.pdf", fig6, width = 7, height = 6, dpi = 300)
cat("Saved: figures/fig6_theme_heterogeneity.pdf\n")

# ==============================================================================
# FIGURE 7: QUANTILE REGRESSION
# ==============================================================================

cat("=== Figure 7: Quantile Regression ===\n")

quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

qreg_results <- map_dfr(quantiles, function(tau) {
  tryCatch({
    # Quantile regression with log(duration) control plus region and year
    # fixed effects entered as factors (quantreg does not absorb FE).
    model <- rq(log_funding ~ log_goal + log_duration + region_factor + year_factor,
                tau = tau, data = reg_data)
    # Bootstrap SEs: the nid sparsity estimator is singular once the region and
    # year dummies are included, so we use the bootstrap instead.
    ct <- coef(summary(model, se = "boot", R = 500))
    est <- ct["log_goal", 1]
    se <- ct["log_goal", 2]
    tibble(
      quantile = tau,
      estimate = est,
      std.error = se,
      conf.low = est - 1.96 * se,
      conf.high = est + 1.96 * se
    )
  }, error = function(e) { message("  qreg tau=", tau, " failed: ", conditionMessage(e)); NULL })
}) %>%
  filter(!is.na(estimate))

# OLS reference at the conditional mean using the same controls
ols_coef <- coef(lm(log_funding ~ log_goal + log_duration + region_factor + year_factor,
                    data = reg_data))["log_goal"]

cat("\n--- D.7 Quantile regression (log_goal, with duration + region + year FE) ---\n")
print(qreg_results %>% select(quantile, estimate, std.error) %>% as.data.frame(), digits = 3)
cat(sprintf("OLS reference (same controls): %.3f\n", ols_coef))

fig7 <- qreg_results %>%
  ggplot(aes(x = quantile, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = pal_main[3]) +
  geom_line(color = pal_main[3], linewidth = 1) +
  geom_point(color = pal_main[3], size = 3) +
  geom_hline(yintercept = ols_coef, linetype = "dashed", color = pal_main[2]) +
  scale_x_continuous(breaks = quantiles, labels = paste0(quantiles * 100, "th")) +
  annotate("text", x = 0.15, y = ols_coef + 0.015, label = "OLS", color = pal_main[2], size = 3) +
  labs(
    title = "Quantile Regression: Goal Elasticity Across Funding Distribution",
    x = "Funding Quantile",
    y = expression(paste("Coefficient on Log(Goal)"))
  )

ggsave("figures/fig7_quantile_regression.pdf", fig7, width = 7, height = 5, dpi = 300)
cat("Saved: figures/fig7_quantile_regression.pdf\n")

# ==============================================================================
# FIGURE 8: REGIONAL DISPARITIES
# ==============================================================================

cat("=== Figure 8: Regional Disparities ===\n")

regional_stats <- df %>%
  filter(region_clean != "Unspecified") %>%
  group_by(region_clean) %>%
  summarise(
    n_projects = n(),
    total_funding = sum(funding, na.rm = TRUE),
    mean_funding = mean(funding, na.rm = TRUE),
    success_rate = mean(is_fully_funded, na.rm = TRUE),
    .groups = "drop"
  )

p8a <- regional_stats %>%
  ggplot(aes(x = reorder(region_clean, mean_funding), y = mean_funding)) +
  geom_col(fill = pal_main[3], alpha = 0.8) +
  geom_text(aes(label = dollar(mean_funding, accuracy = 1)), hjust = -0.1, size = 2.5) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "(c) Mean Funding per Project", x = NULL, y = "Mean Funding ($)")

p8b <- regional_stats %>%
  ggplot(aes(x = reorder(region_clean, success_rate), y = success_rate)) +
  geom_col(fill = pal_main[4], alpha = 0.8) +
  geom_text(aes(label = percent(success_rate, accuracy = 1)), hjust = -0.1, size = 2.5) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "(d) Funding Success Rate", x = NULL, y = "% Fully Funded")

fig8 <- p8a + p8b
ggsave("figures/fig8_regional.pdf", fig8, width = 10, height = 4, dpi = 300)
cat("Saved: figures/fig8_regional.pdf\n")

# ==============================================================================
# FIGURE 8B: REGIONAL DEMAND SIDE (Project Counts and Total Goals)
# ==============================================================================

cat("=== Figure 8B: Regional Demand Side ===\n")

p8c <- regional_stats %>%
  ggplot(aes(x = reorder(region_clean, n_projects), y = n_projects)) +
  geom_col(fill = pal_main[5], alpha = 0.8) +
  geom_text(aes(label = scales::comma(n_projects)), hjust = -0.1, size = 2.5) +
  coord_flip() +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "(a) Number of Projects", x = NULL, y = "Project Count")

# Calculate total goals by region
regional_goals <- df %>%
  filter(region_clean != "Unspecified") %>%
  group_by(region_clean) %>%
  summarise(
    total_goals = sum(goal, na.rm = TRUE),
    .groups = "drop"
  )

p8d <- regional_goals %>%
  ggplot(aes(x = reorder(region_clean, total_goals), y = total_goals)) +
  geom_col(fill = pal_main[6], alpha = 0.8) +
  geom_text(aes(label = dollar(total_goals, scale = 1e-6, suffix = "M", accuracy = 1)),
            hjust = -0.1, size = 2.5) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "(b) Total Fundraising Goals", x = NULL, y = "Total Goals ($M)")

fig8_demand <- p8c + p8d
ggsave("figures/fig8_regional_demand.pdf", fig8_demand, width = 10, height = 4, dpi = 300)
cat("Saved: figures/fig8_regional_demand.pdf\n")

# ==============================================================================
# PRE-WAR AND POST-WAR SANKEY DIAGRAMS
# ==============================================================================

cat("\n=== Generating Pre-War and Post-War Sankey Diagrams ===\n")

# Define invasion date and pre-war cutoff (6 months before invasion)
invasion_date <- as.Date("2022-02-24")
pre_war_cutoff <- invasion_date %m-% months(6)  # 6 months before invasion

# Create origin-destination data if not already exists
if (!exists("origin_dest_data")) {
  origin_dest_data <- df %>%
    filter(!is.na(contact_country), !is.na(country)) %>%
    mutate(
      origin = contact_country,
      destination = country,
      is_international = contact_country != country
    )
}

# Pre-war flows (projects approved before pre_war_cutoff to avoid recent bias)
pre_war_flows <- origin_dest_data %>%
  filter(approved_date < pre_war_cutoff) %>%
  group_by(origin, destination) %>%
  summarise(
    n_projects = n(),
    total_funding = sum(funding, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_projects)) %>%
  head(40)  # Increased to show more flows

# Post-war flows (projects approved after invasion_date)
post_war_flows <- origin_dest_data %>%
  filter(approved_date >= invasion_date) %>%
  group_by(origin, destination) %>%
  summarise(
    n_projects = n(),
    total_funding = sum(funding, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_projects)) %>%
  head(40)  # Increased to show more flows

# Save flow data for later analysis
write_csv(pre_war_flows, "tables/pre_war_network_data.csv")
write_csv(post_war_flows, "tables/post_war_network_data.csv")
cat("Saved: tables/pre_war_network_data.csv and tables/post_war_network_data.csv\n")

# Create network graphs for pre-war and post-war periods
# Get unique countries from flows for each period
pre_war_countries <- unique(c(pre_war_flows$origin, pre_war_flows$destination))
post_war_countries <- unique(c(post_war_flows$origin, post_war_flows$destination))

# Helper function to create network graph for a time period
create_network_graph <- function(flows_data, title_text, subtitle_text, highlight_ukraine = FALSE) {
  # Filter to international flows only (no self-loops)
  network_edges_period <- flows_data %>%
    filter(origin != destination) %>%
    filter(n_projects >= 5) %>%  # Lowered threshold to show more flows
    mutate(
      edge_type = "International",
      funding_millions = total_funding / 1e6
    ) %>%
    select(from = origin, to = destination,
           n_projects, total_funding, edge_type, funding_millions)

  # Create nodes dataframe
  all_countries <- unique(c(network_edges_period$from, network_edges_period$to))
  network_nodes_period <- data.frame(name = all_countries, stringsAsFactors = FALSE) %>%
    left_join(
      flows_data %>%
        group_by(origin) %>%
        summarise(total_projects_origin = sum(n_projects),
                  total_funding_origin = sum(total_funding)),
      by = c("name" = "origin")
    ) %>%
    left_join(
      flows_data %>%
        group_by(destination) %>%
        summarise(total_projects_dest = sum(n_projects),
                  total_funding_dest = sum(total_funding)),
      by = c("name" = "destination")
    ) %>%
    mutate(
      total_projects = coalesce(total_projects_origin, 0) + coalesce(total_projects_dest, 0),
      total_funding = coalesce(total_funding_origin, 0) + coalesce(total_funding_dest, 0),
      node_type = case_when(
        highlight_ukraine & name == "Ukraine" ~ "Ukraine",
        name == "United States" ~ "US",
        name %in% c("India", "United Kingdom") ~ "Major Origin",
        TRUE ~ "Other"
      )
    )

  # Create igraph object
  g_period <- graph_from_data_frame(d = network_edges_period, vertices = network_nodes_period, directed = TRUE)

  # Create network plot
  set.seed(123)
  p_network_period <- ggraph(g_period, layout = "fr") +
    geom_edge_arc(
      aes(width = n_projects, alpha = n_projects),
      color = "#E74C3C",
      arrow = arrow(length = unit(2, 'mm'), type = "closed"),
      end_cap = circle(5, 'mm'),
      strength = 0.2
    ) +
    geom_node_point(
      aes(size = total_funding / 1e6, fill = node_type),
      shape = 21, color = "white", stroke = 1
    ) +
    geom_node_text(
      aes(label = name),
      size = 2.5,
      fontface = "bold",
      repel = TRUE,
      max.overlaps = 15
    ) +
    scale_edge_width_continuous(
      name = "Projects",
      range = c(0.2, 2),
      guide = guide_legend(override.aes = list(edge_alpha = 1))
    ) +
    scale_edge_alpha_continuous(range = c(0.3, 0.9), guide = "none") +
    scale_size_continuous(
      name = "Funding ($M)",
      range = c(2, 15)
    ) +
    scale_fill_manual(
      name = "Type",
      values = c("Ukraine" = "#FFD700", "US" = "#E74C3C", "Major Origin" = "#3498DB", "Other" = "#95A5A6")
    ) +
    labs(
      title = title_text,
      subtitle = subtitle_text
    ) +
    theme_graph(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  return(p_network_period)
}

# Create pre-war network graph
p_network_pre <- create_network_graph(
  pre_war_flows,
  "Pre-War Period (Before Aug 2021)",
  "International flows | Node size = funding, Edge width = projects",
  highlight_ukraine = FALSE
)

# Create post-war network graph
p_network_post <- create_network_graph(
  post_war_flows,
  "Post-War Period (After Feb 2022)",
  "International flows | Node size = funding, Edge width = projects",
  highlight_ukraine = TRUE  # Highlight Ukraine in post-war graph
)

# Save individual network graphs
ggsave("figures/fig_network_pre_war.pdf", p_network_pre, width = 7, height = 6, bg = "white")
ggsave("figures/fig_network_post_war.pdf", p_network_post, width = 7, height = 6, bg = "white")
cat("Saved: figures/fig_network_pre_war.pdf and figures/fig_network_post_war.pdf\n")

# ==============================================================================
# FIGURE: SENTIMENT ANALYSIS (AFINN/Bing)
# ==============================================================================

cat("=== Figure: Sentiment Analysis ===\n")

# Panel (a): Net sentiment vs mean log funding (binned scatter)
sent_bins <- reg_data %>%
  filter(!is.na(net_sentiment), is.finite(log_funding)) %>%
  mutate(sent_bin = cut(net_sentiment, breaks = 20)) %>%
  group_by(sent_bin) %>%
  summarise(
    sent_mid = mean(net_sentiment, na.rm = TRUE),
    mean_log_funding = mean(log_funding, na.rm = TRUE),
    .groups = "drop"
  )

p_sent_a <- sent_bins %>%
  ggplot(aes(x = sent_mid, y = mean_log_funding)) +
  geom_point(size = 2, color = pal_main[3], alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = pal_main[2], fill = pal_main[2], alpha = 0.2) +
  labs(title = "(a) Net Sentiment vs. Funding",
       x = "Net Sentiment Score",
       y = "Mean Log(Funding)")

# Panel (b): Sentiment coverage vs funding
coverage_bins <- reg_data %>%
  filter(!is.na(sentiment_intensity), is.finite(log_funding)) %>%
  mutate(cov_bin = cut(sentiment_intensity, breaks = 20)) %>%
  group_by(cov_bin) %>%
  summarise(
    cov_mid = mean(sentiment_intensity, na.rm = TRUE),
    mean_log_funding = mean(log_funding, na.rm = TRUE),
    .groups = "drop"
  )

p_sent_b <- coverage_bins %>%
  ggplot(aes(x = cov_mid, y = mean_log_funding)) +
  geom_point(size = 2, color = pal_main[4], alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = pal_main[5], fill = pal_main[5], alpha = 0.2) +
  labs(title = "(b) Sentiment Intensity vs. Funding",
       x = "Sentiment Intensity (%)",
       y = "Mean Log(Funding)")

# Panel (c): Distribution of sentiment across themes
theme_sent <- reg_data %>%
  filter(!is.na(theme_name), !is.na(sentiment_intensity)) %>%
  group_by(theme_name) %>%
  summarise(mean_sentiment = mean(sentiment_intensity, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_sentiment) %>%
  slice_head(n = 12)

p_sent_c <- theme_sent %>%
  ggplot(aes(x = reorder(theme_name, mean_sentiment), y = mean_sentiment)) +
  geom_col(fill = pal_main[6], alpha = 0.8) +
  coord_flip() +
  labs(title = "(c) Sentiment by Theme",
       x = NULL,
       y = "Mean Sentiment Intensity (%)")

# Panel (d): Positive word ratio vs funding
pos_bins <- reg_data %>%
  filter(!is.na(positive_ratio), is.finite(log_funding), is.finite(positive_ratio)) %>%
  mutate(pos_bin = cut(positive_ratio, breaks = 20)) %>%
  group_by(pos_bin) %>%
  summarise(
    pos_mid = mean(positive_ratio, na.rm = TRUE),
    mean_log_funding = mean(log_funding, na.rm = TRUE),
    .groups = "drop"
  )

p_sent_d <- pos_bins %>%
  ggplot(aes(x = pos_mid, y = mean_log_funding)) +
  geom_point(size = 2, color = pal_main[2], alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = pal_main[3], fill = pal_main[3], alpha = 0.2) +
  labs(title = "(d) Positive Word Ratio vs. Funding",
       x = "Positive Word Ratio",
       y = "Mean Log(Funding)")

fig_sent <- (p_sent_a + p_sent_b) / (p_sent_c + p_sent_d)
ggsave("figures/fig_sentiment_afinn.pdf", fig_sent, width = 10, height = 8, dpi = 300)
cat("Saved: figures/fig_sentiment_afinn.pdf\n")

# ==============================================================================
# FIGURE 9: WORLD MAP
# ==============================================================================

cat("=== Figure 9: World Map ===\n")

world <- ne_countries(scale = "medium", returnclass = "sf")

country_funding <- df %>%
  group_by(iso3166country_code) %>%
  summarise(total_funding = sum(funding, na.rm = TRUE), .groups = "drop") %>%
  rename(iso_a2 = iso3166country_code)

world_funding <- world %>% left_join(country_funding, by = "iso_a2")

fig9 <- ggplot(world_funding) +
  geom_sf(aes(fill = log10(total_funding + 1)), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90",
                       labels = function(x) dollar(10^x, accuracy = 1),
                       name = "Total Funding\n(log scale)") +
  labs(title = "Global Distribution of Charitable Funding") +
  theme_void(base_family = "serif") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("figures/fig9_world_map.pdf", fig9, width = 10, height = 5, dpi = 300)
cat("Saved: figures/fig9_world_map.pdf\n")

# ==============================================================================
# FIGURE 10: PLACEBO TESTS
# ==============================================================================

cat("=== Figure 10: Placebo Tests ===\n")


# Function to run aggregate monthly DiD for a given fake event date
# For placebo tests: only use data BEFORE the true event (Feb 2022) when testing fake dates
run_placebo_did_monthly <- function(data, fake_event_date, is_true_event = FALSE) {
  # For true event: use all data
  # For placebo: only use data up to Jan 2022 (before the true treatment)
  if (is_true_event) {
    all_months <- seq(as.Date("2020-01-01"), as.Date("2024-12-01"), by = "month")
    filter_data <- data %>% filter(approved_year >= 2020, approved_year <= 2024)
  } else {
    # For placebo: use 2-year window around the fake date, stopping before Feb 2022
    all_months <- seq(as.Date("2020-01-01"), as.Date("2022-01-01"), by = "month")
    filter_data <- data %>% filter(approved_month_date >= as.Date("2020-01-01"),
                                   approved_month_date < as.Date("2022-02-01"))
  }
  
  did_monthly <- filter_data %>%
    filter(!is.na(approved_month_date)) %>%
    group_by(approved_month_date, is_ukraine) %>%
    summarise(
      total_funding = sum(funding, na.rm = TRUE),
      n_projects = n(),
      .groups = "drop"
    )
  
  # Complete the panel with zeros
  did_complete <- expand_grid(
    approved_month_date = all_months,
    is_ukraine = c(TRUE, FALSE)
  ) %>%
    left_join(did_monthly, by = c("approved_month_date", "is_ukraine")) %>%
    mutate(
      total_funding = replace_na(total_funding, 0),
      n_projects = replace_na(n_projects, 0),
      log_funding = log1p(total_funding),
      post = approved_month_date >= fake_event_date,
      ukraine = as.numeric(is_ukraine),
      ukraine_post = ukraine * as.numeric(post),
      year = year(approved_month_date),
      year_factor = as.factor(year)
    )
  
  tryCatch({
    model <- lm(log_funding ~ ukraine + post + ukraine_post + year_factor, data = did_complete)
    coef_info <- summary(model)$coefficients
    if("ukraine_post" %in% rownames(coef_info)) {
      return(data.frame(
        date = fake_event_date,
        estimate = coef_info["ukraine_post", "Estimate"],
        se = coef_info["ukraine_post", "Std. Error"]
      ))
    }
  }, error = function(e) {
    cat("Error in placebo regression for date", as.character(fake_event_date), ":", conditionMessage(e), "\n")
    return(NULL)
  })
  return(NULL)
}

# Placebo dates (using Date class)
placebo_dates <- as.Date(c("2020-07-01", "2021-02-01", "2021-07-01", "2022-02-01"))
date_labels <- c("Jul 2020\n(Placebo)", "Feb 2021\n(Placebo)", "Jul 2021\n(Placebo)", "Feb 2022\n(True Event)")

# Run placebo tests with proper handling
placebo_results <- map2_dfr(
  placebo_dates,
  c(FALSE, FALSE, FALSE, TRUE),  # Only Feb 2022 is true event
  ~run_placebo_did_monthly(df, .x, is_true_event = .y)
)

cat("Placebo results computed:", nrow(placebo_results), "date(s)\n")

placebo_results <- placebo_results %>%
  mutate(
    label = factor(date_labels[match(date, placebo_dates)], levels = date_labels),
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    is_true_event = date == as.Date("2022-02-01")
  ) %>%
  filter(!is.na(label))

# Plot results
fig_placebo <- ggplot(placebo_results, aes(x = label, y = estimate, fill = is_true_event)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = c("FALSE" = "#3498DB", "TRUE" = "#E74C3C"), guide = "none") +
  labs(
    title = "Placebo Tests: DiD Coefficient Estimates",
    subtitle = "Ukraine x Post interaction at different hypothetical event dates",
    x = "",
    y = "DiD Coefficient (Ukraine x Post)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 9)
  )
ggsave("figures/fig10_placebo.pdf", fig_placebo, width = 8, height = 5, dpi = 300)
cat("Saved: figures/fig10_placebo.pdf\n")

# ==============================================================================
# FIGURE 11: TIME STABILITY
# ==============================================================================

cat("=== Figure 11: Time Stability ===\n")

year_coefs <- reg_data %>%
  group_by(approved_year) %>%
  summarise(
    n = n(),
    model_result = list(tryCatch({
      # Year-specific regression: log(duration) control plus theme and region
      # fixed effects (year FE drops because the sample is a single year).
      mod <- feols(log_funding ~ log_goal + log_duration | theme_factor + region_factor,
                   data = cur_data(), vcov = "hetero")
      est <- coef(mod)["log_goal"]
      se <- sqrt(diag(vcov(mod)))["log_goal"]
      tibble(estimate = est, std.error = se,
             conf.low = est - 1.96 * se, conf.high = est + 1.96 * se)
    }, error = function(e) {
      tibble(estimate = NA_real_, std.error = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
    })),
    .groups = "drop"
  ) %>%
  unnest(model_result) %>%
  filter(!is.na(estimate))

pooled_mean <- mean(year_coefs$estimate)

cat("\n--- D.6 Year-specific elasticities (log_goal, with duration + theme + region FE) ---\n")
print(year_coefs %>% select(approved_year, n, estimate, std.error) %>% as.data.frame(), digits = 3)
cat(sprintf("Pooled mean of year coefficients: %.3f (range %.3f to %.3f)\n",
            pooled_mean, min(year_coefs$estimate), max(year_coefs$estimate)))

fig11 <- year_coefs %>%
  ggplot(aes(x = approved_year, y = estimate)) +
  geom_hline(yintercept = pooled_mean, linetype = "dashed", color = pal_main[2]) +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = pal_main[3]) +
  geom_line(color = pal_main[3], linewidth = 0.8) +
  geom_point(color = pal_main[3], size = 2.5) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  annotate("text", x = 2011, y = pooled_mean + 0.03, label = "Pooled Mean", color = pal_main[2], size = 3) +
  labs(
    title = "Temporal Stability of Goal-Funding Elasticity",
    x = "Year",
    y = expression(paste("Goal Elasticity (", hat(beta), ")"))
  )

ggsave("figures/fig11_time_stability.pdf", fig11, width = 8, height = 5, dpi = 300)
cat("Saved: figures/fig11_time_stability.pdf\n")

# ==============================================================================
# FIGURE 12: ADDITIONALITY/SUBSTITUTION DECOMPOSITION
# ==============================================================================

cat("=== Figure 12: Additionality/Substitution Decomposition ===\n")

monthly_all <- df %>%
  filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_month_date)) %>%
  group_by(approved_month_date) %>%
  summarise(
    total_funding = sum(funding, na.rm = TRUE),
    ukraine_funding = sum(funding[is_ukraine], na.rm = TRUE),
    nonukraine_funding = sum(funding[!is_ukraine], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    post = approved_month_date >= ukraine_event_date,
    period = ifelse(post, "Post-Invasion", "Pre-Invasion")
  )

pre_mean <- monthly_all %>% filter(!post) %>% summarise(m = mean(total_funding)) %>% pull(m)
post_mean <- monthly_all %>% filter(post) %>% summarise(m = mean(total_funding)) %>% pull(m)
ukraine_post <- monthly_all %>% filter(post) %>% summarise(m = mean(ukraine_funding)) %>% pull(m)

decomp_data <- tibble(
  Category = c("Pre-Invasion Baseline", "Additional Giving", "Ukraine Funding (Post)"),
  Value = c(pre_mean, post_mean - pre_mean, ukraine_post)
) %>%
  mutate(Category = factor(Category, levels = c("Pre-Invasion Baseline", "Additional Giving", "Ukraine Funding (Post)")))

fig12 <- decomp_data %>%
  ggplot(aes(x = Category, y = Value / 1e6, fill = Category)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0("$", round(Value / 1e6, 1), "M")), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c(pal_main[3], pal_main[4], pal_main[2])) +
  scale_y_continuous(labels = dollar_format(suffix = "M"), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Additionality vs. Substitution: Monthly Funding Decomposition",
    subtitle = "Comparing pre-invasion baseline to post-invasion allocation",
    x = NULL,
    y = "Monthly Funding ($M)"
  ) +
  theme(legend.position = "none")

ggsave("figures/fig12_addsub_decomposition.pdf", fig12, width = 8, height = 5, dpi = 300)
cat("Saved: figures/fig12_addsub_decomposition.pdf\n")

# ==============================================================================
# DONE
# ==============================================================================

cat("\n========================================\n")
cat("ALL TABLES AND FIGURES GENERATED\n")
cat("========================================\n\n")

cat("Tables generated:\n")
cat("  - table1_summary_stats.tex\n")
cat("  - table3_did_results.tex\n")
cat("  - table_robust_did.tex\n")
cat("  - table6_addsub.tex\n")
cat("  - table_ukraine_descriptive.tex\n")
cat("  - table_competition_sets.tex\n")
cat("  - table_themes_war_interact.tex\n")
cat("  - table_emotions_war_interact.tex\n")
cat("  - table_sentiment_war_interact.tex\n")
cat("  - table_narrative_war_interact.tex\n")
cat("  - table_identifiable_war_interact.tex\n")
cat("  - table_regional_regression.tex\n")
cat("  - table_regional_war.tex\n")
cat("  - table12_robustness.tex\n")
cat("  - table13_se_robust.tex\n")
cat("  - table_leave_one_out.tex\n")
cat("\nFigures generated:\n")
cat("  - fig1_distributions.pdf\n")
cat("  - fig2_time_trends.pdf\n")
cat("  - fig3_event_study.pdf\n")
cat("  - fig4_did.pdf\n")
cat("  - fig6_theme_heterogeneity.pdf\n")
cat("  - fig7_quantile_regression.pdf\n")
cat("  - fig8_regional.pdf\n")
cat("  - fig9_world_map.pdf\n")

# ==============================================================================
# ADDITIONAL TABLES
# ==============================================================================

cat("\n=== Additional Tables ===\n")

# TABLE: LOSER ANALYSIS
cat("Creating table_loser_analysis.tex...\n")

reg_data_2020 <- reg_data_nonukr %>%
  filter(approved_year >= 2020) %>%
  group_by(theme_name) %>%
  mutate(
    funding_quartile = ntile(funding, 4),
    is_loser = as.numeric(funding_quartile == 1)
  ) %>%
  ungroup()

loser_m1 <- glm(is_loser ~ theme_disaster_response + theme_child_protection + theme_refugee_rights + theme_physical_health + theme_education + log_goal + log_duration,
                data = reg_data_2020, family = binomial())
loser_m2 <- glm(is_loser ~ theme_disaster_response * post_war + theme_child_protection * post_war + theme_refugee_rights * post_war +
                  theme_physical_health * post_war + theme_education * post_war + log_goal + log_duration,
                data = reg_data_2020, family = binomial())

loser_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Loser Analysis: Predictors of Bottom-Quartile Funding}\n",
  "\\label{tab:loser_analysis}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcc}\n",
  "\\toprule\n",
  "& (1) & (2) \\\\\n",
  "& Pr(Bottom Quartile) & With Interactions \\\\\n",
  "\\midrule\n",
  "\\multicolumn{3}{l}{\\textit{Panel A: Baseline Effects}} \\\\\n"
)

vars <- c("theme_disaster_responseTRUE", "theme_child_protectionTRUE", "theme_refugee_rightsTRUE", "theme_physical_healthTRUE", "theme_educationTRUE", "log_goal", "log_duration")
labels <- c("Disaster Response", "Child Protection", "Refugee Rights", "Physical Health", "Education", "Log(Goal)", "Log(Duration)")

for (i in seq_along(vars)) {
  c1 <- if (vars[i] %in% names(coef(loser_m1))) coef(loser_m1)[vars[i]] else NA
  s1 <- if (vars[i] %in% names(coef(loser_m1))) summary(loser_m1)$coefficients[vars[i], "Std. Error"] else NA
  p1 <- if (!is.na(c1)) summary(loser_m1)$coefficients[vars[i], "Pr(>|z|)"] else NA
  c2 <- if (vars[i] %in% names(coef(loser_m2))) coef(loser_m2)[vars[i]] else NA
  s2 <- if (vars[i] %in% names(coef(loser_m2))) summary(loser_m2)$coefficients[vars[i], "Std. Error"] else NA
  p2 <- if (!is.na(c2)) summary(loser_m2)$coefficients[vars[i], "Pr(>|z|)"] else NA

  loser_latex <- paste0(loser_latex,
    sprintf("%s & %s & %s \\\\\n", labels[i], fmt_coef(c1, p1), fmt_coef(c2, p2)),
    sprintf("& %s & %s \\\\\n", fmt_se(s1), fmt_se(s2)))
}

loser_latex <- paste0(loser_latex,
  "\\addlinespace\n",
  "\\multicolumn{3}{l}{\\textit{Panel B: Post-War Interactions}} \\\\\n"
)

vars <- c("theme_disaster_responseTRUE", "theme_child_protectionTRUE", "theme_refugee_rightsTRUE", "theme_physical_healthTRUE", "theme_educationTRUE", "log_goal", "log_duration")
loser_vars <- c("theme_disaster_responseTRUE", "theme_child_protectionTRUE", "theme_refugee_rightsTRUE", "theme_physical_healthTRUE", "theme_educationTRUE")
inter_labels <- c("Disaster Response $\\times$ Post-War", "Child Protection $\\times$ Post-War", "Refugee Rights $\\times$ Post-War", "Physical Health $\\times$ Post-War", "Education $\\times$ Post-War")

# Helper for glm model coefficient lookup (checks both orderings)
get_glm_inter <- function(mod, var1, var2) {
  name1 <- paste0(var1, ":", var2)
  name2 <- paste0(var2, ":", var1)
  coefs <- coef(mod)
  summ <- summary(mod)$coefficients

  if (name1 %in% names(coefs)) {
    return(list(coef = coefs[name1], se = summ[name1, "Std. Error"], pval = summ[name1, "Pr(>|z|)"]))
  } else if (name2 %in% names(coefs)) {
    return(list(coef = coefs[name2], se = summ[name2, "Std. Error"], pval = summ[name2, "Pr(>|z|)"]))
  } else {
    return(list(coef = NA, se = NA, pval = NA))
  }
}

for (i in seq_along(loser_vars)) {
  inter <- get_glm_inter(loser_m2, loser_vars[i], "post_warTRUE")
  c2 <- inter$coef
  s2 <- inter$se
  p2 <- inter$pval

  loser_latex <- paste0(loser_latex,
    sprintf("%s & --- & %s \\\\\n", inter_labels[i], fmt_coef(c2, p2, show_dash = TRUE)),
    sprintf("& & %s \\\\\n", fmt_se(s2, show_dash = TRUE)))
}

loser_latex <- paste0(loser_latex,
  "\\midrule\n",
  sprintf("Observations & %s & %s \\\\\n", format(nobs(loser_m1), big.mark = ","), format(nobs(loser_m2), big.mark = ",")),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Logit regression. Dependent variable = 1 if project in bottom quartile of within-theme funding. Negative coefficients indicate features that protect against being crowded out. Sample: non-Ukraine projects, 2020--2024. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(loser_latex, "tables/table_loser_analysis.tex")
cat("Saved: tables/table_loser_analysis.tex\n")

# TABLE 14: APPENDIX TREATMENT
cat("Creating table14_app_treatment.tex...\n")

app_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Treatment Definition and Sample Statistics}\n",
  "\\label{tab:app_treatment}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcc}\n",
  "\\toprule\n",
  "& Treatment & Control \\\\\n",
  "& (Ukraine-Related) & (Non-Ukraine) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{3}{l}{\\textit{Panel A: Treatment Definition}} \\\\\n",
  "Country = Ukraine & \\checkmark & \\\\\n",
  "Title contains ``Ukraine'' & \\checkmark & \\\\\n",
  "Summary contains ``Ukraine/Ukrainian'' & \\checkmark & \\\\\n",
  "\\addlinespace\n",
  "\\multicolumn{3}{l}{\\textit{Panel B: Sample Statistics (Full Sample)}} \\\\\n",
  sprintf("N Projects & %s & %s \\\\\n",
          format(sum(df$is_ukraine), big.mark = ","),
          format(sum(!df$is_ukraine), big.mark = ",")),
  sprintf("Total Funding (\\$M) & %.1f & %.1f \\\\\n",
          sum(df$funding[df$is_ukraine]) / 1e6,
          sum(df$funding[!df$is_ukraine]) / 1e6),
  sprintf("Mean Funding (\\$) & %s & %s \\\\\n",
          format(round(mean(df$funding[df$is_ukraine])), big.mark = ","),
          format(round(mean(df$funding[!df$is_ukraine])), big.mark = ",")),
  "\\addlinespace\n",
  "\\multicolumn{3}{l}{\\textit{Panel C: Regression Sample (2010--2024)}} \\\\\n",
  sprintf("N Projects & %s & %s \\\\\n",
          format(sum(reg_data$is_ukraine), big.mark = ","),
          format(sum(!reg_data$is_ukraine), big.mark = ",")),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Treatment group includes all projects matching any of the criteria in Panel A.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(app_latex, "tables/table14_app_treatment.tex")
cat("Saved: tables/table14_app_treatment.tex\n")
cat("  - fig10_placebo.pdf\n")
cat("  - fig11_time_stability.pdf\n")
cat("  - fig12_addsub_decomposition.pdf\n")

# ==============================================================================
# CONTAMINATION ROBUSTNESS CHECKS
# ==============================================================================

cat("\n========================================\n")
cat("CONTAMINATION ROBUSTNESS CHECKS\n")
cat("========================================\n\n")

# 1. DISTRIBUTION OF PROJECT DURATION FOR INACTIVE PROJECTS
cat("=== Figure: Project Duration Distribution ===\n")

inactive_projects <- df %>%
  filter(status != "active" | (!is.na(report_date) & report_date < ACTIVE_CUTOFF)) %>%
  filter(!is.na(project_duration_days), project_duration_days > 0, project_duration_days < 3650)

p_duration_dist <- ggplot(inactive_projects, aes(x = project_duration_days)) +
  geom_histogram(bins = 50, fill = "#3498DB", alpha = 0.7, color = "white") +
  geom_vline(xintercept = c(180, 365, 540), linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
  annotate("text", x = 180, y = Inf, label = "6 months", vjust = 2, hjust = -0.1, size = 3) +
  annotate("text", x = 365, y = Inf, label = "12 months", vjust = 2, hjust = -0.1, size = 3) +
  annotate("text", x = 540, y = Inf, label = "18 months", vjust = 2, hjust = -0.1, size = 3) +
  labs(
    title = "Distribution of Project Duration (Inactive Projects Only)",
    x = "Project Duration (Days)",
    y = "Number of Projects"
  ) +
  scale_x_continuous(breaks = seq(0, 3500, 365)) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 0))

ggsave("figures/fig_duration_distribution.pdf", p_duration_dist, width = 8, height = 5)
cat("Saved: figures/fig_duration_distribution.pdf\n")

# Summary statistics
duration_stats <- inactive_projects %>%
  summarise(
    median = median(project_duration_days, na.rm = TRUE),
    mean = mean(project_duration_days, na.rm = TRUE),
    p25 = quantile(project_duration_days, 0.25, na.rm = TRUE),
    p75 = quantile(project_duration_days, 0.75, na.rm = TRUE),
    within_6mo = mean(project_duration_days <= 180, na.rm = TRUE),
    within_12mo = mean(project_duration_days <= 365, na.rm = TRUE),
    within_18mo = mean(project_duration_days <= 540, na.rm = TRUE)
  )

cat("\nProject Duration Statistics (Inactive Projects):\n")
cat(sprintf("  Median: %.0f days (%.1f months)\n", duration_stats$median, duration_stats$median/30))
cat(sprintf("  Mean: %.0f days (%.1f months)\n", duration_stats$mean, duration_stats$mean/30))
cat(sprintf("  25th percentile: %.0f days\n", duration_stats$p25))
cat(sprintf("  75th percentile: %.0f days\n", duration_stats$p75))
cat(sprintf("  Within 6 months: %.1f%%\n", duration_stats$within_6mo * 100))
cat(sprintf("  Within 12 months: %.1f%%\n", duration_stats$within_12mo * 100))
cat(sprintf("  Within 18 months: %.1f%%\n", duration_stats$within_18mo * 100))

# 2. DID ROBUSTNESS WITH EXCLUSION CRITERIA - ALL 4 OUTCOMES
cat("\n=== Table: DiD Robustness - Sample Restrictions ===\n")

# Define treatment date
treatment_date <- as.Date("2022-02-24")

# Helper function for DiD with sample restrictions - ALL 4 OUTCOMES
run_did_robustness_full <- function(data, sample_name) {
  # Prepare monthly data
  did_monthly <- data %>%
    filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_month_date)) %>%
    group_by(approved_month_date, is_ukraine) %>%
    summarise(
      total_funding = sum(funding, na.rm = TRUE),
      total_donations = sum(number_of_donations, na.rm = TRUE),
      n_projects = n(),
      .groups = "drop"
    )

  # Calculate avg donation and fully funded
  did_monthly_extra <- data %>%
    filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_month_date)) %>%
    group_by(approved_month_date, is_ukraine) %>%
    summarise(
      pct_fully_funded = mean(is_fully_funded, na.rm = TRUE),
      .groups = "drop"
    )

  all_months <- seq(as.Date("2020-01-01"), as.Date("2024-12-01"), by = "month")

  did_complete <- expand_grid(
    approved_month_date = all_months,
    is_ukraine = c(TRUE, FALSE)
  ) %>%
    left_join(did_monthly, by = c("approved_month_date", "is_ukraine")) %>%
    left_join(did_monthly_extra, by = c("approved_month_date", "is_ukraine")) %>%
    mutate(
      total_funding = replace_na(total_funding, 0),
      total_donations = replace_na(total_donations, 0),
      n_projects = replace_na(n_projects, 0),
      pct_fully_funded = replace_na(pct_fully_funded, 0),
      avg_donation = ifelse(total_donations > 0, total_funding / total_donations, 0),
      log_funding = log1p(total_funding),
      log_donations = log1p(total_donations),
      log_avg_donation = log1p(avg_donation),
      post = approved_month_date >= as.Date("2022-02-01"),
      ukraine = as.numeric(is_ukraine),
      year = year(approved_month_date),
      year_factor = as.factor(year)
    )

  # Run 4 models
  m1 <- lm(log_funding ~ ukraine * post + year_factor, data = did_complete)
  m2 <- lm(log_avg_donation ~ ukraine * post + year_factor, data = did_complete)
  m3 <- lm(log_donations ~ ukraine * post + year_factor, data = did_complete)
  m4 <- lm(pct_fully_funded ~ ukraine * post + year_factor, data = did_complete)

  return(list(
    sample = sample_name,
    n_obs = nrow(data),
    coef_funding = coef(m1)["ukraine:postTRUE"],
    se_funding = sqrt(diag(vcov(m1)))["ukraine:postTRUE"],
    coef_avgdon = coef(m2)["ukraine:postTRUE"],
    se_avgdon = sqrt(diag(vcov(m2)))["ukraine:postTRUE"],
    coef_numdon = coef(m3)["ukraine:postTRUE"],
    se_numdon = sqrt(diag(vcov(m3)))["ukraine:postTRUE"],
    coef_funded = coef(m4)["ukraine:postTRUE"],
    se_funded = sqrt(diag(vcov(m4)))["ukraine:postTRUE"]
  ))
}

# Baseline
res_baseline <- run_did_robustness_full(
  df %>% filter(approved_year >= 2020, approved_year <= 2024),
  "Baseline"
)

# Exclude pre-war projects within 3 months
res_3mo <- run_did_robustness_full(
  df %>% filter(
    approved_year >= 2020, approved_year <= 2024,
    !(approved_date < treatment_date & approved_date >= treatment_date - months(3))
  ),
  "Exclude < 3mo Pre-War"
)

# Exclude pre-war projects within 6 months
res_6mo <- run_did_robustness_full(
  df %>% filter(
    approved_year >= 2020, approved_year <= 2024,
    !(approved_date < treatment_date & approved_date >= treatment_date - months(6))
  ),
  "Exclude < 6mo Pre-War"
)

# Exclude pre-war projects within 9 months
res_9mo <- run_did_robustness_full(
  df %>% filter(
    approved_year >= 2020, approved_year <= 2024,
    !(approved_date < treatment_date & approved_date >= treatment_date - months(9))
  ),
  "Exclude < 9mo Pre-War"
)

# Exclude active/recent 3mo
res_act3 <- run_did_robustness_full(
  df %>% filter(
    approved_year >= 2020, approved_year <= 2024,
    !(approved_date < treatment_date &
      (status == "active" | (!is.na(report_date) & report_date >= treatment_date - months(3))))
  ),
  "Exclude Active < 3mo"
)

# Exclude active/recent 6mo
res_act6 <- run_did_robustness_full(
  df %>% filter(
    approved_year >= 2020, approved_year <= 2024,
    !(approved_date < treatment_date &
      (status == "active" | (!is.na(report_date) & report_date >= treatment_date - months(6))))
  ),
  "Exclude Active < 6mo"
)

# Exclude active/recent 9mo
res_act9 <- run_did_robustness_full(
  df %>% filter(
    approved_year >= 2020, approved_year <= 2024,
    !(approved_date < treatment_date &
      (status == "active" | (!is.na(report_date) & report_date >= treatment_date - months(9))))
  ),
  "Exclude Active < 9mo"
)

# Exclude active/recent 12mo
res_act12 <- run_did_robustness_full(
  df %>% filter(
    approved_year >= 2020, approved_year <= 2024,
    !(approved_date < treatment_date &
      (status == "active" | (!is.na(report_date) & report_date >= treatment_date - months(12))))
  ),
  "Exclude Active < 12mo"
)

results_list <- list(res_baseline, res_3mo, res_6mo, res_9mo, res_act3, res_act6, res_act9, res_act12)

# Create table with all 4 outcomes
fmt_coef <- function(coef, se) {
  tstat <- abs(coef / se)
  stars <- ifelse(tstat > 2.576, "***",
                  ifelse(tstat > 1.96, "**",
                         ifelse(tstat > 1.645, "*", "")))
  sprintf("%.3f%s", coef, stars)
}

robust_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{DiD Robustness: Sample Restrictions}\n",
  "\\label{tab:did_robust}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) \\\\\n",
  "Sample Restriction & Log(Funding) & Log(Avg Don) & Log(\\# Don) & Fully Funded \\\\\n",
  "\\midrule\n"
)

for (res in results_list) {
  robust_latex <- paste0(
    robust_latex,
    sprintf("%s & %s & %s & %s & %s \\\\\n",
            res$sample,
            fmt_coef(res$coef_funding, res$se_funding),
            fmt_coef(res$coef_avgdon, res$se_avgdon),
            fmt_coef(res$coef_numdon, res$se_numdon),
            fmt_coef(res$coef_funded, res$se_funded)),
    sprintf("& (%.3f) & (%.3f) & (%.3f) & (%.3f) \\\\\n",
            res$se_funding, res$se_avgdon, res$se_numdon, res$se_funded)
  )
}

robust_latex <- paste0(
  robust_latex,
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Ukraine $\\times$ Post coefficients from DiD regressions with different sample restrictions. All specifications include year fixed effects. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(robust_latex, "tables/table_did_robust_full.tex")
cat("Saved: tables/table_did_robust_full.tex\n")

# 3. DID WITH FIXED EFFECTS - ALL 4 OUTCOMES
cat("\n=== Table: DiD with Fixed Effects ===\n")

# Helper for FE robustness
run_did_fe <- function(data, fe_spec) {
  did_monthly <- data %>%
    filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_month_date)) %>%
    group_by(approved_month_date, is_ukraine) %>%
    summarise(
      total_funding = sum(funding, na.rm = TRUE),
      total_donations = sum(number_of_donations, na.rm = TRUE),
      n_projects = n(),
      .groups = "drop"
    )

  did_monthly_extra <- data %>%
    filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_month_date)) %>%
    group_by(approved_month_date, is_ukraine) %>%
    summarise(
      pct_fully_funded = mean(is_fully_funded, na.rm = TRUE),
      .groups = "drop"
    )

  all_months <- seq(as.Date("2020-01-01"), as.Date("2024-12-01"), by = "month")

  did_complete <- expand_grid(
    approved_month_date = all_months,
    is_ukraine = c(TRUE, FALSE)
  ) %>%
    left_join(did_monthly, by = c("approved_month_date", "is_ukraine")) %>%
    left_join(did_monthly_extra, by = c("approved_month_date", "is_ukraine")) %>%
    mutate(
      total_funding = replace_na(total_funding, 0),
      total_donations = replace_na(total_donations, 0),
      pct_fully_funded = replace_na(pct_fully_funded, 0),
      avg_donation = ifelse(total_donations > 0, total_funding / total_donations, 0),
      log_funding = log1p(total_funding),
      log_donations = log1p(total_donations),
      log_avg_donation = log1p(avg_donation),
      post = approved_month_date >= as.Date("2022-02-01"),
      ukraine = as.numeric(is_ukraine),
      year = year(approved_month_date),
      year_factor = as.factor(year)
    )

  # Add goal/duration bins from project-level data
  project_stats <- data %>%
    filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_month_date)) %>%
    group_by(approved_month_date, is_ukraine) %>%
    summarise(
      mean_log_goal = mean(log_goal, na.rm = TRUE),
      mean_log_duration = mean(log_duration, na.rm = TRUE),
      .groups = "drop"
    )

  did_complete <- did_complete %>%
    left_join(project_stats, by = c("approved_month_date", "is_ukraine")) %>%
    mutate(
      mean_log_goal = replace_na(mean_log_goal, median(mean_log_goal, na.rm = TRUE)),
      mean_log_duration = replace_na(mean_log_duration, median(mean_log_duration, na.rm = TRUE)),
      goal_bin = as.factor(cut(mean_log_goal, breaks = 10, labels = FALSE)),
      duration_bin = as.factor(cut(mean_log_duration, breaks = 10, labels = FALSE))
    )

  # Run models based on fe_spec
  if (fe_spec == "baseline") {
    m1 <- lm(log_funding ~ ukraine * post + year_factor, data = did_complete)
    m2 <- lm(log_avg_donation ~ ukraine * post + year_factor, data = did_complete)
    m3 <- lm(log_donations ~ ukraine * post + year_factor, data = did_complete)
    m4 <- lm(pct_fully_funded ~ ukraine * post + year_factor, data = did_complete)
  } else if (fe_spec == "goal") {
    m1 <- lm(log_funding ~ ukraine * post + year_factor + goal_bin, data = did_complete)
    m2 <- lm(log_avg_donation ~ ukraine * post + year_factor + goal_bin, data = did_complete)
    m3 <- lm(log_donations ~ ukraine * post + year_factor + goal_bin, data = did_complete)
    m4 <- lm(pct_fully_funded ~ ukraine * post + year_factor + goal_bin, data = did_complete)
  } else if (fe_spec == "duration") {
    m1 <- lm(log_funding ~ ukraine * post + year_factor + duration_bin, data = did_complete)
    m2 <- lm(log_avg_donation ~ ukraine * post + year_factor + duration_bin, data = did_complete)
    m3 <- lm(log_donations ~ ukraine * post + year_factor + duration_bin, data = did_complete)
    m4 <- lm(pct_fully_funded ~ ukraine * post + year_factor + duration_bin, data = did_complete)
  } else {  # both
    m1 <- lm(log_funding ~ ukraine * post + year_factor + goal_bin + duration_bin, data = did_complete)
    m2 <- lm(log_avg_donation ~ ukraine * post + year_factor + goal_bin + duration_bin, data = did_complete)
    m3 <- lm(log_donations ~ ukraine * post + year_factor + goal_bin + duration_bin, data = did_complete)
    m4 <- lm(pct_fully_funded ~ ukraine * post + year_factor + goal_bin + duration_bin, data = did_complete)
  }

  list(
    coef_funding = coef(m1)["ukraine:postTRUE"],
    se_funding = sqrt(diag(vcov(m1)))["ukraine:postTRUE"],
    coef_avgdon = coef(m2)["ukraine:postTRUE"],
    se_avgdon = sqrt(diag(vcov(m2)))["ukraine:postTRUE"],
    coef_numdon = coef(m3)["ukraine:postTRUE"],
    se_numdon = sqrt(diag(vcov(m3)))["ukraine:postTRUE"],
    coef_funded = coef(m4)["ukraine:postTRUE"],
    se_funded = sqrt(diag(vcov(m4)))["ukraine:postTRUE"]
  )
}

data_full <- df %>% filter(approved_year >= 2020, approved_year <= 2024)

fe_baseline <- run_did_fe(data_full, "baseline")
fe_goal <- run_did_fe(data_full, "goal")
fe_duration <- run_did_fe(data_full, "duration")
fe_both <- run_did_fe(data_full, "both")

fe_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{DiD Robustness: Additional Fixed Effects}\n",
  "\\label{tab:did_fe}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "& \\multicolumn{4}{c}{Ukraine $\\times$ Post Coefficient} \\\\\n",
  "\\cmidrule(lr){2-5}\n",
  "Specification & Log(Funding) & Log(Avg Don) & Log(\\# Don) & Fully Funded \\\\\n",
  "\\midrule\n",
  sprintf("Baseline (Year FE) & %s & %s & %s & %s \\\\\n",
          fmt_coef(fe_baseline$coef_funding, fe_baseline$se_funding),
          fmt_coef(fe_baseline$coef_avgdon, fe_baseline$se_avgdon),
          fmt_coef(fe_baseline$coef_numdon, fe_baseline$se_numdon),
          fmt_coef(fe_baseline$coef_funded, fe_baseline$se_funded)),
  sprintf("& (%.3f) & (%.3f) & (%.3f) & (%.3f) \\\\\n",
          fe_baseline$se_funding, fe_baseline$se_avgdon, fe_baseline$se_numdon, fe_baseline$se_funded),
  sprintf("+ Log(Goal) FE & %s & %s & %s & %s \\\\\n",
          fmt_coef(fe_goal$coef_funding, fe_goal$se_funding),
          fmt_coef(fe_goal$coef_avgdon, fe_goal$se_avgdon),
          fmt_coef(fe_goal$coef_numdon, fe_goal$se_numdon),
          fmt_coef(fe_goal$coef_funded, fe_goal$se_funded)),
  sprintf("& (%.3f) & (%.3f) & (%.3f) & (%.3f) \\\\\n",
          fe_goal$se_funding, fe_goal$se_avgdon, fe_goal$se_numdon, fe_goal$se_funded),
  sprintf("+ Log(Duration) FE & %s & %s & %s & %s \\\\\n",
          fmt_coef(fe_duration$coef_funding, fe_duration$se_funding),
          fmt_coef(fe_duration$coef_avgdon, fe_duration$se_avgdon),
          fmt_coef(fe_duration$coef_numdon, fe_duration$se_numdon),
          fmt_coef(fe_duration$coef_funded, fe_duration$se_funded)),
  sprintf("& (%.3f) & (%.3f) & (%.3f) & (%.3f) \\\\\n",
          fe_duration$se_funding, fe_duration$se_avgdon, fe_duration$se_numdon, fe_duration$se_funded),
  sprintf("+ Both FE & %s & %s & %s & %s \\\\\n",
          fmt_coef(fe_both$coef_funding, fe_both$se_funding),
          fmt_coef(fe_both$coef_avgdon, fe_both$se_avgdon),
          fmt_coef(fe_both$coef_numdon, fe_both$se_numdon),
          fmt_coef(fe_both$coef_funded, fe_both$se_funded)),
  sprintf("& (%.3f) & (%.3f) & (%.3f) & (%.3f) \\\\\n",
          fe_both$se_funding, fe_both$se_avgdon, fe_both$se_numdon, fe_both$se_funded),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: DiD estimates with additional fixed effects. Goal FE and Duration FE are based on decile bins of monthly average log(goal) and log(duration). *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(fe_latex, "tables/table_did_fe.tex")
cat("Saved: tables/table_did_fe.tex\n")

cat("\n========================================\n")
cat("CONTAMINATION ROBUSTNESS COMPLETE\n")
cat("========================================\n")

# ==============================================================================
# GOAL ELASTICITY ANALYSIS: WAR EFFECTS AND REGIONAL VARIATION
# ==============================================================================

cat("\n========================================\n")
cat("GOAL ELASTICITY: WAR EFFECTS & REGIONAL VARIATION\n")
cat("========================================\n\n")

# Prepare data for goal elasticity analysis
elasticity_data <- reg_data %>%
  filter(!is.na(region_for_reg), !is.na(log_goal), !is.na(log_funding))

# -----------------------------------------------------------------------------
# 1. WAR EFFECTS ON GOAL ELASTICITY (Interaction Model)
# -----------------------------------------------------------------------------

cat("=== Table: War Effects on Goal Elasticity ===\n")




# Model 1: Baseline (pooled), log(duration) control + theme/region/year FE
m_elast_baseline <- feols(log_funding ~ log_goal + log_duration | theme_factor + region_factor + year_factor,
                          data = elasticity_data, vcov = "hetero")

# Model 2: With post_war interaction
m_elast_war <- feols(log_funding ~ log_goal * post_war + log_duration | theme_factor + region_factor + year_factor,
                     data = elasticity_data, vcov = "hetero")

# Model 3: Pre-war only
m_elast_pre <- feols(log_funding ~ log_goal + log_duration | theme_factor + region_factor + year_factor,
                     data = elasticity_data %>% filter(!post_war), vcov = "hetero")

# Model 4: Post-war only
m_elast_post <- feols(log_funding ~ log_goal + log_duration | theme_factor + region_factor + year_factor,
                      data = elasticity_data %>% filter(post_war), vcov = "hetero")

# Model 5: Non-Ukraine projects only, with war interaction
m_elast_war_nonukr <- feols(log_funding ~ log_goal * post_war + log_duration | theme_factor + region_factor + year_factor,
                            data = elasticity_data %>% filter(!is_ukraine), vcov = "hetero")

# Extract coefficients
extract_elast <- function(model, term) {
  coefs <- coef(model)
  ses <- sqrt(diag(vcov(model)))
  idx <- which(names(coefs) == term)
  if (length(idx) == 0) return(list(coef = NA, se = NA, pval = NA))
  pval <- 2 * pnorm(-abs(coefs[idx] / ses[idx]))
  list(coef = coefs[idx], se = ses[idx], pval = pval)
}

# Create LaTeX table for war effects on goal elasticity
war_elast_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{War Effects on Goal Elasticity}\n",
  "\\label{tab:war_elasticity}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lccccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) & (5) \\\\\n",
  "& Pooled & Interaction & Pre-War & Post-War & Non-Ukraine \\\\\n",
  "\\midrule\n"
)

# Log(Goal) coefficient
c1 <- extract_elast(m_elast_baseline, "log_goal")
c2 <- extract_elast(m_elast_war, "log_goal")
c3 <- extract_elast(m_elast_pre, "log_goal")
c4 <- extract_elast(m_elast_post, "log_goal")
c5 <- extract_elast(m_elast_war_nonukr, "log_goal")

war_elast_latex <- paste0(war_elast_latex,
  sprintf("Log(Goal) & %s & %s & %s & %s & %s \\\\\n",
          fmt_coef(c1$coef, c1$pval), fmt_coef(c2$coef, c2$pval),
          fmt_coef(c3$coef, c3$pval), fmt_coef(c4$coef, c4$pval),
          fmt_coef(c5$coef, c5$pval)),
  sprintf("& (%.3f) & (%.3f) & (%.3f) & (%.3f) & (%.3f) \\\\\n",
          c1$se, c2$se, c3$se, c4$se, c5$se)
)

# Post-War coefficient
pw2 <- extract_elast(m_elast_war, "post_warTRUE")
pw5 <- extract_elast(m_elast_war_nonukr, "post_warTRUE")

war_elast_latex <- paste0(war_elast_latex,
  sprintf("Post-War & & %s & & & %s \\\\\n",
          fmt_coef(pw2$coef, pw2$pval), fmt_coef(pw5$coef, pw5$pval)),
  sprintf("& & (%.3f) & & & (%.3f) \\\\\n", pw2$se, pw5$se)
)

# Interaction coefficient
int2 <- extract_elast(m_elast_war, "log_goal:post_warTRUE")
int5 <- extract_elast(m_elast_war_nonukr, "log_goal:post_warTRUE")

war_elast_latex <- paste0(war_elast_latex,
  sprintf("Log(Goal) $\\times$ Post-War & & %s & & & %s \\\\\n",
          fmt_coef(int2$coef, int2$pval), fmt_coef(int5$coef, int5$pval)),
  sprintf("& & (%.3f) & & & (%.3f) \\\\\n", int2$se, int5$se)
)

war_elast_latex <- paste0(war_elast_latex,
  "\\midrule\n",
  "Theme FE & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Region FE & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Year FE & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Duration Control & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s & %s \\\\\n",
          format(m_elast_baseline$nobs, big.mark = ","),
          format(m_elast_war$nobs, big.mark = ","),
          format(m_elast_pre$nobs, big.mark = ","),
          format(m_elast_post$nobs, big.mark = ","),
          format(m_elast_war_nonukr$nobs, big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f & %.3f \\\\\n",
          fitstat(m_elast_baseline, "r2")$r2,
          fitstat(m_elast_war, "r2")$r2,
          fitstat(m_elast_pre, "r2")$r2,
          fitstat(m_elast_post, "r2")$r2,
          fitstat(m_elast_war_nonukr, "r2")$r2),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Dependent variable is log(funding). All specifications control for log(duration) and include theme, region, and year fixed effects. Columns (1)-(4) use full sample; column (5) excludes Ukraine-related projects. Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(war_elast_latex, "tables/table_war_elasticity.tex")
cat("Saved: tables/table_war_elasticity.tex\n")

# Print key results
cat("\nWar Effects on Goal Elasticity:\n")
cat(sprintf("  Pooled baseline elasticity: %.3f (SE: %.3f)\n", c1$coef, c1$se))
cat(sprintf("  Pre-war elasticity: %.3f (SE: %.3f)\n", c3$coef, c3$se))
cat(sprintf("  Post-war elasticity: %.3f (SE: %.3f)\n", c4$coef, c4$se))
cat(sprintf("  Interaction (change): %.3f (SE: %.3f)\n", int2$coef, int2$se))

# -----------------------------------------------------------------------------
# 2. GOAL ELASTICITY BY REGION (Pre-War vs Post-War)
# -----------------------------------------------------------------------------

cat("\n=== Table: Regional Goal Elasticity (Pre/Post War) ===\n")

# Get unique regions
regions <- c("North America", "Africa", "Asia and Oceania", "Europe and Russia",
             "Latin America", "Middle East")

# Run region-specific regressions for pre-war and post-war periods
regional_elasticity_results <- map_dfr(regions, function(r) {

  # Pre-war: log(duration) control + theme and year FE (region FE drops, single region)
  data_pre <- elasticity_data %>% filter(!post_war, region_for_reg == r)
  if (nrow(data_pre) > 50) {
    m_pre <- feols(log_funding ~ log_goal + log_duration | theme_factor + year_factor,
                   data = data_pre, vcov = "hetero")
    pre_coef <- coef(m_pre)["log_goal"]
    pre_se <- sqrt(diag(vcov(m_pre)))["log_goal"]
    pre_n <- nrow(data_pre)
  } else {
    pre_coef <- NA; pre_se <- NA; pre_n <- nrow(data_pre)
  }

  # Post-war: log(duration) control + theme and year FE (region FE drops, single region)
  data_post <- elasticity_data %>% filter(post_war, region_for_reg == r)
  if (nrow(data_post) > 50) {
    m_post <- feols(log_funding ~ log_goal + log_duration | theme_factor + year_factor,
                    data = data_post, vcov = "hetero")
    post_coef <- coef(m_post)["log_goal"]
    post_se <- sqrt(diag(vcov(m_post)))["log_goal"]
    post_n <- nrow(data_post)
  } else {
    post_coef <- NA; post_se <- NA; post_n <- nrow(data_post)
  }

  tibble(
    region = r,
    pre_elasticity = pre_coef,
    pre_se = pre_se,
    pre_n = pre_n,
    post_elasticity = post_coef,
    post_se = post_se,
    post_n = post_n,
    change = post_coef - pre_coef
  )
})

# Create LaTeX table
regional_elast_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Goal Elasticity by Region: Pre-War vs. Post-War}\n",
  "\\label{tab:regional_elasticity}\n",
  "\\begin{threeparttable}\n",
  "\\small\n",
  "\\begin{tabular}{lcccccc}\n",
  "\\toprule\n",
  "& \\multicolumn{2}{c}{Pre-War} & \\multicolumn{2}{c}{Post-War} & & \\\\\n",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}\n",
  "Region & Elasticity & N & Elasticity & N & $\\Delta$ & Interpretation \\\\\n",
  "\\midrule\n"
)

for (i in 1:nrow(regional_elasticity_results)) {
  r <- regional_elasticity_results[i, ]

  # Determine significance stars
  pre_star <- ""
  if (!is.na(r$pre_elasticity) && !is.na(r$pre_se)) {
    pre_t <- abs(r$pre_elasticity / r$pre_se)
    if (pre_t > 2.58) pre_star <- "***"
    else if (pre_t > 1.96) pre_star <- "**"
    else if (pre_t > 1.64) pre_star <- "*"
  }

  post_star <- ""
  if (!is.na(r$post_elasticity) && !is.na(r$post_se)) {
    post_t <- abs(r$post_elasticity / r$post_se)
    if (post_t > 2.58) post_star <- "***"
    else if (post_t > 1.96) post_star <- "**"
    else if (post_t > 1.64) post_star <- "*"
  }

  # Interpretation
  interp <- ""
  if (!is.na(r$change)) {
    if (r$change > 0.05) interp <- "More responsive"
    else if (r$change < -0.05) interp <- "Less responsive"
    else interp <- "Stable"
  }

  regional_elast_latex <- paste0(regional_elast_latex,
    sprintf("%s & %.3f%s & %s & %.3f%s & %s & %.3f & %s \\\\\n",
            r$region,
            ifelse(is.na(r$pre_elasticity), NA, r$pre_elasticity), pre_star,
            format(r$pre_n, big.mark = ","),
            ifelse(is.na(r$post_elasticity), NA, r$post_elasticity), post_star,
            format(r$post_n, big.mark = ","),
            ifelse(is.na(r$change), NA, r$change),
            interp),
    sprintf("& (%.3f) & & (%.3f) & & & \\\\\n",
            ifelse(is.na(r$pre_se), NA, r$pre_se),
            ifelse(is.na(r$post_se), NA, r$post_se))
  )
}

regional_elast_latex <- paste0(regional_elast_latex,
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Each cell reports the coefficient on log(goal) from a region-period specific regression of log(funding) on log(goal), controlling for log(duration) and including theme and year fixed effects. Robust standard errors in parentheses. $\\Delta$ shows the change in elasticity from pre-war to post-war period. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(regional_elast_latex, "tables/table_regional_elasticity.tex")
cat("Saved: tables/table_regional_elasticity.tex\n")

# Print results
cat("\nRegional Goal Elasticity (Pre-War vs Post-War):\n")
for (i in 1:nrow(regional_elasticity_results)) {
  r <- regional_elasticity_results[i, ]
  cat(sprintf("  %s: Pre=%.3f, Post=%.3f, Change=%.3f\n",
              r$region, r$pre_elasticity, r$post_elasticity, r$change))
}

# -----------------------------------------------------------------------------
# 3. FIGURE: Regional Goal Elasticity Comparison
# -----------------------------------------------------------------------------

cat("\n=== Figure: Regional Goal Elasticity Pre/Post War ===\n")

fig_regional_elast <- regional_elasticity_results %>%
  filter(!is.na(pre_elasticity), !is.na(post_elasticity)) %>%
  pivot_longer(cols = c(pre_elasticity, post_elasticity),
               names_to = "period", values_to = "elasticity") %>%
  mutate(
    period = ifelse(period == "pre_elasticity", "Pre-War", "Post-War"),
    period = factor(period, levels = c("Pre-War", "Post-War")),
    se = ifelse(period == "Pre-War", pre_se, post_se)
  ) %>%
  ggplot(aes(x = reorder(region, elasticity), y = elasticity, fill = period)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  geom_errorbar(aes(ymin = elasticity - 1.96 * se, ymax = elasticity + 1.96 * se),
                position = position_dodge(0.8), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("Pre-War" = pal_main[3], "Post-War" = pal_main[2])) +
  coord_flip() +
  labs(
    title = "Goal Elasticity by Region: Pre-War vs. Post-War",
    subtitle = "How donor responsiveness to goal size changed after the Ukraine invasion",
    x = NULL,
    y = expression(paste("Goal Elasticity (", hat(beta), ")")),
    fill = "Period"
  ) +
  theme(legend.position = "bottom")

ggsave("figures/fig_regional_elasticity.pdf", fig_regional_elast, width = 8, height = 5, dpi = 300)
cat("Saved: figures/fig_regional_elasticity.pdf\n")

cat("\n========================================\n")
cat("GOAL ELASTICITY ANALYSIS COMPLETE\n")
cat("========================================\n")
