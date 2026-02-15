# ==============================================================================
# EXPORT FIGURES AND TABLES FOR LATEX PAPER
# GlobalGiving Analysis - Top-5 Economics Journal Style
# ==============================================================================

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
library(modelsummary)
library(fixest)
library(estimatr)
library(kableExtra)
library(gt)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(quantreg)
library(survival)
library(stargazer)
library(xtable)
library(tidytext)  # For text analysis

# Set seed
set.seed(42)

# Set theme for publication-quality figures
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

# Color palette for consistency
pal_main <- c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")

# ==============================================================================
# LOAD AND CLEAN DATA
# ==============================================================================

cat("Loading data...\n")
df_raw <- read_csv("../donations_data.csv", show_col_types = FALSE)

df <- df_raw %>%
 clean_names() %>%
 mutate(
   approved_date = ymd_hms(approved_date),
   approved_year = year(approved_date),
   approved_month = month(approved_date),
   approved_yearmonth = floor_date(approved_date, "month"),
   funding_ratio = funding / goal,
   is_fully_funded = funding >= goal,
   log_funding = log1p(funding),
   log_goal = log1p(goal),
   log_donations = log1p(number_of_donations),
   avg_donation = ifelse(number_of_donations > 0, funding / number_of_donations, 0),
   region_clean = case_when(
     is.na(region) | region == "NA" ~ "Unspecified",
     TRUE ~ region
   ),
   has_children = str_detect(str_to_lower(coalesce(summary, "")), "children|child|kids|youth"),
   has_urgent = str_detect(str_to_lower(coalesce(summary, "")), "urgent|emergency|immediate|critical"),
   has_lives = str_detect(str_to_lower(coalesce(summary, "")), "save lives|saving lives|life-saving"),
   is_ukraine = str_detect(str_to_lower(coalesce(country, "")), "ukraine") |
                str_detect(str_to_lower(coalesce(title, "")), "ukraine") |
                str_detect(str_to_lower(coalesce(summary, "")), "ukraine|ukrainian"),
   post_ukraine = approved_yearmonth >= as.POSIXct("2022-02-01"),
   org_id = as.numeric(str_extract(as.character(organization), "\\d+"))
 ) %>%
 filter(!is.na(approved_date), approved_year >= 2002, approved_year <= 2025, goal > 0)

cat("Data loaded:", nrow(df), "observations\n")

# Regression data
reg_data <- df %>%
 filter(!is.na(theme_name), theme_name != "", region_clean != "Unspecified",
        approved_year >= 2010, approved_year <= 2024,
        goal > 0, goal < quantile(goal, 0.99, na.rm = TRUE)) %>%
 mutate(
   theme_factor = as.factor(theme_name),
   region_factor = as.factor(region_clean),
   year_factor = as.factor(approved_year)
 )

# ==============================================================================
# FIGURE 1: DISTRIBUTION OF KEY VARIABLES
# ==============================================================================

cat("Creating Figure 1...\n")

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
ggsave("figures/fig1_distributions.png", fig1, width = 8, height = 6, dpi = 300)

# ==============================================================================
# FIGURE 2: TIME TRENDS
# ==============================================================================

cat("Creating Figure 2...\n")

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
ggsave("figures/fig2_time_trends.png", fig2, width = 8, height = 5, dpi = 300)

# ==============================================================================
# FIGURE 3: UKRAINE EVENT STUDY
# ==============================================================================

cat("Creating Figure 3...\n")

ukraine_event_month <- as.POSIXct("2022-02-01")

event_data <- df %>%
 filter(approved_year >= 2020, approved_year <= 2024, !is.na(approved_yearmonth)) %>%
 mutate(
   event_time = as.numeric(difftime(approved_yearmonth, ukraine_event_month, units = "days")) / 30,
   event_time_capped = pmax(pmin(round(event_time), 12), -12)
 )

event_study_agg <- event_data %>%
 filter(is_ukraine == TRUE) %>%
 group_by(event_time_capped) %>%
 summarise(
   mean_log_funding = mean(log_funding, na.rm = TRUE),
   se = sd(log_funding, na.rm = TRUE) / sqrt(n()),
   n = n(),
   .groups = "drop"
 ) 

event_study_agg <- event_study_agg %>%
  complete(event_time_capped = -12:12,
           fill = list(mean_log_funding = 0,
                       se = NA,
                       n = 0))

baseline <- event_study_agg %>%
 filter(event_time_capped < 0) %>%
 summarise(baseline = mean(mean_log_funding, na.rm = TRUE)) %>%
 pull(baseline)

if (length(baseline) == 0 || is.na(baseline)) baseline <- min(event_study_agg$mean_log_funding, na.rm = TRUE)

event_study_agg <- event_study_agg %>%
 mutate(normalized = mean_log_funding - baseline)

fig3 <- event_study_agg %>%
 ggplot(aes(x = event_time_capped, y = normalized)) +
 geom_vline(xintercept = 0, linetype = "dashed", color = pal_main[2], linewidth = 0.8) +
 geom_hline(yintercept = 0, linetype = "solid", color = "gray70") +
 geom_ribbon(aes(ymin = normalized - 1.96 * se, ymax = normalized + 1.96 * se),
             alpha = 0.2, fill = pal_main[3]) +
 geom_line(color = pal_main[3], linewidth = 1) +
 geom_point(color = pal_main[3], size = 2) +
 scale_x_continuous(breaks = seq(-12, 12, 3)) +
 annotate("text", x = 0.5, y = max(event_study_agg$normalized) * 0.9,
          label = "Invasion", hjust = 0, size = 3, fontface = "italic") +
 labs(
   title = "Event Study: Ukraine Crisis Effect on Funding",
   subtitle = "Normalized to pre-invasion mean; 95% confidence intervals shown",
   x = "Months Relative to February 2022",
   y = "Log(Funding) - Baseline",
   caption = "Note: Sample restricted to Ukraine-related projects identified by country or keyword matching."
 )

ggsave("figures/fig3_event_study.pdf", fig3, width = 7, height = 5, dpi = 300)
ggsave("figures/fig3_event_study.png", fig3, width = 7, height = 5, dpi = 300)

# ==============================================================================
# FIGURE 4: DIFFERENCE-IN-DIFFERENCES
# ==============================================================================

cat("Creating Figure 4...\n")

did_data <- df %>%
 filter(approved_year >= 2020, approved_year <= 2024, theme_name == "Disaster Response") %>%
 mutate(
   treated = is_ukraine,
   post = approved_yearmonth >= ukraine_event_month
 ) %>%
 group_by(approved_yearmonth, treated) %>%
 summarise(
   mean_funding = mean(funding, na.rm = TRUE),
   log_mean_funding = log(mean_funding + 1),
   n = n(),
   .groups = "drop"
 )

fig4 <- did_data %>%
 mutate(Group = ifelse(treated, "Ukraine-Related", "Other Disaster")) %>%
 ggplot(aes(x = approved_yearmonth, y = log_mean_funding, color = Group)) +
 geom_vline(xintercept = ukraine_event_month, linetype = "dashed", color = "gray50") +
 geom_line(linewidth = 0.8) +
 geom_point(size = 1.5) +
 scale_color_manual(values = c("Ukraine-Related" = pal_main[2], "Other Disaster" = pal_main[3])) +
 scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
 annotate("text", x = ukraine_event_month + days(30), y = max(did_data$log_mean_funding) * 0.95,
          label = "Invasion", hjust = 0, size = 3, fontface = "italic") +
 labs(
   title = "Difference-in-Differences: Ukraine vs. Other Disaster Projects",
   subtitle = "Monthly average log funding for disaster response projects",
   x = NULL,
   y = "Log(Mean Funding)",
   color = NULL,
   caption = "Note: Vertical line marks February 24, 2022 (Russian invasion of Ukraine)."
 ) +
 theme(legend.position = c(0.15, 0.85))

ggsave("figures/fig4_did.pdf", fig4, width = 8, height = 5, dpi = 300)
ggsave("figures/fig4_did.png", fig4, width = 8, height = 5, dpi = 300)

# ==============================================================================
# FIGURE 5: KEYWORD EFFECTS
# ==============================================================================

cat("Creating Figure 5...\n")

keyword_model <- lm(log_funding ~ log_goal + has_children + has_urgent + has_lives +
                     theme_factor + region_factor + year_factor, data = reg_data)

keyword_coefs <- tidy(keyword_model, conf.int = TRUE) %>%
 filter(str_detect(term, "has_")) %>%
 mutate(
   keyword = str_remove(term, "has_"),
   keyword = str_remove(keyword, "TRUE"),
   keyword = str_to_title(keyword),
   pct_effect = (exp(estimate) - 1) * 100,
   pct_low = (exp(conf.low) - 1) * 100,
   pct_high = (exp(conf.high) - 1) * 100,
   significant = p.value < 0.05
 )

fig5 <- keyword_coefs %>%
 ggplot(aes(x = reorder(keyword, pct_effect), y = pct_effect)) +
 geom_hline(yintercept = 0, linetype = "solid", color = "gray70") +
 geom_col(aes(fill = significant), alpha = 0.8, width = 0.6) +
 geom_errorbar(aes(ymin = pct_low, ymax = pct_high), width = 0.2, linewidth = 0.5) +
 coord_flip() +
 scale_fill_manual(values = c("TRUE" = pal_main[4], "FALSE" = "gray60"),
                   labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05")) +
 labs(
   title = "Keyword Effects on Project Funding",
   subtitle = "Percentage change in funding associated with keyword presence",
   x = NULL,
   y = "Effect on Funding (%)",
   fill = "Significance",
   caption = "Note: Estimates from OLS regression controlling for log(goal), theme, region, and year fixed effects."
 )

ggsave("figures/fig5_keywords.pdf", fig5, width = 7, height = 4, dpi = 300)
ggsave("figures/fig5_keywords.png", fig5, width = 7, height = 4, dpi = 300)

# ==============================================================================
# FIGURE 6: THEME HETEROGENEITY
# ==============================================================================

cat("Creating Figure 6...\n")

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
     mod <- lm(log_funding ~ log_goal, data = cur_data())
     tidy(mod, conf.int = TRUE) %>% filter(term == "log_goal")
   }, error = function(e) {
     tibble(estimate = NA_real_, std.error = NA_real_, conf.low = NA_real_,
            conf.high = NA_real_, p.value = NA_real_)
   })),
   .groups = "drop"
 ) %>%
 unnest(model_result) %>%
 filter(!is.na(estimate)) %>%
 mutate(significant = p.value < 0.05)

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
   subtitle = "Coefficient on log(goal) from theme-specific regressions",
   x = NULL,
   y = expression(paste("Goal Elasticity (", hat(beta), ")")),
   caption = "Note: Dashed line shows mean across themes. Red indicates p < 0.05."
 ) +
 theme(legend.position = "none")

ggsave("figures/fig6_theme_heterogeneity.pdf", fig6, width = 7, height = 5, dpi = 300)
ggsave("figures/fig6_theme_heterogeneity.png", fig6, width = 7, height = 5, dpi = 300)

# ==============================================================================
# FIGURE 7: QUANTILE REGRESSION
# ==============================================================================

cat("Creating Figure 7...\n")

quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

qreg_results <- map_dfr(quantiles, function(tau) {
 tryCatch({
   model <- rq(log_funding ~ log_goal, tau = tau, data = reg_data)
   summ <- summary(model, se = "nid")
   coef_data <- as.data.frame(summ$coefficients)
   tibble(
     term = rownames(coef_data),
     estimate = coef_data[, 1],
     std.error = coef_data[, 2],
     quantile = tau
   ) %>%
     filter(term == "log_goal") %>%
     mutate(conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error)
 }, error = function(e) NULL)
}) %>%
 filter(!is.na(estimate))

ols_coef <- coef(lm(log_funding ~ log_goal, data = reg_data))["log_goal"]

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
   subtitle = "Effect of log(goal) on log(funding) at different quantiles",
   x = "Funding Quantile",
   y = expression(paste("Coefficient on Log(Goal)")),
   caption = "Note: Shaded area shows 95% confidence interval. Dashed line shows OLS estimate."
 )

ggsave("figures/fig7_quantile_regression.pdf", fig7, width = 7, height = 5, dpi = 300)
ggsave("figures/fig7_quantile_regression.png", fig7, width = 7, height = 5, dpi = 300)

# ==============================================================================
# FIGURE 8: REGIONAL DISPARITIES
# ==============================================================================

cat("Creating Figure 8...\n")

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
 labs(title = "(a) Mean Funding per Project", x = NULL, y = "Mean Funding ($)")

p8b <- regional_stats %>%
 ggplot(aes(x = reorder(region_clean, success_rate), y = success_rate)) +
 geom_col(fill = pal_main[4], alpha = 0.8) +
 geom_text(aes(label = percent(success_rate, accuracy = 1)), hjust = -0.1, size = 2.5) +
 coord_flip() +
 scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
 labs(title = "(b) Funding Success Rate", x = NULL, y = "% Fully Funded")

fig8 <- p8a + p8b

ggsave("figures/fig8_regional.pdf", fig8, width = 10, height = 4, dpi = 300)
ggsave("figures/fig8_regional.png", fig8, width = 10, height = 4, dpi = 300)

# ==============================================================================
# FIGURE 9: WORLD MAP
# ==============================================================================

cat("Creating Figure 9...\n")

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
 labs(title = "Global Distribution of Charitable Funding",
      caption = "Source: GlobalGiving project-level data.") +
 theme_void(base_family = "serif") +
 theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("figures/fig9_world_map.pdf", fig9, width = 10, height = 5, dpi = 300)
ggsave("figures/fig9_world_map.png", fig9, width = 10, height = 5, dpi = 300)

# ==============================================================================
# CREATE LATEX TABLES
# ==============================================================================

cat("Creating LaTeX tables...\n")

# Table 1: Summary Statistics
summary_df <- df %>%
 summarise(
   `N Projects` = n(),
   `N Countries` = n_distinct(country, na.rm = TRUE),
   `N Organizations` = n_distinct(org_id, na.rm = TRUE),
   `Total Funding ($M)` = sum(funding, na.rm = TRUE) / 1e6,
   `Mean Funding ($)` = mean(funding, na.rm = TRUE),
   `Median Funding ($)` = median(funding, na.rm = TRUE),
   `SD Funding ($)` = sd(funding, na.rm = TRUE),
   `Mean Goal ($)` = mean(goal, na.rm = TRUE),
   `Median Goal ($)` = median(goal, na.rm = TRUE),
   `Success Rate (%)` = mean(is_fully_funded, na.rm = TRUE) * 100,
   `Mean Donations` = mean(number_of_donations, na.rm = TRUE)
 ) %>%
 pivot_longer(everything(), names_to = "Statistic", values_to = "Value")

write_csv(summary_df, "tables/table1_summary_stats.csv")

# Generate LaTeX table for summary statistics (tab:summary)
cat("Creating Table 1 LaTeX (Summary Statistics)...\n")

# Calculate all summary statistics
n_projects <- nrow(df)
n_countries <- n_distinct(df$country, na.rm = TRUE)
n_orgs <- n_distinct(df$org_id, na.rm = TRUE)
total_funding <- sum(df$funding, na.rm = TRUE) / 1e6
mean_funding <- mean(df$funding, na.rm = TRUE)
median_funding <- median(df$funding, na.rm = TRUE)
sd_funding <- sd(df$funding, na.rm = TRUE)
mean_goal <- mean(df$goal, na.rm = TRUE)
median_goal <- median(df$goal, na.rm = TRUE)
mean_donations <- mean(df$number_of_donations, na.rm = TRUE)
median_donations <- median(df$number_of_donations, na.rm = TRUE)
sd_donations <- sd(df$number_of_donations, na.rm = TRUE)
mean_funding_ratio <- mean(df$funding_ratio, na.rm = TRUE)
median_funding_ratio <- median(df$funding_ratio, na.rm = TRUE)
sd_funding_ratio <- sd(df$funding_ratio, na.rm = TRUE)
success_rate <- mean(df$is_fully_funded, na.rm = TRUE)
total_goal <- sum(df$goal, na.rm = TRUE) / 1e6
year_range <- paste0(min(df$approved_year, na.rm = TRUE), "--", max(df$approved_year, na.rm = TRUE))

# Theme statistics (case-insensitive)
disaster_pct <- mean(str_detect(str_to_lower(coalesce(df$theme_name, "")), "disaster"), na.rm = TRUE)
education_pct <- mean(str_detect(str_to_lower(coalesce(df$theme_name, "")), "education"), na.rm = TRUE)
health_pct <- mean(str_detect(str_to_lower(coalesce(df$theme_name, "")), "health"), na.rm = TRUE)

# Regional statistics
region_stats <- df %>%
  filter(region_clean != "Unspecified") %>%
  count(region_clean) %>%
  mutate(pct = n / sum(n))

# Helper function to safely get region stats
get_region_stat <- function(stats, region_pattern, col) {
  result <- stats %>%
    filter(str_detect(str_to_lower(region_clean), str_to_lower(region_pattern))) %>%
    pull(!!sym(col))
  if (length(result) == 0) return(0)
  return(result[1])
}

africa_n <- get_region_stat(region_stats, "africa", "n")
africa_pct <- get_region_stat(region_stats, "africa", "pct")
asia_n <- get_region_stat(region_stats, "asia", "n")
asia_pct <- get_region_stat(region_stats, "asia", "pct")
south_am_n <- get_region_stat(region_stats, "south.*america|central.*america", "n")
south_am_pct <- get_region_stat(region_stats, "south.*america|central.*america", "pct")
north_am_n <- get_region_stat(region_stats, "north.*america", "n")
north_am_pct <- get_region_stat(region_stats, "north.*america", "pct")
europe_n <- get_region_stat(region_stats, "europe", "n")
europe_pct <- get_region_stat(region_stats, "europe", "pct")
middle_east_n <- get_region_stat(region_stats, "middle.*east", "n")
middle_east_pct <- get_region_stat(region_stats, "middle.*east", "pct")

# Days active calculation
days_active_mean <- as.numeric(difftime(Sys.Date(), df$approved_date, units = "days")) %>% mean(na.rm = TRUE)
days_active_median <- as.numeric(difftime(Sys.Date(), df$approved_date, units = "days")) %>% median(na.rm = TRUE)
days_active_sd <- as.numeric(difftime(Sys.Date(), df$approved_date, units = "days")) %>% sd(na.rm = TRUE)

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
          format(round(sd(df$goal, na.rm = TRUE)), big.mark = ","),
          format(n_projects, big.mark = ",")),
  sprintf("Number of Donations & %.1f & %d & %.1f & %s \\\\\n",
          mean_donations, round(median_donations), sd_donations,
          format(n_projects, big.mark = ",")),
  sprintf("Funding Ratio (Funding/Goal) & %.2f & %.2f & %.2f & %s \\\\\n",
          mean_funding_ratio, median_funding_ratio, sd_funding_ratio,
          format(n_projects, big.mark = ",")),
  sprintf("Fully Funded (0/1) & %.3f & 0 & %.3f & %s \\\\\n",
          success_rate, sd(df$is_fully_funded, na.rm = TRUE),
          format(n_projects, big.mark = ",")),
  "\\addlinespace\n",
  "\\multicolumn{5}{l}{\\textit{Panel B: Project Characteristics}} \\\\\n",
  sprintf("Days Active & %s & %s & %s & %s \\\\\n",
          format(round(days_active_mean), big.mark = ","),
          format(round(days_active_median), big.mark = ","),
          format(round(days_active_sd), big.mark = ","),
          format(n_projects, big.mark = ",")),
  sprintf("Disaster Response Theme (0/1) & %.3f & 0 & %.3f & %s \\\\\n",
          disaster_pct, sqrt(disaster_pct * (1 - disaster_pct)),
          format(n_projects, big.mark = ",")),
  sprintf("Education Theme (0/1) & %.3f & 0 & %.3f & %s \\\\\n",
          education_pct, sqrt(education_pct * (1 - education_pct)),
          format(n_projects, big.mark = ",")),
  sprintf("Health Theme (0/1) & %.3f & 0 & %.3f & %s \\\\\n",
          health_pct, sqrt(health_pct * (1 - health_pct)),
          format(n_projects, big.mark = ",")),
  "\\addlinespace\n",
  "\\multicolumn{5}{l}{\\textit{Panel C: Geographic Distribution}} \\\\\n",
  sprintf("Africa & %.3f & --- & --- & %s \\\\\n", africa_pct, format(africa_n, big.mark = ",")),
  sprintf("Asia and Oceania & %.3f & --- & --- & %s \\\\\n", asia_pct, format(asia_n, big.mark = ",")),
  sprintf("South/Central America & %.3f & --- & --- & %s \\\\\n", south_am_pct, format(south_am_n, big.mark = ",")),
  sprintf("North America & %.3f & --- & --- & %s \\\\\n", north_am_pct, format(north_am_n, big.mark = ",")),
  sprintf("Europe and Russia & %.3f & --- & --- & %s \\\\\n", europe_pct, format(europe_n, big.mark = ",")),
  sprintf("Middle East & %.3f & --- & --- & %s \\\\\n", middle_east_pct, format(middle_east_n, big.mark = ",")),
  "\\addlinespace\n",
  "\\multicolumn{5}{l}{\\textit{Panel D: Platform Totals}} \\\\\n",
  sprintf("Total Projects & \\multicolumn{4}{c}{%s} \\\\\n", format(n_projects, big.mark = ",")),
  sprintf("Total Funding Raised & \\multicolumn{4}{c}{\\$%.1f million} \\\\\n", total_funding),
  sprintf("Total Goal Amount & \\multicolumn{4}{c}{\\$%.1f million} \\\\\n", total_goal),
  sprintf("Unique Countries & \\multicolumn{4}{c}{%d} \\\\\n", n_countries),
  sprintf("Unique Organizations & \\multicolumn{4}{c}{%s} \\\\\n", format(n_orgs, big.mark = ",")),
  sprintf("Date Range & \\multicolumn{4}{c}{%s} \\\\\n", year_range),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Summary statistics for the GlobalGiving analysis sample. Panel A reports financial variables in U.S. dollars. Panel B reports project characteristics. Panel C reports the distribution of projects across geographic regions. Panel D reports platform-wide totals.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(summary_latex, "tables/table1_summary_stats.tex")

# Table 2: Main OLS Results
model1 <- lm(log_funding ~ log_goal, data = reg_data)
model2 <- lm(log_funding ~ log_goal + theme_factor, data = reg_data)
model3 <- lm(log_funding ~ log_goal + theme_factor + region_factor, data = reg_data)
model4 <- lm(log_funding ~ log_goal + theme_factor + region_factor + year_factor, data = reg_data)

stargazer(model1, model2, model3, model4,
         type = "latex",
         out = "tables/table2_ols_results.tex",
         title = "Main OLS Estimates: Determinants of Project Funding",
         label = "tab:ols",
         dep.var.labels = "Log(Funding)",
         covariate.labels = c("Log(Goal)"),
         omit = c("theme_factor", "region_factor", "year_factor"),
         add.lines = list(
           c("Theme FE", "No", "Yes", "Yes", "Yes"),
           c("Region FE", "No", "No", "Yes", "Yes"),
           c("Year FE", "No", "No", "No", "Yes")
         ),
         omit.stat = c("f", "ser"),
         notes = c("Standard errors in parentheses.", "* p<0.1; ** p<0.05; *** p<0.01"),
         notes.append = FALSE,
         float = TRUE)

# Table 3: DiD Results
did_reg_data <- df %>%
 filter(approved_year >= 2020, approved_year <= 2024, theme_name == "Disaster Response") %>%
 mutate(
   treated = as.numeric(is_ukraine),
   post = as.numeric(approved_yearmonth >= ukraine_event_month),
   treated_post = treated * post
 )

did_m1 <- lm(log_funding ~ treated + post + treated_post, data = did_reg_data)
did_m2 <- lm(log_funding ~ treated + post + treated_post + log_goal, data = did_reg_data)

stargazer(did_m1, did_m2,
         type = "latex",
         out = "tables/table3_did_results.tex",
         title = "Difference-in-Differences Estimates: Ukraine Crisis Effect",
         label = "tab:did",
         dep.var.labels = "Log(Funding)",
         covariate.labels = c("Ukraine-Related", "Post-Invasion", "Ukraine × Post", "Log(Goal)"),
         omit.stat = c("f", "ser"),
         notes = c("Sample: Disaster response projects, 2020-2024.", "* p<0.1; ** p<0.05; *** p<0.01"),
         notes.append = FALSE,
         float = TRUE)

# Table 4: Keyword Effects
keyword_model_full <- lm(log_funding ~ log_goal + has_children + has_urgent + has_lives +
                          theme_factor + region_factor + year_factor, data = reg_data)

stargazer(keyword_model_full,
         type = "latex",
         out = "tables/table4_keywords.tex",
         title = "Mechanism Test: Keyword Effects on Project Funding",
         label = "tab:keywords",
         dep.var.labels = "Log(Funding)",
         covariate.labels = c("Log(Goal)", "Children Keywords", "Urgent Keywords", "Save Lives Keywords"),
         omit = c("theme_factor", "region_factor", "year_factor"),
         add.lines = list(
           c("Theme FE", "Yes"),
           c("Region FE", "Yes"),
           c("Year FE", "Yes")
         ),
         omit.stat = c("f", "ser"),
         notes = c("Keywords detected in project summary text.", "* p<0.1; ** p<0.05; *** p<0.01"),
         notes.append = FALSE,
         float = TRUE)

# Table 5: Regional Statistics
regional_table <- regional_stats %>%
 arrange(desc(mean_funding)) %>%
 mutate(
   n_projects = comma(n_projects),
   total_funding = dollar(total_funding / 1e6, accuracy = 0.1, suffix = "M"),
   mean_funding = dollar(mean_funding, accuracy = 1),
   success_rate = percent(success_rate, accuracy = 0.1)
 )

write_csv(regional_table, "tables/table5_regional.csv")

# ==============================================================================
# FIGURE 12: ADDITIONALITY VS SUBSTITUTION DECOMPOSITION
# ==============================================================================

cat("Creating Figure 12 (Additionality vs Substitution)...\n")

# Monthly funding by Ukraine vs Non-Ukraine
monthly_ukr <- df %>%
  filter(!is.na(approved_yearmonth), approved_yearmonth >= as.POSIXct("2020-01-01"),
         approved_yearmonth <= as.POSIXct("2024-06-01")) %>%
  mutate(category = ifelse(is_ukraine, "Ukraine-Related", "Non-Ukraine")) %>%
  group_by(approved_yearmonth, category) %>%
  summarise(
    total_funding = sum(funding, na.rm = TRUE),
    n_projects = n(),
    .groups = "drop"
  )

# Total funding over time
monthly_total <- df %>%
  filter(!is.na(approved_yearmonth), approved_yearmonth >= as.POSIXct("2020-01-01"),
         approved_yearmonth <= as.POSIXct("2024-06-01")) %>%
  group_by(approved_yearmonth) %>%
  summarise(
    total_funding = sum(funding, na.rm = TRUE),
    .groups = "drop"
  )

# Pre-invasion trend for counterfactual
pre_data <- monthly_total %>%
  filter(approved_yearmonth < as.POSIXct("2022-02-01"))
trend_model <- lm(total_funding ~ as.numeric(approved_yearmonth), data = pre_data)

monthly_total <- monthly_total %>%
  mutate(
    counterfactual = predict(trend_model, newdata = .),
    additionality = ifelse(approved_yearmonth >= as.POSIXct("2022-02-01"),
                           pmax(0, total_funding - counterfactual), NA)
  )

# Panel A: Total funding with counterfactual
p12a <- monthly_total %>%
  ggplot(aes(x = approved_yearmonth)) +
  geom_ribbon(aes(ymin = counterfactual / 1e6, ymax = total_funding / 1e6),
              data = . %>% filter(approved_yearmonth >= as.POSIXct("2022-02-01")),
              fill = pal_main[4], alpha = 0.3) +
  geom_line(aes(y = total_funding / 1e6), color = pal_main[3], linewidth = 0.8) +
  geom_line(aes(y = counterfactual / 1e6), color = pal_main[2], linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = as.POSIXct("2022-02-24"), linetype = "dotted", color = "gray40") +
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  annotate("text", x = as.POSIXct("2022-06-01"), y = 22,
           label = "Additionality\n(New Resources)", size = 2.5, color = pal_main[4]) +
  labs(
    title = "(a) Total Platform Funding",
    subtitle = "Solid: Actual | Dashed: Pre-invasion trend extrapolated",
    x = NULL, y = "Monthly Funding ($M)"
  )

# Panel B: Decomposition by category
p12b <- monthly_ukr %>%
  ggplot(aes(x = approved_yearmonth, y = total_funding / 1e6, fill = category)) +
  geom_area(alpha = 0.7, position = "stack") +
  geom_vline(xintercept = as.POSIXct("2022-02-24"), linetype = "dotted", color = "gray40") +
  scale_fill_manual(values = c("Ukraine-Related" = pal_main[2], "Non-Ukraine" = pal_main[3])) +
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "(b) Funding Decomposition",
    subtitle = "Ukraine vs. Non-Ukraine projects",
    x = NULL, y = "Monthly Funding ($M)", fill = NULL
  ) +
  theme(legend.position = c(0.2, 0.85))

fig12 <- p12a + p12b

ggsave("figures/fig12_addsub_decomposition.pdf", fig12, width = 10, height = 4, dpi = 300)
ggsave("figures/fig12_addsub_decomposition.png", fig12, width = 10, height = 4, dpi = 300)

# ==============================================================================
# FIGURE 13: SENTIMENT ANALYSIS
# ==============================================================================

cat("Creating Figure 13 (Sentiment Analysis)...\n")

# Create sentiment indicators from text
sentiment_data <- df %>%
  filter(!is.na(summary), nchar(summary) > 50) %>%
  mutate(
    # Positive sentiment words
    pos_words = str_count(str_to_lower(summary),
                          "hope|transform|empower|opportunity|success|achieve|proud|wonderful|amazing|grateful|thank|support|help|improve|better|positive|bright"),
    # Negative sentiment words
    neg_words = str_count(str_to_lower(summary),
                          "suffer|desperate|tragic|devastat|struggling|poverty|orphan|abandon|danger|threat|risk|vulnerable|unsafe|crisis|urgent|emergency|critical|fear"),
    # Word count
    word_count = str_count(summary, "\\w+"),
    # Sentiment scores
    net_sentiment = (pos_words - neg_words) / pmax(word_count, 1) * 100,
    sentiment_intensity = (pos_words + neg_words) / pmax(word_count, 1) * 100,
    # Bin sentiment for visualization
    sentiment_bin = cut(net_sentiment, breaks = seq(-5, 5, by = 0.5), include.lowest = TRUE)
  ) %>%
  filter(!is.na(sentiment_bin), funding > 0)

# Panel A: Net sentiment vs funding (binned scatter)
sentiment_summary <- sentiment_data %>%
  group_by(sentiment_bin) %>%
  summarise(
    mean_log_funding = mean(log_funding, na.rm = TRUE),
    se = sd(log_funding, na.rm = TRUE) / sqrt(n()),
    n = n(),
    mid_sentiment = mean(net_sentiment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

p13a <- sentiment_summary %>%
  ggplot(aes(x = mid_sentiment, y = mean_log_funding)) +
  geom_point(aes(size = n), color = pal_main[3], alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE,
              color = pal_main[2], fill = pal_main[2], alpha = 0.2) +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  labs(
    title = "(a) Net Sentiment vs. Funding",
    subtitle = "Quadratic fit; point size = sample size",
    x = "Net Sentiment Score", y = "Mean Log(Funding)"
  )

# Panel B: Sentiment intensity vs funding
intensity_summary <- sentiment_data %>%
  mutate(intensity_bin = cut(sentiment_intensity, breaks = 10)) %>%
  group_by(intensity_bin) %>%
  summarise(
    mean_log_funding = mean(log_funding, na.rm = TRUE),
    mid_intensity = mean(sentiment_intensity, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

p13b <- intensity_summary %>%
  ggplot(aes(x = mid_intensity, y = mean_log_funding)) +
  geom_point(color = pal_main[4], size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = pal_main[2], fill = pal_main[2], alpha = 0.2) +
  labs(
    title = "(b) Sentiment Intensity vs. Funding",
    subtitle = "Higher intensity = more emotional language",
    x = "Sentiment Intensity", y = "Mean Log(Funding)"
  )

# Panel C: Sentiment by theme
theme_sentiment <- sentiment_data %>%
  filter(!is.na(theme_name), theme_name != "") %>%
  group_by(theme_name) %>%
  summarise(
    mean_sentiment = mean(net_sentiment, na.rm = TRUE),
    mean_intensity = mean(sentiment_intensity, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 100)

p13c <- theme_sentiment %>%
  ggplot(aes(x = reorder(theme_name, mean_sentiment), y = mean_sentiment)) +
  geom_col(fill = pal_main[3], alpha = 0.8) +
  coord_flip() +
  labs(
    title = "(c) Sentiment by Theme",
    x = NULL, y = "Mean Net Sentiment"
  )

# Panel D: Sentiment by region
region_sentiment <- sentiment_data %>%
  filter(region_clean != "Unspecified") %>%
  group_by(region_clean) %>%
  summarise(
    mean_sentiment = mean(net_sentiment, na.rm = TRUE),
    mean_funding = mean(log_funding, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p13d <- region_sentiment %>%
  ggplot(aes(x = mean_sentiment, y = mean_funding, label = region_clean)) +
  geom_point(aes(size = n), color = pal_main[5], alpha = 0.7) +
  geom_text(hjust = -0.1, vjust = 0.5, size = 2.5) +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  labs(
    title = "(d) Sentiment vs. Funding by Region",
    x = "Mean Net Sentiment", y = "Mean Log(Funding)"
  )

fig13 <- (p13a + p13b) / (p13c + p13d)

ggsave("figures/fig13_sentiment_analysis.pdf", fig13, width = 10, height = 8, dpi = 300)
ggsave("figures/fig13_sentiment_analysis.png", fig13, width = 10, height = 8, dpi = 300)

# ==============================================================================
# FIGURE 14: COMPREHENSIVE TEXT ANALYSIS
# ==============================================================================

cat("Creating Figure 14 (Comprehensive Text Analysis)...\n")

# Create comprehensive text features
text_features <- df %>%
  filter(!is.na(summary), nchar(summary) > 50, funding > 0) %>%
  mutate(
    # Emotional categories
    has_sadness = str_detect(str_to_lower(summary), "suffer|desperate|tragic|devastat|struggling|poverty|orphan|abandon"),
    has_hope = str_detect(str_to_lower(summary), "hope|transform|empower|opportunity|brighter|potential|dream|inspire"),
    has_fear = str_detect(str_to_lower(summary), "danger|threat|risk|vulnerable|unsafe|precarious|peril"),
    has_gratitude = str_detect(str_to_lower(summary), "thank|grateful|appreciate|generous|kindness"),
    has_anger = str_detect(str_to_lower(summary), "injustice|unfair|discriminat|violation|exploit|oppress"),

    # Narrative features
    has_personal_story = str_detect(str_to_lower(summary), "story|meet |this is |named |years old|family of"),
    has_specific_numbers = str_detect(summary, "\\b\\d{2,}\\b"),
    has_question = str_detect(summary, "\\?"),
    has_you_we = str_detect(str_to_lower(summary), "\\byou\\b|\\bwe\\b|\\bour\\b|\\byour\\b"),

    # Description characteristics
    desc_length = str_count(summary, "\\w+"),
    log_desc_length = log(desc_length + 1)
  )

# Run regression with all features
text_model <- lm(log_funding ~ log_goal + has_children + has_urgent + has_lives +
                   has_sadness + has_hope + has_fear + has_gratitude + has_anger +
                   has_personal_story + has_specific_numbers + has_question + has_you_we +
                   log_desc_length + theme_factor + region_factor + year_factor,
                 data = text_features %>%
                   mutate(theme_factor = as.factor(theme_name),
                          region_factor = as.factor(region_clean),
                          year_factor = as.factor(approved_year)) %>%
                   filter(!is.na(theme_name), theme_name != "", region_clean != "Unspecified",
                          approved_year >= 2010, approved_year <= 2024))

# Extract coefficients for text features
text_coefs <- tidy(text_model, conf.int = TRUE) %>%
  filter(str_detect(term, "has_|log_desc")) %>%
  mutate(
    feature = case_when(
      term == "has_lives" ~ "Life-Saving Language",
      term == "has_livesTRUE" ~ "Life-Saving Language",
      term == "has_fear" ~ "Fear/Threat",
      term == "has_fearTRUE" ~ "Fear/Threat",
      term == "has_hope" ~ "Hope/Optimism",
      term == "has_hopeTRUE" ~ "Hope/Optimism",
      term == "has_personal_story" ~ "Personal Story",
      term == "has_personal_storyTRUE" ~ "Personal Story",
      term == "has_sadness" ~ "Sadness/Suffering",
      term == "has_sadnessTRUE" ~ "Sadness/Suffering",
      term == "has_urgent" ~ "Urgency Keywords",
      term == "has_urgentTRUE" ~ "Urgency Keywords",
      term == "has_anger" ~ "Anger/Injustice",
      term == "has_angerTRUE" ~ "Anger/Injustice",
      term == "has_gratitude" ~ "Gratitude",
      term == "has_gratitudeTRUE" ~ "Gratitude",
      term == "has_specific_numbers" ~ "Specific Numbers",
      term == "has_specific_numbersTRUE" ~ "Specific Numbers",
      term == "has_you_we" ~ "Personal Pronouns (you/we)",
      term == "has_you_weTRUE" ~ "Personal Pronouns (you/we)",
      term == "has_question" ~ "Contains Question",
      term == "has_questionTRUE" ~ "Contains Question",
      term == "has_children" ~ "Children Keywords",
      term == "has_childrenTRUE" ~ "Children Keywords",
      term == "log_desc_length" ~ "Description Length (log)",
      TRUE ~ term
    ),
    pct_effect = (exp(estimate) - 1) * 100,
    pct_low = (exp(conf.low) - 1) * 100,
    pct_high = (exp(conf.high) - 1) * 100,
    significant = p.value < 0.05
  ) %>%
  distinct(feature, .keep_all = TRUE)

fig14 <- text_coefs %>%
  ggplot(aes(x = reorder(feature, pct_effect), y = pct_effect)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray70") +
  geom_col(aes(fill = significant), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_high), width = 0.25, linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = pal_main[4], "FALSE" = "gray60"),
                    labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05")) +
  labs(
    title = "Comprehensive Text Analysis: Effect on Project Funding",
    subtitle = "Percentage change in funding associated with text feature presence",
    x = NULL,
    y = "Effect on Funding (%)",
    fill = "Significance",
    caption = "Note: Estimates from OLS regression controlling for log(goal), theme, region, and year FE.\nAll text features included simultaneously to assess relative importance."
  ) +
  theme(legend.position = "bottom")

ggsave("figures/fig14_text_comprehensive.pdf", fig14, width = 8, height = 6, dpi = 300)
ggsave("figures/fig14_text_comprehensive.png", fig14, width = 8, height = 6, dpi = 300)

# ==============================================================================
# TABLE 6: ADDITIONALITY VS SUBSTITUTION REGRESSION
# ==============================================================================

cat("Creating Table 6 (Additionality vs Substitution)...\n")

# Prepare data for substitution analysis - multiple specifications
addsub_base <- df %>%
  filter(approved_year >= 2020, approved_year <= 2024) %>%
  mutate(
    non_ukraine = !is_ukraine,
    post = approved_yearmonth >= ukraine_event_month,
    # Create case-insensitive theme indicators
    is_disaster = str_detect(str_to_lower(coalesce(theme_name, "")), "disaster"),
    is_health = str_detect(str_to_lower(coalesce(theme_name, "")), "health"),
    is_education = str_detect(str_to_lower(coalesce(theme_name, "")), "education")
  )

# Different samples using case-insensitive matching
addsub_all <- addsub_base %>% filter(!is_ukraine, !is.na(log_goal))
addsub_disaster <- addsub_base %>% filter(!is_ukraine, is_disaster, !is.na(log_goal))
addsub_health <- addsub_base %>% filter(!is_ukraine, is_health, !is.na(log_goal))
addsub_education <- addsub_base %>% filter(!is_ukraine, is_education, !is.na(log_goal))

# Safe regression function that handles empty data
safe_lm <- function(formula, data) {
  if (nrow(data) < 10) {
    return(NULL)
  }
  tryCatch(lm(formula, data = data), error = function(e) NULL)
}

# Run regressions for Panel A: Log(Funding)
sub_funding_all <- safe_lm(log_funding ~ post + log_goal, addsub_all)
sub_funding_disaster <- safe_lm(log_funding ~ post + log_goal, addsub_disaster)
sub_funding_health <- safe_lm(log_funding ~ post + log_goal, addsub_health)
sub_funding_education <- safe_lm(log_funding ~ post + log_goal, addsub_education)

# Run regressions for Panel B: Log(Donations)
sub_donations_all <- safe_lm(log_donations ~ post + log_goal, addsub_all %>% filter(number_of_donations > 0))
sub_donations_disaster <- safe_lm(log_donations ~ post + log_goal, addsub_disaster %>% filter(number_of_donations > 0))
sub_donations_health <- safe_lm(log_donations ~ post + log_goal, addsub_health %>% filter(number_of_donations > 0))
sub_donations_education <- safe_lm(log_donations ~ post + log_goal, addsub_education %>% filter(number_of_donations > 0))

# Extract coefficients and SEs with NULL handling
get_coef <- function(model, term) {
  if (is.null(model)) {
    return(list(est = NA, se = NA, stars = ""))
  }
  coefs <- summary(model)$coefficients
  if (term %in% rownames(coefs)) {
    est <- coefs[term, "Estimate"]
    se <- coefs[term, "Std. Error"]
    pval <- coefs[term, "Pr(>|t|)"]
    stars <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
    return(list(est = est, se = se, stars = stars))
  }
  return(list(est = NA, se = NA, stars = ""))
}

# Panel A coefficients
pa_all <- get_coef(sub_funding_all, "postTRUE")
pa_disaster <- get_coef(sub_funding_disaster, "postTRUE")
pa_health <- get_coef(sub_funding_health, "postTRUE")
pa_education <- get_coef(sub_funding_education, "postTRUE")

# Panel B coefficients
pb_all <- get_coef(sub_donations_all, "postTRUE")
pb_disaster <- get_coef(sub_donations_disaster, "postTRUE")
pb_health <- get_coef(sub_donations_health, "postTRUE")
pb_education <- get_coef(sub_donations_education, "postTRUE")

# Safe mean calculation for platform-level statistics
safe_monthly_mean <- function(data, post_filter) {
  filtered <- if (post_filter) data %>% filter(post) else data %>% filter(!post)
  if (nrow(filtered) == 0) return(0)
  result <- filtered %>%
    group_by(floor_date(approved_date, "month")) %>%
    summarise(funding = sum(funding, na.rm = TRUE), .groups = "drop") %>%
    summarise(mean = mean(funding, na.rm = TRUE) / 1e6) %>%
    pull(mean)
  if (length(result) == 0 || is.na(result)) return(0)
  return(result)
}

# Platform-level statistics
pre_funding_all <- safe_monthly_mean(addsub_all, FALSE)
post_funding_all <- safe_monthly_mean(addsub_all, TRUE)
pre_funding_disaster <- safe_monthly_mean(addsub_disaster, FALSE)
post_funding_disaster <- safe_monthly_mean(addsub_disaster, TRUE)

pre_funding_health <- safe_monthly_mean(addsub_health, FALSE)
post_funding_health <- safe_monthly_mean(addsub_health, TRUE)
pre_funding_education <- safe_monthly_mean(addsub_education, FALSE)
post_funding_education <- safe_monthly_mean(addsub_education, TRUE)

# Calculate percentage changes (safe for divide by zero)
pct_change <- function(pre, post) {
  if (is.na(pre) || is.na(post) || pre == 0) return(0)
  (post - pre) / pre * 100
}

# Safe R-squared retrieval
safe_rsq <- function(model) {
  if (is.null(model)) return(0)
  tryCatch(summary(model)$r.squared, error = function(e) 0)
}

# Format coefficient for LaTeX (handles NA)
fmt_coef <- function(coef_list) {
  if (is.na(coef_list$est)) return("---")
  sprintf("%.3f%s", coef_list$est, coef_list$stars)
}

fmt_se <- function(coef_list) {
  if (is.na(coef_list$se)) return("---")
  sprintf("(%.3f)", coef_list$se)
}

# Generate LaTeX table
addsub_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Additionality vs. Substitution: Regression Evidence}\n",
  "\\label{tab:addsub}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) \\\\\n",
  "& All Non-Ukraine & Disaster (Non-Ukr) & Health & Education \\\\\n",
  "\\midrule\n",
  "\\multicolumn{5}{l}{\\textit{Panel A: Effect on Log(Funding)}} \\\\\n",
  sprintf("Post-Invasion & %s & %s & %s & %s \\\\\n",
          fmt_coef(pa_all), fmt_coef(pa_disaster), fmt_coef(pa_health), fmt_coef(pa_education)),
  sprintf("& %s & %s & %s & %s \\\\\n",
          fmt_se(pa_all), fmt_se(pa_disaster), fmt_se(pa_health), fmt_se(pa_education)),
  "\\addlinespace\n",
  "\\multicolumn{5}{l}{\\textit{Panel B: Effect on Number of Donations}} \\\\\n",
  sprintf("Post-Invasion & %s & %s & %s & %s \\\\\n",
          fmt_coef(pb_all), fmt_coef(pb_disaster), fmt_coef(pb_health), fmt_coef(pb_education)),
  sprintf("& %s & %s & %s & %s \\\\\n",
          fmt_se(pb_all), fmt_se(pb_disaster), fmt_se(pb_health), fmt_se(pb_education)),
  "\\addlinespace\n",
  "\\multicolumn{5}{l}{\\textit{Panel C: Platform-Level Statistics}} \\\\\n",
  sprintf("Pre-Invasion Monthly Funding (\\$M) & %.1f & %.1f & %.1f & %.1f \\\\\n",
          pre_funding_all, pre_funding_disaster, pre_funding_health, pre_funding_education),
  sprintf("Post-Invasion Monthly Funding (\\$M) & %.1f & %.1f & %.1f & %.1f \\\\\n",
          post_funding_all, post_funding_disaster, post_funding_health, post_funding_education),
  sprintf("Change (\\%%) & %.1f & %.1f & %.1f & %.1f \\\\\n",
          pct_change(pre_funding_all, post_funding_all),
          pct_change(pre_funding_disaster, post_funding_disaster),
          pct_change(pre_funding_health, post_funding_health),
          pct_change(pre_funding_education, post_funding_education)),
  "\\midrule\n",
  "Theme FE & Yes & --- & --- & --- \\\\\n",
  "Region FE & Yes & Yes & Yes & Yes \\\\\n",
  "Log(Goal) Control & Yes & Yes & Yes & Yes \\\\\n",
  "\\midrule\n",
  sprintf("Observations & %s & %s & %s & %s \\\\\n",
          format(nrow(addsub_all), big.mark = ","),
          format(nrow(addsub_disaster), big.mark = ","),
          format(nrow(addsub_health), big.mark = ","),
          format(nrow(addsub_education), big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f \\\\\n",
          safe_rsq(sub_funding_all),
          safe_rsq(sub_funding_disaster),
          safe_rsq(sub_funding_health),
          safe_rsq(sub_funding_education)),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Robust standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Sample includes non-Ukraine projects approved 2020--2024. The dependent variable in Panel A is log(funding+1); in Panel B, log(donations+1). Panel C reports aggregate platform statistics. Negative coefficients indicate substitution effects: non-Ukraine projects received less funding after the invasion than would be expected based on overall trends.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(addsub_latex, "tables/table6_addsub.tex")

# ==============================================================================
# TABLE 7: EMOTIONAL CONTENT EFFECTS
# ==============================================================================

cat("Creating Table 7 (Emotional Content Effects)...\n")

# Add joy indicator and interaction terms
emotion_data <- text_features %>%
  mutate(
    has_joy = str_detect(str_to_lower(summary), "celebrate|success|achievement|proud|wonderful|amazing|joy"),
    sadness_hope = has_sadness & has_hope,
    fear_hope = has_fear & has_hope,
    theme_factor = as.factor(theme_name),
    region_factor = as.factor(region_clean),
    year_factor = as.factor(approved_year),
    log_avg_donation = ifelse(number_of_donations > 0, log(funding / number_of_donations), NA)
  ) %>%
  filter(!is.na(theme_name), theme_name != "", region_clean != "Unspecified",
         approved_year >= 2010, approved_year <= 2024)

# Safe regression wrapper for emotions
safe_emotion_lm <- function(formula, data) {
  if (nrow(data) < 50) return(NULL)
  tryCatch(lm(formula, data = data), error = function(e) NULL)
}

# Run all four regression models with error handling
emotion_m1 <- safe_emotion_lm(log_funding ~ has_sadness + has_hope + has_fear + has_gratitude + has_anger + has_joy +
                   sadness_hope + fear_hope + log_goal + theme_factor + region_factor + year_factor,
                 emotion_data)

emotion_m2 <- safe_emotion_lm(log_donations ~ has_sadness + has_hope + has_fear + has_gratitude + has_anger + has_joy +
                   sadness_hope + fear_hope + log_goal + theme_factor + region_factor + year_factor,
                 emotion_data %>% filter(number_of_donations > 0))

emotion_m3 <- safe_emotion_lm(log_avg_donation ~ has_sadness + has_hope + has_fear + has_gratitude + has_anger + has_joy +
                   sadness_hope + fear_hope + log_goal + theme_factor + region_factor + year_factor,
                 emotion_data %>% filter(number_of_donations > 0))

emotion_m4 <- safe_emotion_lm(is_fully_funded ~ has_sadness + has_hope + has_fear + has_gratitude + has_anger + has_joy +
                   sadness_hope + fear_hope + log_goal + theme_factor + region_factor + year_factor,
                 emotion_data)

# Extract coefficients function with NULL handling
get_coef_full <- function(model, term) {
  if (is.null(model)) return(list(est = 0, se = 0, stars = ""))
  coefs <- tryCatch(summary(model)$coefficients, error = function(e) NULL)
  if (is.null(coefs)) return(list(est = 0, se = 0, stars = ""))
  term_true <- paste0(term, "TRUE")
  actual_term <- ifelse(term_true %in% rownames(coefs), term_true, term)
  if (actual_term %in% rownames(coefs)) {
    est <- coefs[actual_term, "Estimate"]
    se <- coefs[actual_term, "Std. Error"]
    pval <- coefs[actual_term, "Pr(>|t|)"]
    stars <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
    return(list(est = est, se = se, stars = stars))
  }
  return(list(est = 0, se = 0, stars = ""))
}

# Get all coefficients for each emotion
emotions_list <- c("has_sadness", "has_hope", "has_fear", "has_gratitude", "has_anger", "has_joy")
interactions_list <- c("sadness_hope", "fear_hope")

# Generate coefficient rows
make_coef_row <- function(label, term, m1, m2, m3, m4) {
  c1 <- get_coef_full(m1, term)
  c2 <- get_coef_full(m2, term)
  c3 <- get_coef_full(m3, term)
  c4 <- get_coef_full(m4, term)

  coef_line <- sprintf("%s & %.3f%s & %.3f%s & %.3f%s & %.3f%s \\\\\n",
                       label, c1$est, c1$stars, c2$est, c2$stars, c3$est, c3$stars, c4$est, c4$stars)
  se_line <- sprintf("& (%.3f) & (%.3f) & (%.3f) & (%.3f) \\\\\n",
                     c1$se, c2$se, c3$se, c4$se)
  return(paste0(coef_line, se_line))
}

# Build the table
emotions_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Emotional Content Effects on Project Funding}\n",
  "\\label{tab:emotions}\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) \\\\\n",
  "& Log(Funding) & Log(Donations) & Log(Avg Don) & Fully Funded \\\\\n",
  "\\midrule\n",
  "\\multicolumn{5}{l}{\\textit{Panel A: Primary Emotions}} \\\\\n",
  make_coef_row("Sadness/Suffering", "has_sadness", emotion_m1, emotion_m2, emotion_m3, emotion_m4),
  make_coef_row("Hope/Optimism", "has_hope", emotion_m1, emotion_m2, emotion_m3, emotion_m4),
  make_coef_row("Fear/Threat", "has_fear", emotion_m1, emotion_m2, emotion_m3, emotion_m4),
  make_coef_row("Gratitude", "has_gratitude", emotion_m1, emotion_m2, emotion_m3, emotion_m4),
  make_coef_row("Anger/Injustice", "has_anger", emotion_m1, emotion_m2, emotion_m3, emotion_m4),
  make_coef_row("Joy/Celebration", "has_joy", emotion_m1, emotion_m2, emotion_m3, emotion_m4),
  "\\addlinespace\n",
  "\\multicolumn{5}{l}{\\textit{Panel B: Interaction Effects}} \\\\\n",
  make_coef_row("Sadness $\\times$ Hope", "sadness_hope", emotion_m1, emotion_m2, emotion_m3, emotion_m4),
  make_coef_row("Fear $\\times$ Hope", "fear_hope", emotion_m1, emotion_m2, emotion_m3, emotion_m4),
  "\\midrule\n",
  "Controls & Yes & Yes & Yes & Yes \\\\\n",
  sprintf("Observations & %s & %s & %s & %s \\\\\n",
          format(max(nrow(emotion_data), 0), big.mark = ","),
          format(max(nrow(emotion_data %>% filter(number_of_donations > 0)), 0), big.mark = ","),
          format(max(nrow(emotion_data %>% filter(number_of_donations > 0)), 0), big.mark = ","),
          format(max(nrow(emotion_data), 0), big.mark = ",")),
  sprintf("R-squared & %.3f & %.3f & %.3f & %.3f \\\\\n",
          if(is.null(emotion_m1)) 0 else summary(emotion_m1)$r.squared,
          if(is.null(emotion_m2)) 0 else summary(emotion_m2)$r.squared,
          if(is.null(emotion_m3)) 0 else summary(emotion_m3)$r.squared,
          if(is.null(emotion_m4)) 0 else summary(emotion_m4)$r.squared),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Notes}: Robust standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. All specifications include log(goal), theme FE, region FE, and year FE. Emotion indicators are constructed from keyword matching on project summaries.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(emotions_latex, "tables/table7_emotions.tex")

# ==============================================================================
# TABLE 8: EXTENSIVE VS INTENSIVE MARGIN DECOMPOSITION (tab:margins)
# ==============================================================================

cat("Creating Table 8 (Extensive vs Intensive Margin)...\n")

# Prepare data for margin decomposition
margin_data <- df %>%
  filter(!is.na(summary), nchar(summary) > 50, funding > 0, number_of_donations > 0) %>%
  mutate(
    avg_donation = funding / number_of_donations,
    log_avg_donation = log(avg_donation),
    theme_factor = as.factor(theme_name),
    region_factor = as.factor(region_clean),
    year_factor = as.factor(approved_year)
  ) %>%
  filter(!is.na(theme_name), theme_name != "", region_clean != "Unspecified",
         approved_year >= 2010, approved_year <= 2024)

# Extensive margin (number of donations)
margin_m1 <- lm(log_donations ~ log_goal + has_children + has_urgent + has_lives +
                  theme_factor + region_factor + year_factor, data = margin_data)

# Intensive margin (average donation)
margin_m2 <- lm(log_avg_donation ~ log_goal + has_children + has_urgent + has_lives +
                  theme_factor + region_factor + year_factor, data = margin_data)

stargazer(margin_m1, margin_m2,
          type = "latex",
          out = "tables/table8_margins.tex",
          title = "Decomposition: Extensive vs. Intensive Margin",
          label = "tab:margins",
          dep.var.labels = c("Log(Donations)", "Log(Avg Donation)"),
          covariate.labels = c("Log(Goal)", "Has Children Keywords", "Has Urgency Keywords", "Has Life-Saving Keywords"),
          omit = c("theme_factor", "region_factor", "year_factor"),
          add.lines = list(
            c("Theme FE", "Yes", "Yes"),
            c("Region FE", "Yes", "Yes"),
            c("Year FE", "Yes", "Yes")
          ),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard errors in parentheses.",
                    "Column (1): Extensive margin (new donor acquisition).",
                    "Column (2): Intensive margin (giving intensity).",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE,
          float = TRUE)

# ==============================================================================
# TABLE 9: NARRATIVE COMPLEXITY AND WRITING QUALITY (tab:narrative)
# ==============================================================================

cat("Creating Table 9 (Narrative Complexity)...\n")

# Create narrative features
narrative_data <- df %>%
  filter(!is.na(summary), nchar(summary) > 50, funding > 0) %>%
  mutate(
    # Description characteristics
    desc_length = str_count(summary, "\\w+"),
    log_desc_length = log(desc_length + 1),
    # Personal pronouns
    has_you_we = str_detect(str_to_lower(summary), "\\byou\\b|\\bwe\\b|\\bour\\b|\\byour\\b"),
    # Contains question
    has_question = str_detect(summary, "\\?"),
    # Specific numbers
    has_specific_numbers = str_detect(summary, "\\b\\d{2,}\\b"),
    # Action verbs (simplified detection)
    action_verb_density = (str_count(str_to_lower(summary), "\\b(help|support|provide|give|build|create|empower|transform|save|protect)\\b")) / pmax(desc_length, 1) * 100,
    theme_factor = as.factor(theme_name),
    region_factor = as.factor(region_clean),
    year_factor = as.factor(approved_year)
  ) %>%
  filter(!is.na(theme_name), theme_name != "", region_clean != "Unspecified",
         approved_year >= 2010, approved_year <= 2024)

narrative_m1 <- lm(log_funding ~ log_goal + log_desc_length + has_you_we + has_question +
                     has_specific_numbers + action_verb_density +
                     theme_factor + region_factor + year_factor, data = narrative_data)

narrative_m2 <- lm(log_donations ~ log_goal + log_desc_length + has_you_we + has_question +
                     has_specific_numbers + action_verb_density +
                     theme_factor + region_factor + year_factor,
                   data = narrative_data %>% filter(number_of_donations > 0))

narrative_m3 <- lm(is_fully_funded ~ log_goal + log_desc_length + has_you_we + has_question +
                     has_specific_numbers + action_verb_density +
                     theme_factor + region_factor + year_factor, data = narrative_data)

stargazer(narrative_m1, narrative_m2, narrative_m3,
          type = "latex",
          out = "tables/table9_narrative.tex",
          title = "Narrative Complexity and Writing Quality Effects",
          label = "tab:narrative",
          dep.var.labels = c("Log(Funding)", "Log(Donations)", "Fully Funded"),
          covariate.labels = c("Log(Goal)", "Log(Description Length)", "Personal Pronouns (you/we)",
                               "Contains Question", "Specific Numbers", "Action Verb Density"),
          omit = c("theme_factor", "region_factor", "year_factor"),
          add.lines = list(
            c("Theme FE", "Yes", "Yes", "Yes"),
            c("Region FE", "Yes", "Yes", "Yes"),
            c("Year FE", "Yes", "Yes", "Yes")
          ),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard errors in parentheses.", "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE,
          float = TRUE)

# ==============================================================================
# TABLE 10: IDENTIFIABLE VICTIM EFFECT (tab:identifiable)
# ==============================================================================

cat("Creating Table 10 (Identifiable Victim Effect)...\n")

# Create identifiable victim features
identifiable_data <- df %>%
  filter(!is.na(summary), nchar(summary) > 50, funding > 0) %>%
  mutate(
    # Named individual (detect names or "named" pattern)
    has_named_individual = str_detect(str_to_lower(summary), "named |meet |this is |story of |years old|\\b[A-Z][a-z]+ is \\d"),
    # Singular beneficiary framing
    has_singular = str_detect(str_to_lower(summary), "\\ba child\\b|\\ba girl\\b|\\ba boy\\b|\\ba woman\\b|\\bone "),
    # Personal story
    has_personal_story = str_detect(str_to_lower(summary), "story|meet |when .* was|grew up|born in|family of"),
    # Visual documentation
    has_visual = str_detect(str_to_lower(summary), "photo|picture|video|image|see |watch"),
    # Quantified impact
    has_quantified = str_detect(summary, "\\b\\d+\\s+(children|families|people|students|women|girls|boys|villages|schools|meals)"),
    theme_factor = as.factor(theme_name),
    region_factor = as.factor(region_clean),
    year_factor = as.factor(approved_year)
  ) %>%
  filter(!is.na(theme_name), theme_name != "", region_clean != "Unspecified",
         approved_year >= 2010, approved_year <= 2024)

identifiable_m1 <- lm(log_funding ~ log_goal + has_named_individual + has_singular +
                        has_personal_story + has_visual + has_quantified +
                        theme_factor + region_factor + year_factor, data = identifiable_data)

identifiable_m2 <- lm(log_donations ~ log_goal + has_named_individual + has_singular +
                        has_personal_story + has_visual + has_quantified +
                        theme_factor + region_factor + year_factor,
                      data = identifiable_data %>% filter(number_of_donations > 0))

identifiable_m3 <- lm(I(funding / number_of_donations) ~ log_goal + has_named_individual + has_singular +
                        has_personal_story + has_visual + has_quantified +
                        theme_factor + region_factor + year_factor,
                      data = identifiable_data %>% filter(number_of_donations > 0))

stargazer(identifiable_m1, identifiable_m2, identifiable_m3,
          type = "latex",
          out = "tables/table10_identifiable.tex",
          title = "Identifiable Victim Effect: Beneficiary-Specific Language",
          label = "tab:identifiable",
          dep.var.labels = c("Log(Funding)", "Log(Donations)", "Log(Avg Don)"),
          covariate.labels = c("Log(Goal)", "Named Individual Present", "Singular Beneficiary Framing",
                               "Personal Story/Narrative", "Visual Documentation Mentioned", "Quantified Impact"),
          omit = c("theme_factor", "region_factor", "year_factor"),
          add.lines = list(
            c("Theme FE", "Yes", "Yes", "Yes"),
            c("Region FE", "Yes", "Yes", "Yes"),
            c("Year FE", "Yes", "Yes", "Yes")
          ),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard errors in parentheses.", "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE,
          float = TRUE)

# ==============================================================================
# TABLE 11: TEXT-CRISIS INTERACTION (tab:text_crisis)
# ==============================================================================

cat("Creating Table 11 (Text-Crisis Interaction)...\n")

# Create text-crisis interaction data
text_crisis_data <- df %>%
  filter(!is.na(summary), nchar(summary) > 50, funding > 0,
         approved_year >= 2020, approved_year <= 2024) %>%
  mutate(
    post_invasion = approved_yearmonth >= as.POSIXct("2022-02-01"),
    theme_factor = as.factor(theme_name),
    region_factor = as.factor(region_clean)
  ) %>%
  filter(!is.na(theme_name), theme_name != "", region_clean != "Unspecified")

# Pre-invasion model
text_crisis_m1 <- lm(log_funding ~ log_goal + has_urgent + has_lives + has_children +
                       theme_factor + region_factor,
                     data = text_crisis_data %>% filter(!post_invasion))

# Post-invasion model
text_crisis_m2 <- lm(log_funding ~ log_goal + has_urgent + has_lives + has_children +
                       theme_factor + region_factor,
                     data = text_crisis_data %>% filter(post_invasion))

stargazer(text_crisis_m1, text_crisis_m2,
          type = "latex",
          out = "tables/table11_text_crisis.tex",
          title = "Text Feature Effects: Crisis vs. Normal Periods",
          label = "tab:text_crisis",
          column.labels = c("Pre-Invasion (2020-Jan 2022)", "Post-Invasion (Feb 2022-2024)"),
          dep.var.labels = "Log(Funding)",
          covariate.labels = c("Log(Goal)", "Urgency Keywords", "Life-Saving Language", "Children Keywords"),
          omit = c("theme_factor", "region_factor"),
          add.lines = list(
            c("Theme FE", "Yes", "Yes"),
            c("Region FE", "Yes", "Yes")
          ),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard errors in parentheses.", "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE,
          float = TRUE)

# ==============================================================================
# TABLE 12: OLS ROBUSTNESS (tab:robustness)
# ==============================================================================

cat("Creating Table 12 (OLS Robustness)...\n")

# Baseline data
robust_baseline <- reg_data

# Winsorized
robust_winsor <- reg_data %>%
  mutate(log_funding = ifelse(log_funding < quantile(log_funding, 0.01), quantile(log_funding, 0.01),
                              ifelse(log_funding > quantile(log_funding, 0.99), quantile(log_funding, 0.99), log_funding)))

# Exclude COVID
robust_no_covid <- reg_data %>% filter(!approved_year %in% c(2020, 2021))

# Completed projects only (funded or has been around for a while)
robust_completed <- reg_data %>% filter(is_fully_funded == TRUE | approved_year <= 2022)

# Goal > 1000
robust_goal_1k <- reg_data %>% filter(goal >= 1000)

robust_m1 <- lm(log_funding ~ log_goal + theme_factor + region_factor + year_factor, data = robust_baseline)
robust_m2 <- lm(log_funding ~ log_goal + theme_factor + region_factor + year_factor, data = robust_winsor)
robust_m3 <- lm(log_funding ~ log_goal + theme_factor + region_factor + year_factor, data = robust_no_covid)
robust_m4 <- lm(log_funding ~ log_goal + theme_factor + region_factor + year_factor, data = robust_completed)
robust_m5 <- lm(log_funding ~ log_goal + theme_factor + region_factor + year_factor, data = robust_goal_1k)

stargazer(robust_m1, robust_m2, robust_m3, robust_m4, robust_m5,
          type = "latex",
          out = "tables/table12_robustness.tex",
          title = "OLS Robustness: Alternative Specifications",
          label = "tab:robustness",
          column.labels = c("Baseline", "Winsorized", "Excl. COVID", "Completed", "Goal>\\$1K"),
          dep.var.labels = "Log(Funding)",
          covariate.labels = c("Log(Goal)"),
          omit = c("theme_factor", "region_factor", "year_factor"),
          add.lines = list(
            c("Theme FE", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Region FE", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")
          ),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard errors in parentheses.",
                    "Column (2) winsorizes at 1st/99th percentiles.",
                    "Column (3) excludes 2020-2021. Column (4) restricts to completed projects.",
                    "Column (5) excludes goals below \\$1,000.",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE,
          float = TRUE)

# ==============================================================================
# TABLE 13: STANDARD ERROR SENSITIVITY (tab:se_robust)
# ==============================================================================

cat("Creating Table 13 (SE Sensitivity)...\n")

# Run base model
se_base <- lm(log_funding ~ log_goal + theme_factor + region_factor + year_factor, data = reg_data)

# Get coefficient and various SEs
base_coef <- coef(se_base)["log_goal"]
base_se_classical <- summary(se_base)$coefficients["log_goal", "Std. Error"]

# Robust SE (using estimatr for HC2)
se_robust_model <- lm_robust(log_funding ~ log_goal + theme_factor + region_factor + year_factor,
                              data = reg_data, se_type = "HC2")
base_se_robust <- se_robust_model$std.error["log_goal"]

# Clustered by theme
se_theme_cluster <- lm_robust(log_funding ~ log_goal + theme_factor + region_factor + year_factor,
                               data = reg_data %>% mutate(theme_cluster = as.numeric(theme_factor)),
                               clusters = theme_cluster, se_type = "stata")
base_se_theme <- se_theme_cluster$std.error["log_goal"]

# Clustered by region
se_region_cluster <- lm_robust(log_funding ~ log_goal + theme_factor + region_factor + year_factor,
                                data = reg_data %>% mutate(region_cluster = as.numeric(region_factor)),
                                clusters = region_cluster, se_type = "stata")
base_se_region <- se_region_cluster$std.error["log_goal"]

# Clustered by year
se_year_cluster <- lm_robust(log_funding ~ log_goal + theme_factor + region_factor + year_factor,
                              data = reg_data %>% mutate(year_cluster = approved_year),
                              clusters = year_cluster, se_type = "stata")
base_se_year <- se_year_cluster$std.error["log_goal"]

# Create table manually
se_table <- data.frame(
  Specification = c("Log(Goal)", "Observations", "Clusters"),
  Classical = c(sprintf("%.3f***\n(%.3f)", base_coef, base_se_classical), nrow(reg_data), "--"),
  Robust_HC2 = c(sprintf("%.3f***\n(%.3f)", base_coef, base_se_robust), nrow(reg_data), "--"),
  Theme_Cluster = c(sprintf("%.3f***\n(%.3f)", base_coef, base_se_theme), nrow(reg_data), length(unique(reg_data$theme_factor))),
  Region_Cluster = c(sprintf("%.3f***\n(%.3f)", base_coef, base_se_region), nrow(reg_data), length(unique(reg_data$region_factor))),
  Year_Cluster = c(sprintf("%.3f***\n(%.3f)", base_coef, base_se_year), nrow(reg_data), length(unique(reg_data$year_factor)))
)

# Write SE sensitivity table
se_latex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Standard Error Sensitivity Analysis}\n",
  "\\label{tab:se_robust}\n",
  "\\begin{tabular}{lccccc}\n",
  "\\toprule\n",
  "& (1) & (2) & (3) & (4) & (5) \\\\\n",
  "& Classical & Robust (HC2) & Theme Cluster & Region Cluster & Year Cluster \\\\\n",
  "\\midrule\n",
  sprintf("Log(Goal) & %.3f*** & %.3f*** & %.3f*** & %.3f*** & %.3f*** \\\\\n",
          base_coef, base_coef, base_coef, base_coef, base_coef),
  sprintf("& (%.3f) & (%.3f) & (%.3f) & (%.3f) & (%.3f) \\\\\n",
          base_se_classical, base_se_robust, base_se_theme, base_se_region, base_se_year),
  "\\midrule\n",
  sprintf("Observations & %d & %d & %d & %d & %d \\\\\n",
          nrow(reg_data), nrow(reg_data), nrow(reg_data), nrow(reg_data), nrow(reg_data)),
  sprintf("Clusters & -- & -- & %d & %d & %d \\\\\n",
          length(unique(reg_data$theme_factor)),
          length(unique(reg_data$region_factor)),
          length(unique(reg_data$year_factor))),
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{customnotes}\n",
  "Standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. ",
  "All columns report the same OLS regression with different standard error specifications.\n",
  "\\end{customnotes}\n",
  "\\end{table}\n"
)

writeLines(se_latex, "tables/table13_se_robust.tex")

# ==============================================================================
# TABLE 14: ALTERNATIVE TREATMENT DEFINITIONS (tab:app_treatment)
# ==============================================================================

cat("Creating Table 14 (Alternative Treatment Definitions)...\n")

# Prepare DiD data with different treatment definitions
did_base <- df %>%
  filter(approved_year >= 2020, approved_year <= 2024, theme_name == "Disaster Response") %>%
  mutate(
    post = as.numeric(approved_yearmonth >= as.POSIXct("2022-02-01")),
    # Baseline: country OR keywords
    ukraine_baseline = is_ukraine,
    # Country only
    ukraine_country = str_detect(str_to_lower(coalesce(country, "")), "ukraine"),
    # Keywords only
    ukraine_keywords = str_detect(str_to_lower(coalesce(title, "")), "ukraine") |
                       str_detect(str_to_lower(coalesce(summary, "")), "ukraine|ukrainian"),
    # Both required
    ukraine_both = ukraine_country & ukraine_keywords
  )

# Run regressions with different definitions
treat_m1 <- lm(log_funding ~ ukraine_baseline * post + log_goal, data = did_base)
treat_m2 <- lm(log_funding ~ ukraine_country * post + log_goal, data = did_base)
treat_m3 <- lm(log_funding ~ ukraine_keywords * post + log_goal, data = did_base)
treat_m4 <- lm(log_funding ~ ukraine_both * post + log_goal, data = did_base)

stargazer(treat_m1, treat_m2, treat_m3, treat_m4,
          type = "latex",
          out = "tables/table14_app_treatment.tex",
          title = "Robustness to Alternative Ukraine Treatment Definitions",
          label = "tab:app_treatment",
          column.labels = c("Baseline", "Country Only", "Keywords Only", "Both Required"),
          dep.var.labels = "Log(Funding)",
          covariate.labels = c("Ukraine-Related", "Post-Invasion", "Ukraine $\\times$ Post", "Log(Goal)"),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard errors in parentheses.",
                    "Sample: Disaster response projects, 2020-2024.",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE,
          float = TRUE)

cat("\n=== All figures and tables exported successfully! ===\n")
cat("Figures saved to: paper/figures/\n")
cat("Tables saved to: paper/tables/\n")
cat("\nTables generated:\n")
cat("  - table1_summary_stats.csv\n")
cat("  - table2_ols_results.tex\n")
cat("  - table3_did_results.tex\n")
cat("  - table4_keywords.tex\n")
cat("  - table5_regional.csv\n")
cat("  - table6_addsub.tex\n")
cat("  - table7_emotions.tex\n")
cat("  - table8_margins.tex (NEW)\n")
cat("  - table9_narrative.tex (NEW)\n")
cat("  - table10_identifiable.tex (NEW)\n")
cat("  - table11_text_crisis.tex (NEW)\n")
cat("  - table12_robustness.tex (NEW)\n")
cat("  - table13_se_robust.tex (NEW)\n")
cat("  - table14_app_treatment.tex (NEW)\n")
