# --- Packages ---
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

# --- Paths (local) ---
fp_in  <- "~/Desktop/THESIS_INDEX/H2_Dataframes_and_script_VMLI/vMLI_with_ITSKILLPON_rebalanced.csv"
fp_full <- "vMLI_with_ITSKILLPON_rebalanced_CHECKED_full.csv"
fp_filtered <- "vMLI_with_ITSKILLPON_rebalanced_CHECKED_filtered.csv"
fp_dropped  <- "vMLI_with_ITSKILLPON_rebalanced_DROPPED_report.csv"

# --- Load ---
df <- read_csv(fp_in, show_col_types = FALSE)

# --- Ensure numeric types where needed ---
num_cols <- c("Reading_0_100","Math_0_100","Science_0_100","Enrollment",
              "EParticipation","Media_Access","IT","v_MLI")
df <- df %>%
  mutate(across(any_of(num_cols), ~ suppressWarnings(as.numeric(.))))

# --- Weights (education subtotal = 0.40) ---
w <- c(
  Reading_0_100 = 0.15,
  Math_0_100    = 0.05,
  Science_0_100 = 0.05,
  Enrollment    = 0.15,
  EParticipation= 0.10,
  Media_Access  = 0.40,
  IT            = 0.10
)

# --- Recompute with AUTO REBALANCING ---
rebalance_row <- function(row, weights) {
  present <- names(weights)[!is.na(row[names(weights)])]
  if (length(present) == 0) return(NA_real_)
  w_sum <- sum(weights[present])
  sum(row[present] * (weights[present] / w_sum))
}

df <- df %>%
  rowwise() %>%
  mutate(v_MLI_check = rebalance_row(c_across(all_of(names(w))), w)) %>%
  ungroup()

# --- Add diagnostics: missing fields, >100.1 flags, etc. ---
component_cols <- names(w)

df <- df %>%
  mutate(
    missing_any      = if_any(all_of(component_cols), is.na),
    missing_which    = apply(select(., all_of(component_cols)), 1, function(x) {
      paste(names(x)[is.na(x)], collapse = "|")
    }),
    over_100_vMLI        = ifelse(!is.na(v_MLI), v_MLI > 100.1, NA),
    over_100_vMLI_check  = ifelse(!is.na(v_MLI_check), v_MLI_check > 100.1, NA)
  )

# --- Save FULL (no filtering) for inspection ---
write_csv(df, fp_full)

# --- Filter: drop NA or >100.1 in either v_MLI or v_MLI_check ---
df_filtered <- df %>%
  filter(!is.na(v_MLI), !is.na(v_MLI_check)) %>%
  filter(v_MLI <= 100.1, v_MLI_check <= 100.1)

# --- If empty, create a dropped-rows report explaining why ---
if (nrow(df_filtered) == 0) {
  dropped <- df %>%
    mutate(
      reason = case_when(
        is.na(v_MLI) | is.na(v_MLI_check) ~ "NA in v_MLI or v_MLI_check",
        v_MLI > 100.1 | v_MLI_check > 100.1 ~ ">100.1 in v_MLI or v_MLI_check",
        TRUE ~ "Other/unknown"
      )
    ) %>%
    select(Country, all_of(component_cols), v_MLI, v_MLI_check,
           missing_any, missing_which, over_100_vMLI, over_100_vMLI_check, reason)
  write_csv(dropped, fp_dropped)
} else {
  # Otherwise save filtered result
  write_csv(df_filtered, fp_filtered)
}

# --- Console summary ---
message("Input rows: ", nrow(df))
message("Non-missing rows (both v_MLI & v_MLI_check present): ",
        sum(!is.na(df$v_MLI) & !is.na(df$v_MLI_check)))
message(">100.1 (v_MLI): ", sum(df$over_100_vMLI %in% TRUE, na.rm = TRUE))
message(">100.1 (v_MLI_check): ", sum(df$over_100_vMLI_check %in% TRUE, na.rm = TRUE))
message("Wrote full (no filter): ", normalizePath(fp_full, mustWork = FALSE))
if (nrow(df_filtered) == 0) {
  message("All rows filtered. Dropped-rows report: ", normalizePath(fp_dropped, mustWork = FALSE))
} else {
  message("Filtered file: ", normalizePath(fp_filtered, mustWork = FALSE))
}

# --- Packages ---
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

# --- Input file paths ---
vml_path <- "~/Desktop/THESIS_INDEX/H2_Dataframes_and_script_VMLI/NEW vMLI/vMLI_with_ITSKILLPON_rebalanced_CLEAN.csv"
freedom_path <- "~/Desktop/THESIS_INDEX/FITW_INDEX/FIW_2024_total_scores.csv"

# --- Load data ---
vml <- read_csv(vml_path, show_col_types = FALSE)
freedom <- read_csv(freedom_path, show_col_types = FALSE)

# --- Normalize country column names ---
names(vml)[1] <- "Country"
country_col <- names(freedom)[grepl("Country", names(freedom), ignore.case = TRUE)][1]
freedom <- freedom %>% rename(Country = all_of(country_col))

# --- Identify and rename the Freedom score column ---
score_col <- names(freedom)[grepl("Total", names(freedom), ignore.case = TRUE) |
                              grepl("Freedom", names(freedom), ignore.case = TRUE)][1]
freedom <- freedom %>% rename(Freedom_Score = all_of(score_col))

# --- Merge datasets ---
df <- vml %>%
  left_join(freedom %>% select(Country, Freedom_Score), by = "Country") %>%
  mutate(across(c(v_MLI, Freedom_Score), as.numeric)) %>%
  drop_na(v_MLI, Freedom_Score)

# --- Regression ---
model <- lm(v_MLI ~ Freedom_Score, data = df)
summary(model)

# --- Quick interpretation printout ---
cat("\nModel summary:\n")
cat("Intercept:", coef(model)[1], "\nSlope:", coef(model)[2], "\n")
cat("R-squared:", summary(model)$r.squared, "\nP-value:", summary(model)$coefficients[2,4], "\n")

# --- Optional plot ---
ggplot(df, aes(x = Freedom_Score, y = v_MLI)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "Regression of v-MLI on Freedom in the World 2024",
       x = "Freedom in the World 2024 Score",
       y = "v-MLI (Vulnerability Media Literacy Index)") +
  theme_minimal()





# --- Packages ---
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

# --- Load data ---
df <- read_csv("~/Desktop/THESIS_INDEX/H2_Dataframes_and_script_VMLI/NEW vMLI/vMLI_with_ITSKILLPON_rebalanced_CLEAN.csv", show_col_types = FALSE)

# Ensure numeric type
df <- df %>% mutate(v_MLI = as.numeric(v_MLI))

# --- Sort and take bottom 20 ---
bottom_n <- 40
df_bottom <- df %>%
  arrange(v_MLI) %>%
  slice_head(n = bottom_n)

# --- Plot ---
ggplot(df_bottom,
       aes(x = reorder(Country, v_MLI), y = v_MLI, fill = v_MLI)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  labs(
    title = paste("Bottom", bottom_n, "Countries by v-MLI"),
    x = "",
    y = "v-MLI (0–100)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# Ensure numeric
df <- df %>% mutate(v_MLI = as.numeric(v_MLI))

# --- Sort by v_MLI ---
df <- df %>% arrange(desc(v_MLI))

# --- Plot (top 20 for clarity, change n as needed) ---
top_n <- 40
ggplot(df %>% slice_head(n = top_n),
       aes(x = reorder(Country, v_MLI), y = v_MLI, fill = v_MLI)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "C") +
  labs(
    title = paste("Top", top_n, "Countries by v-MLI"),
    x = "",
    y = "v-MLI (0–100)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# --- Packages ---
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})



# Ensure v_MLI is numeric
df <- df %>% mutate(v_MLI = as.numeric(v_MLI))

# --- Compute quintile thresholds ---
quintiles <- quantile(df$v_MLI, probs = seq(0, 1, 0.2), na.rm = TRUE)
print(quintiles)

# --- Assign each country to a quintile ---
df <- df %>%
  mutate(
    vMLI_quintile = cut(
      v_MLI,
      breaks = quintiles,
      include.lowest = TRUE,
      labels = c("Very Low", "Low", "Medium", "High", "Very High")
    )
  )

# --- Check distribution ---
table(df$vMLI_quintile)

# --- Optional: average v-MLI by quintile ---
df_summary <- df %>%
  group_by(vMLI_quintile) %>%
  summarise(
    avg_vMLI = mean(v_MLI, na.rm = TRUE),
    n_countries = n()
  )

print(df_summary)

# --- Save the classified file ---
write_csv(df, "vMLI_with_quintiles.csv")


# --- Packages ---
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

# --- Load data ---
df <- read_csv("vMLI_with_ITSKILLPON_rebalanced_CLEAN.csv", show_col_types = FALSE)
df <- df %>% mutate(v_MLI = as.numeric(v_MLI))

# --- Compute quintiles ---
quintiles <- quantile(df$v_MLI, probs = seq(0, 1, 0.2), na.rm = TRUE)
df <- df %>%
  mutate(
    vMLI_quintile = cut(
      v_MLI,
      breaks = quintiles,
      include.lowest = TRUE,
      labels = c("Very Low", "Low", "Medium", "High", "Very High")
    )
  )

# --- Summarize averages by quintile ---
df_summary <- df %>%
  group_by(vMLI_quintile) %>%
  summarise(
    avg_vMLI = mean(v_MLI, na.rm = TRUE),
    n_countries = n()
  )

# --- Plot average per quintile ---
ggplot(df_summary, aes(x = vMLI_quintile, y = avg_vMLI, fill = vMLI_quintile)) +
  geom_col() +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(
    title = "Average v-MLI by Quintile",
    x = "Quintile",
    y = "Average v-MLI"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


