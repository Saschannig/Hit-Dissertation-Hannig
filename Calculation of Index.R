# Packages
library(readr)
library(dplyr)
library(stringr)
library(purrr)

# -------------------------
# 1) FILE PATHS
# -------------------------
# Your master inputs (already cleaned/unioned across sources)
infile_master <- "Desktop/THESIS_INDEX/VMLI/vMLI_inputs_with_eparticipation_FIXED.csv"

# Your PISA sub-scores file (to recompute Education)
pisa_file <- "~/Desktop/THESIS_INDEX/PISA/Merged_PISA_with_Absolute_Scales.csv"


# -------------------------
# 2) LOAD FILES
# -------------------------
df <- read_csv(infile_master, show_col_types = FALSE)
pisa <- read_csv(pisa_file, show_col_types = FALSE) %>%
  rename_with(~str_replace_all(.x, "\\ufeff", "")) %>%
  select(ISO3C, Country_std,
         Reading_Abs_0_100, Math_Abs_0_100, Science_Abs_0_100) %>%
  mutate(across(c(Reading_Abs_0_100, Math_Abs_0_100, Science_Abs_0_100),
                ~ suppressWarnings(as.numeric(.))))

# -------------------------
# 3) CLEAN COUNTRY NAMES
# -------------------------
clean_country <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("[’`]", "'") %>%
    str_replace_all("[,\\.]", "") %>%
    str_squish()
}

# -------------------------
# 4) RECOMPUTE EDUCATION(i)
# -------------------------
pisa_edu <- pisa %>%
  mutate(
    w_read = 2, w_math = 0.5, w_sc = 0.5,
    p_read = !is.na(Reading_Abs_0_100),
    p_math = !is.na(Math_Abs_0_100),
    p_sc   = !is.na(Science_Abs_0_100),
    w_eff_read = if_else(p_read, w_read, 0),
    w_eff_math = if_else(p_math, w_math, 0),
    w_eff_sc   = if_else(p_sc, w_sc, 0),
    w_sum = w_eff_read + w_eff_math + w_eff_sc,
    `Education(i)` = if_else(
      w_sum > 0,
      (Reading_Abs_0_100 * w_eff_read +
         Math_Abs_0_100 * w_eff_math +
         Science_Abs_0_100 * w_eff_sc) / w_sum,
      NA_real_
    )
  ) %>%
  select(ISO3C, Country_std, `Education(i)`) %>%
  mutate(c_clean_pisa = clean_country(Country_std))

# -------------------------
# 5) MERGE EDUCATION INTO MASTER
# -------------------------
df <- df %>%
  mutate(c_clean_master = clean_country(Country))

df <- df %>%
  left_join(pisa_edu %>% select(ISO3C, `Education(i)`),
            by = "ISO3C", suffix = c("", "_iso")) %>%
  mutate(`Education(i)` = coalesce(`Education(i)_iso`, `Education(i)`)) %>%
  select(-`Education(i)_iso`)

df <- df %>%
  left_join(pisa_edu %>% select(c_clean_pisa, `Education(i)`),
            by = c("c_clean_master" = "c_clean_pisa"), suffix = c("", "_name")) %>%
  mutate(`Education(i)` = coalesce(`Education(i)`, `Education(i)_name`)) %>%
  select(-`Education(i)_name`, -c_clean_master)

# -------------------------
# 6) DROP MISSING COUNTRY NAMES
# -------------------------
df <- df %>% filter(!is.na(Country) & Country != "")

# -------------------------
# 7) COMPUTE vMLI INDEX
# -------------------------
base_w <- c(
  `PressFreedom(i)`    = 0.30,
  `Education(i)`       = 0.30,
  `SocietalTrust(i)`   = 0.10,
  `E-Participation(i)` = 0.02,
  `MediaAccess(i)`     = 0.25,
  `IT(i)`              = 0.03
)
components <- names(base_w)

# Ensure numeric
df <- df %>%
  mutate(across(all_of(components), ~ suppressWarnings(as.numeric(.))))

compute_vMLI <- function(row) {
  vals <- as.numeric(row[components])
  names(vals) <- components
  present <- !is.na(vals)
  if (!any(present)) return(NA_real_)
  w <- base_w[present]
  w <- w / sum(w)
  sum(vals[present] * w)
}

df_out <- df %>%
  rowwise() %>%
  mutate(vMLI = compute_vMLI(cur_data())) %>%
  ungroup()

# -------------------------
# 8) SAVE FINAL OUTPUT
# -------------------------
write_csv(df_out, "vMLI_index_results_FINAL.csv")

# Quick check
df_out %>%
  select(Country, ISO3C, `PressFreedom(i)`, `Education(i)`,
         `SocietalTrust(i)`, `E-Participation(i)`,
         `MediaAccess(i)`, `IT(i)`, vMLI) %>%
  head() %>% print()

