#Media Access CSV Creation

# --- Packages ---
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# --- Input/Output paths ---
fp_in  <- "~/Desktop/THESIS_INDEX/Media Access Data/media_access_index_noTV_v2.csv"
fp_out <- "media_access_index_noTV_v2_checked.csv"

# --- Load data ---
df <- read_csv(fp_in, show_col_types = FALSE)

# --- Recompute components exactly as in your spec ---
# Internet_Diff_Inverted = 100 - 100 * (Diff10 / max(Diff10))
max_delta <- max(df$Diff10, na.rm = TRUE)
df_checked <- df %>%
  mutate(
    Internet_Diff_Inverted_check = 100 - 100 * (Diff10 / max_delta),
    Media_Access_check =
      (LatestValue * 0.3) +
      (Internet_Diff_Inverted_check * 0.2) +
      (Internet_Freedom * 0.2) +
      (MobileAccess * 0.3),
    # Compare with the existing value in file (if present)
    Media_Access_diff = if ("Media_Access" %in% names(.)) Media_Access_check - Media_Access else NA_real_,
    Matches = if ("Media_Access" %in% names(.)) abs(Media_Access_diff) < 1e-6 else NA
  )

# --- (Optional) rounding for easier eyeballing ---
df_checked <- df_checked %>%
  mutate(
    Media_Access_check = round(Media_Access_check, 6),
    Media_Access_diff  = round(Media_Access_diff, 6)
  )

# --- Save a copy with the extra verification column(s) ---
write_csv(df_checked, fp_out)

# Quick console summary (prints when you run the script)
message("Rows checked: ", nrow(df_checked))
if ("Matches" %in% names(df_checked)) {
  message("Exact matches (tol 1e-6): ", sum(df_checked$Matches, na.rm = TRUE),
          " / ", nrow(df_checked))
  message("Output written to: ", fp_out)
} else {
  message("No existing Media_Access column found to compare. Output written to: ", fp_out)
}
