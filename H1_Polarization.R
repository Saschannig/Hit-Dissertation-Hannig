install.packages("dplyr") # if needed
install.packages("readr") # if needed
library(readr)
library(dplyr)

rm(polarization)

df <- read_csv("~/Desktop/THESIS_INDEX/H1_Dataframes_and_scripts/H1_Merged_Data.csv")

df <- df %>%
  mutate(
    Polarization = (GINI__LatestValue * 0.3) +
      (((`WVS_Q57__Need to be very careful` * 0.5) +
          (`WVS_Q61__Little or no trust at all` * 0.5)) * 0.4) +
      (`WVS_Q117__Most_or_all` * 0.3)
  )

# write out if you want:
write_csv(df, "H1_Merged_Data_POL2.csv")

# PLOTTING RESULTS

install.packages("ggplot2") # if not yet installed
library(ggplot2)

# assuming df now includes your Polarization column
ggplot(df, aes(x = GINI__LatestValue, y = Polarization)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(
    title = "Polarization vs. GINI (Inequality)",
    x = "GINI (Latest Value)",
    y = "Polarization Index"
  ) +
  theme_minimal()


install.packages("ggrepel") # if not installed
library(ggplot2)
library(ggrepel)


top_countries <- df %>%
  arrange(desc(Polarization)) %>%
  slice(1:10)

ggplot(df, aes(x = GINI__LatestValue, y = Polarization)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  geom_text_repel(
    data = top_countries,
    aes(label = Country_std),  # <-- Change here
    size = 3.5,
    color = "darkred"
  ) +
  labs(
    title = "Polarization vs. GINI (Top 10 Highlighted)",
    x = "GINI (Latest Value)",
    y = "Polarization Index"
  ) +
  theme_minimal()

# Replace "Country_Name" with your actual country column name
bottom_countries <- df %>%
  arrange(Polarization) %>%
  slice(1:10)

ggplot(df, aes(x = GINI__LatestValue, y = Polarization)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  geom_text_repel(
    data = bottom_countries,
    aes(label = Country_std),
    size = 3.5,
    color = "darkgreen"
  ) +
  labs(
    title = "Polarization vs. GINI (Least Polarized Highlighted)",
    x = "GINI (Latest Value)",
    y = "Polarization Index"
  ) +
  theme_minimal()

# To compare with social media use
ggplot(df, aes(x = WVS_Q207__Daily, y = Polarization)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(
    title = "Polarization vs. Daily Social Media",
    x = "GINI (Latest Value)",
    y = "Polarization Index"
  ) +
  theme_minimal()


# To compare with social media use
ggplot(merged, aes(x = Total, y = Polarization)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(
    title = "Polarization vs. Freedom",
    x = "Freedom In the World",
    y = "Polarization Index"
  ) +
  theme_minimal()



#Now we do a simple regression with freedom, to see if polarization and freedom are correlated and if we find other correlations

library(readr)
library(dplyr)

polarization <- df
freedom <- read_csv("~/Desktop/THESIS_INDEX/FITW_INDEX/FIW_2024_total_scores.csv")

# Check column names to identify the country column in each
names(polarization)
names(freedom)

merged <- df %>%
  left_join(freedom, by = c("Country_std" = "Country/Territory"))

model <- lm(Polarization ~ Total , data = merged)
summary(model)

#Now we do a simple regression with SNS use to see if higher SNS use can be correlated with more polarization

polarization <- df

# We don't need to transform the data

model <- lm(Polarization ~ WVS_Q207__Daily , data = merged)
summary(model)

# We don't find strong correlation between use of SNS and higher polarization under this polarization model