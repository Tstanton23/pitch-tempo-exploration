library(tidyverse)
library(DT)

combined_pitch_data <- read_csv("data/combined_pitch_data.csv")

# list of pitch types

combined_pitch_data |> 
  filter(isPitch) |> 
  count(details.type.description, details.type.code) |> 
  drop_na() |> 
  arrange(desc(n)) |> 
  filter(n > 25000) |> 
  knitr::kable(col.names = c("Pitch Type", "Pitch Code", "Number of Pitches Thrown"))
  
pitch_codes <- combined_pitch_data |> 
  filter(isPitch) |> 
  count(details.type.description, details.type.code) |> 
  drop_na() |> 
  arrange(desc(n)) |> 
  filter(n > 25000) |>
  select(details.type.code, details.type.description)

# pitch_type_leaderboard function

pitch_type_leaderboard <- function(pitch_type, metric, qualifying, year = c(2022, 2023)) {
  
  combined_pitch_data |> 
    filter(details.type.code == {{pitch_type}},
           year %in% {{year}}) |> 
    summarize(velo = round(mean(pitchData.startSpeed, na.rm = TRUE), 2), 
              spin_rate = round(mean(pitchData.breaks.spinRate, na.rm = TRUE), 0),
              n = n(), 
              .by = matchup.pitcher.fullName) |> 
    filter(n > qualifying) |> 
    arrange(desc({{metric}}))
  
}

# pitch type leaderboards
for (x in 1:nrow(pitch_codes)) {
  
  filename <- str_c("tables/pitch_type_leaderboards/", pitch_codes$details.type.code[[x]], "_leaderboard.html")
  
  title <- str_c(pitch_codes$details.type.description[[x]], " leaderboard, 2022-2023 (minimum 100 pitches)")
  
  pitch_type_leaderboard(pitch_codes$details.type.code[[x]], velo, 100)|> 
    datatable(colnames = c("Pitcher", "Average Velocity (mph)", "Average Spin Rate (rpm)", "Number of Pitches Thrown"),
              caption = title) |> 
    saveWidget(filename)
  
}


# velo vs. spin scatterplots
for (x in 1:nrow(pitch_codes)) {
  
  filename <- str_c("plots/velo_spin_scatterplots/", pitch_codes$details.type.code[[x]], "_scatterplot.png")
  
  title <- str_c("Average ", pitch_codes$details.type.description[[x]], " Velocity and Spin Rate, 2022-2023")
  
  p <- pitch_type_leaderboard(pitch_codes$details.type.code[[x]], velo, 100)|> 
    ggplot(aes(velo, spin_rate)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title = title,
         subtitle = "Minimum 100 pitches",
         x = "Average velocity (mph)",
         y = "Average spin rate (rpm)")
  
  ggsave(filename = filename, plot = p, width = 5)
}



# velo change by pitch type, only qualifying pitchers (>100 pitches) ----

combined_pitch_data |> 
  qualified_pitchers(100) |> 
  filter(isPitch) |> 
  summarize(avg_velo = mean(pitchData.startSpeed, na.rm = TRUE),
            .by = c(details.type.description, year)) |> 
  pivot_wider(names_from = year,
              values_from = c(avg_velo)) |>
  mutate(change = `2023` - `2022`) |> 
  semi_join(pitch_codes) |> 
  drop_na() |> 
  arrange(desc(`2023`)) |> 
  knitr::kable(col.names = c("Pitch Type", 
                             "Average Pitch Velocity in 2022", 
                             "Average Pitch Velocity in 2023", 
                             "Change in Average Velocity"))

# spin rate change by pitch type
combined_pitch_data |> 
  qualified_pitchers(100) |> 
  filter(isPitch) |> 
  summarize(avg_spin_rate = mean(pitchData.breaks.spinRate, na.rm = TRUE),
            .by = c(details.type.description, year)) |> 
  pivot_wider(names_from = year,
              values_from = c(avg_spin_rate)) |> 
  mutate(change = `2023` - `2022`) |> 
  semi_join(pitch_codes) |> 
  drop_na() |> 
  arrange(desc(`2023`)) |> 
  knitr::kable(col.names = c("Pitch Type", 
                             "Average Spin Rate in 2022", 
                             "Average Spin Rate in 2023", 
                             "Change in Spin Rate"))


# spin rate change by pitch type
combined_pitch_data |> 
  qualified_pitchers(100) |> 
  filter(isPitch) |> 
  summarize(avg_velo = round(mean(pitchData.startSpeed, na.rm = TRUE), 2),
            avg_spin_rate = round(mean(pitchData.breaks.spinRate, na.rm = TRUE), 0),
            .by = c(details.type.description, year)) |> 
  pivot_wider(names_from = year,
              values_from = c(avg_velo, avg_spin_rate)) |> 
  mutate(velo_change = round(avg_velo_2023 - avg_velo_2022, 2),
         spin_rate_change = round(avg_spin_rate_2023 - avg_spin_rate_2022, 0)) |> 
  semi_join(pitch_codes) |> 
  drop_na() |> 
  arrange(desc(velo_change)) |> 
  datatable(colnames = c("Pitch Type", 
                             "Average Pitch Velocity in 2022", 
                             "Average Pitch Velocity in 2023",
                             "Average Spin Rate in 2022", 
                             "Average Spin Rate in 2023", 
                             "Change in Average Velocity",
                             "Change in Spin Rate"))



# for loop of velo vs pitch tempo scatterplots
for (x in 1:nrow(pitch_codes)) {
  
  filename <- str_c("plots/velo_tempo_scatterplots/", pitch_codes$details.type.code[[x]], "_scatterplot.png")
  
  title <- str_c("Change in average ", pitch_codes$details.type.description[[x]], " velocity vs. change in average pitch tempo")
  subtitle <- str_c("Data only includes pitchers who threw >100 ", pitch_codes$details.type.description[[x]], "s each season")
  
  p <- combined_pitch_data |> 
    filter(isPitch,
           pitch_tempo < 120,
           details.type.code == pitch_codes$details.type.code[[x]]) |> 
    summarize(avg_velo = mean(pitchData.startSpeed, na.rm = TRUE),
              avg_tempo = mean(pitch_tempo, na.rm = TRUE),
              n = n(),
              .by = c(year, matchup.pitcher.fullName)) |> 
    pivot_wider(names_from = year,
                values_from = c(avg_velo, avg_tempo, n)) |> 
    filter(n_2022 > 100, n_2023 > 100) |> 
    mutate(diff_velo = avg_velo_2023 - avg_velo_2022,
           diff_tempo = avg_tempo_2023 - avg_tempo_2022) |> 
    select(!avg_velo_2022:n_2023) |> 
    ggplot(aes(diff_tempo, diff_velo)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = "Change in Pitch Tempo (in seconds)",
         y = "Change in Pitch Velocity (in mph)", 
         title = title,
         subtitle = subtitle)
  
  ggsave(filename = filename, plot = p)
}


# for loop of spin rate vs pitch tempo scatterplots
for (x in 1:nrow(pitch_codes)) {
  
  filename <- str_c("plots/spin_tempo_scatterplots/", pitch_codes$details.type.code[[x]], "_scatterplot.png")
  
  title <- str_c("Change in average ", pitch_codes$details.type.description[[x]], " spin rate vs. change in average pitch tempo")
  subtitle <- str_c("Data only includes pitchers who threw >100 ", pitch_codes$details.type.description[[x]], "s each season")
  
  p <- combined_pitch_data |> 
    filter(isPitch,
           pitch_tempo < 120,
           details.type.code == pitch_codes$details.type.code[[x]]) |> 
    summarize(avg_spin_rate = mean(pitchData.breaks.spinRate, na.rm = TRUE),
              avg_tempo = mean(pitch_tempo, na.rm = TRUE),
              n = n(),
              .by = c(year, matchup.pitcher.fullName)) |> 
    pivot_wider(names_from = year,
                values_from = c(avg_spin_rate, avg_tempo, n)) |> 
    filter(n_2022 > 100, n_2023 > 100) |> 
    mutate(diff_spin_rate = avg_spin_rate_2023 - avg_spin_rate_2022,
           diff_tempo = avg_tempo_2023 - avg_tempo_2022) |> 
    select(!avg_spin_rate_2022:n_2023) |> 
    ggplot(aes(diff_tempo, diff_spin_rate)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = "Change in Average Pitch Tempo (in seconds)",
         y = "Change in Average Pitch Spin Rate (in mph)", 
         title = title,
         subtitle = subtitle)
  
  ggsave(filename = filename, plot = p)
}


velo_tempo_results <- tibble()

# Fastball Regression ----
for (x in 1:nrow(pitch_codes)) {
  
  pitch_tempo_data <- combined_pitch_data |> 
    filter(isPitch,
           pitch_tempo < 120,
           details.type.code == pitch_codes$details.type.code[[x]]) |> 
    summarize(avg_velo = mean(pitchData.startSpeed, na.rm = TRUE),
              avg_tempo = mean(pitch_tempo, na.rm = TRUE),
              n = n(),
              .by = c(year, matchup.pitcher.fullName)) |> 
    pivot_wider(names_from = year,
                values_from = c(avg_velo, avg_tempo, n)) |> 
    filter(n_2022 > 100, n_2023 > 100) |> 
    mutate(diff_velo = avg_velo_2023 - avg_velo_2022,
           diff_tempo = avg_tempo_2023 - avg_tempo_2022) |> 
    select(!avg_velo_2022:n_2023)
  
  velo_tempo_model <- lm(diff_velo ~ diff_tempo, pitch_tempo_data)
  
  velo_tempo_summary <- summary(velo_tempo_model)
  
  velo_tempo_results <- rbind(velo_tempo_results, tibble("pitch_type" = pitch_codes$details.type.description[[x]],
                                                           "diff_tempo_coefficient" = velo_tempo_summary$coefficients[1, "Estimate"], 
                                                           "diff_tempo_p_value" = velo_tempo_summary$coefficients[1, "Pr(>|t|)"], 
                                                           "r-squared" = velo_tempo_summary$r.squared)
  )
}



# for loop of avg change in spin rate vs. avg change in pitch tempo
for (x in 1:nrow(pitch_codes)) {
  
  pitch_tempo_data <- combined_pitch_data |> 
    filter(isPitch,
           pitch_tempo < 120,
           details.type.code == pitch_codes$details.type.code[[x]]) |> 
    summarize(avg_spin_rate = mean(pitchData.breaks.spinRate, na.rm = TRUE),
              avg_tempo = mean(pitch_tempo, na.rm = TRUE),
              n = n(),
              .by = c(year, matchup.pitcher.fullName)) |> 
    pivot_wider(names_from = year,
                values_from = c(avg_spin_rate, avg_tempo, n)) |> 
    filter(n_2022 > 100, n_2023 > 100) |> 
    mutate(diff_spin_rate = avg_spin_rate_2023 - avg_spin_rate_2022,
           diff_tempo = avg_tempo_2023 - avg_tempo_2022) |> 
    select(!avg_spin_rate_2022:n_2023)
  
  fastball_tempo_model <- lm(diff_spin_rate ~ diff_tempo, pitch_tempo_data)
  
  print(summary(fastball_tempo_model))
}







pitch_tempo_data <- combined_pitch_data |> 
  filter(isPitch,
         !inning_count == 1,
         details.type.code == "FF") |> 
  summarize(avg_velo = mean(pitchData.startSpeed, na.rm = TRUE),
            avg_tempo = mean(pitch_tempo, na.rm = TRUE),
            n = n(),
            .by = c(year, matchup.pitcher.fullName)) |> 
  pivot_wider(names_from = year,
              values_from = c(avg_velo, avg_tempo, n)) |> 
  filter(n_2022 > 100, n_2023 > 100) |> 
  mutate(diff_velo = avg_velo_2023 - avg_velo_2022,
         diff_tempo = avg_tempo_2023 - avg_tempo_2022) |> 
  select(!avg_velo_2022:n_2023)

fastball_tempo_model <- lm(diff_velo ~ diff_tempo, pitch_tempo_data)

summary(fastball_tempo_model)  
