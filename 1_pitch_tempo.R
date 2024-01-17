library(tidyverse)

combined_pitch_data <- read_csv("data/combined_pitch_data.csv")

# univariate analysis  ----

## basic histogram
combined_pitch_data |> 
  filter(isPitch) |> 
  ggplot(aes(pitch_tempo)) +
  geom_histogram(binwidth = 1, color = "navy") +
  xlim(0, 120) +
  theme_minimal() + 
  labs(x = "Pitch Tempo (seconds)",
       y = NULL,
       title = "Histogram of pitch tempo values for the 2022 and 2023 MLB seasons",
       subtitle = "Pitch tempo is time (in seconds) between start of pitch and end of previous pitch") +
  theme(plot.title = element_text(face = "bold"))

## density distribution by year
combined_pitch_data |> 
  filter(isPitch) |> 
  ggplot(aes(pitch_tempo, group = as.factor(year)))+
  geom_density(alpha = 0.5, aes(color = as.factor(year), fill = as.factor(year))) +
  xlim(0, 60) +
  labs(title = "Pitch tempo density in 2022 and 2023 MLB seasons",
       subtitle = "Pitch tempo is time (in seconds) between start of pitch and end of previous pitch",
       x = "Pitch Tempo (in seconds)",
       y = NULL,
       group = "Year",
       color = "Year",
       fill = "Year")

## density distribution by year and baserunner state NOT DONE, DON'T LIKE
combined_pitch_data |> 
  filter(isPitch,
         !inning_count == 1) |> 
  mutate(matchup.splits.menOnBase = ifelse(matchup.splits.menOnBase == "Empty",
                                           "Empty",
                                           "Men_On")) |> 
  ggplot(aes(pitch_tempo, group = as.factor(year)))+
  geom_density(alpha = 0.5, aes(color = as.factor(year), fill = as.factor(year))) +
  xlim(0, 60) +
  facet_wrap(~matchup.splits.menOnBase) +
  labs(title = "Pitch tempo density in 2022 and 2023 MLB seasons",
       subtitle = "Pitch tempo is time (in seconds) between start of pitch and end of previous pitch",
       x = "Pitch Tempo (in seconds)",
       y = NULL,
       group = "Year",
       color = "Year",
       fill = "Year")

# Table of pitch tempo by year and baserunner state LIKE
combined_pitch_data |> 
  filter(isPitch,
         pitchNumber != 1) |> 
  mutate(matchup.splits.menOnBase = ifelse(matchup.splits.menOnBase == "Empty",
                                           "Empty",
                                           "Men_On")) |> 
  summarize(
    avg_tempo = mean(pitch_tempo, na.rm = TRUE),
    .by = c(matchup.splits.menOnBase, year)
  ) |> 
  pivot_wider(names_from = "year",
              values_from = "avg_tempo") |> 
  mutate(change = scales::percent((`2023` - `2022`)/`2022`)) |> 
  knitr::kable(
    col.names = c("Baserunner State", "Average pitch tempo in 2022", "Average pitch tempo in 2023", "Change in pitch tempo")
  )

## density distribution by year and pitch number (1st vs not) NOT DONE, I LIKE
combined_pitch_data |> 
  filter(isPitch) |> 
  mutate(pitchNumber = ifelse(pitchNumber == 1, "1st Pitch", "Not 1st Pitch")) |> 
  ggplot(aes(pitch_tempo, group = as.factor(year)))+
  geom_density(alpha = 0.5, aes(color = as.factor(year), fill = as.factor(year))) +
  xlim(0, 60) +
  facet_wrap(~pitchNumber) +
  labs(title = "Pitch tempo density in 2022 and 2023 MLB seasons, faceted by pitch number",
       subtitle = "The pitch clock set a limit of 30 seconds in between batters",
       x = "Pitch Tempo (in seconds)",
       y = NULL,
       group = "Year",
       color = "Year",
       fill = "Year") +
  theme(plot.title = element_text(face = "bold"))


# Table of pitch tempo by year and pitch number DONE, LIKE
combined_pitch_data |> 
  filter(isPitch) |> 
  mutate(pitchNumber = ifelse(pitchNumber == 1, 
                                     "1st pitch of at-bat", 
                                     "Not 1st pitch of at-bat")) |> 
  summarize(
    avg_tempo = mean(pitch_tempo, na.rm = TRUE),
    .by = c(pitchNumber, year)
  ) |> 
  pivot_wider(names_from = "year",
              values_from = "avg_tempo") |> 
  knitr::kable()

combined_pitch_data |> 
  slice_min(pitch_tempo, n = 100) |> 
  view()

# Team leaderboard of change in pitch tempo by year DONE, I LIKE
combined_pitch_data |> 
  filter(isPitch,
         pitchNumber != 1) |> 
  summarize(
    avg_tempo = mean(pitch_tempo, na.rm = TRUE),
    .by = c(fielding_team, year)
  ) |> 
  pivot_wider(names_from = "year",
              values_from = "avg_tempo") |> 
  mutate(tempo_change = `2023` - `2022`) |> 
  arrange(desc(tempo_change)) |> 
  drop_na() |> 
  knitr::kable()


# function that, using semi_join, gives only qualified pitchers based on number of pitches and given condition
qualified_pitchers <- function(data, value, conditions = isPitch) {
  
  data |> 
  semi_join(
    combined_pitch_data |> 
    filter({{conditions}}) |> 
    summarize(
       n = n(),
       .by = c(matchup.pitcher.fullName, year)
    ) |> 
    pivot_wider(names_from = "year",
                values_from = "n") |>
    filter(`2022` > {{value}},
           `2023` > {{value}})
)

}


# Player tempo leaderboards ----
# leaderboard of slowest median tempo in 2022
combined_pitch_data |> 
  qualified_pitchers(1) |> 
  filter(isPitch,
         pitchNumber != 1)|> 
  summarize(
    median_tempo = median(pitch_tempo, na.rm = TRUE),
    .by = c(matchup.pitcher.fullName, year)
  ) |> 
  pivot_wider(names_from = "year",
              values_from = "median_tempo") |> 
  mutate(tempo_change = `2023` - `2022`) |> 
  arrange(tempo_change) |> 
  drop_na() |> 
  slice_max(`2022`, n = 15) |> 
  knitr::kable()

# leaderboard of fastest median tempo in 2023
combined_pitch_data |> 
  qualified_pitchers(1) |> 
  filter(isPitch,
         pitchNumber != 1)|> 
  summarize(
    median_tempo = median(pitch_tempo, na.rm = TRUE),
    .by = c(matchup.pitcher.fullName, year)
  ) |> 
  pivot_wider(names_from = "year",
              values_from = "median_tempo") |> 
  mutate(tempo_change = `2023` - `2022`) |> 
  arrange(tempo_change) |> 
  drop_na() |> 
  slice_min(`2023`, n = 15) |> 
  knitr::kable()


# leaderboard of biggest change in tempo
combined_pitch_data |> 
  qualified_pitchers(100) |> 
  filter(isPitch,
         pitchNumber != 1)|> 
  summarize(
    median_tempo = median(pitch_tempo, na.rm = TRUE),
    .by = c(matchup.pitcher.fullName, year)
  ) |> 
  pivot_wider(names_from = "year",
              values_from = "median_tempo") |> 
  mutate(tempo_change = `2023` - `2022`) |> 
  arrange(tempo_change) |> 
  drop_na() |> 
  slice_min(tempo_change, n = 15) |> 
  knitr::kable()

# leaderboard of smallest change in tempo
combined_pitch_data |> 
  qualified_pitchers(100) |> 
  filter(isPitch,
         pitchNumber != 1)|> 
  summarize(
    median_tempo = median(pitch_tempo, na.rm = TRUE),
    .by = c(matchup.pitcher.fullName, year)
  ) |> 
  pivot_wider(names_from = "year",
              values_from = "median_tempo") |> 
  mutate(tempo_change = `2023` - `2022`) |> 
  arrange(tempo_change) |> 
  drop_na() |> 
  slice_max(tempo_change, n = 15) |> 
  knitr::kable()
