library(tidyverse)
library(baseballr)
library(mlbplotR)
library(ggrepel)

# data
combined_pitch_data <- read_csv("data/combined_pitch_data.csv")
altitude_data <- read_csv("data/altitude_data.csv")


# altitude by park (took way too long lol)

altitude_data |> 
  arrange(desc(altitude)) |> 
  mutate(abbr = clean_team_abbrs(abbr)) |> 
  drop_na() |> 
  ggplot(aes(reorder(home_team, -altitude), y = altitude)) +
  geom_col(aes(color = abbr, fill = abbr),
           width = 0.8) +
  scale_color_mlb(type = "secondary") +
  scale_fill_mlb() +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()+ 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(face = "bold", size = 16)) +
  geom_text_repel(
    min.segment.length = 0, 
    seed = 42,
    data = subset(altitude_data, altitude > 1000),
    aes(label = str_c(park_name, " (", abbr, ")")),
    xlim = c(2, 16),
    ylim = c(1500, 5200)) +
  geom_text_repel(
    min.segment.length = 0, 
    seed = 42,
    data = subset(altitude_data, altitude > 900 & altitude < 1000),
    aes(label = str_c(park_name, " (", abbr, ")")),
    xlim = c(4, 16),
    ylim = c(1300, 6000)) +
  geom_text_repel(
    min.segment.length = 0, 
    seed = 42,
    data = subset(altitude_data, altitude < 900 & altitude > 750),
    aes(label = str_c(park_name, " (", abbr, ")")),
    xlim = c(7, 16),
    ylim = c(900, 1500)) +
  labs(y = "Altitude (in feet)", 
       x = "Home Ballpark", 
       title = "Altitude of each current MLB ballpark (in feet above sea level)")

  
# 2022 and 2023 median tempo along with park name and altitude, no obvious connection
combined_pitch_data |> 
  filter(isPitch,
         !pitchNumber == 1,
         pitch_tempo < 120) |> 
  summarize(
    mean = mean(pitch_tempo, na.rm = TRUE),
    .by = c(home_team, year)
  ) |> 
  pivot_wider(names_from = year,
              values_from = mean) |> 
  arrange(desc(`2023`)) |> 
  left_join(altitude_data) |>
  select(home_team:park_name, altitude) |> 
  drop_na() |> 
  print(n = 30)


# investigating Coors Field specifically
combined_pitch_data |> 
  filter(isPitch,
         !pitchNumber == 1) |>
  left_join(altitude_data) |>
  mutate(abbr = ifelse(abbr == "COL", 
                      "Coors Field",
                      "Everywhere else")) |> 
  drop_na(abbr) |> 
  ggplot(aes(pitch_tempo, color = abbr, fill = abbr)) +
  geom_density(alpha = 0.5) +
  xlim(0, 60) +
  facet_wrap(~year)

combined_pitch_data |> 
  filter(isPitch,
         !pitchNumber == 1) |>
  left_join(altitude_data) |>
  mutate(abbr = ifelse(abbr == "COL", 
                       "Coors Field",
                       "Everywhere else")) |> 
  summarize(
    median = median(pitch_tempo, na.rm = TRUE),
    mean = mean(pitch_tempo, na.rm = TRUE),
    .by = c(abbr, year)
  ) |> 
  pivot_wider(names_from = year,
              values_from = c(mean, median)) |> 
  arrange(desc(median_2023)) |> 
  drop_na()



# Coors Field regression

coors_regression_data <- combined_pitch_data |> 
  qualified_pitchers(100, details.type.code == "FC") |> 
  filter(isPitch,
         pitchNumber != 1,
         pitch_tempo < 120, 
         year == 2023) |> 
  left_join(altitude_data, by = join_by(home_team)) |> 
  mutate(abbr = ifelse(abbr == "COL", 1, 0)) |> 
  select(matchup.pitcher.fullName, pitch_tempo, park_name, abbr, pitchData.startSpeed, pitchData.breaks.spinRate)


coors_regression <- lm(pitchData.startSpeed ~ pitch_tempo + pitch_tempo:abbr + matchup.pitcher.fullName - 1, 
                                  data = coors_regression_data)


coors_summary <- summary(coors_regression)

coors_summary$coefficients |> 
  knitr::kable()

coors_results_table <- tibble(
  "pitch_type" = character(),
  "year" = numeric(),
  "pitch_tempo_coefficient" = numeric(), 
  "pitch_tempo_p_value" = numeric(),
  "pitch_tempo_abbr_coefficient" = numeric(),
  "pitch_tempo_abbr_p_value" = numeric(),
  "r-squared" = numeric())

for (x in 1:nrow(pitch_codes)) {
  
  for(y in c(2022, 2023)) {
    
  coors_regression_data <- combined_pitch_data |> 
    qualified_pitchers(100, details.type.code == pitch_codes$details.type.code[[x]]) |> 
    filter(isPitch,
           pitchNumber != 1,
           pitch_tempo < 120, 
           year == {{y}}) |> 
    left_join(altitude_data, by = join_by(home_team)) |> 
    mutate(abbr = ifelse(abbr == "COL", 1, 0)) |> 
    select(matchup.pitcher.fullName, pitch_tempo, park_name, abbr, pitchData.startSpeed, pitchData.breaks.spinRate)
  
  
  coors_regression <- lm(pitchData.startSpeed ~ pitch_tempo + pitch_tempo:abbr + matchup.pitcher.fullName - 1, 
                         data = coors_regression_data)
  
  
  coors_summary <- summary(coors_regression)
  
  coors_results_table <- rbind(coors_results_table, tibble("pitch_type" = pitch_codes$details.type.description[[x]],
                                                           "year" = y,
                                                           "pitch_tempo_coefficient" = coors_summary$coefficients[1, "Estimate"], 
                                                           "pitch_tempo_p_value" = coors_summary$coefficients[1, "Pr(>|t|)"], 
                                                           "pitch_tempo_abbr_coefficient" = coors_summary$coefficients["pitch_tempo:abbr", "Estimate"], 
                                                           "pitch_tempo_abbr_p_value" = coors_summary$coefficients["pitch_tempo:abbr", "Pr(>|t|)"],
                                                           "r-squared" = coors_summary$r.squared)
    
  )
}
}



coors_results_table |> 
  datatable()







# altitude effect regression
altitude_regression_data <- combined_pitch_data |> 
  qualified_pitchers(100, details.type.code == "FF") |> 
  filter(isPitch,
         pitch_tempo < 120, 
         year == 2022) |> 
  left_join(altitude_data, by = join_by(home_team)) |> 
  mutate(altitude = as.numeric(altitude)) |> 
  select(matchup.pitcher.fullName, pitch_tempo, park_name, altitude, pitchData.startSpeed, pitchData.breaks.spinRate)

options(max.print=10000)

# Does give me significant results, tho it takes FOREVER to load
fastball_velo_2022_altitude <- lm(pitchData.startSpeed ~ pitch_tempo + pitch_tempo:altitude + matchup.pitcher.fullName - 1, 
                                  data = altitude_regression_data)


slm_fb_v_2022_a <- summary(fastball_velo_2022_altitude)

slm_fb_v_2022_a$coefficients[c(1,323),] |> 
  knitr::kable()

nrow(slm_fb_v_2022_a$coefficients)





