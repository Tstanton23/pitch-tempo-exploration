# Loading packages ----
library(tidyverse)
library(naniar)
library(janitor)


# Combined Pitch Data ----

## Reading in datasets ----
pbp_2022_payload <- read_csv("data/raw_data/pbp_2022_payload.csv")
pbp_2023_payload <- read_csv("data/raw_data/pbp_2023_payload.csv")

test_sample <- slice_sample(pbp_2022_payload, n = 20)

## Cleaning ----

### Comparing columns ----
compare_df_cols(pbp_2022_payload, pbp_2023_payload)

### adding violation columns to 2022 ----

pbp_2022_payload <-
  pbp_2022_payload |> 
  mutate(details.violation.description = NA,
         details.violation.player.fullName = NA,
         details.violation.player.id = NA,
         details.violation.type = NA, 
         .after = details.type.description)

### Function for selecting important columns ----
selecting_columns <- function(data) {
  data |> 
    select(game_pk:details.event, about.atBatIndex:about.inning, matchup.batter.id:matchup.pitchHand.description, 
           home_team, away_team, batting_team, fielding_team, details.type.code:pitchData.typeConfidence,
           pitchData.breaks.breakAngle:pitchData.breaks.spinDirection, matchup.splits.menOnBase, details.violation.description:details.violation.type)
}


### Function for adding game and season pitch count ----

pitch_counts <- function (data) {
  
  counts <- data |> 
    filter(isPitch) |>
      group_by(matchup.pitcher.fullName) |> 
        mutate(season_count = row_number(matchup.pitcher.fullName)) |> 
      ungroup() |> 
      group_by(matchup.pitcher.fullName, game_pk) |> 
        mutate(index = cur_group_id(),
               game_count = row_number(index),
               .before = isPitch) |> 
      ungroup() |> 
      group_by(matchup.pitcher.fullName, game_pk, about.inning) |> 
      mutate(index = cur_group_id(),
             inning_count = row_number(index),
             .before = isPitch) |> 
      ungroup() |> 
    select(playId, season_count, game_count, inning_count)
  
  data |> 
    left_join(counts, by = join_by(playId), relationship = "many-to-many") |> 
    relocate(season_count, game_count, inning_count,
             .after = pitchNumber)
}

### Function for adding pitch_tempo to the dataset ----


add_tempo <- function(data) {
  
  tempo <- data |> 
    filter(isPitch) |>
      mutate(pitch_tempo = ifelse(inning_count == 1, 
                                  NA,
                                  startTime - lag(startTime)),
           .before = isPitch) |> 
    select(playId, pitch_tempo)
  
  data |> 
    left_join(tempo, by = join_by(playId), relationship = "many-to-many") |> 
    relocate(pitch_tempo, .before = isPitch)
  
}

## Combining Datasets ----

### Adding year and ordering actions by day, game, and timestamp, running each function, and rbinding ----

combined_pitch_data <- rbind(
  pbp_2022_payload |> 
    selecting_columns() |> 
    mutate(year = 2022)|> 
    arrange(game_date, game_pk, startTime) |> 
    pitch_counts() |> 
    add_tempo(),
  pbp_2023_payload |> 
    selecting_columns() |> 
    mutate(year = 2023)|> 
    arrange(game_date, game_pk, startTime) |> 
    pitch_counts() |> 
    add_tempo()
)

write_csv(combined_pitch_data, "data/combined_pitch_data.csv")


## Loading combined dataset ----
# (I make sure to clear memory before this step, because the large datasets otherwise put too much strain on my computer)

rm(pbp_2022_payload, pbp_2023_payload)

combined_pitch_data <- read_csv("data/combined_pitch_data.csv")


# Pitch Tempo Leaderboard ----

tempo_leaderboard <- read_csv("data/raw_data/pitch_tempo.csv")

qualified_pitchers <- tempo_leaderboard |> 
  count(entity_name, entity_id) |> 
  filter(n > 1) |> 
  separate_wider_delim(entity_name, delim = ", ", names = c("lastName", "firstName")) |> 
  mutate(pitcher_name = paste(firstName, lastName)) |> 
  select(pitcher_name, entity_id)

# Ballpark Altitudes ----

# source: https://www.seamheads.com/ballparks/about.php 
parks <- read_csv("data/raw_data/Parks.csv") |>
  janitor::clean_names()

# adding home team for each park (for join)
home_teams <- c("Los Angeles Angels", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox", "Chicago Cubs",
                "Chicago White Sox", "Cincinnati Reds", "Cleveland Guardians", "Colorado Rockies", "Detroit Tigers",
                "Houston Astros", "Kansas City Royals", NA, "Los Angeles Dodgers", "Miami Marlins", "Milwaukee Brewers",
                "Minnesota Twins", "New York Mets", "New York Yankees", "Oakland Athletics", "Philadelphia Phillies",
                "Arizona Diamondbacks", "Pittsburgh Pirates", "San Diego Padres", "Seattle Mariners", "San Francisco Giants",
                "St. Louis Cardinals", "Tampa Bay Rays", "Toronto Blue Jays", "Washington Nationals")

abbr <- c("LAA", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR", NA, "LAD", "MIA", "MIL", "MIN", "NYM",
          "NYY", "OAK", "PHI", "ARI", "PIT", "SDP", "SEA", "SFG", "STL", "TBR", "TOR", "WSN")

# adding Rangers ballpark (was opened after list was published)
# https://elevation.maplogs.com/poi/globe_life_park_in_arlington_ballpark_way_arlington_tx_usa.220674.html 
rangers <- c("Globe Life Park", "Arlington", "TX", 32.7512802, -97.082504, 541, "Texas Rangers", "TEX")

altitude_data <- parks |> 
  filter(is.na(end)) |> 
  filter(!is.na(start)) |> 
  select(name:state, latitude:altitude) |>
  rename(park_name = name) |> 
  add_column(home_team = home_teams, 
             abbr = abbr) |> 
  rbind(rangers)

altitude_data |> 
  write_csv("data/altitude_data.csv")


# Missingness


combined_pitch_data |> 
  naniar::miss_var_summary() |> 
  view()





# Investigating problem tempo values ----

combined_pitch_data |> 
  filter(isPitch,
         inning_count != 1) |> 
  slice_max(pitch_tempo, n = 50) |> 
  view()

combined_pitch_data |> 
  filter(game_pk == 717421) |> 
  view()



# Investigating non-pitch actions ----


combined_pitch_data |> 
  filter(!isPitch,
         year == 2022) |> 
  count(details.event) |> 
  view()

combined_pitch_data |> 
  filter(!isPitch,
         year == 2023,
         is.na(details.event)) |> 
  count(details.description) |> 
  view()
