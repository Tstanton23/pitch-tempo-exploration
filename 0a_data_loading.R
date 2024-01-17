# Installing packages ----

devtools::install_github(repo = "BillPetti/baseballr")

library(tidyverse)
library(baseballr)
library(rstudioapi)
library(naniar)

# Loading 2022 season pitch-by-pitch data ----

## Getting game_pks from Bill Petti's repository ----
game_info <- baseballr::get_game_info_sup_petti()

game_pks <- game_info |> 
  filter(substr(game_date, 1, 4) == 2022, status_ind == "F",
         game_type == "R")

## Loading pitch-by-pitch data for each game_pk ----
# This takes a long time (>3 hours)
num_of_games <- nrow(game_pks)

pbp_2022_payload <- map_df(.x = seq_along(game_pks$game_pk),
                           ~{
                             
                             message(paste0('Grabbing pbp for game ', .x, ' of ', num_of_games))
                             
                             payload <- get_pbp_mlb(game_pk = game_pks$game_pk[.x])
                             
                             return(payload)
                           })

## Writing csv ----
write_csv(pbp_2022_payload, "data/raw_data/pbp_2022_payload.csv")


# Loading 2023 season pitch-by-pitch data ----

## Establishing date range ----

#end date
end_date <- ymd("2023-10-01")

#start date
start_date <- ymd("2023-3-30")

#range of dates
range <- seq(start_date, end_date,"days") 

date_range <- tibble(date = range)


## Getting game_pks for each day of season ----
game_info <- map_df(.x = seq_along(date_range$date),
                    
                    .f = ~{
                      
                      message(paste0('Grabbing game_pks for day ', .x, ' of 186.'))
                      
                      payload <- get_game_pks_mlb(date = date_range$date[.x])
                      
                      return(payload)
                      
                    })

## Removing duplicates ----
game_pks <- game_info |> 
  summarize(n = n(),
            .by = game_pk)


## Loading pitch-by-pitch data for each game_pk ----
# This takes a long time (>3 hours)
num_of_games <- count(game_pks)

pbp_2023_payload <- map_df(.x = seq_along(game_pks$game_pk),
                           ~{
                             
                             message(paste0('Grabbing pbp for game ', .x, ' of ', num_of_games))
                             
                             payload <- get_pbp_mlb(game_pk = game_pks$game_pk[.x])
                             
                             return(payload)
                           })

## Writing csv ----
write_csv(pbp_2023_payload, "data/raw_data/pbp_2023_payload.csv")
