## MLB Pitch Clock and Pitcher Performance: An EDA

In this EDA, I investigated the effect of the implementation of the pitch clock in Major League Baseball on pitch quality. I did this with pitch-by-pitch data from the 2022 and 2023 MLB seasons, and attempted to find a relationship between pitch tempo (the time duration between pitches) and pitch velocity and/or spin rate, based on a theory that shortened tempo means greater fatigue. Additionally, I explored the intricacies of pitch tempo and pitch quality and investigated if playing at altitude changes the effect of pitch tempo on pitch velocity.

## What's in the repo

### Report Components

- `Stanton_Thomas_final_report.html`: Final report file
- `Stanton_Thomas_executive_summary.html`: Executive summary

### Subfolders

- `/data/raw_data`: Contains all raw data files for the project
- `/data`: Contains all cleaned data files and their associated codebooks
- `/plots`: Contains scatterplots used in the report
- `/tables`: Contains html tables used in the report
- `/progress_memos`: Contains all files used for progress memos

### R scripts

- `0a_data_loading.R`: Modifies code used by Bill Petti, creator of the BaseballR package, to download pitch-by-pitch data for the entire 2022 and 2023 MLB regular seasons
- `0b_data_cleaning.R`: Condenses the raw pitch-by-pitch data, combines the two seasons into one file, and creates important variables like `pitch_tempo`
- `1_pitch_tempo.R`: Contains univariate and bivariate explorations of the variable `pitch_tempo`
- `2_pitch_quality.R`: Explores pitch velocity and spin rate, separated by pitch type, as well as the relationship between pitch tempo, velocity, and spin rate
- `3_altitude.R`: Explores the altitude of MLB ballparks and how it may affect the relationship between pitch tempo and pitch quality