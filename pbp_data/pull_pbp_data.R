library(nflfastR)
library(dplyr)
#install.packages("nfl4th")
library(nfl4th)
library(tidyr)
library(ggplot2)
library(nflplotR)


years <- 2024  #c(2016:2023)


for(year in years) {
pbp <- nfl4th::load_4th_pbp(year) %>%
  filter(down %in% c(2,4)) %>%
  select(go_boost, go, wp,posteam, pass, rush, play_type,
         epa, two_point_conv_result, down, ydstogo,)

file_name <- paste0("pbp_", year, ".csv")

# Write data to CSV file
write.csv(pbp, file = file_name, row.names = FALSE)

# Print a message indicating file creation
cat("CSV file for year", year, "saved as", file_name, "\n")

}

