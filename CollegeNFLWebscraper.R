
library(tidyverse)
library(rvest)

# ALL API's for https://www.sports-reference.com
college_passingid <- c(
    "/cfb/years/2019-passing.html","/cfb/years/2020-passing.html","/cfb/years/2021-passing.html",
    "/cfb/years/2022-passing.html","/cfb/years/2023-passing.html")

# loop over year
for(i in 1 : 5){
  
  # ith dataset
  collegeurl <- paste0("https://www.sports-reference.com",
                        college_passingid[i])

  passingcollege <-  read_html(collegeurl) |>
    html_element("table") |>
    html_table() |>
    mutate( season = i+2018)
  
  # for the first dataset 
  if(i == 1){
    passing_data <- passingcollege 
  }
  # use join for all other datasets
  else{
    passing_data <- passing_data |>
      rbind(passingcollege) 

  }
  
}
names(passing_data)[1] <- "Rank"
names(passing_data)[2] <- "Player"
names(passing_data)[3] <- "Team"
names(passing_data)[4] <- "Conf"
names(passing_data)[5] <- "games_played"
names(passing_data)[6] <- "completed_passes"
names(passing_data)[7] <- "attempted_passes"
names(passing_data)[8] <- "completion%"
names(passing_data)[9] <- "Yards"
names(passing_data)[10] <- "TDs"
names(passing_data)[11] <- "TDsPerGame"
names(passing_data)[12] <- "Interceptions"
names(passing_data)[13] <- "Interception%"
names(passing_data)[14] <- "YdsPerAttepmt"
names(passing_data)[15] <- "AdjustedYdsPerAttempt"
names(passing_data)[16] <- "YdsPerCompletion"
names(passing_data)[17] <- "YdsPerGame"
names(passing_data)[18] <- "PassEfficiencyRating"



# spits out passing_data
##########################################################################################

college_rushingid <- c(
  "/cfb/years/2019-rushing.html","/cfb/years/2020-rushing.html","/cfb/years/2021-rushing.html",
  "/cfb/years/2022-rushing.html","/cfb/years/2023-rushing.html","/cfb/years/2024-rushing.html")

college_rushingyear <- c("rushing_2019","rushing_2020","rushing_2021",
                         "rushing_2022","rushing_2023")

column_names <- c("Rank", "Player", "Team", "Conference", "Games_Played", 
                  "rushing_attempts", "rushing_yds", "rushing_ydsperattempt", 
                  "rushing_tds", "rushing_ydspergame", "receiving_attempts", 
                  "receiving_yds", "receiving_ydsperreception", "receiving_tds", 
                  "receiving_ydspergame", "scrimmageplays", "scrimmage_yds", 
                  "scrimmage_avg", "scrimmage_tds", "Awards")

# Initialize an empty list to store data frames
rushing_data <- list()

# Loop over each year
for (i in 1:5) {
  collegeurl <- paste0("https://www.sports-reference.com", college_rushingid[i])
  
  rushingcollege <- tryCatch(
    {
      read_html(collegeurl) |> 
        html_element("table") |> 
        html_table(fill = TRUE)
    },
    error = function(e) {
      message(paste(" ", 2018 + i))
      return(NULL) 
    }
  )
  
  if (is.null(rushingcollege)) next
  
  if (ncol(rushingcollege) < length(column_names)) {
    message(paste(" ", 2018 + i))
    next
  }
  
  colnames(rushingcollege) <- column_names
  rushingcollege <- rushingcollege |> mutate(season = 2018 + i)
  
  # Append to the list
  rushing_data[[i]] <- rushingcollege
}

# Combine all datasets, ignore NULL entries
rushing_data <- bind_rows(rushing_data, .id = "source")
rushing_data <- rushing_data[-1,-1]
# spits out rushing_data

# Join passing and rushing on name and year

allcollege <- passing_data |>
  left_join(rushing_data, by = c("Player", "Team"))

###########################################################################################


# ALL API's for https://www.pro-football-reference.com
draft_id <- c(
  "/years/2024/draft.htm", "/years/2023/draft.htm", "/years/2022/draft.htm", "/years/2021/draft.htm",
  "/years/2020/draft.htm", "/years/2019/draft.htm"
)

for(i in 1 : 6){
  
  # ith dataset
  nflurl <- paste0("https://www.pro-football-reference.com",
                       draft_id[i])
  
  draft_data <-  read_html(nflurl) |>
    html_element("table") |>
    html_table()
  # names(college)[2] <- college_passingyear[i]
  
  # get rid of first row
  draft_data <- draft_data[-1, ]
  
  # for the first dataset 
  if(i == 1){
    nflplayers <- draft_data
  }
  # use join for all other datasets
  else{
    nflplayers <- nflplayers |>
      rbind(draft_data)
  }
  
}

names(nflplayers)[1] <- "Round"
names(nflplayers)[2] <- "Pick#"
names(nflplayers)[3] <- "Team"
names(nflplayers)[4] <- "Player"
names(nflplayers)[5] <- "Position"
names(nflplayers)[6] <- "Age"
names(nflplayers)[7] <- "Year"


# spits out nflplayers

###########################################################################################

# Join passing and rushing data, keeping only players from the passing dataset because that will filter out anyone who is not a quarterback
qb_stats <- passing_data |>
  left_join(rushing_data, by = c("Player", "season"))

# Keep only the player's final season in college
qb_final_season <- qb_stats |>
  group_by(Player) |>
  filter(season == max(season)) |>
  ungroup()

qb_final_season$Player <- str_extract(qb_final_season$Player, "^[^*]+")

# Add an 'NFL' column indicating whether the player made it to the NFL (1 is yes, 0 is no)
qb_final_season <- qb_final_season |>
  mutate(NFL = if_else(Player %in% nflplayers$Player, 1, 0))

write_csv(qb_final_season, "qb_final_season.csv")

