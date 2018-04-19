library("here")
library("tidyverse")

url <- "https://fixturedownload.com/download/afl-2018-AUSEasternStandardTime.csv"
fixture_raw <- read_csv(url,
                        col_types = cols(
                            .default = col_character(),
                            `Round Number` = col_integer(),
                            Date = col_datetime(format = " %d/%m/%Y %H:%M")
                        ))

fixture <- fixture_raw %>%
    select(Date, Round = `Round Number`, HomeTeam = `Home Team`,
           AwayTeam = `Away Team`, Ground = Location) %>%
    mutate(Date = as.Date(Date),
           Season = format(Date, "%Y")) %>%
    mutate(Round = paste0("R", Round)) %>%
    mutate(HomeTeam = case_when(
        HomeTeam == "GWS Giants" ~ "GW Sydney",
        TRUE                     ~ HomeTeam
    )) %>%
    mutate(AwayTeam = case_when(
        HomeTeam == "GWS Giants" ~ "GW Sydney",
        TRUE                     ~ AwayTeam
    )) %>%
    mutate(Ground = case_when(
        Ground == "Adelaide Arena, Shanghai"       ~ "Jiangwan Stadium",
        Ground == "Blundstone Arena"               ~ "Bellerive Oval",
        Ground == "Etihad Stadium"                 ~ "Docklands",
        Ground == "GMHBA Stadium"                  ~ "Kardinia Park",
        Ground == "Mars Stadium"                   ~ "Eureka Stadium",
        Ground == "MCG"                            ~ "M.C.G.",
        Ground == "Metricon Stadium"               ~ "Carrara",
        Ground == "Optus Stadium"                  ~ "Perth Stadium",
        Ground == "SCG"                            ~ "S.C.G.",
        Ground == "Spotless Stadium"               ~ "Sydney Showground",
        Ground == "TIO Stadium"                    ~ "Marrara Oval",
        Ground == "University of Tasmania Stadium" ~ "York Park",
        Ground == "UNSW Canberra Oval"             ~ "Manuka Oval",
        TRUE                                       ~ Ground
    )) %>%
    select(Date, Season, everything()) %>%
    as.data.frame()

write_tsv(fixture, here("data/fixture_2018.tsv"))
