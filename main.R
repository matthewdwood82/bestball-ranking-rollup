




# load dependencies -------------------------------------------------------


library(tibble)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)


# download files into dat/date, find way to automate? ---------------------

# UD
# ETR - https://establishtherun.com/etrs-top-300-for-underdogfantasy/
# 4for4 - https://www.4for4.com/underdog/rankings
# LegUp - https://www.legendaryupside.com/underdog-best-ball-rankings/
# SW - https://spikeweek.com/rankings/?app=UNDERDOG&sport=NFL

# DK
# ETR - https://establishtherun.com/etrs-top-300-for-draftkings-best-ball-rankings-updates-9am-daily/
# LegUp - https://www.legendaryupside.com/underdog-best-ball-rankings/
# SW - https://spikeweek.com/rankings/?app=UNDERDOG&sport=NFL


# Drafters



# get vector of files from latest date and slam into same tibble-----------

v_files_UD <- dir(path = "dat/UD", full.names = TRUE) %>%
  sort() %>%
  tail(1) %>% 
  list.files(full.names = TRUE)


v_files_DK <- dir(path = "dat/DK", full.names = TRUE) %>%
  sort() %>%
  tail(1) %>% 
  list.files(full.names = TRUE)

# v_files_Drafters <- dir(path = "dat") %>% sort() %>% tail(1)


# aggregate ranks ---------------------------------------------------------

df_UD <- v_files_UD %>% 
  purrr::map(~ readr::read_csv(.x)) %>% 
  purrr::set_names(nm = v_files_UD) %>% 
  purrr::map(~ dplyr::rename_with(.x, tolower)) %>% 
  purrr::map(~ dplyr::select(.x, "id")) %>% 
  purrr::map(~ dplyr::mutate(.x, rank = dplyr::row_number())) %>% 
  dplyr::bind_rows(.id = "filename") %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(rank = mean(rank, na.rm = TRUE)) %>% 
  dplyr::arrange(rank)

df_DK <- v_files_DK %>% purrr::map(~ readr::read_csv(.x)) %>% 
  purrr::set_names(nm = v_files_DK) %>% 
  purrr::map(~ dplyr::rename_with(.x, tolower)) %>% 
  purrr::map(~ dplyr::select(.x, "id")) %>% 
  purrr::map(~ dplyr::mutate(.x, rank = dplyr::row_number())) %>% 
  dplyr::bind_rows(.id = "filename") %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(rank = mean(rank, na.rm = TRUE)) %>% 
  dplyr::arrange(rank) %>% 
  dplyr::select(id) %>% 
  dplyr::mutate(name = NA_character_,
                position = NA_character_,
                adp = NA_character_,
                team = NA_character_)
  

# print output ------------------------------------------------------------

# UD
file_path_UD <- dir(path = "dat/UD", full.names = TRUE) %>%
  sort() %>%
  tail(1)
file_date_UD <- dir(path = "dat/UD", full.names = TRUE) %>%
  sort() %>%
  tail(1) %>% 
  stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
readr::write_csv(df_UD, file = file.path(file_path_UD, stringr::str_c(file_date_UD, "_UD_agg_ranks.csv")))

#DK
file_path_DK <- dir(path = "dat/DK", full.names = TRUE) %>%
  sort() %>%
  tail(1)
file_date_DK <- dir(path = "dat/DK", full.names = TRUE) %>%
  sort() %>%
  tail(1) %>% 
  stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
readr::write_csv(df_DK, file = file.path(file_path_DK, stringr::str_c(file_date_DK, "_DK_agg_ranks.csv")), na = "")
