

rm(list=ls())
gc()


# load dependencies -------------------------------------------------------


library(tibble)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)
library(janitor)
library(writexl)

# download files into dat/date, find way to automate? ---------------------

# UD
# ETR - https://establishtherun.com/etrs-top-300-for-underdogfantasy/
# LegUp - https://www.legendaryupside.com/underdog-best-ball-rankings/
# SW - https://spikeweek.com/2024-nfl-best-ball-almanac-rankings/

# UD Marathon
# ETR - https://establishtherun.com/underdog-best-ball-marathon-rankings-updates-9am-daily/
# SW - https://spikeweek.com/2024-nfl-best-ball-almanac-rankings/

# DK
# ETR - https://establishtherun.com/etrs-top-300-for-draftkings-best-ball-rankings-updates-9am-daily/
# LegUp - https://www.legendaryupside.com/underdog-best-ball-rankings/
# SW - https://spikeweek.com/2024-nfl-best-ball-almanac-rankings/

# Drafters
# ETR - https://establishtherun.com/drafters-best-ball-rankings-updated-9am-daily/
# SW - https://spikeweek.com/2024-nfl-best-ball-almanac-rankings/

# FFPC
# ETR - 
# LegUp - 

# get vector of files from latest date and slam into same tibble-----------

v_files_UD <- dir(path = "dat/UD", full.names = TRUE) %>%
  sort() %>%
  tail(1) %>% 
  list.files(full.names = TRUE) %>% 
  # remove agg_ranks from list if you already did a run so you don't ruin your aggregation 
  .[stringr::str_detect(., "agg_ranks", negate=TRUE)]

v_files_UD_Marathon <- dir(path = "dat/UD_Marathon", full.names = TRUE) %>%
  sort() %>%
  tail(1) %>% 
  list.files(full.names = TRUE) %>% 
  # remove agg_ranks from list if you already did a run so you don't ruin your aggregation 
  .[stringr::str_detect(., "agg_ranks", negate=TRUE)]

v_files_DK <- dir(path = "dat/DK", full.names = TRUE) %>%
  sort() %>%
  tail(1) %>% 
  list.files(full.names = TRUE) %>% 
  .[stringr::str_detect(., "agg_ranks", negate=TRUE)]

v_files_Drafters <- dir(path = "dat/Drafters", full.names = TRUE) %>% 
  sort() %>% 
  tail(1) %>% 
  list.files(full.names = TRUE) %>% 
  .[stringr::str_detect(., "agg_ranks", negate=TRUE)]

v_files_FFPC <- dir(path = "dat/FFPC", full.names = TRUE) %>% 
  sort() %>% 
  tail(1) %>% 
  list.files(full.names = TRUE) %>% 
  .[stringr::str_detect(., "agg_ranks", negate = TRUE)]


# aggregate ranks ---------------------------------------------------------

df_UD <- v_files_UD %>% 
  purrr::map(~ readr::read_csv(.x)) %>% 
  purrr::map(~ dplyr::mutate_all(.x, as.character)) %>% 
  purrr::set_names(nm = v_files_UD) %>% 
  purrr::map(~ dplyr::rename_with(.x, tolower)) %>% 
  purrr::map(~ dplyr::select(.x, "id")) %>% 
  purrr::map(~ dplyr::mutate(.x, rank = dplyr::row_number())) %>% 
  dplyr::bind_rows(.id = "filename") %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(rank = mean(rank, na.rm = TRUE)) %>% 
  dplyr::arrange(rank)

df_UD_Marathon <- v_files_UD_Marathon %>% 
  purrr::map(~ readr::read_csv(.x)) %>% 
  purrr::map(~ dplyr::mutate_all(.x, as.character)) %>% 
  purrr::set_names(nm = v_files_UD_Marathon) %>% 
  purrr::map(~ dplyr::rename_with(.x, tolower)) %>% 
  purrr::map(~ dplyr::select(.x, "id")) %>% 
  purrr::map(~ dplyr::mutate(.x, rank = dplyr::row_number())) %>% 
  dplyr::bind_rows(.id = "filename") %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(rank = mean(rank, na.rm = TRUE)) %>% 
  dplyr::arrange(rank)

df_DK <- v_files_DK %>% purrr::map(~ readr::read_csv(.x)) %>% 
  purrr::map(~ dplyr::mutate_all(.x, as.character)) %>% 
  purrr::set_names(nm = v_files_DK) %>% 
  purrr::map(~ dplyr::rename_with(.x, tolower)) %>% 
  purrr::map(~ dplyr::select(.x, "id")) %>% 
  purrr::map(~ dplyr::mutate(.x, rank = dplyr::row_number(),
                             id = as.numeric(id))) %>% 
  dplyr::bind_rows(.id = "filename") %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(rank = mean(rank, na.rm = TRUE)) %>% 
  dplyr::arrange(rank) %>% 
  dplyr::select(id) %>% 
  dplyr::mutate(name = NA_character_,
                position = NA_character_,
                adp = NA_character_,
                team = NA_character_)
  
df_Drafters <- v_files_Drafters %>% purrr::map(~ readr::read_csv(.x)) %>%
  purrr::map(~ dplyr::mutate_all(.x, as.character)) %>% 
  purrr::set_names(nm = v_files_Drafters) %>% 
  purrr::map(~ dplyr::rename_with(.x, tolower)) %>% 
  purrr::map(~ dplyr::select(.x, "id")) %>% 
  purrr::map(~ dplyr::mutate(.x, rank = dplyr::row_number(),
                             id = as.numeric(id))) %>% 
  dplyr::bind_rows(.id = "filename") %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(rank = mean(rank, na.rm = TRUE)) %>% 
  dplyr::arrange(rank) %>% 
  dplyr::select(id) %>% 
  dplyr::mutate(name = NA_character_,
                position = NA_character_,
                adp = NA_character_,
                team = NA_character_)


# clean_player_name <- purrr::as_mapper(\(x) dplyr::mutate(x = stringr::str_replace_all(x, "[^[:alnum:]]", " ") %>% stringr::str_to_lower(.)))
l_FFPC <- v_files_FFPC %>% purrr::map(~ readr::read_csv(.x)) %>%
  purrr::map(~ dplyr::mutate_all(.x, as.character)) %>%
  purrr::set_names(nm = v_files_FFPC) %>%
  purrr::map(~ janitor::clean_names(.x)) %>%
  purrr::map(~ dplyr::rename_with(.x, ~ stringr::str_replace(.x, "player_name", "name"))) %>%
  purrr::map(~ dplyr::rename_with(.x, ~ stringr::str_replace(.x, "etr_rank", "rank"))) %>%
  # purrr::map(~ dplyr::select(.x, "name", "rank")) %>% 
  purrr::map(~ dplyr::mutate(
    .x,
    dplyr::mutate(
      .x,
      name = stringr::str_replace_all(name, "[[:punct:]]", "") %>%
        # remove spaces between a single first letter at the front of a name and the next thing to follow it
        stringr::str_to_lower(.) %>% 
        stringr::str_replace(., "^([a-z]{1})[\\s]+", "\\1") %>% 
        stringr::str_replace(., "marquise", "hollywood") %>% 
        stringr::str_replace(., "josh palmer", "joshua palmer") %>% 
        stringr::str_replace(., "\\sjr|\\ssr", ""),
      rank = as.numeric(rank)
    )
  )) 

df_FFPC <- l_FFPC %>% 
  dplyr::bind_rows(.id = "filename") %>% 
  dplyr::group_by(name) %>% 
  dplyr::summarize(mean_rank = mean(rank, na.rm = TRUE)) %>% 
  dplyr::arrange("rank") %>% 
  dplyr::mutate(idx = dplyr::row_number())  %>% 
  dplyr::select(idx, dplyr::everything()) %>% 
  dplyr::left_join(., l_FFPC[[1]], by = "name") %>% 
  dplyr::left_join(., l_FFPC[[2]], by = "name") %>% 
  purrr::discard(~all(is.na(.x)))
# %>% 
#   tidyr::pivot_wider(id_cols = c(idx, name, mean_rank))
  
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

# UD
file_path_UD <- dir(path = "dat/UD_Marathon", full.names = TRUE) %>%
  sort() %>%
  tail(1)
file_date_UD <- dir(path = "dat/UD_Marathon", full.names = TRUE) %>%
  sort() %>%
  tail(1) %>% 
  stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
readr::write_csv(df_UD_Marathon, file = file.path(file_path_UD, stringr::str_c(file_date_UD, "_UD_Marathon_agg_ranks.csv")))

# DK
file_path_DK <- dir(path = "dat/DK", full.names = TRUE) %>%
  sort() %>%
  tail(1)
file_date_DK <- dir(path = "dat/DK", full.names = TRUE) %>%
  sort() %>%
  tail(1) %>% 
  stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
readr::write_csv(df_DK, file = file.path(file_path_DK, stringr::str_c(file_date_DK, "_DK_agg_ranks.csv")), na = "")

# Drafters
file_path_Drafters <- dir(path = "dat/Drafters", full.names = TRUE) %>% 
  sort() %>% 
  tail(1)
file_date_Drafters <- dir(path = "dat/Drafters", full.names = TRUE) %>% 
  sort() %>% 
  tail(1) %>% 
  stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
readr::write_csv(df_Drafters, file = file.path(file_path_Drafters, stringr::str_c(file_date_Drafters, "_Drafters_agg_ranks.csv")), na = "")

# FFPC
file_path_FFPC <- dir(path = "dat/FFPC", full.names = TRUE) %>% 
  sort() %>% 
  tail(1)
file_date_FFPC <- dir(path = "dat/FFPC", full.names = TRUE) %>% 
  sort() %>% 
  tail(1) %>% 
  stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
# readr::write_csv(df_FFPC, file = file.path(file_path_FFPC, stringr::str_c(file_date_FFPC, "_FFPC_agg_ranks.csv")), na = "")
writexl::write_xlsx(df_FFPC, path = file.path(file_path_FFPC, stringr::str_c(file_date_FFPC, "_FFPC_agg_ranks.xlsx")))
