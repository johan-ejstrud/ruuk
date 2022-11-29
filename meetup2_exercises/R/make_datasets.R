if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("devtools")) install.packages(c("devtools"))
if (!require("statgl")) devtools::install_github("StatisticsGreenland/statgl")

library(tidyverse)
library(lubridate)
library(statgl)

get_px <- function(px, ...){
  px %>%
    statgl_url(lang = "da") %>%
    statgl_fetch(..., .col_code = TRUE) %>%
    janitor::clean_names() %>%
    as_tibble()
}

saveRDS_with_own_name <- function(df) {
  saveRDS(df,
          file = file.path('data', paste0(deparse(substitute(df)), '.rds')))
}


befolkningsalder <-
  "BEXSTA" %>%
  get_px(age = px_all(), time = px_top(), gender = c("M", "K")) %>%
  mutate(
    age = as.integer(age),
    value = replace_na(value, 0)
  )
saveRDS_with_own_name(befolkningsalder)


befolkningsalder2 <-
  befolkningsalder %>%
  mutate(
    value = if_else(gender == "Mænd", value, -value)
  )
saveRDS_with_own_name(befolkningsalder2)


boern_i_dagtilbud <-
  "OFXUKN1" %>%
  get_px("daycare institutions" = px_all()) %>%
  filter(keyfigures == "Alle børn i dagtilbud",
         daycare_institutions != "Alle dagtilbud") %>%
  mutate(value = replace_na(value, 0)) %>%
  select(-keyfigures)
saveRDS_with_own_name(boern_i_dagtilbud)


energiforbrug <-
  "https://bank.stat.gl:443/api/v1/da/Greenland/EN/EN20/ENX1ACT.px" %>%
  statgl_fetch(time = px_all()) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  filter(type == 'Faktisk energiforbrug',
         energivare != 'Alle energivarer') %>%
  mutate(tid = as.numeric(tid),
         value = replace_na(value, 0)) %>%
  select(-type)
saveRDS_with_own_name(energiforbrug)


valg_inatsisartut <-
  "SAXLANST" %>%
  get_px() %>%
  mutate(
    time = dmy(str_replace(time, "maj", "may"))
  ) %>%
  filter(votes_cast %in% c("Inuit Ataqatigiit",
                           "Siumut",
                           "Naleraq (Partii Naleraq)",
                           "Demokraterne",
                           "Atassut",
                           "Nunatta Qitornai",
                           "Samarbejdspartiet"
                           )
  ) %>%
  group_by(constituencies, time) %>%
  mutate(percentage = value/sum(value, na.rm = TRUE)) %>%
  ungroup()
saveRDS_with_own_name(valg_inatsisartut)


krydstogt <-
  "TUXKRH" %>%
  get_px(time = 2019, port = px_all()) %>%
  filter(port != 'I alt') %>%
  left_join(select(world.cities, name, lat, long), by=c("port" = "name"))
saveRDS_with_own_name(krydstogt)


under_medianindkomst <-
  "SOXOU01" %>%
  get_px("inventory variable" = px_all("Andel*")) %>%
  rename("variable" = inventory_variable, "time" = year) %>%
  mutate(variable = variable %>% str_to_sentence(),
         value = value/1e2
         )
saveRDS_with_own_name(under_medianindkomst)


stemmeprocent <-
  "SAXFOLK" %>%
  get_px(municipality = 0, "votes cast" = c(12, 16)) %>%
  rename(variable = votes_cast) %>%
  mutate(variable = ifelse(variable == "Stemmeberettigede", "nævner", "tæller")) %>%
  spread(variable, value) %>%
  mutate(
    date = dmy(str_replace(time, "maj", "may")),
    time  = year(date),
    value = tæller / nævner
  ) %>%
  select(time, value) %>%
  arrange(time)
saveRDS_with_own_name(stemmeprocent)


fangst_hellefisk <-
  "FIX021" |>
  get_px(species = 1, area = px_all(), form = px_all()) |>
  mutate(time = as.integer(time))
saveRDS_with_own_name(fangst_hellefisk)


bnp <-
  "NRX10" %>%
  get_px(units = "K", account = "04") %>%
  mutate(time = as.integer(aar), value = value/1e2, .keep = "used")
saveRDS_with_own_name(bnp)
