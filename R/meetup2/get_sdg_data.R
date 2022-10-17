library(tidyverse)
library(lubridate)
library(stringr)
library(statgl)

get_px <- function(px, ...){
  px |>
    statgl_url(lang = "da") |>
    statgl_fetch(..., .col_code = TRUE) |>
    as_tibble()
}

# ------------------------------------------------------------------------------
# primo befolkingen ------------------------------------------------------------
# ------------------------------------------------------------------------------

BEXSTA <- "BEXSTA" |>
  get_px(age = px_all(), time = px_top(), gender = c("M", "K")) |>
  mutate(
    age   = as.integer(age),
    value = ifelse(gender == "Mænd", value, -value)
    )

# ------------------------------------------------------------------------------
# andel af befolkningen med en indkomst 50/60% under medianindkosmten i 3 sammenhængende år
# ------------------------------------------------------------------------------

SOXOU01 <- "SOXOU01" |>
  get_px("inventory variable" = px_all("Andel*")) |>
  rename("variable" = `inventory variable`, "time" = year) |>
  mutate(variable = variable |> str_to_sentence())


# ------------------------------------------------------------------------------
# stemmeprocent, folketingsvalg (Grønland) --------------------------------------
# ------------------------------------------------------------------------------

SAXFOLK <- "SAXFOLK" |>
  get_px(municipality = 0, "votes cast" = c(12, 16)) |>
  rename("variable" = "votes cast") |>
  mutate(variable = ifelse(variable == "Stemmeberettigede", "nævner", "tæller")) |>
  spread(variable, value) |>
  mutate(
    time  = lubridate::dmy(time) |> lubridate::year(),
    value = tæller / nævner
    ) |>
  select(time, value) |>
  arrange(time)


### Johan data
befolkningsalder <-
  "BEXSTA" |>
  get_px(age = px_all(), time = px_top(), gender = c("M", "K")) |>
  mutate(
    age   = as.integer(age)
  )

valg_inatsisartut <-
  "SAXLANST" %>%
  get_px() %>%
  janitor::clean_names() %>%
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

library(sf)
library(maps)
library(rnaturalearth)
kl_sf <- ne_countries(country = "greenland",
                      returnclass = "sf",
                      scale = "medium")

krydstogt <-
  "TUXKRH" %>%
  get_px(time = 2019, port = px_all()) %>%
  filter(port != 'I alt') %>%
  left_join(select(world.cities, name, lat, long), by=c("port" = "name"))

ggplot(kl_sf) +
  geom_sf()
