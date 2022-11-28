
# installation
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("devtools")) install.packages(c("devtools"))
if (!require("statgl")) devtools::install_github("StatisticsGreenland/statgl")

# load
library("tidyverse")
library("statgl")

# settings
get_px <- function(px, ..., .lang = "da"){
  px |> 
    statgl_url(lang = .lang) |> 
    statgl_fetch(..., .col_code = TRUE) |> 
    as_tibble()
}


# -----------------------------------------------------------------------------------
# befolkningspyramide ---------------------------------------------------------------
# -----------------------------------------------------------------------------------

# data
BEXSTA <- "BEXSTA" |> 
  get_px(age = px_all(), time = px_top(), gender = c("M", "K")) |> 
  mutate(age = as.integer(age), value = ifelse(gender == "Mænd", value, -value))

# plot
BEXSTA |> 
  ggplot(aes(
    x    = age,
    y    = value,
    fill = gender
  )) + 
  geom_col(width = 1) +
  coord_flip() +
  scale_y_continuous(labels = abs)


"pyramide.png" |> ggsave(width = 20, height = 15, units = "cm")

# hints 
browseURL("https://ggplot2.tidyverse.org/reference/coord_flip.html")
browseURL("https://stackoverflow.com/questions/64462326/absolute-values-in-axis-labels")


# -----------------------------------------------------------------------------------
# andel af befolkning med en indkomst 50 eller 60 % under 
# mediankomsten i 3 sammenhængende år (økonomisk udsatte)
# -----------------------------------------------------------------------------------

# data
SOXOU01 <- "SOXOU01" |> 
  get_px("inventory variable" = px_all("Andel*")) |> 
  rename("variable" = `inventory variable`, "time" = year) |> 
  mutate(variable = variable |> str_to_sentence(), value = value/1e2)

# plot
SOXOU01 |> 
  ggplot(aes(
    x    = time,
    y    = value,
    fill = variable
  )) +
  geom_col(position = "dodge2") +
  scale_y_continuous(labels = scales::percent)
  

"udsatte.png" |> ggsave(width = 20, height = 15, units = "cm")

# hints
browseURL("https://community.rstudio.com/t/scale-y-continuous-labels-scales-percent-question/112649")
browseURL("https://ggplot2.tidyverse.org/reference/position_dodge.html")

# -----------------------------------------------------------------------------------
# stemmeprocent, folketingsvalg / Grønland ------------------------------------------
# -----------------------------------------------------------------------------------

# data
SAXFOLK <- "SAXFOLK" |> 
  get_px(municipality = 0, "votes cast" = c(12, 16)) |> 
  rename("variable" = "votes cast") |> 
  mutate(variable = ifelse(variable == "Stemmeberettigede", "nævner", "tæller")) |> 
  spread(variable, value) |> 
  mutate(
    time  = lubridate::dmy(stringr::str_replace(time, "maj", "may")) |> lubridate::year(), 
    value = tæller / nævner
    ) |> 
  select(time, value) |> 
  arrange(time)

# plot
SAXFOLK |> 
  ggplot(aes(
    x     = time,
    y     = value,
    label = time
  )) + 
  geom_col(width = 0.1) +
  geom_point(size = 6) +
  geom_smooth(linetype = 0) +
  geom_text(vjust = -1) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0.4, 0.7))

"stemmeprocent.png" |> ggsave(width = 20, height = 15, units = "cm")


# hints
browseURL("https://ggplot2.tidyverse.org/reference/coord_cartesian.html")
browseURL("https://ggplot2.tidyverse.org/reference/geom_smooth.html")
browseURL("https://ggplot2.tidyverse.org/reference/geom_text.html")


# -----------------------------------------------------------------------------------
# realvæksten i BNP -----------------------------------------------------------------
# -----------------------------------------------------------------------------------

# data
NRX10 <- "NRX10" |> 
  get_px(units = "K", account = "04") |> 
  mutate(Aar = as.integer(Aar), value = value/1e2, .keep = "used") |> 
  rename("time" = Aar)
  

# plot
NRX10 |> 
  ggplot(aes(
    x = time,
    y = value
  )) +
  geom_line() +
  scale_y_continuous(labels = scales::percent)

"bnp.png" |> ggsave(width = 20, height = 15, units = "cm")

# -----------------------------------------------------------------------------------
# Forbrug af havets ressourcer / hellefisk ------------------------------------------
# -----------------------------------------------------------------------------------

# data
FIX021 <- "FIX021" |> 
  get_px(species = 1, area = px_all(), form = px_all()) |> 
  mutate(time = as.integer(time))


# plot
FIX021 |> 
  ggplot(aes(
    x = time,
    y = value,
    color = form
  )) +
  geom_line(size = 2) +
  facet_wrap(~ area, scales = "free")

"hellfisk.png" |> ggsave(width = 20, height = 15, units = "cm")

