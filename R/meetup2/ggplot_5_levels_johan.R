library(tidyverse)
library(lubridate)
library(stringr)
library(statgl)

img_dir <- file.path('img', 'meetup2')

get_px <- function(px, ...){
  px |>
    statgl_url(lang = "da") |>
    statgl_fetch(..., .col_code = TRUE) |>
    as_tibble()
}

befolkningsalder <-
  "BEXSTA" |>
  get_px(age = px_all(), time = px_top(), gender = c("M", "K")) |>
  mutate(
    age   = as.integer(age)
  )

ggplot(befolkningsalder, aes(x=age, y=value)) +
  geom_col(width = 1) +
  labs(title = "Befolkningsalder")

ggsave(file.path(img_dir, "johan_plot1.png"), width = 8, height = 6)



dagtilbud <-
  "OFXUKN1" %>%
  get_px("daycare institutions" = px_all()) %>%
  janitor::clean_names() %>%
  filter(keyfigures == "Alle børn i dagtilbud",
         daycare_institutions != "Alle dagtilbud") %>%
  select(-keyfigures)

ggplot(dagtilbud, aes(x = time, y = value, fill = daycare_institutions)) +
  geom_col() +
  theme(legend.title = element_blank())

ggsave(file.path(img_dir, "johan_plot2.png"), width = 8, height = 6)



energiforbrug <-
  "https://bank.stat.gl:443/api/v1/da/Greenland/EN/EN20/ENX1ACT.px" %>%
  statgl_fetch(time = px_all()) %>%
  as_tibble() %>%
  filter(type == 'Faktisk energiforbrug',
         energivare != 'Alle energivarer') %>%
  mutate(tid = as.numeric(tid),
         value = replace_na(value, 0)) %>%
  select(-type)

ggplot(energiforbrug, aes(x=tid, y=value, fill=energivare)) +
  geom_area() +
  scale_x_continuous(expand = expansion()) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  labs(title = 'Engergiforbrug',
       x = NULL,
       y = 'Terajoule'
       )
ggsave(file.path(img_dir, "johan_plot3.png"), width = 8, height = 6)


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

ggplot(valg_inatsisartut, aes(x=time, y=percentage, color=votes_cast)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~constituencies)

ggsave(file.path(img_dir, "johan_plot4.png"), width = 16, height = 12)


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
  geom_sf() +
  geom_point(data = krydstogt, aes(x=long, y=lat, size=value), shape = 1, color = "red")

ggsave(file.path(img_dir, "johan_plot5.png"), width = 8, height = 6)


