# Author: Johan
# Test to see how difficult Daniels plots are

library("tidyverse")
library("statgl")

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


# BEGIN Johans code

# Plot 1
#  4 min Main plot ready
# 29 min Finished
library(ggtext)

img_dir <- file.path('img', 'meetup2')

BEXSTA %>%
  ggplot(aes(x=age, y=value, fill=gender)) +
  geom_col(width=1) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-500, 500, by = 100),
                     labels = abs) +
  scale_x_continuous(breaks = seq(0, 100, by = 20),
                     labels = function(x) {paste(x, "år")}) +
  annotate("text",
           label = c("♀", "♂"),
           y = c(-425, 425),
           x = rep(95, 2),
           size = 15) +
  labs(title = "Befolkningspyramide, 2022",
       subtitle = "Hele befolkningen",
       y = 'Antal personer',
       caption = '**Kilde:** https:/bank.stat.gl/BEDSTA') +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.caption = element_markdown())

ggsave(file.path(img_dir, "daniel_plot1_reverse_enginered.png"),
       width = 8, height = 6)

# Plot 2
#  9 min Everything except bar labels
# 19 min Finished
SOXOU01 %>%
  ggplot(aes(x=time, y=value, fill=variable)) +
  geom_col(position = position_dodge(0.9), width = 0.8) +
  scale_y_continuous(labels = function(x){paste(x, "%")}) +
  annotate('text',
           x=c(0.75, 1.2),
           y=rep(0.05,2),
           label=c("Andel under 50%", "Andel under 60%"),
           color="white",
           angle=90,
           hjust = 0,
           size = 5) +
  labs(title = "Økonomisk udsatte",
       subtitle = "Andel af befolkningen med en inkomst 50 % eller 60 % under medianinkomsten i 3 sammenhængende år",
       y="",
       x="",
       caption = '**Kilde:** https:/bank.stat.gl/SODOU01') +
  theme_statgl() +
  theme(legend.position = "none",
        plot.caption = element_markdown())

ggsave(file.path(img_dir, "daniel_plot2_reverse_enginered.png"),
       width = 8, height = 6)

# Plot 3
# 13 min Finished
SAXFOLK %>%
  ggplot(aes(x=time, y=value, label=time)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(.4, .7, by=.05)) +
  coord_cartesian(ylim = c(0.4, 0.7)) +
  labs(y = "",
       title = "Stemmeprocent, folketingsvalg",
       subtitle = "Grønland i alt",
       caption = '**Kilde:** https:/bank.stat.gl/SADFOLK') +
  geom_smooth(linetype = 0) +
  geom_col(width=.1) +
  geom_point(size = 4) +
  geom_text(vjust = -1) +
  theme(plot.caption = element_markdown(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

ggsave(file.path(img_dir, "daniel_plot3_reverse_enginered.png"),
       width = 8, height = 6)

