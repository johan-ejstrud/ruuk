library(tidyverse)
library(glue)

meetup2_excercies_project_dir <- file.path('meetup2_excercies')
data_dir <- file.path(meetup2_excercies_project_dir, 'data')
img_dir <- file.path(meetup2_excercies_project_dir, 'img')

save_plot <- function(n, width=8, height=6) {
  ggsave(file.path(img_dir, glue('plot{n}.png')), width = width, height = height)
}

read_data_set <- function(name) {
  file.path(data_dir, glue('{name}.rds')) %>%
    readRDS()
}

'befolkningsalder' %>%
  read_data_set() %>%
  ggplot(aes(x = age, y = value)) +
  geom_col() +
  labs(title = 'Befolkningsalder')
save_plot(1)

'boern_i_dagtilbud' %>%
  read_data_set() %>%
  ggplot(aes(x = time, y = value, fill = daycare_institutions)) +
  geom_col() +
  labs(title = 'Børn i dagtilbud') +
  theme(legend.title = element_blank())
save_plot(3)

'energiforbrug' %>%
  read_data_set() %>%
  ggplot(aes(x=tid, y=value, fill=energivare)) +
  geom_area() +
  scale_x_continuous(expand = expansion()) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  labs(title = 'Engergiforbrug',
       x = NULL,
       y = 'Terajoule'
  )
save_plot(5)

'valg_inatsisartut' %>%
  read_data_set() %>%
  ggplot(aes(x=time, y=percentage, color=votes_cast)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~constituencies)
save_plot(7, width = 12, height = 8)
