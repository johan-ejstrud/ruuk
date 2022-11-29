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


'bnp' %>%
  read_data_set() %>%
  ggplot(aes(x = time, y = value)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'BNP')
save_plot(2)


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
  labs(title = 'Engergiforbrug',
       x = NULL,
       y = 'Terajoule'
  )
save_plot(4)


'fangst_hellefisk' %>%
  read_data_set() %>%
  ggplot(aes(x = time, y = value, color = form)) +
  geom_line(size = 2) +
  facet_wrap(~ area, scales = "free") +
  labs(title = "Fangst af hellefisk")
save_plot(5)


'under_medianindkomst' %>%
  read_data_set() %>%
  ggplot(aes(x    = time, y    = value, fill = variable)) +
  geom_col(position = "dodge2") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Økonomisk udsatte')
save_plot(6)


'valg_inatsisartut' %>%
  read_data_set() %>%
  ggplot(aes(x=time, y=percentage, color=votes_cast)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~constituencies)
save_plot(7, width = 12, height = 8)


'befolkningsalder2' %>%
  read_data_set() %>%
  ggplot(aes(x = age, y = value, fill = gender)) +
  geom_col(width = 1) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = 'Befolkningsalder')
save_plot(8)


'stemmeprocent' %>%
  read_data_set() %>%
  ggplot(aes(x = time, y = value, label = time)) +
  geom_col(width = 0.1) +
  geom_point(size = 6) +
  geom_smooth(linetype = 0) +
  geom_text(vjust = -1) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0.4, 0.7))
  labs(title = 'Stemmeprocent ved valg til folketinget')
save_plot(9)


'energiforbrug' %>%
  read_data_set() %>%
  ggplot(aes(x=tid, y = value, color = energivare)) +
  geom_line(size = 1) +
  labs(title = 'Energiforbrug')
ggsave(file.path(img_dir, glue('plot_example.png')), width = 8, height = 6)
