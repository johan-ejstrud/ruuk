# Load pakages
library(tidyverse)


# Read data
valg_inatsisartut <-
  file.path('data', 'valg_inatsisartut.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot7_mine.png')), width = 12, height = 8)
