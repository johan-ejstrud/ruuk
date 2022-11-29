# Load pakages
library(tidyverse)


# Read data
stemmeprocent <-
  file.path('data', 'stemmeprocent.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot9_mine.png')), width = 8, height = 6)
