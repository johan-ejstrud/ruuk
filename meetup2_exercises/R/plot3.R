# Load pakages
library(tidyverse)


# Read data
boern_i_dagtilbud <-
  file.path('data', 'boern_i_dagtilbud.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot3_mine.png')), width = 8, height = 6)
