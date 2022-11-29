# Load pakages
library(tidyverse)


# Read data
under_medianindkomst <-
  file.path('data', 'under_medianindkomst.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot6_mine.png')), width = 8, height = 6)
