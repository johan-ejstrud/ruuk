# Load pakages
library(tidyverse)


# Read data
befolkningsalder <-
  file.path('data', 'befolkningsalder.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot1_mine.png')), width = 8, height = 6)
