# Load pakages
library(tidyverse)


# Read data
bnp <-
  file.path('data', 'bnp.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot2_mine.png')), width = 8, height = 6)
