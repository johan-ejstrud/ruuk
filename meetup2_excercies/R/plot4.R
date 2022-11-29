# Load pakages
library(tidyverse)


# Read data
energiforbrug <-
  file.path('data', 'energiforbrug.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot4_mine.png')), width = 8, height = 6)
