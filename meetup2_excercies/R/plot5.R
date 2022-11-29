# Load pakages
library(tidyverse)


# Read data
fangst_hellefisk <-
  file.path('data', 'fangst_hellefisk.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot5_mine.png')), width = 8, height = 6)
