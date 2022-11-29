# Load pakages
library(tidyverse)


# Read data
energiforbrug <-
  file.path('data', 'energiforbrug.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot_example_mine.png')), width = 8, height = 6)



























































# SOLUTION
#
# ggplot(energiforbrug, aes(x=tid, y = value, color = energivare)) +
#   geom_line(size = 1) +
#   labs(title = 'Energiforbrug')
