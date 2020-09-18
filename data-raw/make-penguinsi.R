
library(palmerpenguins)
library(usethis)
library(missForest)

# Base
penguinsi <- penguins
penguinsi <- as_tibble(missForest(as.data.frame(penguinsi))$ximp)

# Tidy version (for reference)
#library(dplyr)
#penguinsi <- penguins %>%
#  as.data.frame() %>%
#  missForest() %>%
#  `$`(ximp) %>%
#  as_tibble()

use_data(penguinsi)
