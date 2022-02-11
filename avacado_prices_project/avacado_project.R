library(tidyverse)
library(ggplot2)
avacado <- read_csv("C:/Users/Andrew/Desktop/CSC_324/CSC324Repo/avacado_prices_project/avocado.csv")

av_tib <- as_tibble(avacado)

filter(av_tib, AveragePrice > 1, region == "Albany")
filter(av_tib, region == "atlanta" | region == "Boise")

ggplot(av_tib, aes(x=year)) + geom_histogram()
