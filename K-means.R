options(stringsAsFactors = FALSE)

library(dplyr)
library(purrr)
library(cluster)
library(factoextra)
library(tidyverse)
library(skimr)

skim(transaction)

j = scale(transaction)

View(transaction)

