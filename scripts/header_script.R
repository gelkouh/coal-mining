library(tidyverse)
library(magrittr)
library(readxl)
library(stringr)
library(data.table)
library(reshape2)
library(lubridate)

NA_to_0 <- function(var) ifelse(is.na(var), 0, var)
