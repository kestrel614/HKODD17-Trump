#!/usr/bin/env Rscript

### Author: Yiming Li

### Text Mining with R (http://tidytextmining.com/) was extensively referred to in this project.

# install.packages("tidytext")
# install.packages("lubridate")
# install.packages("tidyr")
# install.packages("purrr")

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(qdapRegex)
library(tidyr)
library(scales)
library(purrr)
library(broom)


##### Trump on Android is angry in most afternoons
# By time, by device
