##  Dataset Specific Cleaning

##  Unicef every other row is empty.
unicef <- read.csv("foiunicef2015_Standard.csv", encoding = "UTF-8")
nr <- seq(from = 2, to = 2648, by = 2)
unicef <- unicef[-nr,]