load("kyrgyzstan_clean.RData")
source("ds_preprocess.R")
source("var_table.R")
source("ds_builder.R")

data <- ds.process(clean)

rows <- NULL
cols <- NULL

r1 <- names(data)[113:123]
r2 <- names(data)[5]

c1 <- names(data)[53:57]

rows <- var.table(rows, r2, "Triads", range = c(1, 6))

cols <- var.table(cols, "Q1.Feeling", "Questions", c1, NULL)

stats <- ds.builder(rows, cols, data)

perc <- ds.builder(rows, cols, data, perc = TRUE)
