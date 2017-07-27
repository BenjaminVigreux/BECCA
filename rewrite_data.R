sets <- list.files(paste0(getwd(), "/data/"))
for (d in 1:length(sets)) {
  load(paste0(getwd(), "/data/", sets[d], "/data.RData"))
  write.csv(clean, file = paste0(getwd(), "/data/", sets[d], "/data.csv"), fileEncoding = "UTF-8", row.names = FALSE)
}
