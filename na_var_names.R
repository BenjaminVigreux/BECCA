library(data.table)
org.data <- c("moldova", "kyrgyzstan", "unicef", "serbia", "tajikistan", "yemen")
wd <- getwd()

# Triads and Dyads
for (d in 1:length(org.data)) {
  # Load dataset
  load(paste0(wd, "/data/", org.data[d], "/data.RData"))
  # Get positions of variable names containing unknown characters
  unknown.char <- grep("U.[0-9]{4}", names(clean))
  # Of those, which are triads/dyads?
  tri.dyads <- grep("^T[1-9]|^D[1-9]", names(clean))
  both <- tri.dyads[match(unknown.char, tri.dyads)]
  both <- both[!is.na(both)]
  # Convert unknowns to ".NA"
  old.names <- names(clean)[both]
  if (length(old.names)) {
    new.names <- transpose(lapply(old.names, function (old.names) 
      paste0(strsplit(old.names, "\\.\\.")[[1]][1], ".NA")))[[1]]
    names(clean)[both] <- new.names
    # Save dataset
    save("clean", file = paste0(wd, "/data/", org.data[d], "/data.RData"))
  }
}

# Stones

for (d in 1:length(org.data)) {
  # Load dataset
  load(paste0(wd, "/data/", org.data[d], "/data.RData"))
  # Get positions of variable names containing unknown characters
  unknown.char <- grep("U.[0-9]{4}", names(clean))
  # Of those, which are stones?
  stone.names <- grep("^S[1-9]", names(clean))
  both <- stone.names[match(unknown.char, stone.names)]
  both <- both[!is.na(both)]
  # Convert unknowns to ".NA"
  old.names <- names(clean)[both]
  if (length(old.names)) {
    new.names <- transpose(lapply(old.names, function (old.names) {
      x <- strsplit(old.names, "\\.\\.")[[1]]
      paste0(x[1], "..", x[length(x)])
    }))[[1]]
    names(clean)[both] <- new.names
    # Save dataset
    save("clean", file = paste0(wd, "/data/", org.data[d], "/data.RData"))
  }
}

for (d in 1:length(org.data)) {
  # Load dataset
  load(paste0(wd, "/data/", org.data[d], "/data.RData"))
  stone.names <- grep("^S[1-9]", names(clean))
  new.names <- rep(NA, length(stone.names))
  for (i in 1:length(stone.names)) {
    old.names <- names(clean)[stone.names]
    if (length(old.names)) {
    x <- strsplit(old.names[i], "\\.\\.")[[1]]
    new.names[i] <- paste0(x[1], ".", ceiling(i/2), ".", x[length(x)])
    } else {
      new.names[i] <- old.names[i]
    }
  }
  names(clean)[stone.names] <- new.names
  save("clean", file = paste0(wd, "/data/", org.data[d], "/data.RData"))
}

for (d in 1:length(org.data)) {
  # Load dataset
  load(paste0(wd, "/data/", org.data[d], "/data.RData"))
  stone.names <- grep(NA, names(clean))
  names(clean)[stone.names] <- y[1:7]
  save("clean", file = paste0(wd, "/data/", org.data[d], "/data.RData"))
}