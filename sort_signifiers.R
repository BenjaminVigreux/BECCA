sigtypes <- function(data, unpack = c(TRUE, FALSE)) {
  
  # Function to sort the signifiers into separate dataframes, according to type.
  # Version 3.
  
  stopifnot(!missing(data))
  if (missing(unpack))
    unpack <- FALSE
  
  triads <- data[, grep("^T[1-9]", names(data))]
  if (length(grep(".X$|.Y$", names(triads))))
    triads <- triads[, -grep(".X$|.Y$", names(data))]
  
  dyads <- data[, grep("^D[1-9]", names(data))]
  if (length(grep(".X$|.Y$", names(dyads))))
    dyads <- dyads[, -grep(".X$|.Y$", names(data))]
  
  if (length(grep("^S[1-9]", names(data))))
    stones <- data[, grep("^S[1-9]", names(data))]
  if (length(grep("^Q[1-9]", names(data))))
    questions <- data[, grep("^Q[1-9]", names(data))]
  if (length(grep("^DQ[1-9]", names(data))))
    descriptors <- data[, grep("^DQ[1-9]", names(data))]
  if (length(grep("^C[1-9]", names(data))))
    collector <- data[, grep("^C[1-9]", names(data))]
  if (length(grep("^R[1-9]", names(data))))
    respondent <- data[, grep("^R[1-9]", names(data))]
  
  # Texts and titles
  if (match("texts_org", names(data), nomatch = FALSE)) {
    texts_org <- data$texts_org
  } else {
    alt.names <- c("Your.experience", "FragmentEntry", "Describe.your.example.here")
    if (match(alt.names, names(data), nomatch = FALSE)) {
      texts_org <- data[, match(alt.names, names(data))]
    } else {
      cat(file = stderr(), "No texts found!")
    }
  }
  
  if (match("texts_eng", names(data), nomatch = FALSE)) {
    texts_eng <- data$texts_eng
  } else {
    alt.names <- c("English", "FragEntryEng")
    if (match(alt.names, names(data), nomatch = FALSE))
      texts_eng <- data[, match(alt.names, names(data))]
  }
  
  if (match("titles_org", names(data), nomatch = FALSE)) {
    titles_org <- data$titles_org
  } else {
    alt.names <- c("StoryTitle", "Story.Title")
    if (match(alt.names, names(data), nomatch = FALSE))
      titles_org <- data[, match(alt.names, names(data))]
  }
  if (match("titles_eng", names(data), nomatch = FALSE)) {
    titles_org <- data$titles_eng
  } else {
    alt.names <- "EngTitle"
    if (match(alt.names, names(data), nomatch = FALSE))
      titles_org <- data[, match(alt.names, names(data))]
  }
  
  # Aggregate objects
  # out <- list(triads, dyads, stones, questions, descriptors, respondents, collector, texts_org, texts_eng, titles_org, titles_eng)
  objects <- ls()
  objects <- objects[-match(c("data", "unpack"), objects)]
  out <- mget(objects)
  
  if (unpack) {
    for (i in 1:length(objects)) {
      assign(objects[i], out[[i]], pos = parent.frame(n = 1))
    }
  } else {
      return(out)
    }
  }