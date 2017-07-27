sigtypes <- function(data, unpack = c(TRUE, FALSE)) {
  
  # Function to sort the signifiers into separate dataframes, according to type.
  # Version 2.
  
  stopifnot(!missing(data))
  if (missing(unpack))
    unpack <- FALSE
  
  triads <- NULL
  dyads <- NULL
  stones <- NULL
  questions <- NULL
  descriptors <- NULL
  respondents <- NULL
  collector <- NULL
  texts_org <- NULL
  texts_eng <- NULL
  titles_org <- NULL
  titles_eng <- NULL
  
  triads <- data[, grep("^T[1-9]", names(data))]
  triads <- triads[, -grep(".X$|.Y$")]
  
  dyads <- data[, grep("^D[1-9]", names(data))]
  dyads <- dyads[, -grep(".X$|.Y$")]
  
  stones <- data[, grep("^S[1-9]", names(data))]
  questions <- data[, grep("^Q[1-9]", names(data))]
  descriptors <- data[, grep("^DQ[1-9]", names(data))]
  collector <- data[, grep("^C[1-9]", names(data))]
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
    titles_org <- titles_org
  } else {
    alt.names <- c("StoryTitle", "Story.Title")
    if (match(alt.names, names(data), nomatch = FALSE))
      titles_org <- data[, match(alt.names, names(data))]
  }
  if (match("titles_eng", names(data), nomatch = FALSE)) {
    titles_org <- titles_eng
  } else {
    alt.names <- "EngTitle"
    if (match(alt.names, names(data), nomatch = FALSE))
      titles_org <- data[, match(alt.names, names(data))]
  }
  
  # Aggregate objects
  out <- list(triads, dyads, stones, questions, descriptors, respondents, collector, texts_org, texts_eng, titles_org, titles_eng)
  
  if (unpack) {
    assign("triads", triads, pos = parent.frame(n=1))
    assign("dyads", dyads, pos = parent.frame(n=1))
    assign("stones", stones, pos = parent.frame(n=1))
    assign("questions", questions, pos = parent.frame(n=1))
    assign("descriptors", descriptors, pos = parent.frame(n=1))
    assign("respondent", respondent, pos = parent.frame(n=1))
    assign("collector", collector, pos = parent.frame(n=1))
    assign("texts_org", texts_org, pos = parent.frame(n=1))
    assign("texts_eng", texts_eng, pos = parent.frame(n=1))
    assign("titles_org", titles_org, pos = parent.frame(n=1))
    assign("titles_eng", titles_eng, pos = parent.frame(n=1))
  } else {
      return(out)
    }
  }