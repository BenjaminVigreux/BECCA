#
# Process general parameters
#

for (ncountry in 1:Ncountry) {
  
  N <- dim(data[[ncountry]])[1]
  
  # Age
  ind <- grep(Country[[ncountry]]$var_age,names(data[[ncountry]]))
  if (length(ind) > 0) {
    data[[ncountry]]$Age <- data[[ncountry]][,ind[1]]
    data[[ncountry]]$Age[data[[ncountry]]$Age==""]=NA
    data[[ncountry]]$Age <- factor(data[[ncountry]]$Age) # Eventually remove level ""
    # Levels of Age ordered alphabetically, therefore under 17 is the last onstead of first
    # Eventually move it as the first level
    indlev <- which(levels(data[[ncountry]]$Age) == "under 17")
    if (length(indlev) >0) {
      levnew <- levels(data[[ncountry]]$Age)
      levnew <- c(levnew[indlev],levnew[-indlev])
      data[[ncountry]]$Age <- factor(data[[ncountry]]$Age,levels=levnew)
    }
    # Change selected levels
    levels(data[[ncountry]]$Age)[levels(data[[ncountry]]$Age)=="60 and over"] <- "60 over"
    levels(data[[ncountry]]$Age)[levels(data[[ncountry]]$Age)=="65 and over"] <- "65 over"
  } else {
    data[[ncountry]]$Age <- factor(rep("",N))
  }
  
  # Gender
  
  ind <- grep(Country[[ncountry]]$var_gender,names(data[[ncountry]]))
  if (length(ind) > 0) {
    data[[ncountry]]$Gender <- data[[ncountry]][,ind[1]]
    data[[ncountry]]$Gender[data[[ncountry]]$Gender==""]=NA
    data[[ncountry]]$Gender <- factor(data[[ncountry]]$Gender) # Eventually remove level ""
    # Gender already sorted alphabetically as female male; change names as Female Male eventually
    levels(data[[ncountry]]$Gender)[levels(data[[ncountry]]$Gender)=="female"] <- "Female"
    levels(data[[ncountry]]$Gender)[levels(data[[ncountry]]$Gender)=="woman"] <- "Female"
    levels(data[[ncountry]]$Gender)[levels(data[[ncountry]]$Gender)=="male"] <- "Male"
    levels(data[[ncountry]]$Gender)[levels(data[[ncountry]]$Gender)=="man"] <- "Male"
    # Restore the alphabetical order Frmale, Male if changed by substitutions
    data[[ncountry]]$Gender <- factor(data[[ncountry]]$Gender,levels = c("Female","Male"))
  } else {
    data[[ncountry]]$Gender <- factor(rep("",N))
  }
  
  # Employment # Include most "Others" as new classes
  
  ind <- grep(Country[[ncountry]]$var_employment,names(data[[ncountry]]))
  if (length(ind) > 0) {
    data[[ncountry]]$Employment <- data[[ncountry]][,ind[1]]
    data[[ncountry]]$Employment[data[[ncountry]]$Employment==""]=NA
    data[[ncountry]]$Employment <- factor(data[[ncountry]]$Employment) # Eventually remove level ""
    # Simplify legends
    levels(data[[ncountry]]$Employment)[grep("non-salaried",levels(data[[ncountry]]$Employment))] <- "Non salaried"
    levels(data[[ncountry]]$Employment)[grep("other",levels(data[[ncountry]]$Employment))] <- "Other"
    levels(data[[ncountry]]$Employment)[grep("retired",levels(data[[ncountry]]$Employment))] <- "Retired"
    levels(data[[ncountry]]$Employment)[grep("professional",levels(data[[ncountry]]$Employment))] <- "Salaried"
    levels(data[[ncountry]]$Employment)[grep("unemployed",levels(data[[ncountry]]$Employment))] <- "Unemployed"
    levels(data[[ncountry]]$Employment)[grep("student",levels(data[[ncountry]]$Employment))] <- "Student"
    levels(data[[ncountry]]$Employment)[grep("self",levels(data[[ncountry]]$Employment))] <- "Self-employed"
  } else {
    data[[ncountry]]$Employment <- factor(rep("",N))
  }
  
  # Education
  
  ind <- grep(Country[[ncountry]]$var_education,names(data[[ncountry]]))
  if (length(ind) > 0) {
    data[[ncountry]]$Education <- data[[ncountry]][,ind[1]]
    data[[ncountry]]$Education[data[[ncountry]]$Education==""]=NA
    data[[ncountry]]$Education <- factor(data[[ncountry]]$Education) # Eventually remove level ""
    newlev <- levels(data[[ncountry]]$Education)
    # Simplify legends
    newlev[grep("primary",levels(data[[ncountry]]$Education))] <- "Primary"
    newlev[grep("secondary",levels(data[[ncountry]]$Education))] <- "Secondary"
    newlev[grep("technical/vocational",levels(data[[ncountry]]$Education))] <- "Technical"
    newlev[grep("professional",levels(data[[ncountry]]$Education))] <- "Technical"
    newlev[grep("no formal",levels(data[[ncountry]]$Education))] <- "No education"
    newlev[grep("none",levels(data[[ncountry]]$Education))] <- "No education"
    newlev[grep("masters",levels(data[[ncountry]]$Education))] <- "Post-grad master"
    newlev[grep("university",levels(data[[ncountry]]$Education))] <- "University"
    newlev[grep("PhD",levels(data[[ncountry]]$Education))] <- "PhD"
    newlev[grep("postgraduate other",levels(data[[ncountry]]$Education))] <- "Post-grad other"
    ind2 <- grep("incomplete",levels(data[[ncountry]]$Education))
    newlev[ind2] <- paste(newlev[ind2],"\n(incomplete)")
    ind2 <- grep("no degree",levels(data[[ncountry]]$Education))
    newlev[ind2] <- paste(newlev[ind2],"\n(incomplete)")
    levels(data[[ncountry]]$Education) <- newlev
  } else {
    data[[ncountry]]$Education <- factor(rep("",N))
  }
  
  # Live
  
  ind <- grep(Country[[ncountry]]$var_live,names(data[[ncountry]]))
  if (length(ind) > 0) {
    data[[ncountry]]$Live <- data[[ncountry]][,ind[1]]
    data[[ncountry]]$Live[data[[ncountry]]$Live==""]=NA
    data[[ncountry]]$Live <- factor(data[[ncountry]]$Live) # Eventually remove level ""
    # Simplify legends
    levels(data[[ncountry]]$Live)[grep("rural area",levels(data[[ncountry]]$Live))] <- "Rural"
    levels(data[[ncountry]]$Live)[grep("urban area",levels(data[[ncountry]]$Live))] <- "Urban"
  } else {
    data[[ncountry]]$Live <- factor(rep("",N))
  }
  
  # Marital
  
  ind <- grep(Country[[ncountry]]$var_marital,names(data[[ncountry]]))
  if (length(ind) > 0) {
    data[[ncountry]]$Marital <- data[[ncountry]][,ind[1]]
    data[[ncountry]]$Marital[data[[ncountry]]$Marital==""]=NA
    data[[ncountry]]$Marital <- factor(data[[ncountry]]$Marital)  # Eventually remove level ""
    # Simplify legends
    levels(data[[ncountry]]$Marital)[grep("divorced",levels(data[[ncountry]]$Marital))] <- "Divorced"
    levels(data[[ncountry]]$Marital)[grep("married with children",levels(data[[ncountry]]$Marital))] <- "Married w/children"
    levels(data[[ncountry]]$Marital)[grep("married without children",levels(data[[ncountry]]$Marital))] <- "Married w/o children"
    levels(data[[ncountry]]$Marital)[grep("widowed",levels(data[[ncountry]]$Marital))] <- "Widowed"
    levels(data[[ncountry]]$Marital)[grep("single",levels(data[[ncountry]]$Marital))] <- "Single"
    levels(data[[ncountry]]$Marital)[grep("other",levels(data[[ncountry]]$Marital))] <- "Other"
    # Put selected levels on 2 lines
    levels(data[[ncountry]]$Marital)[levels(data[[ncountry]]$Marital)=="Married w/children"] <- "Married\nw/children"
    levels(data[[ncountry]]$Marital)[levels(data[[ncountry]]$Marital)=="Married w/o children"] <- "Married\nw/o children"
  } else {
    data[[ncountry]]$Marital <- factor(rep("",N))
  }
  
  # Group
  
  ind <- grep(Country[[ncountry]]$var_group$V,names(data[[ncountry]]))
  if (length(ind) > 0) {
    ind <- ind[1]
    data[[ncountry]]$Group <- array(NA,N)
    for (ngroups in 1:length(Country[[ncountry]]$var_group$VV))
      data[[ncountry]]$Group[data[[ncountry]][,ind+ngroups-1]=="1"] <- Country[[ncountry]]$var_group$VV[ngroups]
    data[[ncountry]]$Group <- factor(data[[ncountry]]$Group)
    # Put selected levels on 2 lines
    levels(data[[ncountry]]$Group)[levels(data[[ncountry]]$Group)=="internally displaced persons"] <- "internally\ndisplaced persons"
    levels(data[[ncountry]]$Group)[levels(data[[ncountry]]$Group)=="resident/host"] <- "resident/\nhost"
  } else {
    data[[ncountry]]$Group <- factor(rep("",N))
  }
  
  
  
  # Feeling
  
  # Different countries have different field names for the Feeling, all starting with Q1.Feeling
  # First find the column matching "Q1."
  ind <- grep("Q1",names(data[[ncountry]]))
  
  data[[ncountry]]$Feeling <- data[[ncountry]][,ind[1]]
  # Change levels so that the first letter is capital
  levels(data[[ncountry]]$Feeling)[levels(data[[ncountry]]$Feeling)=="strongly negative"] <- "Strongly negative"
  levels(data[[ncountry]]$Feeling)[levels(data[[ncountry]]$Feeling)=="negative"] <- "Negative"
  levels(data[[ncountry]]$Feeling)[levels(data[[ncountry]]$Feeling)=="neutral"] <- "Neutral"
  levels(data[[ncountry]]$Feeling)[levels(data[[ncountry]]$Feeling)=="positive"] <- "Positive"
  levels(data[[ncountry]]$Feeling)[levels(data[[ncountry]]$Feeling)=="strongly positive"] <- "Strongly positive"
  data[[ncountry]]$Feeling[data[[ncountry]]$Feeling==""] <- NA
  Feeling_Nlev <- length(levels(data[[ncountry]]$Feeling)) # Numer of levels of Feeling
  # Some missing data generate a new level. Discard them
  if ("" %in% levels(data[[ncountry]]$Feeling))
    Feeling_Nlev <- Feeling_Nlev-1
  # Some countries have data only on 3 levels (strongly is lacking)
  # For uniformity revert Feeling to 3 levels
  if (Feeling_Nlev==3) {
    Lev <- Lev3
    clev <- clev3
  } else {
    if (Feeling_Nlev==5) {
      Lev <- Lev5
      clev <- clev5
    } else {
      print("Number of levels of Feeling different from 3 or 5")
    }
  }
  if (myLevs == 3) {
    if (Feeling_Nlev==5) {
      # Some countries have 5 levels of Feeling (include "strongly")
      # Convert "strongly negative" to "negative" and "strongly positive" to "positive"
      data[[ncountry]]$Feeling[data[[ncountry]]$Feeling=="Strongly negative"] <- "Negative"
      data[[ncountry]]$Feeling[data[[ncountry]]$Feeling=="Strongly positive"] <- "Positive"
      # Drop former "strongly" levels
      data[[ncountry]]$Feeling <- droplevels(data[[ncountry]]$Feeling)
    }
    # Set Nlev to 3
    Feeling_Nlev <- 3
    Lev <- Lev3
    clev <- clev3
  }
  if (myLevs == 5) {
    if (Feeling_Nlev==3) {
      levels(data[[ncountry]]$Feeling) <- Lev5
      # Some countries have only 3 levels of Feeling ("negative","neutral","positive")
      # Convert "negative" to "strongly negative" and "positive" to "strongly positive"
      data[[ncountry]]$Feeling[data[[ncountry]]$Feeling=="negative"] <- "strongly negative"
      data[[ncountry]]$Feeling[data[[ncountry]]$Feeling=="positive"] <- "strongly positive"
    }
    # Set Nlev to 5
    Feeling_Nlev <- 5
    Lev <- Lev5
    clev <- clev5
  }
  
  # Order the levels of Feeling in the right order (otherwise ordered alphabetically)
  data[[ncountry]]$Feeling <- factor(data[[ncountry]]$Feeling,levels=Lev)
  
  # Define numeric Feeling
  data[[ncountry]]$Feeling_num <- rep(NA,N)
  for (nlev in 1:Feeling_Nlev)
    data[[ncountry]]$Feeling_num[data[[ncountry]]$Feeling==levels(data[[ncountry]]$Feeling)[nlev]] <- clev[nlev]
  
  # Date
  
  ind <- grep("Date",names(data[[ncountry]]))
  # dum (the date and hour) is integer as coming from read.xlsx2
  dum <- data[[ncountry]][,ind[1]]
  # treated as character and then as numeric
  data[[ncountry]]$timedate_num <- as.numeric(as.character(dum))
  # Convert to day-time format
  # from p. 31 of the xlsx pdf guide https://cran.r-project.org/web/packages/xlsx/xlsx.pdf
  data[[ncountry]]$timedate <- as.POSIXct((data[[ncountry]]$timedate_num-25569)*86400, tz="GMT", origin="1970-01-01")
  
}


# Compute global data (all countries, needed by map plot) 

timedate_num_all <- numeric(0)
Country_all <- character(0)
Feeling_all <- numeric(0)

for (ncountry in 1:Ncountry){
  
  dumFeeling <- data[[ncountry]]$Feeling
  if (length(levels(dumFeeling)) == 3) {
    levels(dumFeeling)[levels(dumFeeling)=="Negative"] <- "Strongly negative"
    levels(dumFeeling)[levels(dumFeeling)=="Positive"] <- "Strongly positive"
  }
  Feeling_all <- c(Feeling_all,as.character(dumFeeling))
  timedate_num_all <- c(timedate_num_all,data[[ncountry]]$timedate_num)
  Country_all <- c(Country_all,rep(Country[[ncountry]]$country,length(data[[ncountry]]$Feeling)))
  
}







