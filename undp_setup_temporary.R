Ncountry <- 5
Country <- array(list(), Ncountry)


# Country 1: Moldova
Country[[1]] <- list(country = "Moldova", countrycode = "MDA",
                     latmin = 28, latmax = 48.5, lonmin = 26.5, lonmax = 30.5,
                     variables_to_transalte = c("Your.experience",
                                                "StoryTitle",
                                                "Q4.Who.influenced..Other",
                                                "Q5.Story.involves..Other",
                                                "Q6.WhoImpacted..Other"))
# Country 2: Kyrgyzstan
Country[[2]] <- list(country = "Kyrgyzstan", countrycode = "KGZ", 
                     latmin = 39, latmax = 43.5, lonmin = 72, lonmax = 80.5,
                     variables_to_transalte = c("Your.experience",
                                                "StoryTitle",
                                                "Q4.Who.influenced..Other",
                                                "Q5.Story.involves..Other",
                                                "Q6.WhoImpacted..Other"))

# Country 3: Serbia
Country[[3]] <- list(country = "Serbia", countrycode = "SRB", 
                     latmin = 42, latmax = 46.5, lonmin = 18.5, lonmax = 23,
                     variables_to_transalte = c("Your.experience",
                                                "StoryTitle",
                                                "Q4.Who.influenced..Other",
                                                "Q5.Story.involves..Other",
                                                "Q6.WhoImpacted..Other"))

# Country 4: Tajikistan
Country[[4]] <- list(country = "Tajikistan", countrycode = "TJK", 
                     latmin = 36.5, latmax = 41.5, lonmin = 67, lonmax = 75.5,
                     variables_to_transalte = c("Describe.your.example.here",
                                                "lackingKnowledgeOther",
                                                "Q5.In.your.example.what.knowledge.were.people.lacking.most..select.up.to.2.._N.A",
                                                "ImprovedYourSituationOther",
                                                "sectorWomanEngagedInOther",
                                                "MCQ7_Others",
                                                "C4.Other.notes"))

# Country 5: Yemen
Country[[5]] <- list(country = "Yemen", countrycode = "YEM",
                     latmin = 12, latmax = 19, lonmin = 42, lonmax = 55,
                     variables_to_translate = c("Your.experience",
                                                "StoryTitle",
                                                "Q4.Who.influenced..Other",
                                                "Q5.Story.involves..Other",
                                                "Q6.WhoImpacted..Other",
                                                "DQ3.Employment.status..Other",
                                                "DQ7.Which.group.identify.with..Other"))
# Country 6: Unicef

Country[[6]] <- list(country = "Unicef", countrycode = "UNI",
                     latmin = 12, latmax = 19, lonmin = 42, lonmax = 55,
                     variables_to_translate = c("Your.experience",
                                                "StoryTitle",
                                                "Q4.Who.influenced..Other",
                                                "Q5.Story.involves..Other",
                                                "Q6.WhoImpacted..Other",
                                                "DQ3.Employment.status..Other",
                                                "DQ7.Which.group.identify.with..Other"))
# myLevs: Number of levels of Feeling: 3 or 5 (leave original #levels unaltered?)
myLevs <- 3
Lev3 <- c("negative","neutral","positive")
clev3 <- c(-1,0,1)
Lev5 <- c("strongly negative","negative","neutral","positive","strongly positive")
clev5 <- c(-1,-0.5,0,0.5,1)

# Api key for the Yandex translation service (Chiara)
my_api_Yandex <- "trnsl.1.1.20160706T224335Z.fe93198853798e28.5a7788a7b9d5bc7a65ac5a0e94f85c9923a743d5"

