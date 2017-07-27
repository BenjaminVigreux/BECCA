# Save translated data the first time with
#
# datasave <- read.xlsx2("foi_Yemen_save.xlsx",1)
# Nsave <- length(datasave$FragmentID)
# save(datasave,Nsave,file="save_Yemen.RData")
#
#dataset <- c("kyrgyzstan", "moldova", "unicef", "serbia", "tajikistan", "yemen")
# print(dataset)
# 
# browser()
# for (d in 1:length(dataset)){
#   load(paste0(wd,"/clean_data/",dataset[d],"_clean.RData"))
#   dataset <- c("kyrgyzstan", "moldova", "unicef", "serbia", "tajikistan", "yemen")
#   cat(dataset)
#   print(cat(dataset))
#   assign(dataset[d],clean)
# }
# 
# browser()

data <- array(list(),Ncountry)
data[[1]] <- yemen
data[[2]] <- kyrgyzstan
data[[3]] <- moldova
data[[4]] <- tajikistan
data[[5]] <- serbia
data[[6]] <- unicef

# data <- array(list(),Ncountry)
# for (ncountry in 1:Ncountry){
#   data[[ncountry]] <- read.xlsx2(Country[[ncountry]]$fname,1)
#   N <- length(data[[ncountry]]$FragmentID)
#   print(paste(Country[[ncountry]]$country,"- Records:",N))
# 
#   # Number of variables to be translated (list in Country[[ncountry]]$variables_to_transalte[nvar])
#   Nvarsave <- length(Country[[ncountry]]$variables_to_transalte)
#   
#   # Make variables to be translated as character instead of factors otherwise they
#   # cannot be updated because not included in previous factors
#   for (nvar in 1:Nvarsave) {
#     ind <- which(names(data[[ncountry]]) == Country[[ncountry]]$variables_to_transalte[nvar])
#     # print(paste(ind,Country[[ncountry]]$variables_to_transalte[nvar]))
#     data[[ncountry]][,ind] <- as.character(data[[ncountry]][,ind])
#   }
#   
#   fnamesave <- paste("save_",Country[[ncountry]]$country,".RData",sep="")
#   # flag newfile: check what to do with save file
#   # newfile = 0: Data not updated. Save file up to date. Do not touch it
#   # newfile = 1: Data updated. Translate and save new records
#   # newfile = 2: Data new or messed up. Translate and save all records
#   if (!file.exists(fnamesave)) {
#     newfile <- 2 # Save file does not exist. Build save file
#   } else {
#     load(fnamesave)
#     # Make variables to be translated as character instead of factors otherwise they
#     # cannot be updated because not included in previous factors
#     # Must be done variable by variable otherwise structure is lost
#     for (nvar in 1:(2*Nvarsave))
#       datasave[,nvar+1] <- as.character(datasave[,nvar+1])
#     if (Nsave == N){
#       newfile <- 0 # Size of saved data = size of data. Do not update save file
#     } else {
#       if (Nsave < N) {
#         newfile <- 1 # Size of saved data < size of data. Update save file
#       } else {
#         newfile <- 2  # Size of saved data > size of data. Rebuild save file
#       }
#     }
#   }
#   N1save <- Nsave+1 # First record to translate and save - newfile = 0,1
#   N2save <- N # Last record to translate and save - newfile = 0,1
#   if (newfile == 2) {
#     N1save <- 1 # First record to translate and save - newfile = 2
#   }
#   
#   # Translate records from N1save to N2save
#   
#   if (N2save >= N1save){
#     
#     # Locate variables in data[[ncountry]]
#     # First add the new fragments for a future use (search for really new records)
#     # Note that fragments are always the first variable
#     datasave[N1save:N2save,1] <- data[[ncountry]][N1save:N2save,1]
#     # Then add actual translations
#     for (nvar in 1:Nvarsave) {
#       ind <- which(names(data[[ncountry]]) == Country[[ncountry]]$variables_to_transalte[nvar])
#       indsave <- which(names(datasave) == Country[[ncountry]]$variables_to_transalte[nvar])
#       print(paste(nvar,Country[[ncountry]]$variables_to_transalte[nvar],ind,indsave))
#       
#       
#       # fist add the original variables
#       datasave[N1save:N2save,indsave] <- data[[ncountry]][N1save:N2save,ind]
#       
#       # Then add the translations, record by record
#       for (nrec in N1save:N2save) {
#         dum <- translate(my_api_Yandex,data[[ncountry]][nrec,ind],"en")$text
#         print(paste(nvar,nrec,dum))
#         if (length(dum)>0)
#           datasave[nrec,(indsave+Nvarsave)] <- dum
#       }
#       
#     }
#     
#   } # End of if (N2save >= N1save){
#   
#   # Add all translations from datasave to data[[ncountry]]
#   # Note that the variables have the same name as the original ones with suffix 1 at the end
#   
#   Nvar <- length(data[[ncountry]])
#   for (nvar in 1:Nvarsave) { # FragID; Original vars; English vars
#     data[[ncountry]][,Nvar+nvar] <- datasave[,Nvarsave+1+nvar]
#     names(data[[ncountry]])[Nvar+nvar] <- paste(names(datasave)[nvar+1],".en",sep="")
#   }
#   
# # Finally write back the save xlsx file and the .RData
#   Nsave <- N
#   save(datasave,Nsave,file=fnamesave)
#   # write.xlsx2(datasave,paste("foi_",Country[[ncountry]]$country,"_save2.xlsx",sep=""))
#   
#   
# } # end of for (ncountry=1:Ncountry)
# 
