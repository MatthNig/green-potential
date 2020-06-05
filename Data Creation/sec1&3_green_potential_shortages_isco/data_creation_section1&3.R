############################################
# Purpose: Use this Script to get the data #      
#          on green potential and shortages#
#          of occupations in the report    #
# Date:    05.06.2020                      #
# Authors: Matthias Niggli, CIEB/Uni Basel #
#          Christian Rutzer, CIEB/Uni Basel# 
############################################


############# load packages ############# 
library("dplyr")

############# set paths ############# 
# rm(list = ls())
# mainDir1 <- ""
# mainDir2 <- ""
# 
# if (file.exists(mainDir1)){
# 	setwd(file.path(mainDir1))
# } else {
# 	if (file.exists(mainDir2)){
# 		setwd(file.path(mainDir2))
# 	}
# }

## Read green potential and shortage of isco occupations
isco_list <- read.csv2(paste0(getwd(), "/Data Creation/sec1&3_green_potential_shortages_isco/isco_list.csv"))
colnames(isco_list) <- c("ISCO", "Occupation", "Estimated Green Potential", "Shortage Indicator")
isco_list$ISCO <- as.character(isco_list$ISCO)

## save a temporary data.frame with green potential only
isco_green <- select(isco_list, -`Shortage Indicator`)

## indicate ISCOs whose shortage is based on few SAKE observations
isco_shortage <- isco_list[complete.cases(isco_list), ]
isco_shortage$Occupation <- as.character(isco_shortage$Occupation)
few_obs_idx <- grep("\\(", isco_shortage$`Shortage Indicator`)
few_obs <- as.character(isco_list$Occupation[few_obs_idx])
few_obs <- paste(few_obs, "*", sep = "")
isco_shortage$Occupation[few_obs_idx] <- few_obs
isco_shortage$`Shortage Indicator` <- gsub("\\(", "", isco_shortage$`Shortage Indicator`)
isco_shortage$`Shortage Indicator` <- gsub("\\)", "", isco_shortage$`Shortage Indicator`)
isco_shortage$`Shortage Indicator` <- gsub(",", ".", isco_shortage$`Shortage Indicator`)
isco_shortage$`Shortage Indicator` <- as.numeric(isco_shortage$`Shortage Indicator`)

## save the list of data.frames in a list
isco_list <- list(isco_green = isco_green,
                  isco_shortage = isco_shortage)
isco_list %>% saveRDS(paste0(getwd(), "/Report/data_section1&3.RDS"))

