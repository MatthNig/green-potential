## Datengenerierung für Web-App "green potential" part III green potential shortages isco / CR 13.5.2020

require(foreign)
require(dplyr)
require(ggplot2)
require(data.table)
require(plyr)
library(viridis)
library(Rmisc)
library(scales)
require(cowplot)
require(grid)
require(tidyverse)
require(ggalt)
library(glmnet)
library(kableExtra)
require(xtable)
require(countrycode)
library('scales')
require(rlist)
library("rworldmap")
library("maps")

rm(list = ls())
mainDir1 <- ""
mainDir2 <- "C:/Users/christian rutzer/Dropbox/NFP 73 (WWZ intern)/R Codes/Dynamic output"

if (file.exists(mainDir1)){
	setwd(file.path(mainDir1))
} else {
	if (file.exists(mainDir2)){
		setwd(file.path(mainDir2))
	}
}

## Read green potential of isco
isco_list <- read.csv2(paste0(getwd(), "/Data/sec3_green_potential_shortages_isco/isco_list.csv"))

## Read shortage indicator of isco from BSS
short <- read.csv2("C:/Users/christian rutzer/Dropbox/NFP 73/Output/paper green potential in europe/Daten/indicators_4digit.csv")
short <- dplyr::select(short, job, index1)
isco_list <- left_join(isco_list, short, by = c("ISCO" = "job"))
colnames(isco_list) <- c("ISCO", "Occupation", "Estimated Green Potential", "Shortage Indicator")

isco_list %>% saveRDS(paste0(getwd(), "/data_section3.RDS"))


