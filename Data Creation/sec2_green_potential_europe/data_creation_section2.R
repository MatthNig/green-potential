## Datengenerierung für Web-App "green potential" / CR fully functional 20.5.2020


############# load packages ############# 
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

############# set path #############
mainDir1 <- "C:/Users/nigmat01/Dropbox"
mainDir2 <- "C:/Users/Matthias/Dropbox"
mainDir3 <- "C:/Users/christian rutzer/Dropbox"
if (file.exists(mainDir1)==T){
        path <- mainDir1}else{
                if (file.exists(mainDir2)){
                        path <- mainDir2}else{
                                path <- mainDir3}
        }

## Load data of green potential estimates
lasso.pred.table <- read.csv2(paste0(path, "/NFP 73/Output/paper green potential in europe/Daten/lasso_three_isco.csv"))

## Following data has to saved on the wwz-shared driver due to privacy and security contract with the BFS 
dat.2011 <- readRDS("X:/_shared/Projekt - Green Open Economy/dat.2011.ml.RDS")
dat.2011$num.new <- 1
setDT(dat.2011)[, ges := sum(num.new), list(isco, iso, year, emp)]
dat.2011 <- mutate(dat.2011, y.2012 = case_when(emp == 1 & year == 2012 & ges >= 20 ~ 1, TRUE ~ 0), y.2016 = case_when(emp == 1 & year == 2016 & ges >= 20 ~ 1, TRUE ~ 0))
setDT(dat.2011)[, test.20 := y.2012[year == 2012]*y.2016, list(isco, iso)]
setDT(dat.2011)[, test.2012.2016 := sum(test.20), list(isco, iso)]
dat.2011 <- filter(dat.2011, test.2012.2016 >= 1) %>% dplyr::select(-ges, -y.2012, -y.2016,-test.20,-test.2012.2016)

##########################################################################################################
## Add informtion on employment at the sector level. Not implemented so far in the web output / 20.5.2020
# dat.2011 <- subset(dat.2011,	!(iso == "DE" & year == 2011))
# dat.2011 <- mutate(dat.2011, migr = case_when(iso != "CH" & as.numeric(migr) > 0 & as.numeric(migr) < 11 | iso == "CH" & as.numeric(migr) > 0 & as.numeric(migr) < 3652.5 ~ "yes", TRUE ~ "no"))
# dat.2011 <- mutate(dat.2011, sector = case_when(sector %in% c("A", "B") ~ "Resources Sector",
# 																								sector %in% c("C") ~ "Manufacturing Sector",
# 																								sector %in% c("D", "E", "F", "DE") ~ "Energy & Construction",
# 																								T ~ "Services Sector")) 
##########################################################################################################

dat.2011 <- filter(dat.2011, !(isco %in% c("-5", "00", "010", "011", "021", "031")))
dat.2011 <- mutate(dat.2011, isco = as.character(isco))
lasso.pred.table <- mutate(lasso.pred.table, three = as.character(three), lasso.ohne.weight = as.numeric(as.character(lasso.ohne.weight)), lasso.econ.weight = as.numeric(as.character(lasso.econ.weight)))

dat_agg <- aggregate(num ~ isco + iso + year + sector, data = filter(dat.2011, age < 68 & year == 2016), FUN = sum) 
dat_agg <- left_join(dat_agg, lasso.pred.table, by = c("isco" = "three"))
dat_agg <- filter(dat_agg, is.na(lasso.econ.weight) != T)

share_green <- function(cut_off){
dat_agg <- mutate(dat_agg, green = ifelse(lasso.econ.weight >= cut_off, "green", "non.green"))
dat_agg_tot <- aggregate(num ~ iso + year + green, FUN = sum, data = dat_agg)
dat_agg_tot <- dcast(dat_agg_tot, iso + year ~ green, value.var = c("num"))
dat_agg_tot <- mutate(dat_agg_tot, share_green = green / (green + non.green), cut_off = cut_off)
return(dat_agg_tot)
}

dat_fin <- do.call(rbind, lapply(seq(0.4, 0.8, .05), function(x) share_green(x)))
dat_fin <- mutate(dat_fin, iso = countrycode(iso, "iso2c", "iso3c"))
dat_fin$iso[is.na(dat_fin$iso) == T] <- "GBR" 

## Add countries shown in the map but having not green potential value 
c_added <- as.data.frame(cbind(do.call(rbind, replicate(length(unique(dat_fin$cut_off)), as.matrix(data.frame(iso = countrycode(c("EE","PL","BY","LV","LU","UA","RS","BA","HR","AL","BG","RO","SI","ME","MD","MK","LT"), "iso2c", "iso3c"), share_green = NA)), simplify = FALSE)),
cut_off = rep(seq(0.4, 0.8, .05), length(unique(dat_fin$cut_off))))) %>% mutate(cut_off = as.numeric(as.character(cut_off)), share_green = as.numeric(as.character(share_green)))

dat_fin <- rbind.fill(dat_fin, c_added)

### Create data for EU map of green potential 
world_map <- map_data("world")
world_map <- mutate(world_map, iso = countrycode(region, "country.name.en", "iso3c"))
world_map[world_map$region=="Kosovo","iso"]<-"KOS"
world_map <- filter(world_map, !(region %in% c("Antarctica", "Greenland",
                                               "French Southern and Antarctic Lands")) &
                            !subregion %in% c("Ile d'Oleron","Svalbard","Jan Mayen"))
countries<-as.character(unique(dat_fin$iso))
countries<-c(countries,"KOS")
eu_map <- filter(world_map, iso %in% countries)

## Add map and green potential together
plot.data <- left_join(eu_map, dat_fin, by = "iso", all.y = T)
plot.data <- filter(plot.data, is.na(iso) != T) 
saveRDS(plot.data, paste0(getwd(), "/Report/data_section2.rds"))


