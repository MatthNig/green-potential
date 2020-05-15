## Datengenerierung für Web-App "green potential" / CR 10.2.2020


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
mainDir1 <- "C:/Users/nigmat01/Dropbox/NFP 73 (WWZ intern)"
mainDir2 <- "C:/Users/Matthias/Dropbox/NFP 73 (WWZ intern)"
mainDir3 <- "C:/Users/christian rutzer/Dropbox/NFP 73 (WWZ intern)"
if (file.exists(mainDir1)==T){
        path <- mainDir1}else{
                if (file.exists(mainDir2)){
                        path <- mainDir2}else{
                                path <- mainDir3}
        }


lasso.pred.table <- read.csv2("C:/Users/christian rutzer/Dropbox/NFP 73/Output/paper green potential in europe/Daten/lasso_three_isco.csv")
dat.2011 <- readRDS("X:/_shared/Projekt - Green Open Economy/dat.2011.ml.RDS")
dat.2011$num.new <- 1
setDT(dat.2011)[, ges := sum(num.new), list(isco, iso, year, emp)]
dat.2011 <- mutate(dat.2011, y.2012 = case_when(emp == 1 & year == 2012 & ges >= 20 ~ 1, TRUE ~ 0), y.2016 = case_when(emp == 1 & year == 2016 & ges >= 20 ~ 1, TRUE ~ 0))
setDT(dat.2011)[, test.20 := y.2012[year == 2012]*y.2016, list(isco, iso)]
setDT(dat.2011)[, test.2012.2016 := sum(test.20), list(isco, iso)]
dat.2011 <- filter(dat.2011, test.2012.2016 >= 1) %>% dplyr::select(-ges, -y.2012, -y.2016,-test.20,-test.2012.2016)
# dat.2011 <- subset(dat.2011,	!(iso == "DE" & year == 2011))
# dat.2011 <- mutate(dat.2011, migr = case_when(iso != "CH" & as.numeric(migr) > 0 & as.numeric(migr) < 11 | iso == "CH" & as.numeric(migr) > 0 & as.numeric(migr) < 3652.5 ~ "yes", TRUE ~ "no"))
# dat.2011 <- mutate(dat.2011, sector = case_when(sector %in% c("A", "B") ~ "Resources Sector",
# 																								sector %in% c("C") ~ "Manufacturing Sector",
# 																								sector %in% c("D", "E", "F", "DE") ~ "Energy & Construction",
# 																								T ~ "Services Sector")) 
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

# setDT(dat_agg_tot)[, share := num[green == "green"] / (num[green == "green"] + num[green != "green"]), .(iso, year, sector)]
	
dat_fin <- do.call(rbind, lapply(seq(0.1, 0.9, 0.01), function(x) share_green(x)))
dat_fin <- mutate(dat_fin, iso = countrycode(iso, "iso2c", "iso3c"))
dat_fin$iso[is.na(dat_fin$iso) == T] <- "GBR" 

### Plot EU map greenness for conference in lucern / 21.1.2020
world_map <- map_data("world")
world_map <- mutate(world_map, iso = countrycode(region, "country.name.en", "iso3c"))
world_map[world_map$region=="Kosovo","iso"]<-"KOS"
world_map <- filter(world_map, !(region %in% c("Antarctica", "Greenland",
																							 "French Southern and Antarctic Lands")) &
											!subregion %in% c("Ile d'Oleron","Svalbard","Jan Mayen"))

eu_map <- filter(world_map, iso %in% dat_fin$iso)
plot.data <- left_join(eu_map, dat_fin, by = "iso", all.y = T)
plot.data <- filter(plot.data, is.na(iso) != T)
saveRDS(plot.data, "C:/Arbeit/Forschungsstelle/R Ideen/Markdown_shiny/green_pot_country.RDS")


