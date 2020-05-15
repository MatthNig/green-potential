############################################
# Purpose: Use this Script to get the data #      
#          on emissions (Eurostat),        #
#          trade (EZV) and GDP (OECD)      #
# Date:    15.05.2020                      #
# Authors: Matthias Niggli, CIEB/Uni Basel #
#          Christian Rutzer, CIEB/Uni Basel# 
############################################


############# load packages ############# 
library("dplyr")
library("OECD")
library("readxl")
library("httr")

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


############# load OECD value added data #############
# dataset_list<-get_datasets()
# oecd.info <- search_dataset("STAN", data = dataset_list)
# oecd.info<-get_data_structure("STANI4_2016")
# str(oecd.info)

year <- 2015 # 2015 data is the latest year with complete data
ValueAdded <- get_dataset("STANI4_2016", filter = list( c("CHE"),c("VALU")), 
                          start_time = year, end_time = year) 
ValueAdded <- ValueAdded %>% 
        select(IND, obsValue) %>%
        rename(NOGA2digit = IND, ValueAdded_MioCHF = obsValue)
ValueAdded <- filter(ValueAdded, nchar(NOGA2digit) == 3)
ValueAdded$NOGA2digit <- substr(ValueAdded$NOGA2digit, 2, 3)


############# load EUROSTAT greenhouse gas emission data #############
GHG_CH <- read.csv(paste(path,"/Daten/erstellte daten/EUROSTAT_GHG_CH.csv",
                         sep=""),
                   sep=",", na.strings = ":", dec = ".", header=T) # data is from 2013-2017 i.e. 5 year window
GHG_CH <- select(GHG_CH, NACE_R2, TIME, Value)
GHG_CH$NACE_R2 <- as.character(GHG_CH$NACE_R2)
GHG_CH$Value <- as.character(GHG_CH$Value)
GHG_CH$Value <- sapply(GHG_CH$Value, function(x)as.numeric(gsub(pattern = " ", replacement = "", x = x)))

# get values for multi-coded NOGA-classes ------------------------
multipleNOGAS <- GHG_CH[nchar(GHG_CH$NACE_R2) == 7, ]$NACE_R2
multipleNOGAS <- unique(multipleNOGAS)
multipleNOGAS <- multipleNOGAS[-length(multipleNOGAS)] # drop class for households

multiple_fun <- function(multipleNOGA){
        
        # create new codes
        industry <- substr(multipleNOGA, 1, 1)
        firstNOGA <- as.numeric(substr(x = multipleNOGA, 2, 3))
        lastNOGA <- as.numeric(substr(x = multipleNOGA, 6, 7))
        NACE_R2 <- c(firstNOGA:lastNOGA)
        NACE_R2 <- paste(industry, NACE_R2, sep = "")
        tmp <- data.frame(NACE_R2 = rep(NACE_R2, each = length(unique(GHG_CH$TIME))))
        
        # get GHG values and combine
        df <- GHG_CH[GHG_CH$NACE_R2 == multipleNOGA, c("TIME", "Value")]
        df <- df[rep(rownames(df),length(NACE_R2)), ]
        df <- cbind(tmp, df)
        
        return(df)
}


tmp <- lapply(multipleNOGAS, function(x) multiple_fun(x))
tmp <- bind_rows(tmp)

# Now combine and calculate averages of all NOGA2digit industries -----------------
GHG_CH <- GHG_CH[nchar(GHG_CH$NACE_R2) == 3, ]
GHG_CH <- rbind(GHG_CH, tmp)
tmp <- NULL
GHG_CH <- GHG_CH %>%
        filter(is.na(Value) == FALSE) %>%
        group_by(NACE_R2) %>%
        summarise(GHG_emission_average = mean(Value, na.rm = TRUE))
GHG_CH <- rename(GHG_CH, NOGA2digit = NACE_R2)
GHG_CH$NOGA2digit <- substr(x = GHG_CH$NOGA2digit, 2, 3)

############# load 2017 trade data from EZV: #############

# EXPORTS: ----------------
url_Export <- "https://www.ezv.admin.ch/dam/ezv/en/dokumente/abgaben/Aussenhandelstatistik/Diffusion/Daten/NOGA/Exporte%20der%20Schweiz%20nach%20Wirtschaftzweigen%20und%20Unternehmensgr%C3%B6ssen.xlsx.download.xlsx/2_10_NOGA_Unternehmensgr%C3%B6ssen_EXP_en.xlsx"
temp_path <- tempfile(fileext = ".xlsx")
GET(url_Export, write_disk(temp_path))
df <- read_excel(path = temp_path, sheet = 1L, skip = 3, na = "*")
df <- df[-c(1:2),c("NOGA Code", "...4")]
names(df) <- c("NOGA", "Exports_MioCHF")

# IMPORTS: ---------------- 
url_Import <- "https://www.ezv.admin.ch/dam/ezv/en/dokumente/abgaben/Aussenhandelstatistik/Diffusion/Daten/NOGA/Importe%20der%20Schweiz%20nach%20Wirtschaftzweigen%20und%20Unternehmensgr%C3%B6ssen.xlsx.download.xlsx/2_10_NOGA_Unternehmensgr%C3%B6ssen_IMP_en.xlsx"
temp_path <- tempfile(fileext = ".xlsx")
GET(url_Import, write_disk(temp_path))
df1 <- read_excel(path = temp_path, sheet = 1L, skip = 3, na = "*")
df1 <- df1[-c(1:2),c("NOGA Code", "...4")]
names(df1) <- c("NOGA", "Imports_MioCHF")

# Combine imports and exports to trade volume at NOGA2digit-level: ---------------- 
df <- merge(df, df1, by = "NOGA")
df1 <- NULL
df <- df[complete.cases(df), ]
df <- df[nchar(df$NOGA)==3, ]
df$NOGA2digit <- substr(df$NOGA, 2, 3)
df$NOGA <- substr(df$NOGA, 1, 1)
df$Exports_MioCHF <- as.numeric(df$Exports_MioCHF)
df$Imports_MioCHF <- as.numeric(df$Imports_MioCHF)
df <- df %>% group_by(NOGA2digit) %>% summarise(Exports_MioCHF = mean(Exports_MioCHF, na.rm = TRUE),
                                                Imports_MioCHF = mean(Imports_MioCHF, na.rm = TRUE))
df$TradeVolume_MioCHF <- df$Exports_MioCHF + df$Imports_MioCHF

############# merge value added (OECD), trade data (EZV) and GHG emissions (Eurostat) together #############

df <- merge(df, ValueAdded, by= "NOGA2digit")
df$Handelbarkeit <- df$TradeVolume_MioCHF / df$ValueAdded_MioCHF # calculate Trade (Mio) / ValueAdded (Mio)
df$Handelbarkeit <- round(df$Handelbarkeit, digits = 3)
Handelbarkeit <- df
df <- NULL

Handelbarkeit <- merge(Handelbarkeit, GHG_CH, by = "NOGA2digit", all = TRUE)
Handelbarkeit$GHG_per_ValueAdded <- Handelbarkeit$GHG_emission_average / Handelbarkeit$ValueAdded_MioCHF # calculate GHG (T) / ValueAdded (Mio)
Handelbarkeit$GHG_per_ValueAdded <- round(Handelbarkeit$GHG_per_ValueAdded, digits = 3)
Handelbarkeit <- select(Handelbarkeit, NOGA2digit, Handelbarkeit, GHG_per_ValueAdded)
GHG_CH <- NULL
ValueAdded <- NULL

############# load employment and industry data (SAKE) with shortage indicators (from BSS): #############
Greenness_Shortage_ISCO_NOGA <- read.csv(paste(path, "/Daten/erstellte daten/Greenness_Shortage_NOGA_Region.csv", sep= ""),
                                         encoding = "UTF-8", sep=";", 
                                         header=T, row.names = 1)
NOGAS_NAMES <- read.csv2(paste(getwd(),"/Data/sec4_sectoral_imbalances_CH/NOGAsectorsTitle.csv",sep = ""),
                         stringsAsFactors = FALSE, colClasses = "character")[, 1:2]

############# define subsetting parameters: ############# 
# Regions:
REGIONEN <- as.character(unique(Greenness_Shortage_ISCO_NOGA$Region))
REGIONEN <- c("Schweiz", REGIONEN)

# industries codes to choose (subset to manufacturing and energy sector):
NOGAS <- unique(Greenness_Shortage_ISCO_NOGA$NOGA2digit)
NOGAS <- NOGAS[order(NOGAS, decreasing = FALSE)]
NOGAS <- NOGAS[NOGAS > 0]
NOGAS <- NOGAS[NOGAS < 36]

# add names of the industries to data.frame:
NOGAS_NAMES <- subset(NOGAS_NAMES, Codes %in% NOGAS) %>%
        rename(NOGA2digit = Codes)
NOGAS_NAMES$Kurztitel.2008..max..40.Zeichen.inkl..Leerstellen. <- paste(NOGAS_NAMES$NOGA2digit, 
                                                                        NOGAS_NAMES$Kurztitel.2008..max..40.Zeichen.inkl..Leerstellen., sep = " ")
NOGAS_NAMES <- rename(NOGAS_NAMES, NOGAS_NAMES = Kurztitel.2008..max..40.Zeichen.inkl..Leerstellen.)

#  cut-off values:
THRES <- seq(0.4, 0.8, by = 0.01)

############# create dataset: ############# 
# get helper functions:
source(paste(getwd(), "/Data/sec4_sectoral_imbalances_CH/functions_section4.R", sep=""))

# Note: for every possible green potential cut-off, the code below calculates the folowwing:
#       (see the code for the helper functions "weighting_fun" and "plot_data_fun" for further infos)
#       1) share of green jobs per industry and region
#       2) weighted shortage of these green jobs per industry and regions

df_list <- lapply(THRES, function(x) plot_data_fun(thres = x, Regionen = REGIONEN, NOGAs = NOGAS))
df_list <- lapply(df_list, as.data.frame)
for(e in 1:length(df_list)){
        df_list[[e]] <- mutate(df_list[[e]], THRES = THRES[e])
}

############# save the dataset for the ShinyApp: ############# 
# concated to a single data.frame and add NOGA_NAMES
df <- bind_rows(df_list)
df <- merge(df, NOGAS_NAMES, by = "NOGA2digit")
#saveRDS(df, paste(getwd(), "/Report/data_section4.rds", sep = ""))

