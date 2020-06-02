#%% LOAD LIBRARIES & SET DIRECTORIES
import numpy as np
import pandas as pd
import os

# repo_path = xxxxxxxxxxxxxxxxxxxx # state the path of of the repository
repo_path = "C:/Users/Matthias/Documents/GithubRepos/green-potential/"

os.chdir(repo_path)

def SAKE_path_fun(laufwerk):
    SAKE_path=laufwerk+":/Daten/"
    return SAKE_path
SAKE_path=SAKE_path_fun("X") # set the name where the SAKE data is stored on your computer
# def shortage_path_fun(user):
#     shortage_path="C:/Users/"+user+"/Dropbox/NFP 73/Output/paper green potential in europe/Daten/"
#     return shortage_path
# 

# specify the paths ----------------------------------

#dropbox_path=dropbox_path_fun("Matthias") # set the user name of your machine
#shortage_path=shortage_path_fun("Matthias") # set the user name of your machine
print("Libraries are loaded and directories are set.")

#%% LOAD SAKE DATA SUBSET TO VARIABLES OF INTEREST

def data_loading(year):
    df = pd.read_csv(SAKE_path+"SAKE"+year+".csv",sep=";",na_values=-9, keep_default_na = True)
    df["year"] = year
    return df

##### available sample weights in SAKE:
# IXHHH         =  "(TOT) Gewichtung Hochrechnung Haushalt - Jahresdaten"                                                                               
# IXPXH         =  "(TOT) Gewichtung Hochrechnung Zielperson - Quartalsdaten"                                                                           
# IXPXHJ        =  "(TOT) Gewichtung Hochrechnung Zielperson - Jahresdaten"                                                                             
# IXPXM1        =  "(TOT) Gewichtung Zielperson - Europäisches Modul (1/3 der Welle 1)"                                                                 
# IXPXM2        =  "(TOT) Gewichtung Zielperson - Nationales Modul (2/3 der Welle 1)" 
# => choose "IXPXHJ"-Variable

##### available region variables:
# EM06             "UV (E) Geografische Lage des Betriebs: Kanton"
# B030          =  "(TOT) Raumplanungsregionen (RPR)"
# B029          =  "(TOT) MS-Region (räumliche Mobilität) 2000"
# B027          =  "(TOT) Arbeitsmarktregion 2000"
# B023          =  "(TOT) Grossregion der Wohngemeinde der Zielperson (NUTS II)"
# => choose "B023" for analysis

#### Subset to "Erwerbstätige" (= "B000") and retrieve the following information:
# BFU5I            "UV (E, L, EL, N) Ausgeübter Beruf: klassiert nach Berufsstruktur ISCO-08"       
# EM03          =  "(E) Wirtschaftliche Tätigkeit des Betriebs: gemäss BUR (NOGA 2008)"
# B023          =  "(TOT) Grossregion der Wohngemeinde der Zielperson (NUTS II)"
# IXPXHJ        =  "(TOT) Gewichtung Hochrechnung Zielperson - Jahresdaten"                                                                             

def subSAKE_fun(df):
    df = df.loc[df["B0000"]==1, ["BFU5I", "EM03", "B023", "IXPXHJ", "year"]]
    df.dropna(inplace = True)
    df.rename(columns = {"BFU5I":"isco","EM03":"NOGA","B023":"Region", "IXPXHJ": "Gewicht","year": "year"}, inplace=True)
    return df


#SAKE_dat=data_loading("2019")
SAKE_dat = data_loading("2015")
SAKE_dat=subSAKE_fun(SAKE_dat)


for year in range(2016, 2020):
    df = data_loading(year = str(year))
    df = subSAKE_fun(df = df)
    SAKE_dat = pd.concat([SAKE_dat, df], axis = 0)
    
# summarize the data
print(SAKE_dat.head())
print("number of observations: ",len(SAKE_dat))
print("number of variables: ", len(SAKE_dat.columns))
print("available years: ", SAKE_dat.year.unique())

#%% MERGE WITH GREEN POTENTIAL

def load_green():
    # df=pd.read_csv(dropbox_path+"ISCO/isco_list.csv",sep=";").loc[:,["isco","norm.lasso.task"]]
    df=pd.read_csv(repo_path+"Report/isco_list.csv",sep=";").loc[:,["ISCO","green"]]
    df.rename(columns = {df.columns[1]: df.columns[1].replace(".","_")}, inplace=True)
    df.rename(columns = {df.columns[0]: "isco"}, inplace=True)
    return df

green=load_green()
print("number of 4-digit ISCO occupations :", len(green))
SAKE_dat=SAKE_dat.merge(green,on="isco", how = "left")

def data_proc(NOGAdigit, df):

    # create NOGA x-digit variable:
    digit_name="NOGA"+str(NOGAdigit)+"digit"
    
    # add a "0" to those NOGAs that are not already 6-digit (use 8 letters because of formating as a float)
    df["NOGA"] = df["NOGA"].astype(str)
    df["Digits"] = df["NOGA"].apply(lambda x: len(x))
    
    tmp = pd.Series(df["Digits"].value_counts().index)
    tmp = tmp[tmp > 4]
    tmp = tmp.sort_values(ascending = False)
        
    for d in tmp[1:]:
        fill_zeros = 8-d
        df.loc[df["Digits"] == d, "NOGA" ] = ("0"*fill_zeros)+df["NOGA"]
        
    # check if all NOGAS are 8 digit now:
    df["Digits"] = df["NOGA"].apply(lambda x: len(x))
    if len(list(df["Digits"].value_counts().index)) > 2:
        return print("There is an error with NOGAs")

    df = df.drop(["Digits"], axis = 1)
    df[digit_name]=pd.Series(df["NOGA"].astype(str)).apply(lambda x: x[0:(NOGAdigit)])

    # set correct names for the Grossregion from codes:
    df["Region"]=df["Region"].astype("category")
    df=df.loc[df["Region"].astype(int).isin(np.arange(1,8))==True,:]
    df["Region"].cat.remove_unused_categories(inplace=True)
    # Reg=list(pd.read_csv(dropbox_path[:-6]+"R Codes/Dynamic output/data_creation/sec3/SAKEGrossregion.txt",
    #                      encoding="latin",header=None)[0].astype(str))
    Reg=list(pd.read_csv(repo_path+"Data Creation/sec4_sectoral_imbalances_CH/SAKEGrossregion.txt",
                         encoding="latin",header=None)[0].astype(str))
    df["Region"].cat.rename_categories(Reg,inplace=True)

    return df

SAKE_dat=data_proc(NOGAdigit = 2, df = SAKE_dat)
print(SAKE_dat.head())

#%% MERGE WITH SHORTAGE INDICATORS

# !!! UPDATE AS SOON AS WE HAVE A NEW LIST FROM MICHAEL

def shortage_fun(df_NOGA):
    shortage=pd.read_excel("C:/Users/Matthias/Dropbox/NFP 73/Output/paper green potential in europe/Daten/indicators_4digit.xlsx")
    shortage = shortage.loc[:, ["job", "index1"]]
    shortage.rename(columns={"job": "isco", "index1": "shortage_index"}, inplace=True)
    
    tmp=shortage["shortage_index"]
    shortage["shortage_index_norm"]=tmp.map(lambda x: (x-tmp.min())/(tmp.max()-tmp.min()))
    
    df_NOGA=pd.merge(df_NOGA, shortage, on = "isco", how="left")
    return df_NOGA
        
SAKE_dat=shortage_fun(SAKE_dat)
SAKE_dat.head()
len(SAKE_dat)

#%% WRITE CSV

#SAKE_dat.to_csv(dropbox_path+"erstellte daten/Greenness_Shortage_NOGA_Region.csv", sep=';') # old version
SAKE_dat.to_csv("C:/Users/Matthias/Dropbox/NFP 73 (WWZ intern)/Daten/erstellte daten/Greenness_Shortage_NOGA_Region_AllYears.csv", sep=';')


