#%% LOAD LIBRARIES & SET DIRECTORIES
import numpy as np
import pandas as pd

def SAKE_path_fun(laufwerk):
    SAKE_path=laufwerk+":/Daten/"
    return SAKE_path

def dropbox_path_fun(user):
    dropbox_path="C:/Users/"+user+"/Dropbox/NFP 73 (WWZ intern)/Daten/"
    return dropbox_path

def shortage_path_fun(user):
    shortage_path="C:/Users/"+user+"/Dropbox/NFP 73/Output/paper green potential in europe/Daten/"
    return shortage_path

# specify the paths ----------------------------------
SAKE_path=SAKE_path_fun("X") # set the name where the SAKE data is stored on your computer
dropbox_path=dropbox_path_fun("Matthias") # set the user name of your machine
shortage_path=shortage_path_fun("Matthias")
print("Libraries are loaded and directories are set.")

#%% LOAD SAKE DATA

def data_loading(year):
    df=pd.read_csv(SAKE_path+"SAKE"+year+".csv",sep=";",na_values=-9, keep_default_na=True)
    df["year"]=year
    return df

SAKE_dat=data_loading("2019")

# summarize the data
print(SAKE_dat.head())
print("number of observations: ",len(SAKE_dat))
print("number of variables: ", len(SAKE_dat.columns))

#%% SUBSET TO SAKE VARIABLES OF INTEREST

##### sample weights in SAKE 2017:
# IXHHH         =  "(TOT) Gewichtung Hochrechnung Haushalt - Jahresdaten"                                                                               
# IXPXH         =  "(TOT) Gewichtung Hochrechnung Zielperson - Quartalsdaten"                                                                           
# IXPXHJ        =  "(TOT) Gewichtung Hochrechnung Zielperson - Jahresdaten"                                                                             
# IXPXM1        =  "(TOT) Gewichtung Zielperson - Europäisches Modul (1/3 der Welle 1)"                                                                 
# IXPXM2        =  "(TOT) Gewichtung Zielperson - Nationales Modul (2/3 der Welle 1)" 
# => choose IXPXHJ-Variable

##### Region variables:
# EM06             "UV (E) Geografische Lage des Betriebs: Kanton"
# B030          =  "(TOT) Raumplanungsregionen (RPR)" -> very disaggregated does not make sense
# B029          =  "(TOT) MS-Region (räumliche Mobilität) 2000" -> 106 categories i.e. also too disaggregated
# B027          =  "(TOT) Arbeitsmarktregion 2000" -> 16 categories
# B023          =  "(TOT) Grossregion der Wohngemeinde der Zielperson (NUTS II)" 7 categories
# => choose B023

#### Subset to "Erwerbstätige" (=B000) and retrieve the following information:
# BFU5I            "UV (E, L, EL, N) Ausgeübter Beruf: klassiert nach Berufsstruktur ISCO-08"       
# EM03          =  "(E) Wirtschaftliche Tätigkeit des Betriebs: gemäss BUR (NOGA 2008)"
# B023          =  "(TOT) Grossregion der Wohngemeinde der Zielperson (NUTS II)"
# IXPXHJ        =  "(TOT) Gewichtung Hochrechnung Zielperson - Jahresdaten"                                                                             


def subSAKE_fun(df):
    df=df.loc[df["B0000"]==1, ["BFU5I", "EM03", "B023", "IXPXHJ", "year"]]
    df.dropna(inplace = True)
    df.rename(columns = {"BFU5I":"isco","EM03":"NOGA","B023":"Region", "IXPXHJ": "Gewicht","year": "year"}, inplace=True)
    return df

SAKE_dat=subSAKE_fun(SAKE_dat)
print(SAKE_dat.head())
print("number of remaining observations :", len(SAKE_dat))

#%% MERGE WITH GREEN POTENTIAL

def load_green():
    df=pd.read_csv(dropbox_path+"ISCO/isco_list.csv",sep=";").loc[:,["isco","norm.lasso.task"]]
    df.rename(columns = {df.columns[1]: df.columns[1].replace(".","_")}, inplace=True)
    return df

green=load_green()
print("number of 4-digit ISCO occupations :", len(green))
SAKE_dat=SAKE_dat.merge(green,on="isco", how = "inner")

def data_proc(NOGAdigit, df):

    # create NOGA x-digit variable:
    digit_name="NOGA"+str(NOGAdigit)+"digit"
    
    # add a "0" to those NOGAs that are not already 8-digit
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
    Reg=list(pd.read_csv(dropbox_path[:-6]+"R Codes/Dynamic output/data_creation/sec3/SAKEGrossregion.txt",
                         encoding="latin",header=None)[0].astype(str))
    df["Region"].cat.rename_categories(Reg,inplace=True)

    return df

SAKE_dat=data_proc(NOGAdigit = 2, df = SAKE_dat)
print(SAKE_dat.head())

#%% MERGE WITH SHORTAGE INDICATORS

def shortage_fun(df_NOGA):
    shortage=pd.read_excel(shortage_path+"indicators-4digit.xlsx").loc[:, ["job", "index1"]]
    shortage.rename(columns={"job": "isco", "index1": "shortage_index"}, inplace=True)
    
    tmp=shortage["shortage_index"]
    shortage["shortage_index_norm"]=tmp.map(lambda x: (x-tmp.min())/(tmp.max()-tmp.min()))
    
    df_NOGA=pd.merge(df_NOGA, shortage, on = "isco", how="left")
    return df_NOGA
        


SAKE_dat=shortage_fun(SAKE_dat)
SAKE_dat.head()
len(SAKE_dat)
#%% WRITE CSV

#SAKE_dat.to_csv(dropbox_path+"erstellte daten/Greenness_Shortage_NOGA_Region.csv", sep=';')


