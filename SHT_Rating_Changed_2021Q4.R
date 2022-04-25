library(tidyverse)
library(dplyr)
library(reshape2)
library(stringr)

'Function to merge CQ & PQ files & manipulate the data'
Spread_Risk = function(CQ_PositionData,PQ_PositionData)
  {
  #Extract Asset ID for CQ data & add flag if look through applied
  CQ_PositionData = CQ_PositionData%>%
    mutate(LookThrough_Flag = ifelse(substr(CQ_PositionData$Asset.ID,1,1) == "C" , "Y" , "N"),
           SearchID = ifelse(LookThrough_Flag == "Y",str_locate(pattern = "-", Asset.ID) - 1,str_locate(pattern = "_", Asset.ID) - 1),
           SearchID2 = ifelse(is.na(SearchID),"NA",SearchID),
           SearchID_3 = ifelse(SearchID2 == "NA", nchar(Asset.ID),SearchID2),
           Initial_ID = ifelse(LookThrough_Flag == "N", substr(Asset.ID,1,as.numeric(SearchID_3)),substr(Asset.ID,3,as.numeric(SearchID_3))),
           search_new = ifelse(LookThrough_Flag == "Y",str_locate(pattern = "_", Initial_ID)-1 ,nchar(Initial_ID)),
           search_v2 = ifelse(is.na(search_new),nchar(Initial_ID),search_new),
           Mod_Asset_ID = substr(Initial_ID,1,search_v2))
           
                                 
  
  #Extract Asset ID for PQ data & add flag if look through applied
  PQ_PositionData = PQ_PositionData%>%
    mutate(LookThrough_Flag = ifelse(substr(PQ_PositionData$Asset.ID,1,1) == "C" , "Y" , "N"),
           SearchID = ifelse(LookThrough_Flag == "Y",str_locate(pattern = "-", Asset.ID) - 1,str_locate(pattern = "_", Asset.ID) - 1),
           SearchID2 = ifelse(is.na(SearchID),"NA",SearchID),
           SearchID_3 = ifelse(SearchID2 == "NA", nchar(Asset.ID),SearchID2),
           Initial_ID = ifelse(LookThrough_Flag == "N", substr(Asset.ID,1,as.numeric(SearchID_3)),substr(Asset.ID,3,as.numeric(SearchID_3))),
           search_new = ifelse(LookThrough_Flag == "Y",str_locate(pattern = "_", Initial_ID)-1 ,nchar(Initial_ID)),
           search_v2 = ifelse(is.na(search_new),nchar(Initial_ID),search_new),
           Mod_Asset_ID = substr(Initial_ID,1,search_v2))
  
  ##Create a pivot of Market Value & Capital Charge using CQ data
  CQ_pivot = CQ_PositionData%>%
    group_by(Mod_Asset_ID)%>%
    summarize(
      Market_Val = sum(Market.Value..MVi.),
      Capital_Charge = sum(Capital.Charge)
    )
  
  #lookup the look through flag for the asset id
  CQ_pivot$LookThrough_Flag = with(CQ_PositionData, LookThrough_Flag[match(CQ_pivot$Mod_Asset_ID, Mod_Asset_ID)]) #Lookup LT Flag
  CQ_pivot$Asset_Name = with(CQ_PositionData, Asset.Name[match(CQ_pivot$Mod_Asset_ID, Mod_Asset_ID)]) #Lookup LT Flag
  
  ##Create a pivot of Market Value & Capital Charge using PQ data
  PQ_pivot = PQ_PositionData%>%
    group_by(Mod_Asset_ID)%>%
    summarize(
      Market_Val = sum(Market.Value..MVi.),
      Capital_Charge = sum(Capital.Charge)
    )
  
  #lookup the look through flag for the asset id
  PQ_pivot$LookThrough_Flag = with(PQ_PositionData, LookThrough_Flag[match(PQ_pivot$Mod_Asset_ID, Mod_Asset_ID)]) #Lookup LT Flag
  PQ_pivot$Asset_Name = with(PQ_PositionData, Asset.Name[match(PQ_pivot$Mod_Asset_ID, Mod_Asset_ID)]) #Lookup LT Flag
  
  
  #Merge Asset ID columns of CQ & PQ data
  Merged_PositionData_1 = bind_rows(CQ_pivot, PQ_pivot)
  Merged_PositionData = as.data.frame(Merged_PositionData_1[,c("Mod_Asset_ID")])
  
  #Remove duplicates
  Merged_PositionData = as.data.frame(Merged_PositionData[!duplicated(Merged_PositionData), ])
  names(Merged_PositionData)[1] = "Asset_ID"
  
  #Enrich AssetName
  Merged_PositionData$Asset_Name = with(Merged_PositionData_1, Asset_Name[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)])
  
  #Enrich Data with the rating in CQ & PQ for an asset ID 
  Merged_PositionData$Rating_PQ = with(PQ_PositionData, Rating[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ Rating
  Merged_PositionData$Rating_CQ = with(CQ_PositionData, Rating[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup CQ Rating
  
  #If asset id missing in a quarter, rating is classified as NA
  Merged_PositionData$Rating_PQ[is.na(Merged_PositionData$Rating_PQ)] <- "NA"
  Merged_PositionData$Rating_CQ[is.na(Merged_PositionData$Rating_CQ)] <- "NA"
  
  #Lookup Market value in CQ & PQ
  Merged_PositionData$MV_PQ = with(PQ_pivot, Market_Val[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ Market Value
  Merged_PositionData$MV_CQ = with(CQ_pivot, Market_Val[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup CQ Market Value
  
  #Lookup Capital charge in CQ & PQ
  Merged_PositionData$CC_PQ = with(PQ_pivot, Capital_Charge[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ Capital Charge
  Merged_PositionData$CC_CQ = with(CQ_pivot, Capital_Charge[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup CQ Capital Charge
  
  #Lookup look through flag in CQ & PQ
  Merged_PositionData$LT_Flag_PQ = with(PQ_pivot, LookThrough_Flag[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ LT Flag
  Merged_PositionData$LT_Flag_CQ = with(CQ_pivot, LookThrough_Flag[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ LT Flag
  
  #Replace NA values
  Merged_PositionData$MV_PQ[is.na(Merged_PositionData$MV_PQ)] <- 0
  Merged_PositionData$CC_PQ[is.na(Merged_PositionData$CC_PQ)] <- 0
  Merged_PositionData$MV_CQ[is.na(Merged_PositionData$MV_CQ)] <- 0
  Merged_PositionData$CC_CQ[is.na(Merged_PositionData$CC_CQ)] <- 0
  Merged_PositionData$LT_Flag_PQ[is.na(Merged_PositionData$LT_Flag_PQ)] <- "NA"
  Merged_PositionData$LT_Flag_CQ[is.na(Merged_PositionData$LT_Flag_CQ)] <- "NA"
  
  #Change in Market Value & Capital charge
  Merged_PositionData = Merged_PositionData %>%
    mutate(deltaMV = MV_CQ - MV_PQ) %>%
    mutate(deltaCC = CC_CQ - CC_PQ) 
  
  #Check if rating & look through flag changed
  Merged_PositionData = Merged_PositionData%>%
    mutate(CheckRating = ifelse(Rating_PQ == "NA" |Rating_CQ == "NA" , "Missing Asset ID", ifelse(Rating_PQ == Rating_CQ, "No Change in Rating","Change in Rating")))%>% #If rating differs, flag as Change in Rating
    mutate(Check_LT_Flag = ifelse(LT_Flag_PQ == "NA" |LT_Flag_CQ == "NA" , "Missing Asset ID", ifelse(LT_Flag_PQ == LT_Flag_CQ, "No Change in LT_Flag","Change in LT_Flag"))) #If rating differs, flag as Change in Rating
  
  #Capital charge to Market Value ratio
  Merged_PositionData = Merged_PositionData%>%
    mutate(CC_MV_Ratio_PQ = as.numeric(CC_PQ/MV_PQ))%>%
    mutate(CC_MV_Ratio_CQ = as.numeric(CC_CQ/MV_CQ))
  
  Merged_PositionData$CC_MV_Ratio_PQ[is.na(Merged_PositionData$CC_MV_Ratio_PQ)] <- 0
  Merged_PositionData$CC_MV_Ratio_CQ[is.na(Merged_PositionData$CC_MV_Ratio_CQ)] <- 0
  
  #Change in CC to MV ratio
  Merged_PositionData = Merged_PositionData%>%
    mutate(delta_CC_MV_ratio = CC_MV_Ratio_CQ - CC_MV_Ratio_PQ )
  

}


#write outputs
#write.csv(Merged_PositionData, paste('./Output/','Position_Tab_Other_Bonds_details.csv'))


'----------------------------------------------------------------------------------------------------'
'Setup work environment, define input & output paramaters'
#Working directory
setwd("C:/Users/abuxi/OneDrive/Desktop/Desktop/Work/Allianz/Spread Risk Control/v2/")

#list the filenames in the folder "csv files"
filenames = as.data.frame(dir(path = "./csv files", pattern = ".csv", full.names = TRUE, recursive = TRUE))
names(filenames)[1] = "Filename"

#Input files for CQ 
CQ_Other_bonds = read.csv(filenames$Filename[13])
CQ_Covered_bonds = read.csv(filenames$Filename[9])
CQ_Non_EEA_Sov_bonds = read.csv(filenames$Filename[12])
CQ_EEA_Sov_bonds = read.csv(filenames$Filename[10])
CQ_Supranational_bonds = read.csv(filenames$Filename[16])
CQ_Local_Auth_bonds = read.csv(filenames$Filename[11])
CQ_STS_securities = read.csv(filenames$Filename[15])
CQ_Other_securities = read.csv(filenames$Filename[14])

#Input files for PQ 
PQ_Other_bonds = read.csv(filenames$Filename[5])
PQ_Covered_bonds = read.csv(filenames$Filename[1])
PQ_Non_EEA_Sov_bonds = read.csv(filenames$Filename[4])
PQ_EEA_Sov_bonds = read.csv(filenames$Filename[2])
PQ_Supranational_bonds = read.csv(filenames$Filename[8])
PQ_Local_Auth_bonds = read.csv(filenames$Filename[3])
PQ_STS_securities = read.csv(filenames$Filename[7])
PQ_Other_securities = read.csv(filenames$Filename[6])


'Output csv files'
#Other Bonds
write.csv(Spread_Risk(CQ_PositionData=CQ_Other_bonds,PQ_PositionData=PQ_Other_bonds), file = "./Output/Position_Tab_Other_Bonds_details.csv",row.names=FALSE)

#Covered Bonds
write.csv(Spread_Risk(CQ_PositionData=CQ_Covered_bonds,PQ_PositionData=PQ_Covered_bonds), file = "./Output/Position_Tab_Covered_Bonds_details.csv",row.names=FALSE)

#Non_EEA Bonds
write.csv(Spread_Risk(CQ_PositionData=CQ_Non_EEA_Sov_bonds,PQ_PositionData=PQ_Non_EEA_Sov_bonds), file = "./Output/Position_Tab_Non_EEA_Bonds_details.csv",row.names=FALSE)

#EEA Bonds
write.csv(Spread_Risk(CQ_PositionData=CQ_EEA_Sov_bonds,PQ_PositionData=PQ_EEA_Sov_bonds), file = "./Output/Position_Tab_EEA_Bonds_details.csv",row.names=FALSE)

#Supranational Bonds
write.csv(Spread_Risk(CQ_PositionData=CQ_Supranational_bonds,PQ_PositionData=PQ_Supranational_bonds), file = "./Output/Position_Tab_Supranational_Bonds_details.csv",row.names=FALSE)

#Local Author Bonds
write.csv(Spread_Risk(CQ_PositionData=CQ_Local_Auth_bonds,PQ_PositionData=PQ_Local_Auth_bonds), file = "./Output/Position_Tab_Local_Auth_Bonds_details.csv",row.names=FALSE)

#STS Securities
write.csv(Spread_Risk(CQ_PositionData=CQ_STS_securities,PQ_PositionData=PQ_STS_securities), file = "./Output/Position_Tab_STS_securities_details.csv",row.names=FALSE)

#Other Securities
write.csv(Spread_Risk(CQ_PositionData=CQ_Other_securities,PQ_PositionData=PQ_Other_securities), file = "./Output/Position_Tab_Other_securities_details.csv",row.names=FALSE)

