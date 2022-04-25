library(tidyverse)
library(dplyr)
library(reshape2)
library(stringr)

setwd("C:/Users/abuxi/OneDrive/Desktop/Desktop/Work/Allianz/Spread Risk Control/v2/")


#list the filenames in the folder "csv files"
filenames = as.data.frame(dir(path = "./csv files", pattern = ".csv", full.names = TRUE, recursive = TRUE))
names(filenames)[1] = "Filename"

##############Position Tab Other Bonds##############
#Read the input csv files for Other Bonds
CQ_PositionData = read.csv(filenames$Filename[13])
PQ_PositionData = read.csv(filenames$Filename[5])

#Enrich Data CQ
CQ_PositionData$LookThrough_Flag = ifelse(substr(CQ_PositionData$Asset.ID,1,1) == "C" , "Y" , "N")
CQ_PositionData$SearchID = ifelse(CQ_PositionData$LookThrough_Flag == "Y",str_locate(pattern = "-", CQ_PositionData$Asset.ID) - 1,str_locate(pattern = "_", CQ_PositionData$Asset.ID) - 1)
CQ_PositionData$SearchID[is.na(CQ_PositionData$SearchID)] <- "NA"
CQ_PositionData$SearchID_2 = ifelse(CQ_PositionData$SearchID == "NA", nchar(CQ_PositionData$Asset.ID),CQ_PositionData$SearchID)
CQ_PositionData$Mod_Asset_ID = ifelse(CQ_PositionData$LookThrough_Flag == "N", substr(CQ_PositionData$Asset.ID,1,as.numeric(CQ_PositionData$SearchID_2)),substr(CQ_PositionData$Asset.ID,2,as.numeric(CQ_PositionData$SearchID_2)))


#Enrich Data PQ
PQ_PositionData$LookThrough_Flag = ifelse(substr(PQ_PositionData$Asset.ID,1,1) == "C" , "Y" , "N")
PQ_PositionData$SearchID = ifelse(PQ_PositionData$LookThrough_Flag == "Y",str_locate(pattern = "-", PQ_PositionData$Asset.ID) - 1,str_locate(pattern = "_", PQ_PositionData$Asset.ID) - 1)
PQ_PositionData$SearchID[is.na(PQ_PositionData$SearchID)] <- "NA"
PQ_PositionData$SearchID_2 = ifelse(PQ_PositionData$SearchID == "NA", nchar(PQ_PositionData$Asset.ID),PQ_PositionData$SearchID)
PQ_PositionData$Mod_Asset_ID = ifelse(PQ_PositionData$LookThrough_Flag == "N", substr(PQ_PositionData$Asset.ID,1,as.numeric(PQ_PositionData$SearchID_2)),substr(PQ_PositionData$Asset.ID,2,as.numeric(PQ_PositionData$SearchID_2)))


##Create a pivot of Market Value & Capital Charge using CQ data
CQ_PositionData_pivot_MV = dcast(CQ_PositionData, Mod_Asset_ID ~ Solo, value.var="Market.Value..MVi.", fun.aggregate=sum)
CQ_PositionData_pivot_CC = dcast(CQ_PositionData, Mod_Asset_ID ~ Solo, value.var="Capital.Charge", fun.aggregate=sum)
names(CQ_PositionData_pivot_MV)[2] = "Market_Val"
names(CQ_PositionData_pivot_CC)[2] = "Capital_Charge"
CQ_PositionData_pivot_MV = CQ_PositionData_pivot_MV[order(CQ_PositionData_pivot_MV$Mod_Asset_ID),]
CQ_PositionData_pivot_CC = CQ_PositionData_pivot_CC[order(CQ_PositionData_pivot_CC$Mod_Asset_ID),]
CQ_pivot = cbind(CQ_PositionData_pivot_MV,CQ_PositionData_pivot_CC)
CQ_pivot <- CQ_pivot[,-3] 
CQ_pivot$LookThrough_Flag = with(CQ_PositionData, LookThrough_Flag[match(CQ_pivot$Mod_Asset_ID, Mod_Asset_ID)]) #Lookup LT Flag


##Create a pivot of Market Value & Capital Charge using PQ data
PQ_PositionData_pivot_MV = dcast(PQ_PositionData, Mod_Asset_ID ~ Solo, value.var="Market.Value..MVi.", fun.aggregate=sum)
PQ_PositionData_pivot_CC = dcast(PQ_PositionData, Mod_Asset_ID ~ Solo, value.var="Capital.Charge", fun.aggregate=sum)
names(PQ_PositionData_pivot_MV)[2] = "Market_Val"
names(PQ_PositionData_pivot_CC)[2] = "Capital_Charge"
PQ_PositionData_pivot_MV = PQ_PositionData_pivot_MV[order(PQ_PositionData_pivot_MV$Mod_Asset_ID),]
PQ_PositionData_pivot_CC = PQ_PositionData_pivot_CC[order(PQ_PositionData_pivot_CC$Mod_Asset_ID),]
PQ_pivot = cbind(PQ_PositionData_pivot_MV,PQ_PositionData_pivot_CC)
PQ_pivot <- PQ_pivot[,-3] 
PQ_pivot$LookThrough_Flag = with(PQ_PositionData, LookThrough_Flag[match(PQ_pivot$Mod_Asset_ID, Mod_Asset_ID)]) #Lookup LT Flag


#Merge Asset ID columns of CQ & PQ data
x = as.data.frame(CQ_pivot$Mod_Asset_ID)
names(x)[1] = "Asset_ID"
y = as.data.frame(PQ_pivot$Mod_Asset_ID)
names(y)[1] = "Asset_ID"
Merged_PositionData = rbind(x,y)

#Remove duplicates
Merged_PositionData = as.data.frame(Merged_PositionData[!duplicated(Merged_PositionData), ])
names(Merged_PositionData)[1] = "Asset_ID"

#Enrich Data
Merged_PositionData$Rating_PQ = with(PQ_PositionData, Rating[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ Rating
Merged_PositionData$Rating_CQ = with(CQ_PositionData, Rating[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup CQ Rating
Merged_PositionData$Rating_PQ[is.na(Merged_PositionData$Rating_PQ)] <- "NA"
Merged_PositionData$Rating_CQ[is.na(Merged_PositionData$Rating_CQ)] <- "NA"

Merged_PositionData$MV_PQ = with(PQ_pivot, Market_Val[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ Market Value
Merged_PositionData$MV_CQ = with(CQ_pivot, Market_Val[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup CQ Market Value

Merged_PositionData$CC_PQ = with(PQ_pivot, Capital_Charge[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ Capital Charge
Merged_PositionData$CC_CQ = with(CQ_pivot, Capital_Charge[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup CQ Capital Charge

Merged_PositionData$LT_Flag_PQ = with(PQ_pivot, LookThrough_Flag[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ LT Flag
Merged_PositionData$LT_Flag_CQ = with(CQ_pivot, LookThrough_Flag[match(Merged_PositionData$Asset_ID, Mod_Asset_ID)]) #Lookup PQ LT Flag

Merged_PositionData$MV_PQ[is.na(Merged_PositionData$MV_PQ)] <- 0
Merged_PositionData$CC_PQ[is.na(Merged_PositionData$CC_PQ)] <- 0
Merged_PositionData$MV_CQ[is.na(Merged_PositionData$MV_CQ)] <- 0
Merged_PositionData$CC_CQ[is.na(Merged_PositionData$CC_CQ)] <- 0
Merged_PositionData$LT_Flag_PQ[is.na(Merged_PositionData$LT_Flag_PQ)] <- "NA"
Merged_PositionData$LT_Flag_CQ[is.na(Merged_PositionData$LT_Flag_CQ)] <- "NA"

Merged_PositionData$deltaMV = Merged_PositionData$MV_CQ - Merged_PositionData$MV_PQ #Calculate change in Market Value from PQ to CQ
Merged_PositionData$deltaCC = Merged_PositionData$CC_CQ - Merged_PositionData$CC_PQ #Calculate change in Capital Charge from PQ to CQ

Merged_PositionData$CheckRating = ifelse(Merged_PositionData$Rating_PQ == "NA" |Merged_PositionData$Rating_CQ == "NA" , "Missing Asset ID", ifelse(Merged_PositionData$Rating_PQ == Merged_PositionData$Rating_CQ, "No Change in Rating","Change in Rating")) #If rating differs, flag as Change in Rating
Merged_PositionData$Check_LT_Flag = ifelse(Merged_PositionData$LT_Flag_PQ == "NA" |Merged_PositionData$LT_Flag_CQ == "NA" , "Missing Asset ID", ifelse(Merged_PositionData$LT_Flag_PQ == Merged_PositionData$LT_Flag_CQ, "No Change in LT_Flag","Change in LT_Flag")) #If rating differs, flag as Change in Rating

Merged_PositionData$CC_MV_Ratio_PQ = as.numeric(Merged_PositionData$CC_PQ/Merged_PositionData$MV_PQ)
Merged_PositionData$CC_MV_Ratio_CQ = as.numeric(Merged_PositionData$CC_CQ/Merged_PositionData$MV_CQ)

Merged_PositionData$CC_MV_Ratio_PQ[is.na(Merged_PositionData$CC_MV_Ratio_PQ)] <- 0
Merged_PositionData$CC_MV_Ratio_CQ[is.na(Merged_PositionData$CC_MV_Ratio_CQ)] <- 0

Merged_PositionData$delta_CC_MV_ratio = Merged_PositionData$CC_MV_Ratio_CQ-Merged_PositionData$CC_MV_Ratio_PQ 

#Create file with only Asset IDs which were present in bth PQ & CQ for which the rating has changed
Merged_PositionData_ChangedRating = filter(Merged_PositionData, Check == "Change in Rating" )

#write outputs
write.csv(Merged_PositionData, paste('./Output/','Position_Tab_Other_Bonds_details.csv'))
write.csv(Merged_PositionData_ChangedRating, paste('./Output/','Position_Tab_Other_Bonds_rating_movement.csv'))

