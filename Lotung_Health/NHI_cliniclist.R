rm(list=ls())

NHI_clinic <- read.csv("NHI_clinic.csv", fileEncoding = "UTF-8")
ILan_clinic <- NHI_clinic[substr(NHI_clinic$地址,1,2) %in% "宜蘭",]
ILan_clinic <- ILan_clinic[nchar(as.character(ILan_clinic$終止合約或歇業日期)) == 0,]

ILan_clinic_D <- ILan_clinic[grepl("牙", ILan_clinic$醫事機構種類) %in% TRUE,]
ILan_clinic_W <- ILan_clinic[grepl("西", ILan_clinic$醫事機構種類) %in% TRUE,]
ILan_clinic_C <- ILan_clinic[grepl("中", ILan_clinic$醫事機構種類) %in% TRUE,]
ILan_clinic_G <- ILan_clinic[grepl("醫務", ILan_clinic$醫事機構種類) %in% TRUE,]
ILan_clinic_S <- ILan_clinic[grepl("專科", ILan_clinic$醫事機構種類) %in% TRUE,]
ILan_clinic_S <- ILan_clinic_S[grepl("牙", ILan_clinic_S$醫事機構種類) %in% FALSE,]

write.csv(ILan_clinic, "ILanNHIclinic.csv", row.names = FALSE, fileEncoding = "UTF-8")

#Add address and long lat
ILan_clinic <- read.csv("ILanNHIclinic.csv", fileEncoding = "UTF-8")

NHI_geocode0 <- read.csv("NHI_geocode.csv", fileEncoding = "UTF-8")
ILan_clinic <- ILan_clinic[,-c(6:8,10:12)]
NHI_geocode <- NHI_geocode0[1:321,]

ILan_clinic$FullAddre <- NHI_geocode$Response_Address
ILan_clinic$TOWN <- substr(as.character(NHI_geocode$Response_Address), 4,6)
ILan_clinic$VILL <- substr(as.character(NHI_geocode$Response_Address), 7,9)
ILan_clinic$Response_X <- NHI_geocode$Response_X
ILan_clinic$Response_Y <- NHI_geocode$Response_Y

colnames(ILan_clinic) <- c("ClinCode","ClinName","ClinType","Phone","Addre","Speci","FullAddre","Town","Vill","Response_X","Response_Y")

write.csv(ILan_clinic, "ILanClinicFull.csv", row.names = FALSE, fileEncoding = "UTF-8")



# Merge HomeDocGroup and ILanClinicAddre
HDGroup <- read.csv("ILanHomeDocGroup.csv", fileEncoding = "UTF-8")
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre$HomeDocGroupID <- ""
Addre$HomeDocGroupName <- ""
k <- 0
for(i in c(1:nrow(HDGroup))){
  ClinCode <- as.character(HDGroup$診所代號[i])
  j <- which(Addre$ClinCode == ClinCode)
  if (length(j) > 0){
    Addre$HomeDocGroupID[j] <- as.character(HDGroup$醫療群代號[i])
    Addre$HomeDocGroupName[j] <- as.character(HDGroup$醫療群名稱[i])
    k <- k + 1
  }
}

#Merge HomeInteCareGroup and ILanClinicAddre
HICare <- read.csv("HomeIntegratedCare.csv", fileEncoding = "UTF-8")
Addre$HomeInteCareGroupID1 <- ""
Addre$HomeInteCareGroupName1 <- ""
Addre$HomeInteCareGroupID2 <- ""
Addre$HomeInteCareGroupName2 <- ""
Addre$HomeInteCareGroupID3 <- ""
Addre$HomeInteCareGroupName3 <- ""
k <- 0
for(i in c(1:nrow(HICare))){
  ClinCode <- HICare$醫事機構代碼[i]
  j <- which(Addre$ClinCode == ClinCode)
  if ((length(j) > 0) & (substr(HICare$醫事機構地址[i], 1, 2)=="宜蘭")){
    k <- k + 1
    if (Addre$HomeInteCareGroupID1[j] == ""){
      Addre$HomeInteCareGroupID1[j] <- as.character(HICare$整合團隊代碼[i])
      Addre$HomeInteCareGroupName1[j] <- as.character(HICare$整合團隊名稱[i])
    }else{
      if (Addre$HomeInteCareGroupID2[j] == ""){
        Addre$HomeInteCareGroupID2[j] <- as.character(HICare$整合團隊代碼[i])
        Addre$HomeInteCareGroupName2[j] <- as.character(HICare$整合團隊名稱[i])
      }else{
        Addre$HomeInteCareGroupID3[j] <- as.character(HICare$整合團隊代碼[i])
        Addre$HomeInteCareGroupName3[j] <- as.character(HICare$整合團隊名稱[i])
      }
    }
  }
}
write.csv(Addre, "Test.csv", row.names = FALSE, fileEncoding = "UTF-8")
