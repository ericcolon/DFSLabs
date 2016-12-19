j <- 10
AllTE <- foreach(i=1:17) %dopar% getNflLabModel("2015",i,PositionID = "TE")
readTE <- foreach(i=1:17) %dopar% readNFLCSVs("2015",i,PositionID = "TE")
AllTE16<- foreach(i=1:j) %dopar% getNflLabModel("2016",i,PositionID = "TE")
readTE16 <- foreach(i=1:j) %dopar% readNFLCSVs("2016",i,PositionID = "TE")
TE <- bind_rows(readTE16)#,readTE)
#week <- 1    
TEModel <- getNflLabModel("2016",week,PositionID = "TE")
TEModel <- na.zero(readNFLCSVs("2016",week,PositionID = "TE"))
TEnums <- na.zero(TE[,sapply(TE,is.numeric)])
TEnums <- TEnums %>% select(-X,-Properties.ActualPoints)
yTE <- TEnums$ActualPoints
xTE <- TEnums %>% select(-ActualPoints)
                                                   

TEFit <- lm(yTE~.,data = xTE)
summary(TEFit)
TEFit <- step(TEFit)
summary(TEFit)
#addToPred <- min(TEFit$residuals)
TEP <- predict(TEFit,TEModel)
#TEP <- TEP-addToPred
TEFile <- cbind(TEModel$Properties.Player_Name,TEP)
write.csv(TEFile, file = paste0("TEP",week,".csv"))
TEFile
