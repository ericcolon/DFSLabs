j <- 10
AllWR <- foreach(i=1:17) %dopar% getNflLabModel("2015",i,PositionID = "WR")
readWR <- foreach(i=1:17) %dopar% readNFLCSVs("2015",i,PositionID = "WR")
AllWR16<- foreach(i=1:j) %dopar% getNflLabModel("2016",i,PositionID = "WR")
readWR16 <- foreach(i=1:j) %dopar% readNFLCSVs("2016",i,PositionID = "WR")
WR <- bind_rows(readWR16)#,readWR)
#week <- 1
WRModel <- getNflLabModel("2016",week,PositionID = "WR")
WRModel <- na.zero(readNFLCSVs("2016",week,PositionID = "WR"))
WRnums <- na.zero(WR[,sapply(WR,is.numeric)])
WRnums <- WRnums %>% select(-X,-Properties.ActualPoints)
yWR <- WRnums$ActualPoints
xWR <- WRnums %>% select(-ActualPoints)
                                                      

WRFit <- lm(yWR~.,data = xWR)
summary(WRFit)
WRFit <- step(WRFit)
summary(WRFit)
#addToPred <- min(WRFit$residuals)
WRP <- predict(WRFit,WRModel)
#WRP <- WRP-addToPred
WRFile <- cbind(WRModel$Properties.Player_Name,WRP)
write.csv(WRFile, file = paste0("WRP",week,".csv"))
WRFile
