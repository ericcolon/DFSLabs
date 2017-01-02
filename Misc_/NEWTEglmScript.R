foreach(i=1:16) %dopar% getNflLabModel("2016",i,PositionID="TE")
readTE16 <- foreach(i=1:14) %dopar% readNFLCSVs("2016",i,PositionID = "TE")
TE <- bind_rows(readTE16)
week <- 17
TEModel <- getNflLabModel("2016",week,PositionID = "TE")
TEModel <- na.zero(readNFLCSVs("2016",week,PositionID = "TE"))
nfl_TEOwnP <- data.frame(c(TEModel$Properties.OwnRank))

TEX <- TE %>% select(-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)
TEY <- TE$ActualPoints
TEX <- na.zero(TEX)
TEglm <- glm(TEY~.,data=TEX)
TER2 <- 1-TEglm$deviance/TEglm$null.deviance
TER2
TEP <- predict(TEglm,TEModel)
naTE <- TEModel$Score

TEFile <- data.frame(TEModel$Properties.Player_Name,TEP,TEModel$Properties.Position,TEModel$Properties.Salary,TEModel$Properties.Salary/TEP,TEModel$ActualPoints,TEModel$Properties.Salary/TEModel$ActualPoints,nfl_TEOwnP,naTE,TEModel$Properties.ProjPlusMinus,TEModel$Properties.OppPlusMinus,TEModel$TeamName)
write.csv(TEFile, file = paste0("TEP",week,".csv"))
TEFile

