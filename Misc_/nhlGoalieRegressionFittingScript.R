nhlNov16G <- foreach(i=1:30) %dopar% getNhlPlayerModel(paste0("11_",i,"_2016"))
nhlDec16G <- foreach(i=1:23) %dopar% getNhlPlayerModel(paste0("12_",i,"_2016"))
nhlNov16G <- foreach(i=1:30) %do% readNHLCSVs(paste0("11_",i,"_2016"),modelPosition="G")
nhlDec16G <- foreach(i=1:23) %do% readNHLCSVs(paste0("12_",i,"_2016"),modelPosition="G")
nhl16allG <- bind_rows(nhlNov16G,nhlDec16G)
nhl16allGX <- nhl16allG %>% select(-X,-ActualPoints,-Properties.ActualPoints)
nhl16allGY <- nhl16allG$ActualPoints
nhl16allGX <- na.zero(nhl16allGX)
nhl16allGglmFit <- glm(nhl16allGY~.,data=nhl16allGX)
summary(nhl16allGglmFit)
nhlGglmR2 <- 1 - nhl16allGglmFit$deviance/nhl16allGglmFit$null.deviance
nhlGglmR2
nhl16allGNums <- nhl16allG[sapply(nhl16allG,is.numeric)]
nhl16allGX <- nhl16allGNums %>% select(-X,-ActualPoints,-Properties.ActualPoints)
nhl16allGX <- na.zero(nhl16allGX)
nhl16allGY <- nhl16allGNums$ActualPoints
nhl16allGlmfit <- lm(nhl16allGY~.,data=nhl16allGX)
summary(nhl16allGlmfit)
nhl16allGAICfit <- MASS::stepAIC(nhl16allGlmfit,direction="both")
summary(nhl16allGAICfit)
nhl12_22GDF <- readNHLCSVs(modelDate="12_22_2016","G")
nhl12_22GDF <- nhl12_22GDF[nhl12_22GDF$Properties.PositionId == 402,]
nhl12_22GDF <- na.zero(nhl12_22GDF)
nhl12_22GPredictionsAIC <- predict(nhl16allGAICfit,nhl12_22GDF)
nhl12_22GActual_Predictions <- data.frame(nhl12_22GDF$Properties.Player_Name,nhl12_22GPredictionsAIC,nhl12_22GDF$ActualPoints,nhl12_22GDF$Salary,nhl12_22GDF$Salary/nhl12_22GPredictionsAIC,nhl12_22GDF$Salary/nhl12_22GDF$ActualPoints)
names(nhl12_22GActual_Predictions) <- c("Name","Projection","Actual","Salary","Proj$/Pt.","Actual$/Pt.")
write.csv(nhl12_22GActual_Predictions,file="~/Desktop/nhl_Daily/12_22_2016_G_Projections.csv")
