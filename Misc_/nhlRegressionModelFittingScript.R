nhlNov16S <- foreach(i=1:30) %dopar% getNhlPlayerModel(paste0("11_",i,"_2016"))
nhlDec16S <- foreach(i=1:23) %dopar% getNhlPlayerModel(paste0("12_",i,"_2016"))
nhlNov16S <- foreach(i=1:30) %do% readNHLCSVs(paste0("11_",i,"_2016"),modelPosition="S")
nhlDec16S <- foreach(i=1:23) %do% readNHLCSVs(paste0("12_",i,"_2016"),modelPosition="S")
nhl16AllS <- bind_rows(nhlNov16S,nhlDec16S)
nhl16AllSX <- nhl16AllS %>% select(-X,-ActualPoints,-Properties.ActualPoints)
nhl16AllSY <- nhl16AllS$ActualPoints
nhl16AllSX <- na.zero(nhl16AllSX)
nhl16AllSSlmFit <- glm(nhl16AllSY~.,data=nhl16AllSX)
summary(nhl16AllSglmFit)
nhlSglmR2 <- 1 - nhl16AllSglmFit$deviance/nhl16AllSglmFit$null.deviance
nhlSglmR2
nhl16AllSNums <- nhl16AllS[sapply(nhl16AllS,is.numeric)]
nhl16AllSX <- nhl16AllSNums %>% select(-X,-ActualPoints,-Properties.ActualPoints)
nhl16AllSX <- na.zero(nhl16AllSX)
nhl16AllSY <- nhl16AllSNums$ActualPoints
nhl16AllSlmfit <- lm(nhl16AllSY~.,data=nhl16AllSX)
summary(nhl16AllSlmfit)
nhl16AllSAICfit <- MASS::stepAIC(nhl16AllSlmfit,direction="both")
summary(nhl16AllSAICfit)
nhl12_22SDF <- readNHLCSVs(modelDate="12_22_2016","S")
nhl12_22SDF <- nhl12_22SDF[nhl12_22SDF$Properties.PositionId == 401,]
nhl12_22SDF <- na.zero(nhl12_22SDF)
nhl12_22SPredictionsAIC <- predict(nhl16AllSAICfit,nhl12_22SDF)
nhl12_22SActual_Predictions <- data.frame(nhl12_22SDF$Properties.Player_Name,nhl12_22SPredictionsAIC,nhl12_22SDF$ActualPoints,nhl12_22SDF$Salary,nhl12_22SDF$Salary/nhl12_22SPredictionsAIC,nhl12_22SDF$Salary/nhl12_22SDF$ActualPoints)
names(nhl12_22SActual_Predictions) <- c("Name","Projection","Actual","Salary","Proj$/Pt.","Actual$/Pt.")
write.csv(nhl12_22SActual_Predictions,file="~/Desktop/nhl_Daily/12_22_2016_S_Projections.csv")


