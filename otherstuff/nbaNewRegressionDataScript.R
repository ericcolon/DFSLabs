library(MASS)

#GamesToModel..(Add accordingly as season goes on)
nba16Oct_Lebron <- foreach(i=25:31) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("10_",i,"_2016"),other = "318717", mName = "Lebrontourage")
nba16Nov1_Lebron <- foreach(i=1:23) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("11_",i,"_2016"),other = "318717", mName = "Lebrontourage")
nba16Nov2_Lebron <- foreach(i=25:30) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("11_",i,"_2016"),other = "318717", mName = "Lebrontourage")
nba16Dec_Lebron <- foreach(i=1:18) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("12_",i,"_2016"),other = "318717", mName = "Lebrontourage")
#CombineAllGames to one dataframe
nba16All_Lebron <- bind_rows(nba16Oct_Lebron,nba16Nov1_Lebron,nba16Nov2_Lebron,nba16Dec_Lebron)
#Remove Identical Variables
nba16All_LebronX <- nba16All_Lebron %>% dplyr::select(-X,-Properties.ActualPoints,-ActualPoints)
#GetActual Points into one column
nba16All_LebronY <- nba16All_Lebron$ActualPoints
#Remove all NAs
nba16All_LebronX <- na.zero(nba16All_LebronX)
#Get rid of all non numerics in data frame
nba16All_LebronX <- numericOnly(nba16All_LebronX)
#Basic LM model,AIC Model, stepChoice Model, glm Model
Lebron1 <- lm(nba16All_LebronY~.,data=nba16All_LebronX)
summary(Lebron1)
RFAIC1 <- stepAIC(Lebron1)
summary(RFAIC1)
stepLebron1 <- step(Lebron1)
summary(stepLebron1)
glmLebron1 <- glm(nba16All_LebronY~.,data = nba16All_LebronX)
summary(glmLebron1)
glmLebron1rsqr <- 1-glmLebron1$deviance/glmLebron1$null.deviance
glmLebron1rsqr

#basic plots
plot(Lebron1)
plot(LebronAIC1)
plot(stepLebron1)
plot(glmLebron1)

saveRDS(Lebron1,file = paste0("~/Documents/Lebrontourage.5885.rds"))
saveRDS(LebronAIC1,file = paste0("~/Documents/Lebrontourage.5878.rds"))
saveRDS(stepLebron1,file = paste0("~/Documents/Lebrontourage.5878.5861.rds"))
saveRDS(glmLebron1,file = paste0("~/Documents/Lebrontourage.5890409.rds"))

