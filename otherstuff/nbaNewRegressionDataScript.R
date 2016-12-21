library(MASS)

doParallel::registerDoParallel(cl=cluster1)
cluster1 <- parallel::makePSOCKcluster(3)
doParallel::registerDoParallel(cluster1)

#GamesToModel..(Add accordingly as season goes on)
nba16Oct_Phan <- foreach(i=25:31) %dopar% DFSLabs::getN(modelDate = paste0("10_",i,"_2016"),other = "779922", mName = "Phan")
nba16Nov1_Phan <- foreach(i=1:23) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("11_",i,"_2016"),other = "779922", mName = "Phan")
nba16Nov2_Phan <- foreach(i=25:30) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("11_",i,"_2016"),other = "779922", mName = "Phan")
nba16Dec_Phan <- foreach(i=1:18) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("12_",i,"_2016"),other = "779922", mName = "Phan")
#CombineAllGames to one dataframe
nba16All_Phan <- bind_rows(nba16Oct_Phan,nba16Nov1_Phan,nba16Nov2_Phan,nba16Dec_Phan)
#Remove Identical Variables
nba16All_PhanX <- nba16All_Phan %>% dplyr::select(-X,-Properties.ActualPoints,-ActualPoints)
#GetActual Points into one column
nba16All_PhanY <- nba16All_Phan$ActualPoints
#Remove all NAs
nba16All_PhanX <- na.zero(nba16All_PhanX)
#Get rid of all non numerics in data frame
nba16All_PhanX <- numericOnly(nba16All_PhanX)
#Basic LM model,AIC Model, stepChoice Model, glm Model
Phan1 <- lm(nba16All_PhanY~ Salary + Properties.Ceiling + Properties.Floor +
              Properties.ProjPlusMinus + Properties.UsageProj + Properties.MinutesProj +
              Properties.FantasyPerMinute + Properties.PER + Properties.Usage +
              Properties.Trend + Properties.OppPlusMinus + Properties.PaceD +
              Properties.TrueShootingPct + Properties.PointsPerTouch +
              Properties.Touches + Properties.OppPts + Properties.Spread +
              Properties.Total + Properties.Month_Salary_Change + Properties.Season_PPG +
              Properties.Season_X2 + Properties.Season_Count + Properties.ProjPlusMinusPct +
              Properties.Consistency + Properties.Upside + Properties.Season_PPG_Percentile,data=nba16All_PhanX)
summary(Phan1)

RFAIC1 <- stepAIC(Phan1,direction = "both")
summary(RFAIC1)
stepPhan1 <- step(Phan1,direction = "forward")
summary(stepPhan1)
glmPhan1 <- glm(nba16All_PhanY~.,data = nba16All_PhanX)
summary(glmPhan1)
glmPhan1rsqr <- 1-glmPhan1$deviance/glmPhan1$null.deviance
glmPhan1rsqr

#basic plots
plot(Phan1)
plot(RFAIC1)
plot(stepPhan1)
plot(glmPhan1)

saveRDS(Phan1,file = paste0("~/Documents/Phan.5885.rds"))
saveRDS(PhanAIC1,file = paste0("~/Documents/Phan.5878.rds"))
saveRDS(stepPhan1,file = paste0("~/Documents/Phan.5878.5861.rds"))
saveRDS(glmPhan1,file = paste0("~/Documents/Phan.5890409.rds"))

