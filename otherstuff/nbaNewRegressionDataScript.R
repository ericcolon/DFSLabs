library(MASS)

doParallel::registerDoParallel(cl=cluster1)
cluster1 <- parallel::makePSOCKcluster(2)
doParallel::registerDoParallel(cluster1)

#GamesToModel..(Add accordingly as season goes on)
nba16Oct <- foreach(i=25:31) %dopar% getNBAPlayerModel(modelDate = paste0("10_",i,"_2016"))
nba16Nov1 <- foreach(i=1:23) %dopar% getNBAPlayerModel(modelDate = paste0("11_",i,"_2016"))
nba16Nov2 <- foreach(i=25:30) %dopar% getNBAPlayerModel(modelDate = paste0("11_",i,"_2016"))
nba16Dec1 <- foreach(i=1:23) %dopar% getNBAPlayerModel(modelDate = paste0("12_",i,"_2016"))
nba16Dec2 <- foreach(i=25:30) %dopar% getNBAPlayerModel(modelDate = paste0("12_",i,"_2016"))

nba16Oct <- foreach(i=25:31) %do% DFSLabs::readNBACSVs(modelDate=paste0("10_",i,"_2016"))
nba16Nov1 <- foreach(i=1:23) %do% DFSLabs::readNBACSVs(modelDate=paste0("11_",i,"_2016"))
nba16Nov2 <- foreach(i=25:30) %do% DFSLabs::readNBACSVs(modelDate=paste0("11_",i,"_2016"))
nba16Dec1 <- foreach(i=1:23) %do% DFSLabs::readNBACSVs(modelDate=paste0("12_",i,"_2016"))
nba16Dec2 <- foreach(i=25:30) %do% DFSLabs::readNBACSVs(modelDate=paste0("12_",i,"_2016"))


#CombineAllGames to one dataframe
nba16All_NewYear <- bind_rows(nba16Oct,nba16Nov1,nba16Nov2,nba16Dec1,nba16Dec2)
nba16All_NewYear <- na.zero(nba16All_NewYear)
#Remove Identical Variables
nba16All_X <- nba16All_NewYear %>% dplyr::select(-X,-Properties.ActualPoints,-ActualPoints)
#GetActual Points into one column
nba16All_Y <- nba16All_NewYear$ActualPoints
#Remove all NAs
#nba16All_X <- na.zero(nba16All_X)
#Get rid of all non numerics in data frame
nba16All_X <- numericOnly(nba16All_X)
#Basic LM model,AIC Model, stepChoice Model, glm Model
newYearlm <- lm(nba16All_Y~ Salary + Properties.Ceiling + Properties.Floor +
              Properties.ProjPlusMinus + Properties.UsageProj + Properties.MinutesProj +
              Properties.FantasyPerMinute + Properties.PER + Properties.Usage +
              Properties.Trend + Properties.OppPlusMinus + Properties.PaceD +
              Properties.TrueShootingPct + Properties.PointsPerTouch +
              Properties.Touches + Properties.OppPts + Properties.Spread +
              Properties.Total + Properties.Month_Salary_Change + Properties.Season_PPG +
              Properties.Season_X2 + Properties.Season_Count + Properties.ProjPlusMinusPct +
              Properties.Consistency + Properties.Upside + Properties.Season_PPG_Percentile,data=nba16All_X)
summary(newYearlm)

RFAIC1 <- stepAIC(newYearlm,direction = "both")
summary(RFAIC1)
step1 <- step(newYearlm,direction = "forward")
summary(step1)
glm1 <- glm(nba16All_Y~.,data = nba16All_X)
summary(glm1)
glm1rsqr <- 1-glm1$deviance/glm1$null.deviance
glm1rsqr

#basic plots
plot(newYearlm)
plot(RFAIC1)
plot(step1)
plot(glm1)

saveRDS(newYearlm,file = paste0("~/Documents/.5885.rds"))
saveRDS(AIC1,file = paste0("~/Documents/.5878.rds"))
saveRDS(step1,file = paste0("~/Documents/.5878.5861.rds"))
saveRDS(glm1,file = paste0("~/Documents/.5890409.rds"))

