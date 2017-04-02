library(MASS)

cluster1 <- parallel::makePSOCKcluster(2)
doParallel::registerDoParallel(cl=cluster1)
doParallel::registerDoParallel(cluster1)

#GamesToModel..(Add accordingly as season goes on)
nba16Oct <- foreach(i=25:31) %dopar% getNBAPlayerModel(modelDate = paste0("10_",i,"_2016"))
nba16Nov1 <- foreach(i=1:23) %dopar% getNBAPlayerModel(modelDate = paste0("11_",i,"_2016"))
nba16Nov2 <- foreach(i=25:30) %dopar% getNBAPlayerModel(modelDate = paste0("11_",i,"_2016"))
nba16Dec1 <- foreach(i=1:23) %dopar% getNBAPlayerModel(modelDate = paste0("12_",i,"_2016"))
nba16Dec2 <- foreach(i=25:30) %dopar% getNBAPlayerModel(modelDate = paste0("12_",i,"_2016"))

nba16Oct <- foreach(i=25:31) %do% read.csv(file=paste0("~/Desktop/NBA_Daily/10_",i,"_2016.csv"))
nba16Nov1 <- foreach(i=1:23) %do% read.csv(file=paste0("~/Desktop/NBA_Daily/11_",i,"_2016.csv"))
nba16Nov2 <- foreach(i=25:30) %do% read.csv(file=paste0("~/Desktop/NBA_Daily/11_",i,"_2016.csv"))
nba16Dec1 <- foreach(i=1:23) %do% read.csv(file=paste0("~/Desktop/NBA_Daily/12_",i,"_2016.csv"))
nba16Dec2 <- foreach(i=25:30) %do% read.csv(file=paste0("~/Desktop/NBA_Daily/12_",i,"_2016.csv"))


#CombineAllGames to one dataframe
nba16_17All <- bind_rows(nba16Oct,nba16Nov1,nba16Nov2,nba16Dec1,nba16Dec2,nba17Jan,nba17Feb1,nba17Feb2,nba17Mar)
nba16_17All <- na.zero(nba16_17All)
#Remove Identical Variables
nba16_17All_X <- nba16_17All %>% dplyr::select(-X,-Properties.ActualPoints,-ActualPoints)
#GetActual Points into one column
nba16_17All_Y <- nba16_17All$ActualPoints
#Remove all NAs
#nba16All_X <- na.zero(nba16All_X)
#Get rid of all non numerics in data frame
nba16_17All_X <- numericOnly(nba16_17All_X)
#Basic LM model,AIC Model, stepChoice Model, glm Model
nba16_17AllLm1 <- lm(nba16_17All_Y~.,data=nba16_17All_X)
nba16_17AllLm1Step <- stepAIC(nba16_17AllLm1,direction="both")
summary(nba16_17AllLm1Step)
save(nba16_17AllLm1,file="~/Desktop/NBA_Daily/nba16_17AllLm1.rda")
save(nba16_17AllLm1Step,file="~/Desktop/NBA_Daily/nba16_17AllLm1Step.rda")
newLm <- lm(nba16_17All_Y~ Salary + Properties.Ceiling + Properties.Floor +
              Properties.ProjPlusMinus + Properties.UsageProj + Properties.MinutesProj +
              Properties.FantasyPerMinute + Properties.PER + Properties.Usage +
              Properties.Trend + Properties.OppPlusMinus + Properties.PaceD +
              Properties.TrueShootingPct + Properties.PointsPerTouch +
              Properties.Touches + Properties.OppPts + Properties.Spread +
              Properties.Total + Properties.Month_Salary_Change + Properties.Season_PPG +
              Properties.Season_X2 + Properties.Season_Count + Properties.ProjPlusMinusPct +
              Properties.Consistency + Properties.Upside + Properties.Season_PPG_Percentile,data=nba16_17All_X)
summary(newLm)
save(newLm,file="~/Desktop/NBA_Daily/newLm.rda")

newLmStep <- stepAIC(newLm)
summary(newLmStep)
save(newLmStep,file="~/Desktop/NBA_Daily/newLmStep.rda")

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

