#register foreach parallel backend
library(doParallel)
myCluster = makeCluster(6)
registerDoParallel(myCluster)
clusterCall(myCluster, function() library(magrittr,dplyr))

#MLB2015SeasonModelsSave
mlb4_2015 <- foreach(i=26:30) %dopar% getMlbModel(modelDate=paste0("4_",i,"_2015"),cookie=mlbCookies,View="H")
mlb5_2015 <- foreach(i=1:31) %dopar% getMlbModel(modelDate=paste0("5_",i,"_2015"),cookie=mlbCookies,View="H")
mlb6_2015 <- foreach(i=1:30) %dopar% getMlbModel(modelDate=paste0("6_",i,"_2015"),cookie=mlbCookies,View="H")
mlb7_2015_1 <- foreach(i=1:12) %dopar% getMlbModel(modelDate=paste0("7_",i,"_2015"),cookie=mlbCookies,View="H")
mlb7_2015_2 <- foreach(i=17:31) %dopar% getMlbModel(modelDate=paste0("7_",i,"_2015"),cookie=mlbCookies,View="H")
mlb8_2015 <- foreach(i=1:31) %dopar% getMlbModel(modelDate=paste0("8_",i,"_2015"),cookie=mlbCookies,View="H")
mlb9_2015 <- foreach(i=1:30) %dopar% getMlbModel(modelDate=paste0("9_",i,"_2015"),cookie=mlbCookies,View="H")
mlb10_2015 <- foreach(i=1:4) %dopar% getMlbModel(modelDate=paste0("10_",i,"_2015"),cookie=mlbCookies,View="H")

#MLB2016SeasonModelsSave
mlb4_2016 <- foreach(i=3:30) %dopar% getMlbModel(modelDate=paste0("4_",i,"_2016"),cookie=mlbCookies,View="H")
mlb5_2016 <- foreach(i=1:31) %dopar% getMlbModel(modelDate=paste0("5_",i,"_2016"),cookie=mlbCookies,View="H")
mlb6_2016 <- foreach(i=1:30) %dopar% getMlbModel(modelDate=paste0("6_",i,"_2016"),cookie=mlbCookies,View="H")
mlb7_2016_1 <- foreach(i=1:10) %dopar% getMlbModel(modelDate=paste0("7_",i,"_2016"),cookie=mlbCookies,View="H")
mlb7_2016_2 <- foreach(i=15:31) %dopar% getMlbModel(modelDate=paste0("7_",i,"_2016"),cookie=mlbCookies,View="H")
mlb8_2016 <- foreach(i=1:31) %dopar% getMlbModel(modelDate=paste0("8_",i,"_2016"),cookie=mlbCookies,View="H")
mlb9_2016 <- foreach(i=1:30) %dopar% getMlbModel(modelDate=paste0("9_",i,"_2016"),cookie=mlbCookies,View="H")
mlb10_2016 <- foreach(i=1:2) %dopar% getMlbModel(modelDate=paste0("10_",i,"_2016"),cookie=mlbCookies,View="H")

#MLB2015SeasonModelsImport
mlb4_2015H <- foreach(i=26:30) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_4_",i,"_2015_DK_Hitters.csv"))
mlb4_2015P <- foreach(i=26:30) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_4_",i,"_2015_DK_Pitchers.csv"))

mlb5_2015H <- foreach(i=1:31) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_5_",i,"_2015_DK_Hitters.csv"))
mlb5_2015P <- foreach(i=1:31) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_5_",i,"_2015_DK_Pitchers.csv"))

mlb6_2015H <- foreach(i=1:30) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_6_",i,"_2015_DK_Hitters.csv"))
mlb6_2015P <- foreach(i=1:30) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_6_",i,"_2015_DK_Pitchers.csv"))

mlb7_2015_1H <- foreach(i=1:12) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_7_",i,"_2015_DK_Hitters.csv"))
mlb7_2015_1P <- foreach(i=1:12) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_7_",i,"_2015_DK_Pitchers.csv"))

mlb7_2015_2H <- foreach(i=17:31) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_7_",i,"_2015_DK_Hitters.csv"))
mlb7_2015_2P <- foreach(i=17:31) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_7_",i,"_2015_DK_Pitchers.csv"))

mlb8_2015H <- foreach(i=1:31) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_8_",i,"_2015_DK_Hitters.csv"))
mlb8_2015P <- foreach(i=1:31) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_8_",i,"_2015_DK_Pitchers.csv"))

mlb9_2015H <- foreach(i=1:30) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_9_",i,"_2015_DK_Hitters.csv"))
mlb9_2015P <- foreach(i=1:30) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_9_",i,"_2015_DK_Pitchers.csv"))

mlb10_2015H <- foreach(i=1:4) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_10_",i,"_2015_DK_Hitters.csv"))
mlb10_2015P <- foreach(i=1:4) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_10_",i,"_2015_DK_Pitchers.csv"))



#MLB2016SeasonModelsImport
mlb4_2016H <- foreach(i=3:30) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_4_",i,"_2016_DK_Hitters.csv"))
mlb4_2016P <- foreach(i=3:30) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_4_",i,"_2016_DK_Pitchers.csv"))

mlb5_2016H <- foreach(i=1:31) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_5_",i,"_2016_DK_Hitters.csv"))
mlb5_2016P <- foreach(i=1:31) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_5_",i,"_2016_DK_Pitchers.csv"))

mlb6_2016H <- foreach(i=1:30) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_6_",i,"_2016_DK_Hitters.csv"))
mlb6_2016P <- foreach(i=1:30) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_6_",i,"_2016_DK_Pitchers.csv"))

mlb7_2016_1H <- foreach(i=1:10) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_7_",i,"_2016_DK_Hitters.csv"))
mlb7_2016_1P <- foreach(i=1:10) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_7_",i,"_2016_DK_Pitchers.csv"))

mlb7_2016_2H <- foreach(i=15:31) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_7_",i,"_2016_DK_Hitters.csv"))
mlb7_2016_2P <- foreach(i=15:31) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_7_",i,"_2016_DK_Pitchers.csv"))

mlb8_2016H <- foreach(i=1:31) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_8_",i,"_2016_DK_Hitters.csv"))
mlb8_2016P <- foreach(i=1:31) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_8_",i,"_2016_DK_Pitchers.csv"))

mlb9_2016H <- foreach(i=1:30) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_9_",i,"_2016_DK_Hitters.csv"))
mlb9_2016P <- foreach(i=1:30) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_9_",i,"_2016_DK_Pitchers.csv"))

mlb10_2016H <- foreach(i=1:2) %do% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_10_",i,"_2016_DK_Hitters.csv"))
mlb10_2016P <- foreach(i=1:2) %dopar% read.csv(file=paste0("~/Desktop/MLB_Daily/MLB_10_",i,"_2016_DK_Pitchers.csv"))

#BindHitters
suppressWarnings(bind4_15H <- bind_rows(mlb4_2015H))
suppressWarnings(bind5_15H <- bind_rows(mlb5_2015H))
suppressWarnings(bind6_15H <- bind_rows(mlb6_2015H))
suppressWarnings(bind7_15H1 <- bind_rows(mlb7_2015_1H))
suppressWarnings(bind7_15H2 <- bind_rows(mlb7_2015_2H))
suppressWarnings(bind8_15H <- bind_rows(mlb8_2015H))
suppressWarnings(bind9_15H <- bind_rows(mlb9_2015H))
suppressWarnings(bind10_15H <- bind_rows(mlb10_2015H))
suppressWarnings(bind4_16H <- bind_rows(mlb4_2016H))
suppressWarnings(bind5_16H <- bind_rows(mlb5_2016H))
suppressWarnings(bind6_16H <- bind_rows(mlb6_2016H))
suppressWarnings(bind7_16H1 <- bind_rows(mlb7_2016_1H))
suppressWarnings(bind7_16H2 <- bind_rows(mlb7_2016_2H))
suppressWarnings(bind8_16H <- bind_rows(mlb8_2016H))
suppressWarnings(bind9_16H <- bind_rows(mlb9_2016H))
suppressWarnings(bind10_16H <- bind_rows(mlb10_2016H))

suppressWarnings(mlbAllHitters <- bind_rows(bind4_15H,bind5_15H,bind6_15H,bind7_15H1,bind7_15H2,bind8_15H,bind9_15H,bind10_15H,bind4_16H,bind5_16H,bind6_16H,bind7_16H1,bind7_16H2,bind8_16H,bind9_16H,bind10_16H))

#BindPitchers
suppressWarnings(bind4_15P <- bind_rows(mlb4_2015P))
suppressWarnings(bind5_15P <- bind_rows(mlb5_2015P))
suppressWarnings(bind6_15P <- bind_rows(mlb6_2015P))
suppressWarnings(bind7_15P1 <- bind_rows(mlb7_2015_1P))
suppressWarnings(bind7_15P2 <- bind_rows(mlb7_2015_2P))
suppressWarnings(bind8_15P <- bind_rows(mlb8_2015P))
suppressWarnings(bind9_15P <- bind_rows(mlb9_2015P))
suppressWarnings(bind10_15P <- bind_rows(mlb10_2015P))
suppressWarnings(bind4_16P <- bind_rows(mlb4_2016P))
suppressWarnings(bind5_16P <- bind_rows(mlb5_2016P))
suppressWarnings(bind6_16P <- bind_rows(mlb6_2016P))
suppressWarnings(bind7_16P1 <- bind_rows(mlb7_2016_1P))
suppressWarnings(bind7_16P2 <- bind_rows(mlb7_2016_2P))
suppressWarnings(bind8_16P <- bind_rows(mlb8_2016P))
suppressWarnings(bind9_16P <- bind_rows(mlb9_2016P))
suppressWarnings(bind10_16P <- bind_rows(mlb10_2016P))

suppressWarnings(mlbAllPitchers <- bind_rows(bind4_15P,bind5_15P,bind6_15P,bind7_15P1,bind7_15P2,bind8_15P,bind9_15P,bind10_15P,bind4_16P,bind5_16P,bind6_16P,bind7_16P1,bind7_16P2,bind8_16P,bind9_16P,bind10_16P))

#Setup DataFrames for regression modeling
mlbAllHitters <- na.zero(mlbAllHitters)
mlbAllPitchers <- na.zero(mlbAllPitchers)
mlbAllHitters_Nums <- mlbAllHitters[sapply(mlbAllHitters,is.numeric)]
mlbAllPitchers_Nums <- mlbAllPitchers[sapply(mlbAllPitchers,is.numeric)]
mlbAllHitters_Nums_Filtered <- mlbAllHitters_Nums %>% filter(Properties.wOBA_Split!=0)
mlbAllHitters_Nums_Filtered <- mlbAllHitters_Nums_Filtered %>% filter(Properties.ISO_Split!=0)
mlbAllHitters_Nums_Filtered <- mlbAllHitters_Nums_Filtered %>% filter(Properties.SLG_Split!=0)
#Find and remove any linear combinations from dataFrame
mlbAllHittersLinComb <- caret::findLinearCombos(mlbAllHitters_Nums_Filtered)
mlbAllPitchersLinComb <- caret::findLinearCombos(mlbAllPitchers_Nums)

mlbAllHitters_Nums_Filtered_LCRemoved <- mlbAllHitters_Nums_Filtered[,-mlbAllHittersLinComb$remove]
mlbAllPitchers_Nums <- mlbAllPitchers_Nums[,-mlbAllPitchersLinComb$remove]

#get regression Model setup
mlbAllHittersX <- mlbAllHitters_Nums_Filtered_LCRemoved %>% select(-ActualPoints)
mlbAllHittersY <- mlbAllHitters_Nums_Filtered_LCRemoved$ActualPoints

mlbAllPitchersX <- mlbAllPitchers_Nums %>% select(-ActualPoints)
mlbAllPitchersY <- mlbAllPitchers_Nums$ActualPoints
#compute regression
mlbAllHittersLm1 <- lm(mlbAllHittersY~.,data=mlbAllHittersX)
summary(mlbAllHittersLm1)

mlbAllPitchersLm1 <- lm(mlbAllPitchersY~.,data=mlbAllPitchersX)
summary(mlbAllPitchersLm1)

#Advanced regressions
#mlbHitterAdvancedStatsLm1 <- lm(mlbAllHittersY ~ mlbAllHittersX$Properties.OppDistDiffPct + mlbAllHittersX$Properties.OppHHPct15 + mlbAllHittersX$Properties.airtime_r + mlbAllHittersX$Properties.airtime_l + mlbAllHittersX$Properties.VegasPercentile_Slate + mlbAllHittersX$Properties.evd + mlbAllHittersX$Properties.ev_y + mlbAllHittersX$Properties.cnt_l + mlbAllHittersX$Properties.OfficialPlusMinus + mlbAllHittersX$Properties.SB_Per_Game + mlbAllHittersX$Properties.SOPerAB + mlbAllHittersX$Salary + mlbAllHittersX$Properties.Trend + mlbAllHittersX$Properties.Moneyline_Pct + mlbAllHittersX$Properties.Season_Count + mlbAllHittersX$Properties.Month_PPG + mlbAllHittersX$Properties.OppIsoAllowedPct15 + mlbAllHittersX$Properties.WOBA_MTH_PCT + mlbAllHittersX$Properties.wOBADiffPercentile_Slate + mlbAllHittersX$Properties.OwnRank_Slate + mlbAllHittersX$Properties.Season_Salary_Change + mlbAllHittersX$Properties.hh_l + mlbAllHittersX$Properties.distance_diff + mlbAllHittersX$Properties.gb_r + mlbAllHittersX$Properties.ev_r + mlbAllHittersX$Properties.HRPerAB + mlbAllHittersX$Properties.OppSOPerABAvg + mlbAllHittersX$Properties.Wind_Speed + mlbAllHittersX$Properties.cnt_r + mlbAllHittersX$Properties.fb_r + mlbAllHittersX$Properties.OppBullpenRating + mlbAllHittersX$Properties.so_pred_pct + mlbAllHittersX$Properties.OppSOPerABPct)
mlbHitterAdvancedStatsLm1 <- lm(mlbAllHittersY ~ Properties.OppDistDiffPct + Properties.OppHHPct15 + Properties.airtime_r + Properties.airtime_l + Properties.VegasPercentile_Slate + Properties.evd + Properties.ev_y + Properties.cnt_l + Properties.OfficialPlusMinus + Properties.SB_Per_Game + Properties.SOPerAB + Salary + Properties.Trend + Properties.Moneyline_Pct + Properties.Season_Count + Properties.Month_PPG + Properties.OppIsoAllowedPct15 + Properties.WOBA_MTH_PCT + Properties.wOBADiffPercentile_Slate + Properties.OwnRank_Slate + Properties.Season_Salary_Change + Properties.hh_l + Properties.distance_diff + Properties.gb_r + Properties.ev_r + Properties.HRPerAB + Properties.OppSOPerABAvg + Properties.Wind_Speed + Properties.cnt_r + Properties.fb_r + Properties.OppBullpenRating + Properties.so_pred_pct + Properties.OppSOPerABPct,mlbAllHittersX)

#mlbPitcherAdvancedStatsLm1 <- lm(mlbAllPitchersY ~ + mlbAllPitchersX$Properties.OfficialPlusMinus + mlbAllPitchersX$Properties.Moneyline_Pct + mlbAllPitchersX$Properties.airtime_r + mlbAllPitchersX$Properties.WobaAllowedPct15 + mlbAllPitchersX$Properties.Month_Count + mlbAllPitchersX$Properties.OppHHPct15 + mlbAllPitchersX$Properties.strball_l + mlbAllPitchersX$Properties.PVPct15 + mlbAllPitchersX$Properties.IPPct + mlbAllPitchersX$Properties.Trend + mlbAllPitchersX$Score + mlbAllPitchersX$Properties.Site_Salary + mlbAllPitchersX$Properties.SLG_Split + mlbAllPitchersX$Properties.Humidity + mlbAllPitchersX$Properties.hh_r + mlbAllPitchersX$Properties.Season_Salary_Change + mlbAllPitchersX$Properties.Vegas + mlbAllPitchersX$Properties.so_pred_pct + mlbAllPitchersX$Properties.Upside + mlbAllPitchersX$Properties.SO_Per_9Pct + mlbAllPitchersX$Properties.ExitVelocityPct15 + mlbAllPitchersX$Properties.OppEVPct15 + mlbAllPitchersX$Properties.WHIP_Pct + mlbAllPitchersX$Properties.LineupPercentile_Slate)
mlbPitcherAdvancedStatsLm1 <- lm(mlbAllPitchersY ~ + Properties.OfficialPlusMinus + Properties.Moneyline_Pct + Properties.airtime_r + Properties.WobaAllowedPct15 + Properties.Month_Count + Properties.OppHHPct15 + Properties.strball_l + Properties.PVPct15 + Properties.IPPct + Properties.Trend + Score + Properties.Site_Salary + Properties.SLG_Split + Properties.Humidity + Properties.hh_r + Properties.Season_Salary_Change + Properties.Vegas + Properties.so_pred_pct + Properties.Upside + Properties.SO_Per_9Pct + Properties.ExitVelocityPct15 + Properties.OppEVPct15 + Properties.WHIP_Pct + Properties.LineupPercentile_Slate,mlbAllPitchersX)














#All Regression Model Fits(Hitters)
load(file="~/Desktop/MLB_Daily/mlbHitterAdvancedLm1.rda")
load(file="~/Desktop/MLB_Daily/mlbAllHittersLm1.rda")
###load(file="~/Desktop/MLB_Daily/mlbHitterAdvancedStatsLm1.rda")
###load(file="~/Desktop/MLB_Daily/mlbHitterAdvancedStatsLm1Step.rda")















#All Regression Model Fits(Pitchers)
load(file="~/Desktop/MLB_Daily/mlbPitcherAdvancedLm1.rda")
load(file="~/Desktop/MLB_Daily/mlbAllPitchersLm1.rda")
###load(file="~/Desktop/MLB_Daily/mlbPitcherAdvancedStatsLm1.rda")
###load(file="~/Desktop/MLB_Daily/mlbPitcherAdvancedStatsLm1Step.rda")
