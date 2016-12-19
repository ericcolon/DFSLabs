j <- 10#last completed week on the season
AllQB <- foreach(i=1:17) %dopar% getNflLabModel("2015",i,PositionID = "QB")
readQB <- foreach(i=1:17) %dopar% readNFLCSVs("2015",i,PositionID = "QB")
AllQB16<- foreach(i=1:j) %dopar% getNflLabModel("2016",i,PositionID = "QB")
readQB16 <- foreach(i=1:j) %dopar% readNFLCSVs("2016",i,PositionID = "QB")
QB <- bind_rows(readQB16)#,readQB)
# QB <- QB %>% select(-Properties.MyTrends.custom)
week <- 11  #Week to project
QBModel <- getNflLabModel("2016",week,PositionID = "QB")
QBModel <- na.zero(readNFLCSVs("2016",week,PositionID = "QB"))

                   QBnums <- na.zero(QB[,sapply(QB,is.numeric)])
                   QBnums <- QBnums %>% select(-X,-Properties.ActualPoints)
                   yQB <- QBnums$ActualPoints
                   xQB <- QBnums %>% select(-ActualPoints)
                                                      
QBFormula <- yQB ~ Score + FantasyResultId + Salary + Properties.AvgPts + 
  Properties.Ceiling + Properties.ProjPlusMinus + Properties.Site_Salary + 
  Properties.OppPlusMinus + Properties.OffensiveSnapsPlayed + 
  Properties.Pts + Properties.OppPts + Properties.Spread + 
  Properties.Total + Properties.InterceptionPct + Properties.PassingSuccessfulPct + 
  Properties.RushingSuccessfulAllowedPct + Properties.SackPct + 
  Properties.TakeawayPct + Properties.YardsPerPassingAttempt + 
  Properties.ReceivingTouchdownsMarketShare + Properties.ReceivingYardsMarketShare + 
  Properties.PassingCompletions + Properties.PassingCompletionPercentage + 
  Properties.AdjYPA + Properties.PassingLong + Properties.PassSackPct + 
  Properties.RushingAttempts + Properties.RushingLong + Properties.RushYards + 
  Properties.RushingYardsPerAttempt + Properties.RushingSuccessfulPct + 
  Properties.ReceivingYards + Properties.ReceivingLong + Properties.ReceivingTouchdowns + 
  Properties.ReceivingYardsPerTarget + Properties.RushingTouchdownsRedZonePct + 
  Properties.ReceivingTouchdownsRedZonePct + Properties.RedZoneSnaps10 + 
  Properties.RedZoneTouchdownPct + 
  Properties.OppPlusMinusPct + 
  Properties.ProjPlaysPct + Properties.MktShrPct + Properties.ProjPct + 
  Properties.PtPerDPct + Properties.FloorPct + Properties.Vegas + 
  Properties.Salary_Movement + Properties.Upside + Properties.Pro_Pct + 
  Properties.RushingFantasyPct  + 
  Properties.EventTeamId
QBFit <- lm(yQB~.,data = xQB)
summary(QBFit)
QBFit <- step(QBFit)
summary(QBFit)
#addToPred <- min(QBFit$residuals)
QBP <- predict(QBFit,QBModel)
#QBP <- QBP-addToPred
QBFile <- cbind(QBModel$Properties.Player_Name,QBP)
write.csv(QBFile, file = paste0("QBP",week,".csv"))
QBFile
