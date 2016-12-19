nbaFirstQuantTopDfnums <- na.zero(nbaFirstQuantTopDf[,sapply(nbaFirstQuantTopDf,is.numeric)])
nbaFirstQuantTopDfnums <- nbaFirstQuantTopDfnums %>% select(-X,-Properties.ActualPoints)
ynbaFirstQuantTopDf <- nbaFirstQuantTopDfnums$ActualPoints
xnbaFirstQuantTopDf <- nbaFirstQuantTopDfnums %>% select(-ActualPoints)
nbaFirstQuantTopDfFit <- lm(ynbaFirstQuantTopDf~.,data = xnbaFirstQuantTopDf)
summary(nbaFirstQuantTopDfFit)
