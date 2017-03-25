labsAll <- bind_rows(labsOct,labsNov,labsNov2,labsDec,labsDec2,labsJan)
labsAll3000 <- labsAll3000 %>% filter(Salary>0,Salary<4100)
labsAll3000 <- na.zero(labsAll3000)
labsAll3000X <- labsAll3000 %>% select(-X,-ActualPoints,-Properties.ActualPoints)
labsAll3000X <- labsAll3000X[sapply(labsAll3000X,is.numeric)]
labsAll3000Y <- labsAll3000$ActualPoints
labsAll3000LM <- lm(labsAll3000Y~.,data=labsAll3000X)
all3000Proj <- predict(all3000LM3FitStep,labsAll3000)
all3000withProj <- labsAll3000 %>% mutate(CheapProjection=all3000Proj)
all3000withProj <- data.frame(all3000withProj$Properties.Player_Name,all3000withProj$CheapProjection,all3000withProj$Salary,all3000withProj$Salary/all3000withProj$CheapProjection,all3000withProj$ActualPoints,all3000withProj$Salary/all3000withProj$ActualPoints,all3000withProj$Properties.p_own)
names(all3000withProj)<- list("Name","CheapProj","Salary","Proj$/Pt.","ActualPoints","Actual$/Pt.","Ownership")
write.csv(all3000withProj,file="~/Desktop/NBA_Daily/labsAll3000withProjections.csv")

bbmAll <- bind_rows(bbmOct,bbmNov,bbmNov2,bbmDec,bbmDec2,bbmJan)
bbmAll3000 <- bbmAll3000 %>% filter(Price>0,Price<4100)
bbmAll3000 <- na.zero(bbmAll3000)
bbmAll3000X <- bbmAll3000 %>% select(-ActualV)
bbmAll3000X <- bbmAll3000X[sapply(bbmAll3000X,is.numeric)]
bbmAll3000Y <- bbmAll3000$ActualV
bbmAll3000LM <- lm(bbmAll3000Y~.,data=bbmAll3000X)
bbmAll3000Proj <- predict(bbmAll3000LM2,bbmAll3000)
bbmAll3000 <- bbmAll3000 %>% mutate(bbmCheapProjection=bbmAll3000Proj)
bbmAll3000withProj <- data.frame(bbmAll3000$Name,bbmAll3000$bbmCheapProjection,bbmAll3000$Price,bbmAll3000$Price/bbmAll3000$bbmCheapProjection,bbmAll3000$ActualV,bbmAll3000$Price/bbmAll3000$ActualV)
names(bbmAll3000withProj)<- list("Name","bbmCheapProj","Salary","Proj$/Pt.","ActualPoints","Actual$/Pt.")
write.csv(bbmAll3000withProj,file="~/Desktop/NBA_Daily/bbmAll3000withProjections.csv")
