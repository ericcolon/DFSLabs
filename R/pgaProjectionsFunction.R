projectPGA <- function(modelDate="4_10_2016",fit=pgaLm1Step,cookie=labsCookie){
  pgaModel <- getPgaModel(modelDate=modelDate,cookie=cookie)
  modelImport <- data.frame(na.zero(data.frame(read.csv(file=paste0("~/Desktop/PGA_Daily/pga",modelDate,".csv")))))
  pgaProj <- predict(fit,data.frame(modelImport))
  modelWithProj <- modelImport %>% mutate(LabProjections=pgaProj)
fullModel <- data.frame(modelWithProj$Properties.Player_Name,modelWithProj$Salary,modelWithProj$LabProjections,modelWithProj$Salary/modelWithProj$LabProjections,modelWithProj$ActualPoints,modelWithProj$Salary/modelWithProj$ActualPoints)
names(fullModel) <- c("Name","Salary","Projection","$/Pt.","Actual","Actual$/Pt.")
  write.csv(fullModel,file=paste0("~/Desktop/PGA_Daily/","PGA_Projections_",modelDate,".csv"))
  return(fullModel)
}
