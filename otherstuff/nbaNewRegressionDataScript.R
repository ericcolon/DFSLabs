#GamesToModel..(Add accordingly as season goes on)
nba16Oct_RandF <- foreach(i=25:31) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("10_",i,"_2016"),other = "705700", mName = "Russell_and_Flow")
nba16Nov1_RandF <- foreach(i=1:23) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("11_",i,"_2016"),other = "705700", mName = "Russell_and_Flow")
nba16Nov2_RandF <- foreach(i=25:30) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("11_",i,"_2016"),other = "705700", mName = "Russell_and_Flow")
nba16Dec_RandF <- foreach(i=1:18) %dopar% DFSLabs::getNbaPlayerModels_Other(modelDate = paste0("12_",i,"_2016"),other = "705700", mName = "Russell_and_Flow")

nba16All_RandF <- bind_rows(nba16Oct_RandF,nba16Nov1_RandF,nba16Nov2_RandF,nba16Dec_RandF)

