#Pull a playermodel into R
mlbModel7.26H <- getMlbPlayerModel()
#get only the numeric parts to prepare for use with corr
mlbModelNums7.26H <- mlbNumericOnly(model = mlbModel7.26H)
#run and save the correlation file
mlbModelCorr7.26H <- mlbCorrToCsv(model = mlbModelNums7.26H, y = "MLB_7.26.2016_correlatedH")

#old method...
# top10H_7.25 <- extract10Mlb(n1 = "Jay Bruce",
#             n2 = "Javier Baez",
#             n3 = "Eugenio Suarez",
#             n4 = "Brandon Belt",
#             n5 = "Albert Pujols",
#             n6 = "Todd Frazier",
#             n7 = "Angel Pagan",
#             n8 = "Luis Valbuena",
#             n9 = "Salvador Perez",
#             n10 = "George Springer",
#             model = mlbModel7.25H)


#get top 10 highest fantasy scorers for a given day
top10H7.26.2016playerList <- mlbModel7.26H$Properties.Player_name[1:10]
#subset playerModel to include only those 10
top10H7.26.2016 <- extractSpecific(playerNames, model = mlbModel7.26H)
#only numeric vector for top 10
top10H_7.26_Nums <- mlbNumericOnly(model = top10H7.26.2016)
#run correlations and save the file
mlbCorrToCsv(model = top10H_7.26_Nums, y = "MlbTop10_7.26_CorrHitters")

#swishHitters()
#retrieve swishanalytics batter comparison table for specified date
swish7.26.2016 <- swishHitters()

#get the rank of comparison based on swish's data...returns row number of the player
which(grepl(playerName, swish7.26.2016,fixed = TRUE))

#examples and usage of playerModelFilter function
sum(playerModelFilter(name = "", model = top10H_7.25, filter = "ev_r"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "ev_y"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "evd"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "humidity"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "distance_r"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "distance_diff"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "so_pred_pct"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "so_pred"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "ev_pct"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "hh_pct"))
sum(playerModelFilter(name = "", model = top10P_7.24, filter = "Oppsoper9pct"))
#without sum argument(view all values)
playerModelFilter(name = "", model = top10P_7.24, filter = "OppSOPer9Pct")
playerModelFilter(name = "", model = top10P_7.24, filter = "season_x1")
playerModelFilter(name = "", model = top10P_7.24, filter = "strball_l")
playerModelFilter(name = "", model = top10P_7.24, filter = "moneyline_pct")
playerModelFilter(name = "", model = top10P_7.24, filter = "consistency")
playerModelFilter(name = "", model = top10P_7.24, filter = "pro_pct")
playerModelFilter(name = "", model = top10P_7.24, filter = "score")
playerModelFilter(name = "", model = top10P_7.24, filter = "trend")
playerModelFilter(name = "", model = top10P_7.24, filter = "season_ppg_percentile")
playerModelFilter(name = "", model = top10P_7.24, filter = "plus_minus")