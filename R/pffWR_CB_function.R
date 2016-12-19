#' ProFootballFocus WR/CB Chart Function
#'
#' @param week 
#'
#' @return WR/CB Chart
#' @export
#'
#' @examples WR_CBWk1 <- pffWR_CB(1)
pffWR_CB <-function(week){
y <- paste0("pffWR_CBWeek",week,"2016.html")
system(paste0("curl 'https://www.profootballfocus.com/toolkit/wr-cb-chart/' -H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Cache-Control: max-age=0' -H 'Cookie: __cfduid=d6128fd5806b45622d9a353610259c7d01474176858; PHPSESSID=ku3uhigat3851e70code3a5260; viewFormat=; __smListBuilderShown=true; __smListBuilderOptOut=true; bb2_screener_=1474180066+172.31.57.142+173.18.249.10%2C+162.158.79.189; __smToken=0SOqWfl3kYwUsSi1A2FGdAJC; amember_nr=36a2c4bedc4d9d1b92333564b7a00a6c; wordpress_logged_in_a17477efefdaada2e7975a61ae9e7b86=benjaminryanclarke%7C1474352983%7CRoXDH0UMWgCZlAHWKquvFS2Nwpaz1n0m3ORji2pUCnE%7C1eeae4043a0dcf9e45226d42550149024cd462f1ee50fa47cfafd30e3c1f8b1f; _ga=GA1.2.232422623.1474176860; __utma=136397809.232422623.1474176860.1474176864.1474176864.1; __utmc=136397809; __utmz=136397809.1474176864.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); AWSELB=ED837F270CF5614E633BDE64F8F0E2005A87D7885A88EC19B84302AF878DF03422F74DF44D976AEF47A41FAE4206621D8A1C58BC20B3A35F89CF91422E2CFA5BB6EAB6BFA5' -o ",y," -H 'Connection: keep-alive' --compressed"))
wrcbTable <- data.frame(readHTMLTable(y),stringsAsFactors = FALSE)
write.csv(wrcbTable,file=paste0("~/Desktop/NFL_Daily/NFL_2016_Week",week,"/pffWR_CBWeek",week,"_2016.csv"))
return(wrcbTable)
}