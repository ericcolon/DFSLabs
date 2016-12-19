systemsURL <- "http://www.fantasylabs.com/api/systems/models/1/"

cookie <- "__cfduid=d19da73f21ae9852e137438d8ffd398371475796569; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; __distillery=a88774e_344d8f47-a8f7-4094-81ba-d3336d4c2d4d-ba08c6bef-19ba082fafba-d101; LD_R=http%3A%2F%2Fwww.fantasylabs.com%2Fnfl%2Fplayer-models%2F; .AspNet.Cookies=I3uyNArip8P2mwm4iZUdp1aswKJvJUJsgXxxhq2yU-UayOPc51mQDfzLkb2HihonPOWkz3ZPkcBeFOk1SeaGwDn5-Oh61Xu1cUsarqZkutXgbTBwc2V6ITx_u4lMXjuodLaUjd3x0DcSg2Y7YFMQwjduY9bLFWkLyDqr5rutPNS5bxAxO80joxoa7tfhKOC0vLGmO3ZCDMTWGNXAMqiGpfG5p7eiS7drDO5q9AEwYYux_fLWATa04cZXGRkHc3eae_7B_eaGUaFPNqom5dZ2ewqZRHktUEGo4JavxHfv4TMxS4t9URXN3YxK6YGh7edIh1pFHuZ1RQpVolIHQzbuedjxPb_dkSwZ1iVPcplBM10TV3g3NRDZXZVQ0LYWbkFUdqEvbMeiBMos-kNeurxSrlKdwbQTro0vhq_ckdsRFtduXzQ-zxsbcwWHiUrNNOshYTZMau5j2tZzPsPyCOLGrPXJPJd-ZaKSJNIL_gY5pz8; flid=yijf7wz4JEegj3XR4O5vLw; __zlcmid=cyfuDnxjy8XwIK; LD_S=1479022223409; LD_T=6a763f78-d230-4827-f05d-37ca01630a5d; _ga=GA1.2.407536382.1475796573; _gat=1'"

systemsFileAs <- paste0("nflFantasyLabsSystemModels",".json")
systemsCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models-preview/?date=11022016' -H 'Cookie: ",cookie,"  -o ",systemsFileAs," -H 'Connection: keep-alive' --compressed")
curlSystemsAddress <- paste0("curl ",systemsURL," ",systemsCurlHandles)
system(curlSystemsAddress)

allLabsSystems <- jsonlite::fromJSON(systemsFileAs)
systemNames <- allLabsSystems$Models$SystemName
systemIDs <- allLabsSystems$Models$SystemId

nflSystemsDf <- data.frame(systemNames,systemIDs,stringsAsFactors = FALSE)
nflSystemsDf <- sapply(nflSystemsDf,as.character)
write.csv(nflSystemsDf, file = paste0("~/Desktop/NFL_Daily/nflLabModelIds",".csv"))
nflSystemsDf
