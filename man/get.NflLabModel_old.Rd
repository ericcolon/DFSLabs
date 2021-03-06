% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NFLBasicLabsModelRetrieve.R
\name{get.NflLabModel_old}
\alias{get.NflLabModel_old}
\title{NFL FantasyLabs Model Retrieve}
\usage{
get.NflLabModel_old(modelYear = "2016", modelWeek = NULL, model = NULL,
  showModelList = FALSE, PositionID = NULL,
  cookie = "__cfduid=d46b64495b55215841566c89ce01d6fbd1480519747; LD_U=http\%3A\%2F\%2Fwww.fantasylabs.com\%2F; __distillery=bb3e53d_b333f0ea-b6ab-4d8b-bcbb-e8f78b8d5c3f-86a4ffd60-c4a9ff71f0fe-aef4; _gat=1; LD_S=1481767246798; LD_R=; .AspNet.Cookies=EVGJH0IuWLy8RuLAWqA3JVB3L8lvyXCaqIDX7mPnHvFo-X76ra1UZRbqo5URKoMo2lrt_BKDiQXclEtWa_hgP7lB6nJsVKo-4dCzLhGJnKZUhq7vWHYc0dHsl2_3m8Wjy-PgYHci5gghAw5y8Lp-rKbi1hRYrHaLQH1_d58PZIZ5q9spxBoCCbZ2ooD-HG0xYfHcPot8_DNrfbeW0XMnByaFVebeNnfem-Mb11nh-wHR3De5peqOHz0-BaRMpG3CnjW8N_pa3OO6BgVMdQ-kr16OSk82gNYDIAAMAvWaqfghUMUjoouY8bJPvqMLcEYX7E2-4K0VWR6usBYAWeZhYOPWPmK9oKhn38Y7RnTawoCNOnqhh6mQyCblU5YwhP7U2xztRCzaNk1N0hvKMvHfQbS35ExGTqeVin7bQIMynh2nYqkjFxhjIT9USeCxQj1zu0UFVZ937dHIARI47-A_QBmier5CJ0Q29n4Isdcu8Bw; flid=-t582kVLjUizx14ZPB2BXQ; __zlcmid=e6g4jX86CRpGap; _ga=GA1.2.351079370.1480519759; LD_T=d014f701-2492-4486-bcec-b421e46d54b2'")
}
\arguments{
\item{modelYear}{(2015,2016)}

\item{modelWeek}{(1-17)}

\item{model}{(1...42)}

\item{showModelList}{default FALSE(takes thru interactive modeling if TRUE)}

\item{PositionID}{(QB.RB.WR,TE,DST)}

\item{cookie}{cookies from browser needed to get into fantasylabs account}
}
\value{
FantasyLabs NFL model
}
\description{
NFL FantasyLabs Model Retrieve
}
\examples{
get.NflLabModel_old(modelYear = "2015",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL)
}

