install.packages("D:/FinanceApps/iFind/iFinD/bin/x86/RJSONIO_1.2-0.2.zip",repos=NULL, type="source")
install.packages("D:/FinanceApps/iFind/iFinD/bin/x86/iFinDR_1.1.tar.gz",repos=NULL,type="source")

library(iFinDR)
library(RJSONIO)
THS_iFinDLogin('fdjyjj034','94331017L')
ToolBar()

#Download stock data
ids = read.table("id.txt",header = TRUE)
ids

stock_ids = ids[,1]
stock_ipo_dates = ids[,3]

for(i in 1:length(stock_ids))
{
  id = as.character(stock_ids[i])
  date1 = as.Date(stock_ipo_dates[i])
  date2 = date1+100
  stock = THS_HistoryQuotes(id,'close;changeper;volume;amount;hsl',
                            'period:D,pricetype:1,rptcategory:0,fqdate:1900-01-01,hb:YSHB',
                            as.character(date1),as.character(date2))
  c1 = stock$tables[[1]]$time
  c2 = as.numeric(unlist(stock$tables[[1]]$table$close))
  c3 = as.numeric(unlist(stock$tables[[1]]$table$changeper))
  c4 = as.numeric(unlist(stock$tables[[1]]$table$volume))
  c5 = as.numeric(unlist(stock$tables[[1]]$table$amount))
  c6 = as.numeric(unlist(stock$tables[[1]]$table$hsl))
  df = data.frame(Date = c1, Close = c2, ChgPer = c3, Vol = c4, Amt = c5, Hsl = c6)
  write.csv(df,paste0("stocks/",id,".csv"))
  print(i)
}

#Select data
#From 2014-01-01 to 2016-12-31
ids = read.table("id.txt",header = TRUE)
ids = ids[137:711,]
stock_ids = ids[,1]
stock_ipo_dates = ids[,3]

ipo = read.table("ipo.txt",header = TRUE)
names(ipo) = c("Code","Name","PE","APS","Rate1","Rate2")

industry = read.table("industry.txt",header = TRUE)
names(industry) = c("Code","Name","IndustryName","IndustryCode")

df = data.frame(ids,Forty4 = FALSE,Days = 0,FirstHslRe = 0,PE = 0,APS = 0,Rate = 0)

for(i in 1:length(stock_ids))
{
  id = as.character(stock_ids[i])
  stock = read.csv(paste0("stocks/",id,".csv"))
  #stock = na.omit(stock)
  forty4 = round(stock$ChgPer[1])>=44
  if(forty4==FALSE){
    print(id) 
    next
  }
  
  close = stock$Close
  tmp1 = floor(close*1.1*100+0.5)/100
  tmp2 = tmp1[-length(tmp1)] == close[-1] 
  #tmp2 = stock$ChgPer > 0
  
  days = 1
  for(j in tmp2)
  {
    if(j==TRUE)
    {
      days = days+1
    }else{
      break
    }
  }
  
  firstHslRe = 1/stock$Hsl[1]
  pe = as.numeric(as.character(ipo[which(ipo$Code==id),3]))
  aps = as.numeric(as.character(ipo[which(ipo$Code==id),4]))
  rate1 = as.numeric(as.character(ipo[which(ipo$Code==id),5]))
  if(is.na(rate1)) 
  {
    rate1 = 0
  }
  rate2 = as.numeric(as.character(ipo[which(ipo$Code==id),6]))
  if(is.na(rate2)) 
  {
    rate2 = 0
  }
  rate = max(rate1,rate2)
  
  df[i,]$Forty4 = forty4
  df[i,]$Days = days
  df[i,]$FirstHslRe = firstHslRe
  df[i,]$PE = pe
  df[i,]$APS = aps
  df[i,]$Rate = rate
  #df[i,]$Loss = (stock$Close[days+5]/stock$Close[days]-1)*100

}
names(df) = c("Code","Name","IPO_Date","Forty4","Days","FirstHslRe","PE","APS","Rate")

df = na.omit(df)
df = df[which(df$Forty4==TRUE),]
df = df[which(df$Rate>0),]


# #reg days on FirstHslRe
# lgDays = log(df$Days)
# lgFirstHslRe = log(df$FirstHslRe)
# lm0 = lm(lgDays~lgFirstHslRe)
# summary(lm0)
# 
# #reg days on PE
# lgDays = log(df$Days)
# lgPE = log(df$PE)
# plot(lgPE,lgDays)
# lm0 = lm(lgDays~lgPE)
# summary(lm0)#why 22.99?
# 
# #reg days on APS
# lgDays = log(df$Days)
# lgAPS = log(df$APS)
# plot(df$APS,df$Days)
# plot(lgAPS,lgDays)
# lm0 = lm(lgDays~lgAPS)
# summary(lm0)
# 
# #reg days on FirstHslRe, APS
# lm0 = lm(lgDays~lgFirstHslRe+lgAPS)
# summary(lm0)
# 
# #reg days on Rate
# lgDays = log(df$Days)
# lgRate = log(df$Rate)
# #Why there are two groups
# plot(df$Rate,df$Days)
# plot(df$Rate,lgDays)
# plot(lgRate,df$Days)
# plot(lgRate,lgDays)
# lm0 = lm(lgDays~lgRate)
# summary(lm0)
# 
# #reg days on FirstHslRe, APS, Rate
# lm0 = lm(lgDays~lgFirstHslRe+lgAPS+df$Rate)
# summary(lm0)
# 
# #reg days on FirstHslRe, Rate
# lm0 = lm(lgDays~lgFirstHslRe+df$Rate)
# summary(lm0)

df$IPO_Date = as.Date(df$IPO_Date)
df$Stage1 = 0
df$Stage2 = 0
df$Stage3 = 0
df$Stage4 = 0
#stage5
df$Stage1[which(df$IPO_Date<as.Date("2014-06-01"))] = 1
df$Stage2[which(as.Date("2014-06-01")<=df$IPO_Date & df$IPO_Date<as.Date("2015-03-01"))] = 1
df$Stage3[which(as.Date("2015-03-01")<=df$IPO_Date & df$IPO_Date<as.Date("2015-06-01"))] = 1
df$Stage4[which(as.Date("2015-06-01")<=df$IPO_Date & df$IPO_Date<as.Date("2015-12-01"))] = 1

# #reg days on FirstHslRe, APS, Rate, Stages
# lm0 = lm(lgDays~lgFirstHslRe+lgAPS+df$Rate+df$Stage1+df$Stage2+df$Stage3+df$Stage4)
# summary(lm0)
# df$Prediction = exp(predict(lm0))
# df$Corret = abs(round(df$Prediction)-df$Days) ==0

df$Code = as.character(df$Code)
#sector0 SZ small
#sector1 SH main 600000
#sector2 SZ startup 300000
df$Sector1 = 0
df$Sector2 = 0
df$Sector1[which(substr(df$Code,1,1)=='6')] = 1
df$Sector2[which(substr(df$Code,1,1)=='3')] = 1

#reg days on FirstHslRe, APS, Rate, Stages, Sectors
# lm0 = lm(lgDays~lgFirstHslRe+lgAPS+df$Rate+df$Stage1+df$Stage2+df$Stage3+df$Stage4+df$Sector1+df$Sector2)
# summary(lm0)
#df$Prediction = exp(predict(lm0))
#df$Corret = abs(round(df$Prediction)-df$Days) ==1
#df$Error = df$Prediction - df$Days
#summary(abs(df$Error))

#new = c(1,log(1/0.03),log(11.77),0.0266,0,0,0,0,0,1)
#new = c(1,log(1/0.02),log(1.86),0.2212,0,0,0,0,1,0)
#exp(sum(lm0$coefficients*new))

df = merge(df,industry,by.x = "Code", by.y = "Code")
df = df[,-(ncol(df)-1)]
df = df[,-(ncol(df)-1)]

df2 = df
tmp = names(table(as.factor(as.character(df2$IndustryCode))))
for(i in tmp)
{
  df2 = cbind(df2,0)
}
names(df2) = c(names(df2)[1:16],tmp)
for(i in 1:length(tmp))
{
  df2[,16+i][which(df2$IndustryCode==tmp[i])] = 1
}

df2 = df2[,!(names(df2) %in% c("Code","Name.x","IPO_Date","Forty4","PE","IndustryCode"))]
df2$Days = log(df2$Days)
df2$FirstHslRe = log(df2$FirstHslRe)
df2$APS = log(df2$APS)

df2 = df2[,-ncol(df2)] #colinearity

#reg days on FirstHslRe, APS, Rate, Stages, Sectors, Industries
lm0 = lm(Days~.,df2)
summary(lm0)

df3 = df2
  
# df2$Prediction = exp(predict(lm0))
# df2$ActDays = exp(df2$Days)
# df2$Error = round(abs(df2$Prediction-df2$ActDays),2)
# exp(sum(lm0$coefficients*c(1,log(1/0.03),log(11.77),0.0266,0,0,0,0,0,1,rep(0,5),1,rep(0,19))))

#Reg Days on Significant factors
df3 = df3[,names(df3) %in% c("Days","FirstHslRe","APS","Rate","Stage1","Stage3","Stage4","Sector1","S48")]
lm0 = lm(Days~.,df3)
summary(lm0)

df3$Prediction = exp(predict(lm0))
df3$ActDays = exp(df3$Days)
df3$Error = df3$Prediction-df3$ActDays

exp(sum(lm0$coefficients*c(1,log(1/0.03),log(11.77),0.0266,0,0,0,0,0)))#300623
exp(sum(lm0$coefficients*c(1,log(1/0.02),log(1.86),0.2212,0,0,0,1,0)))#601228

df3$Code = df$Code
write.csv(df3,"./results/results_model1.csv")


#Try square




