# install.packages("D:/FinanceApps/iFind/iFinD/bin/x86/RJSONIO_1.2-0.2.zip",repos=NULL, type="source")
# install.packages("D:/FinanceApps/iFind/iFinD/bin/x86/iFinDR_1.1.tar.gz",repos=NULL,type="source")
# 
# library(iFinDR)
# library(RJSONIO)
# THS_iFinDLogin('fdjyjj034','94331017L')
# ToolBar()
# 
# #Download stock data
# ids = read.table("id.txt",header = TRUE)
# ids
# 
# stock_ids = ids[,1]
# stock_ipo_dates = ids[,3]
# 
# for(i in 1:length(stock_ids))
# {
#   id = as.character(stock_ids[i])
#   date1 = as.Date(stock_ipo_dates[i])
#   date2 = date1+100
#   stock = THS_HistoryQuotes(id,'close;changeper;volume;amount;hsl',
#                             'period:D,pricetype:1,rptcategory:0,fqdate:1900-01-01,hb:YSHB',
#                             as.character(date1),as.character(date2))
#   c1 = stock$tables[[1]]$time
#   c2 = as.numeric(unlist(stock$tables[[1]]$table$close))
#   c3 = as.numeric(unlist(stock$tables[[1]]$table$changeper))
#   c4 = as.numeric(unlist(stock$tables[[1]]$table$volume))
#   c5 = as.numeric(unlist(stock$tables[[1]]$table$amount))
#   c6 = as.numeric(unlist(stock$tables[[1]]$table$hsl))
#   df = data.frame(Date = c1, Close = c2, ChgPer = c3, Vol = c4, Amt = c5, Hsl = c6)
#   write.csv(df,paste0("stocks/",id,".csv"))
#   print(i)
# }

#Select data
#From 2014-01-01 to 2016-12-31
ids = read.table("id.txt",header = TRUE)
ids = ids[137:711,]
stock_ids = ids[,1]
stock_ipo_dates = ids[,3]

model1 = read.csv("results_model1.csv",header = TRUE)
df_model1 = data.frame(Code = model1$Code,Pred = model1$Prediction)

# ipo = read.table("ipo.txt",header = TRUE)
# names(ipo) = c("Code","Name","PE","APS","Rate1","Rate2")
# 
# industry = read.table("industry.txt",header = TRUE)
# names(industry) = c("Code","Name","IndustryName","IndustryCode")

df = data.frame(Code = 0, X = 0,Hsl = 0,HslChg = 0, HslChg1 = 0 ,HslChg7 = 0,DiffToPred = 0,OpenNextDay = 0,Days = 0)
for(i in 1:length(stock_ids))
{
  id = as.character(stock_ids[i])
  stock = read.csv(paste0("stocks/",id,".csv"))
  forty4 = round(stock$ChgPer[1])>=44
  if(forty4==FALSE) next
  
  close = stock$Close
  tmp1 = floor(close*1.1*100+0.5)/100
  tmp2 = tmp1[-length(tmp1)] == close[-1]
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
  if (days == 1) next
  
  stock = stock[1:(days),]
  stock = stock[,-c(2,3,4,5,6)]
  stock = cbind(Code = id,stock)
  
  stock$HslChg = c(0,stock[-1,]$Hsl/stock[-nrow(stock),]$Hsl)
  stock$HslChg1 = as.numeric(stock$HslChg<1)
  stock$HslChg7 = as.numeric(stock$HslChg>7)
  #stock$HslChg_3 = stock$HslChg*stock$HslChg3
  
  if(!(id %in% df_model1$Code)) next
  pred = df_model1[which(df_model1$Code==id),2]
  stock$DiffToPred = (stock$X - pred)
  
  stock$OpenNextDay = 0
  stock$OpenNextDay[nrow(stock)] = 1
  stock = stock[-1,]
  stock$Days = days
  
  df = rbind(df,stock)
}
names(df) = c("Code","T","Hsl","HslChg","HslChg1","HslChg7","DiffToPred","OpenNextDay","Days")
df = df[-1,]

df2 = df[,c(-1,-ncol(df))]

lm0 = glm(OpenNextDay~.,family = binomial(link = 'probit'),data = df2)
summary(lm0)
# 
# res1 = c()
# res2 = c()
# for(i in seq(0.3,0.7,0.05)){
#   for(j in seq(2,5,0.5)){
#     for(k in seq(0.1,0.5,0.05)){
#       res1 = c(res1,paste(i,j,k))
# 
#       df2$Prediction = predict(lm0,df2[,-ncol(df2)],type = "response")
#       df2$Correct = (df2$Prediction>i | c(0,(df2$Prediction[-1]/df2$Prediction[-nrow(df2)] > j  & df2$Prediction[-1]>k ) ) )
#       df2$Correct = (df2$Correct == df2$OpenNextDay)
# 
#       df2$Code = df$Code
# 
#       agg = aggregate(df2,by = list(df2$Code),FUN = min)
#       tmp = data.frame(Code = agg$Group.1, Correct = agg$Correct)
#       res2 = c(res2,summary(tmp$Correct)[4])
# 
#     }
#   }
# }
# res1[which.max(res2)]
  
df2$Prediction = predict(lm0,df2[,-ncol(df2)],type = "response")
df2$Correct = (df2$Prediction>0.5 | c(0,(df2$Prediction[-1]/df2$Prediction[-nrow(df2)] > 2  & df2$Prediction[-1]>0.2 ) ) )
df2$Correct = (df2$Correct == df2$OpenNextDay)

df2$Code = df$Code

agg = aggregate(df2,by = list(df2$Code),FUN = min)
tmp = data.frame(Code = agg$Group.1, Correct = agg$Correct)
summary(tmp$Correct)

# tmp_code = df2$Code[which(df$T>5)]
# View(df2[which(df2$Code %in% tmp_code),])

df2$Days = df$Days

subset = df2[which(df2$Correct==FALSE),]
subset = subset[order(subset$T),]
for (i in 1:nrow(subset)){
  this = subset[i,]
  code = this$Code
  tmp = c(which(subset$Code==code))
  tmp = tmp[which(tmp>i)]
  if(length(tmp)==0) next
  subset = subset[-tmp,]
}

error = subset$Days-subset$T

num_total = length(union(df2$Code,df2$Code))
num_error = sum(table(error)[-1])
num_total
num_error
num_correct = num_total-num_error

res = c()
for(bench in seq(-0.5,0.5,0.05))
{
  table_title = as.numeric(names(table(error)))
  table_num = table(error)
  sum1 = -sum((1.1^table_title[-1]-1)*(table_num[-1]/sum(table_num[-1])))
  sum2 = -bench*num_correct/num_total
  sum1 = (sum1 - bench)*num_error/num_total
  sum = sum1+sum2
  res = c(res,sum)
}
res
res_df = data.frame(hpr = seq(-0.5,0.5,0.05), res = res)
write.csv(res_df,"tmp.csv")







