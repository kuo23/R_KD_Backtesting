rm(list=ls());gc()
############套件##############

if(!require(quantmod)) {install.packages("quantmod")}
if(!require(lubridate)) {install.packages("lubridate")}
if(!require(dplyr)) {install.packages("dplyr")}
if(!require(RMySQL)) {install.packages("RMySQL")}
if(!require(TTR)) {install.packages("RMySQL")}



library("quantmod", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("lubridate", lib.loc="~/R/win-library/3.4")
library("TTR", lib.loc="~/R/win-library/3.4")
library("RMySQL", lib.loc="~/R/win-library/3.4")


############資料############
# channel <- dbConnect(dbDriver("MySQL"),host="140.117.70.217",user='nsysu_quant',password='5l jp6wj062jo4',DBMSending="UTF8")
# dbSendQuery(channel,"set names big5;")
# sqlString <- paste0("SELECT code, name, date, open, high, low, close, trade_volume ,industry FROM stock_market.adj_stock_price_data  where date>=20070102;")
# res <- dbSendQuery(channel,sqlString)
# data <- fetch(res, n = -1) %>% as_data_frame()
# dbDisconnect(channel)
# data<-rename(data,vol=trade_volume)
# save(data,file="stockdata.Rdata")
############參數###########
load("stockdata.Rdata")
# data<-rename(data,vol=trade_volume)
Kmin<-20
start_stop_loss<-(-0.2)
stop_loss_down<-0.03 #單筆獲利>start_stop_profit時，獲利向下掉stop_profit_down出場
start_stop_profit<-0.0715
stop_profit_down<-0.03 #單筆獲利>start_stop_profit時，獲利向下掉stop_profit_down出
show<-NULL
nfastK<-14
nfstD<-3
nslowD<-3
# for (nfastK in c(6,9,14)){
#   for(nfstD in c(3,6,9)){
#     for(nslowD in c(3,6,9)){

#########合併##############

data <- data %>%
    # filter(code==1101) %>%
  # filter(date<=20170615) %>% 
  arrange(code,date) %>%
  group_by(code) %>%
  mutate(sampleNums=n()) %>% 
  filter(sampleNums>nfastK+nfstD+nslowD-3) %>%

  mutate(
    indate=lead(date,1),
    price5ma = SMA(close, n=5),
    buyprice = lead(open,1),
    sellprice = close
        ) %>%
  
  mutate(
    maxHigh=runMax(high, nfastK),#rollmax(high, nfastK, fill=NA, align="right"),
    minLow=runMin(low, nfastK),#-rollmax(-low, nfastK, fill=NA, align="right"),
    fastK=(close-minLow)/(maxHigh-minLow)
        ) %>%
  na.omit() %>% 
  
   
  mutate(
         fastD=EMA(fastK,nfstD),
         slowD=EMA(fastD,nslowD)
        ) %>% 
  
  mutate(
         K=fastD*100,
         D=slowD*100,
         K1=lag(K,1),
         D1=lag(D,1)
        ) %>% 
  
  select(-(fastK:slowD)) %>% 
  
  mutate(
    
         buy_signal=ifelse(K<Kmin & K1<D1 & K>=D & close>price5ma & K<50 ,1,0)
        
        ) %>% 

na.omit() 
  

# filter(sampleNums>nNums) %>%  # 過濾樣本數不足股票，防止後面運行出錯
#   mutate(
#     hlcMean=(high+low+close)/3,
#     mavg=SMA(hlcMean,nNums),
#     mavgSd=runSD(hlcMean,nNums,sample=FALSE),
#     up=mavg+sdNums*mavgSd,
#     dn=mavg-sdNums*mavgSd,
#     pctB=(hlcMean-dn)/(up-dn),
#     widthBB = (up-dn)/price20ma,
#     widthBB1 = lag(widthBB,1),
#     widthBB2 = lag(widthBB,2),
#     widthBB3 = lag(widthBB,3),
#     widthBB4 = lag(widthBB,4),
#     widthBB5 = lag(widthBB,5)
#   )

ChangeDateFormat <- function(date){
  
  as.Date(ISOdate(substring(date,1,4), substring(date,5,6), substring(date,7,8)))
}


#########條件##############
buy_signal<-data %>%
  
  filter(
    
         buy_signal==1
        
        ) %>%
  
  select(code,date,indate,buyprice,K,D) %>% 
  rename(Buy_K=K,Buy_D=D)

sell_signal<-data %>%
  
  filter(
    
         K<D,
         K>50
       
        )%>%
  
  select(code,date,sellprice,K,D) %>% 
  rename(Sell_K=K,Sell_D=D)
#########迴圈##############

if(nrow(buy_signal)>0){
  
  result <- buy_signal %>%
    group_by() %>%  # 取消group_by，避免出現錯誤
    mutate(
           outDate=sapply(c(1:nrow(buy_signal)),
                     
                     function(x)
                     {                                      # 找尋離該支股票訊號最近的出場日
                       trading <- which(sell_signal$code==buy_signal$code[x] &
                                        sell_signal$date>buy_signal$date[x])
                       ifelse(length(trading)>0, sell_signal$date[trading[1]], NA)
                     }
                         
          )
          ) %>%
    
    left_join(sell_signal,by=c("code"="code","outDate"="date"))
                                             
}else{
  stop("此策略組合無任何進場訊號可供計算報酬率!")
}
 save(result,file="result.Rdata")
###############停利損########
load("result.Rdata")
 result<-result %>% na.omit()
 output<-NULL 
 
 for(ix in 1:nrow(result))#nrow(result)
{
   
   cat(paste0("目前正在執行第 ",ix," 個，進度: ",round(ix/nrow(result),5)*100," % " ,"\n"))
  stop<-data %>% group_by() %>% 
    
         filter(code==result$code[ix],
                  date>=result$date[ix],
                  date<=result$outDate[ix]) %>% 
    
  mutate(profit=(sellprice/buyprice[1])-1) %>% 
  select(code,name,date,open,high,low,close,vol,buyprice,sellprice,profit) %>% 
  mutate(max_profit=cummax(profit),
         min_profit=cummin(profit),
        stop_profit_signal=
          ifelse(max_profit>=start_stop_profit & profit<=max_profit-stop_profit_down,1,0),
        stop_loss_signal=
          ifelse(min_profit<=start_stop_loss-stop_loss_down & profit<=start_stop_profit-stop_profit_down,1,0),
        stop_signal=ifelse(stop_profit_signal|stop_loss_signal==1,1,0),
        stop_price=ifelse(stop_signal==1,low,NA),
        stop_date=ifelse(stop_signal==1,date,NA)) %>% 
        rename(out_date_check=date) %>% 
    select(out_date_check,stop_loss_signal,stop_profit_signal,stop_signal,stop_price,stop_date)
  
    if(sum(stop$stop_signal)>=1){
           (stop1<-stop[which(stop$stop_signal==1)[1],])
    }else{
          (stop1<-stop[nrow(stop),])
    }

  
  output<-rbind(output, stop1)
  
 }
 
 final<-NULL
 final<-bind_cols(result,output)
 
 final<-final %>% 
   
   mutate(
     check=ifelse(stop_signal==1,
                  (ifelse(out_date_check==stop_date,0,1)),
                   (ifelse(out_date_check==outDate,0,1)))
     )
   if(sum(final$check)>0){
     (print(c("錯誤")))}else{(print(c("正確")))}

 save(final,"loop_final.Rdata")
#########################################
 # load("loop_final.Rdata")
 analysis<-final %>% 
  mutate(
         inPriceTc=buyprice*(1+0.0014),
         outPrice=ifelse(stop_signal==1,stop_price,sellprice),
         outPriceTc=outPrice*(1-0.0044),
         ret=(outPrice/buyprice)-1,
         retTc=(outPriceTc/inPriceTc)-1,
         
         holdDays=ifelse(stop_signal==1,ChangeDateFormat(stop_date) - ChangeDateFormat(indate)+1,ChangeDateFormat(outDate) - ChangeDateFormat(indate)),   # 每次交易報酬率(不含交易成本)
         
         original_holdDays=ChangeDateFormat(outDate) - ChangeDateFormat(indate)+1,   # 計算各次交易持有日數
         original_ret=sellprice/buyprice-1,                                          # 每次交易報酬率(不含交易成本)
         original_inPriceTc=buyprice*(1+0.0014),                                  # 含交易成本進場價
         original_outPriceTc=sellprice*(1-0.0044),                               # 含交易成本出場價
         original_retTc=(original_outPriceTc/original_inPriceTc-1),
         
         stop_profit_retTc=ifelse(stop_profit_signal==1,retTc,original_retTc),
         
         stop_loss_retTc=ifelse(stop_loss_signal==1,retTc,original_retTc)
         
         ) 
show1<-
tibble(
  holddays=mean(as.numeric(analysis$holdDays)),
  avgret=mean(as.numeric(analysis$retTc)),
  tradeNum=length(analysis$retTc),
  winRatio=length(which(analysis$retTc>0))/tradeNum,
  minRET = min(as.numeric(analysis$retTc)),
  maxRET = max(as.numeric(analysis$retTc)),
  sdRET = sd(as.numeric(analysis$retTc)),
  stoptimes=sum(as.numeric(analysis$stop_signal)),
  stop_profit_times=sum(as.numeric(analysis$stop_profit_signal)),
  stop_loss_times=sum(as.numeric(analysis$stop_loss_signal)),
  stop_profit_retTc=mean(as.numeric(analysis$stop_profit_retTc)),
  stop_loss_retTc=mean(as.numeric(analysis$stop_loss_retTc)),
  
  original_holddays=mean(as.numeric(analysis$original_holdDays)),
  original_avgret=mean(as.numeric(analysis$original_ret)),
  original_tradeNum=length(analysis$original_retTc),
  original_winRatio=length(which(analysis$original_retTc>0))/tradeNum,
  original_minRET = min(as.numeric(analysis$original_retTc)),
  original_maxRET = max(as.numeric(analysis$original_retTc)),
  original_sdRET = sd(as.numeric(analysis$original_retTc))
  )
show<-NULL
show<-rbind(show, show1)

# }}}
# save(show,file="final_result.Rdata")
#########繪圖##############
# png("001.png",width=1180,height=451)
sampleNum<-2
# 
final1<-final %>% group_by() %>% 
  
  
  mutate(
         real_sellprice=ifelse(stop_signal==1,stop_price,sellprice),
         real_outdate=ifelse(stop_signal==1,stop_date,outDate),
         holddays=ChangeDateFormat(real_outdate) - ChangeDateFormat(indate)+1
         ) %>% 
  select(code,indate,real_outdate,buyprice,real_sellprice,holddays) 
  final1<-bind_cols(final1,analysis[,4:5]) %>%
  arrange(desc(holddays))

plotCode <- final1$code[sampleNum]
plotInDate <- final1$indate[sampleNum]-1
plotInPrice <- final1$buyprice[sampleNum]
plotOutDate <- final1$real_outdate[sampleNum]
plotOutPrice <- final1$real_sellprice[sampleNum]

# 建立日期序列
data1<-data %>% group_by()
tradeDate <- data1  %>%  distinct(date)
#
# 繪圖資料起始點
matchStartSite <- which(tradeDate$date==plotInDate)-30
matchStartSite <- ifelse(matchStartSite<1,1,matchStartSite)
matchEndSite <- which(tradeDate$date==plotOutDate)+30
matchEndSite<- ifelse(matchEndSite>nrow(tradeDate),nrow(tradeDate),matchEndSite)
plotStartDate <- ifelse(length(matchStartSite)>0,tradeDate$date[matchStartSite],tradeDate$date[1])
plotEndDate <- ifelse(length(matchEndSite)>0,tradeDate$date[matchEndSite],tradeDate$date[nrow(tradeDate)])

# 整理股票價格
stock <- data1 %>%
  filter(code==plotCode) %>%
  rename(volume=vol) %>%
  mutate(Buy=ifelse(date==plotInDate,plotInPrice,NA),
         Sell=ifelse(date==plotOutDate,plotOutPrice,NA),
         MA_5=SMA(close,5),
         MA_10=SMA(close,10),
         MA_20=SMA(close,20),
         K = K,
         D = D,
         Buy_K=ifelse(date==plotInDate,K,NA),
         Sell_K=ifelse(date==plotOutDate,K,NA)

  ) %>%
  filter(date>=plotStartDate & date<=plotEndDate) %>%
  select(-(code))

# 轉換為xts格式
stock1<-stock %>% 
select(open:volume,Buy:Sell_K,K:D)
  stock1 <-xts(stock1, order.by=as.Date(ISOdate(year=substring(stock$date,1,4),
                                                       month=substring(stock$date,5,6),
                                                       day=substring(stock$date,7,8)),format="%Y%m%d"))
myTheme <- chart_theme()
myTheme$col$dn.col <- c("chartreuse3")
myTheme$col$up.col <- c("firebrick3")


# up<-function(x){add_TA(stock$up, on=1, col="darkblue", lwd=1.5)}
# dn<-function(x){add_TA(stock$dn, on=1, col="darkblue", lwd=1.5)}
# mavg<-function(x){add_TA(stock$mavg, on=1, col="darkblue", lwd=1.5)}
chart_Series(stock1,name=final1$code[sampleNum],theme=myTheme)
add_Vo()

add_TA(stock1$Buy, on=1, col="black", type = 'p', cex = 3, pch = 1, lwd = 3)
add_TA(stock1$Sell, on=1, col="black", type = 'p', cex = 3, pch = 1, lwd = 3)
add_TA(stock1$MA_5, on=1, col="blue", lwd=1.5)
add_TA(stock1$MA_10, on=1, col="orange", lwd=1.5)
add_TA(stock1$MA_20, on=1, col="green", lwd=1.5)
add_TA(stock1$K,on=NA,col="blue",lwd=1.5)
add_TA(stock1$D,on=3,col="red",lwd=1.5)
add_TA(stock1$Buy_K,on=3,col="black",type = 'p', cex = 3, pch = 1, lwd = 3)
add_TA(stock1$Sell_K,on=3,col="black",type = 'p', cex = 3, pch = 1, lwd = 3)


# dev.off()
ggplot(final1, aes(x=retTc))+
geom_histogram(color="darkblue", fill="lightblue",bins = 100)

final2<-final1

final2$indate<-as.Date(ISOdate(year=substring(final2$indate,1,4),
                               month=substring(final2$indate,5,6),
                               day=substring(final2$indate,7,8)),format="%Y%m%d")
final2<-final2 %>% 
  mutate(year=year(indate),
         month=month(indate)) %>% 
  group_by(year,month)
by <- final2 %>% group_by(year,month) %>% 
  
  summarise(count=n(),
            ret=mean(retTc),
            win=(sum(retTc>0))/count,
            holddays=mean(holdDays))%>% 
  mutate(date=year*100+month) 

ggplot(by, aes(x=date,y=win))+
  geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))



#############結果####
cat(
  " 平均持有天數",round(show$holddays,2),"天","\n",
  "平均報酬率",(round(show$avgret,5))*100,"%"," \n",
  "最小報酬率",(round(show$minRET,5))*100,"%","\n",
  "最大報酬率",(round(show$maxRET,5))*100,"%","\n",
  "報酬率標準差",(round(show$sdRET,5))*100,"%","\n",
  "總交易筆數",show$tradeNum,"\n",
  "策略勝率",(round(show$winRatio,5))*100,"%","\n","\n",
  "停利停損次數",(show$stoptimes),"\n",
  "停利次數",(show$stop_profit_times),"\n",
  "停利次數",(show$stop_loss_times),"\n",
  "平均停利報酬率",(round(show$stop_profit_retTc,5))*100,"%"," \n",
  "平均停損報酬率",(round(show$stop_loss_retTc,5))*100,"%"," \n",
  
  "原始平均持有天數",round(show$original_holddays,2),"天","\n",
  "原始平均報酬率",(round(show$original_avgret,5))*100,"%"," \n",
  "原始最小報酬率",(round(show$original_minRET,5))*100,"%","\n",
  "原始最大報酬率",(round(show$original_maxRET,5))*100,"%","\n",
  "原始報酬率標準差",(round(show$original_sdRET,5))*100,"%","\n",
  "原始總交易筆數",show$original_tradeNum,"\n",
  "原始策略勝率",(round(show$original_winRatio,5))*100,"%","\n"
  )


