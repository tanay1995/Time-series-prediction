library("readr")
library('tidyr') 
library('dplyr') 
library('ggplot2') 
library('ggthemes') 
library('corrplot') 
library('lubridate') 
library('purrr') 
library('cowplot')
library('maps')
library('viridis')
library('treemap')
library('leaflet')
library('dygraphs')
library('graphics')
library('forecast')
library('xts')
library('IRdisplay')
options(scipen = 999)
options(warn = -1)
fao=read.csv('FAO.csv')
summary(fao)
str(fao)
options(repr.plot.width=6, repr.plot.height=6)
missing_data <- fao %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
colnames(fao)[1] <- "area_abb"
colnames(fao)[2] <- "area_code"
colnames(fao)[3] <- "area"
colnames(fao)[4] <- "item_code"
colnames(fao)[5] <- "item"
colnames(fao)[6] <- "element_code"
colnames(fao)[7] <- "element"
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +xlab('variables')+
  geom_bar(stat = "identity", fill = "yellow", aes(color = I('blue')), size = 0.3)+coord_flip()+ theme_bw()
head(fao)
options(repr.plot.width=10, repr.plot.height=4)
fao <- mutate(fao, Total=apply(fao[11:63], 1, sum, na.rm = T))
fao <- mutate(fao, last5=apply(fao[58:63], 1, sum, na.rm = T))
#top10foodproducers
p2 <- fao %>% group_by(area_abb, element) %>%filter(element == 'Food')%>% 
  summarise(TFO = sum(Total)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(area_abb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightblue", aes(color = I('black')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,
            vjust = -0.5, size = 3)+ theme_bw()+ xlab("Country") + ylab("Food production since 1961")

p3 <- fao %>% group_by(area_abb, element) %>%filter(element == 'Food')%>% 
  summarise(TFO = sum(last5)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(area_abb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightblue", aes(color = I('black')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,
            vjust = -0.5, size =3)+ theme_bw()+ xlab("Country") + ylab("Food production since 2008-13")

plot_grid(p2,p3, align = "h")
#time series
options(repr.plot.width=10, repr.plot.height=12)
options(scipen = 99999)
fao4 <- fao %>% gather(11:63 ,key = "year", value = "Total")%>%
  group_by(year, area_abb) %>% 
  summarise(TFO = sum(Total)) %>% spread(key = area_abb, value = TFO)
fao4$year <- as.Date(gsub("Y", "", fao4$year), format = '%Y')

df_ts <- as.xts(x = fao4[,-1], order.by = fao4$year)
#foodvsfeed
fao1 <- fao %>% gather(11:63 ,key = "year", value = "Total")%>%
  group_by(year, element) %>% na.omit()%>%
  summarise(TFO = sum(Total)) 

ggplot(fao1, aes(year, TFO)) + 
  geom_line(aes(group = element), colour = "yellow") + 
  geom_point(aes(colour = element))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Year") + ylab("Total Production")+ ggtitle("Comparison of Food & Feed Production")
#top10food
p6 <- fao %>% group_by(item) %>%filter(element == 'Food')%>% 
  
  summarise(TFO = sum(Total)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(item, pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightyellow", aes(color = I('black')), size = 0.1) + coord_flip()+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 1.2,
            vjust = -0.5, size = 3)+ theme_bw()+ xlab("Food Item") + ylab("since 1961")

p7 <- fao %>% group_by(item) %>%filter(element == 'Food')%>% 
  summarise(TFO = sum(last5)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(item, pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightyellow", aes(color = I('black')), size = 0.1) + coord_flip()+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 1.2,
            vjust = -0.5, size =3)+ theme_bw()+ xlab("Food Item") + ylab("since 2008-13")

plot_grid(p6,p7, align = "h")
plot_grid(p6, align = "h")

#top10feed
p8 <- fao %>% group_by(item) %>%filter(element == 'Feed')%>% 
  summarise(TFO = sum(Total)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(item, pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightpink", aes(color = I('black')), size = 0.1) + coord_flip()+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.01,
            vjust = -0.5, size = 3)+ theme_bw()+ xlab("Food Item") + ylab("Feed Item production since 1961")

p9 <- fao %>% group_by(item) %>%filter(element == 'Feed')%>% 
  summarise(TFO = sum(last5)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(item, pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightpink", aes(color = I('black')), size = 0.1) + coord_flip()+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.01,
            vjust = -0.5, size =3)+ theme_bw()+ xlab("Food Item") + ylab("Feed Item production since 2008-13")

plot_grid(p8,p9, align = "h")
#top feed producers
p4 <- fao %>% group_by(area_abb, element) %>%filter(element == 'Feed')%>% 
  summarise(TFE = sum(Total)) %>% ungroup()%>%mutate(pct = prop.table(TFE)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(area_abb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightgreen", aes(color = I('black')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), 
            vjust = -.5, size = 3)+ theme_bw()+ xlab("Country") + ylab("Feed production since 1961")

p5 <- fao %>% group_by(area_abb, element) %>%filter(element == 'Feed')%>% 
  summarise(TFE = sum(last5)) %>% ungroup()%>%mutate(pct = prop.table(TFE)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(area_abb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightgreen", aes(color = I('black')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), 
            vjust = -.5, size = 3)+ theme_bw()+ xlab("Country") + ylab("Feed production since 2008-13")
plot_grid(p4,p5, align = "h")
#production of feed items
options(repr.plot.width=10, repr.plot.height=12)
fao3 <- fao %>% gather(11:63 ,key = "year", value = "Total")%>%
  group_by(year, item, element) %>% na.omit()%>%
  summarise(Total_production = sum(Total))%>%filter(element == 'Feed')
ggplot(data = fao3) +
  theme_bw() +
  geom_tile(aes(x = year, y = item, fill = Total_production)) +
  scale_fill_viridis(direction = -1) + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5), 
                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year") + ylab("Feed Items")+ ggtitle("Production of feed items over the years")

#TIME MODELLING

options(repr.plot.width=6, repr.plot.height=4)
library(graphics)
library(forecast)
library(tseries)
fao5 <- fao %>% gather(11:63 ,key = "year", value = "Total")%>%
  group_by(year) %>% na.omit()%>%
  summarise(TP = sum(Total))
timeser <- ts(fao5$TP)
plot(timeser)
#regression
timevals <- c(1961:2013)
timeserdf <- as.data.frame(cbind(timevals, as.vector(timeser)))
colnames(timeserdf) <- c('Year', 'Total_Production')
lmfit <- lm(Total_Production ~ Year, data=timeserdf)
globalpred <- predict(lmfit, Year=timevals)
plot(globalpred, col='red', type ="l" ,lwd=2)
localpred <- timeser - globalpred
plot(localpred, col='red', type = "l")
acf(localpred)
acf(localpred, type="partial")
armafit <- auto.arima(localpred)
#tsdiag(armafit)
armafit
armapred <- fitted(armafit)
plot(armapred)
residual <- localpred - armapred

plot(residual)
adf.test(residual,alternative = "stationary")
kpss.test(residual)
checkresiduals(armafit)
faoall= read.csv("faoall.csv")

library(dplyr)
indgdp=faoall %>%
  select(year, IND)
timGDP <- ts(indgdp$IND)
plot(timGDP)
timeind <- ts(indgdp$IND)
timevals <- c(1961:2013)
timeinddf <- as.data.frame(cbind(timevals, as.vector(timeind)))
colnames(timeinddf) <- c('Year', 'IND')
lmfitind <- lm(indgdp)
globalpredind <- predict(lmfitind, Year=timevals)
plot(globalpredind, col='green', type ="l" ,lwd=2)
localpredind <- timeind - globalpredind
str(localpredind)
plot(localpredind, col='blue', type = "l")
acf(localpredind)
acf(localpredind, type="partial")
armafit <- auto.arima(ts(faoall[,"IND"]))
armafit
armapredind <- fitted(armafit)
plot(armapredind)
mean(faoall[,"IND"])
str(faoall[,"IND"])
mean(armapred)
str(armapred)
residual <- localpred - armapred
plot(residual)
max(faoall[,"IND"])
adf.test(residual,alternative = "stationary")
kpss.test(residual)

accuracy(armafit)
checkresiduals(armafit)
head(fao4)

fao4new= fao4
fao4new <- na.omit(fao4new) 
fao4new <- scale(fao4new) 
library(mclust)
fit <- Mclust(fao4new)
plot(fit)  
summary(fit) 
valuedel= c(1,8,9,12,13,18,19,40,50,51,57,67,81,83,95,96,97,99,106,122,133,136,141,144,145,150,151,152,159,162)
fao4new= fao4[,-valuedel]
valuedel2= c(6,8,12,32,41,46,55,68,69,80,81,87,102,112,114,118,120,124,130,132)
f4new= fao4new[,-valuedel2]
fao4new= fao4[]

library(TSrepr) 
data1 <- repr_matrix(f4new, func = repr_seas_profile,
                     args = list(freq = 10, func = mean),
                     normalise = TRUE, func_norm = norm_z)
library(cluster)
clusterings <- lapply(c(2:15), function(x)
  pam(data1, x)) 

library(clusterCrit)
DB_values <- sapply(seq_along(clusterings), function(x) 
  intCriteria(data1, as.integer(clusterings[[x]]$clustering),
              c("Davies_Bouldin")))


library(ggplot2)
ggplot(data.table(Clusters = 2:7, DBindex = unlist(DB_values)),
       aes(Clusters, DBindex)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw()

data_plot <- data.table(melt(data.table(class = as.factor(clusterings[[1]]$clustering),
                                        data1)))
data_plot[, Time := rep(1:ncol(data1), each = nrow(data1))]
data_plot[, ID := rep(1:nrow(data1), ncol(data1))]

# prepare medoids
centers <- data.table(melt(clusterings[[1]]$medoids))
setnames(centers, c("Var1", "Var2"), c("class", "Time"))
centers[, ID := class]

ggplot(data_plot, aes(Time, value, group = ID)) +
  facet_wrap(~class, ncol = 2, scales = "free_y") +
  geom_line(color = "grey10", alpha = 0.65) +
  geom_line(data = centers, aes(Time, value),
            color = "firebrick1", alpha = 0.80, size = 1.2) +
  labs(x = "Time", y = "Load (normalised)") +
  theme_bw()

fit <- HoltWinters(timGDP, beta=FALSE, gamma=FALSE)

fit <- HoltWinters(timGDP, gamma=FALSE)

library(forecast)
accuracy(fit)

library(forecast)
forecast(fit, 20)
plot(forecast(fit, 20))
library(forecast)

fit <- ets(timGDP)

fit <- auto.arima(timGDP)
accuracy(fit)
#france
timefra= ts(fao4$FRA)
plot(timefra)
tsclean(faoall$FRA)
adf.test(faoall$FRA)
faoall$year = as.Date(faoall$year)

ggplot(faoall, aes(faoall$year, faoall$FRA)) + geom_line() + scale_x_date('years')  + ylab("production") +
  xlab("")
auto.arima(timefra, seasonal=FALSE)
franceMOD<-auto.arima(timefra, seasonal=FALSE)
tsdisplay(residuals(franceMOD), lag.max=45, main='france prediction')
accuracy(franceMOD)
FRAcast <- forecast(franceMOD, h=30)
plot(FRAcast)
#ireland
timeire= ts(fao4$IRL)
plot(timeire)
tsclean(faoall$IRE)
adf.test(faoall$IRE)
faoall$year = as.Date(faoall$year)

ggplot(faoall, aes(faoall$year, faoall$FRA)) + geom_line() + scale_x_date('years')  + ylab("production") +
  xlab("")
auto.arima(timeire, seasonal=FALSE)
irelandMOD<-auto.arima(timeire, seasonal=FALSE)
tsdisplay(residuals(irelandMOD), lag.max=45, main='Ireland prodiction')
accuracy(irelandMOD)
IRLcast <- forecast(irelandMOD, h=30)
plot(IRLcast)
accuracy(IRLcast)
irefit <- HoltWinters(timeire, beta=FALSE, gamma=FALSE)
irefit <- ets(timeire)
irefit <- auto.arima(timeire)
accuracy(irefit)
summary(df_ts$RUS)
tsr = ts(timeser)
auto.arima(tsr, seasonal=FALSE)
allmOD<-auto.arima(tsr, seasonal=FALSE)
tsdisplay(residuals(allmOD), lag.max=45, main='global production')
accuracy(allmOD)
allcast <- forecast(allmOD, h=30)
plot(allcast)
#crucial modelling
demand <- ts(fao4$CHN, start = c(1961, 1), frequency = 1)
plot(demand)
summary(demand)
demfitchi <- HoltWinters(demand, beta=FALSE, gamma=FALSE)
plot(demfitchi)
demforchi <- forecast(demfitchi, h=30)
plot(demforchi)
predict(demforchi)
accuracy(demforchi)
chiMOD<-auto.arima(demand, seasonal=FALSE)
tsdisplay(residuals(chiMOD), lag.max=45, main='China prediction')
accuracy(chiMOD)
chicast <- forecast(chiMOD, h=30)
plot(chicast)
summary(chicast)
expofit <- ets(demand)
forecast(expofit, 30)
plot(forecast(expofit, 30))
accuracy(expofit)

#tsdiag(armafit)
global <- ts(fao5$TP, start = c(1961, 1), frequency = 1)
lmfitchi <- lm(Total_Production ~ Year, data=timeserdf)
globalpredchi <- predict(lmfitchi, Year=timevals)
plot(globalpredchi, col='red', type ="l" ,lwd=2)
localpredchi <- global - globalpredchi
plot(localpredchi, col='green', type = "l")
acf(localpredchi)
acf(localpredchi, type="partial")
armafitchi <- auto.arima(global)
armafitchi
armapredchi <- fitted(armafitchi)
plot(armapredchi)
accuracy(armafitchi)
forchi=forecast(armapredchi, 30)
plot(forchi)
accuracy(forchi)
residualchi <- localpredchi - armapredchi
adf.test(residualchi,alternative = "stationary")
kpss.test(residualchi)
checkresiduals(armapredchi)
accuracy(lmfitchi)
