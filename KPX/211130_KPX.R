library(tidyverse)
library(RColorBrewer)
library(scales)
library(lubridate)
library(ggh4x)
library(plotly)



KPX.raw<-read.csv("211129_KPX_MWh.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 
## 연료원별 전력 거래량




KPX.demand.raw<-read.csv("211129_KPX_demand.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 
### 수요량



str(KPX.raw)


head(KPX.raw)




KPX.datetime<-KPX.raw %>%
  mutate(datetime = ymd_h(str_c(date, hour, sep = " "))) %>% 
  select(datetime, source, MWh)



head(KPX.datetime)

str(KPX.datetime)

KPX.trade.2020.area<-KPX.datetime %>% 
  filter(datetime > ymd_hms("2020-01-01 01:00:00")) %>% 
  ggplot(aes(x = datetime, y = MWh, group= source, fill = source))+
  geom_area(position = "stack", stat ="identity")+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)






head(KPX.trade.area)

str(KPX.datetime)

KPX.trade.2020.12.area<-KKPX.trade.area<-KPX.datetime %>% 
  filter(datetime > ymd_hms("2020-12-01 01:00:00")) %>% 
  ggplot(aes(x = datetime, y = MWh, group= source, fill = source))+
  geom_area(position = "stack", stat ="identity")+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)

KPX.trade.2020.12.area


KPX.trade.2020.12.line<-KKPX.trade.area<-KPX.datetime %>% 
  filter(datetime > ymd_hms("2020-12-01 01:00:00")) %>% 
  ggplot(aes(x = datetime, y = MWh,color = source))+
  geom_line()+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)

KPX.trade.2020.12.line



## facet_wrap2를 이용해서 각 plot에 x 축 값 넣어주기
library(ggh4x)


#### facet wrap


#### 2020년 전체
KPX.datetime %>% 
  filter(datetime > ymd_hms("2020-01-01 01:00:00")) %>% 
  ggplot(aes(x = datetime, y = MWh, group= source, fill = source))+
  geom_area(position = "stack", stat ="identity")+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)+
  facet_wrap2(~source, ncol = 4, axes = "all", remove_labels ="y")+
  labs( x = NULL, y = "MWh",
        title = " 2020 대한민국 연료원별 전력거래량",
        caption = "데이터 출처 : 한국전력거래소, 제작 : KIER")+
  theme(legend.position = "none")



#### 2020년 7월 한 달만 보기(전체 다 보려면 rendering 시간이 오래 걸려서;;)
KPX.datetime %>% 
  filter(datetime > ymd_hms("2020-07-01 01:00:00") & datetime < ymd_hms("2020-08-01 01:00:00")) %>% 
  ggplot(aes(x = datetime, y = MWh, group= source, fill = source))+
  geom_area(position = "stack", stat ="identity")+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)+
  facet_wrap2(~source, ncol = 4, axes = "all", remove_labels ="y")+
  labs( x = NULL, y = "MWh",
        title = " 2020년 7월 연료원별 전력거래량",
        caption = "데이터 출처 : 한국전력거래소, 제작 : KIER")+
  theme(legend.position = "none")



#### 2020년 7월 한 주(7~13(월~일))만 보기(전체 다 보려면 rendering 시간이 오래 걸려서;;)
KPX.datetime %>% 
  filter(datetime > ymd_hms("2020-07-07 0:00:00") & datetime < ymd_hms("2020-07-14 01:00:00")) %>% 
  ggplot(aes(x = datetime, y = MWh, group= source, fill = source))+
  geom_area(position = "stack", stat ="identity")+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)+
  facet_wrap2(~source, ncol = 4, axes = "all", remove_labels ="y")+
  labs( x = NULL, y = "MWh",
        title = " 2020년 7월 한 주(7~13일) 연료원별 전력거래량",
        caption = "데이터 출처 : 한국전력거래소, 제작 : KIER")+
  theme(legend.position = "none")



#### 2020년 7월 1일만 보기(전체 다 보려면 rendering 시간이 오래 걸려서;;)
KPX.datetime %>% 
  filter(datetime > ymd_hms("2020-07-01 01:00:00") & datetime < ymd_hms("2020-07-02 01:00:00")) %>% 
  ggplot(aes(x = datetime, y = MWh, group= source, fill = source))+
  geom_area(position = "stack", stat ="identity")+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)+
  facet_wrap2(~source, ncol = 4, axes = "all", remove_labels ="y")+
  labs( x = NULL, y = "MWh",
        title = " 2020년 7월 1일 연료원별 전력거래량",
        caption = "데이터 출처 : 한국전력거래소, 제작 : KIER")+
  theme(legend.position = "none")




## 필터로 2020년 10월 이후 자료만 

KPX.datetime %>% 
  filter(datetime > ymd_hms("2020-10-01 01:00:00")) %>% 
  ggplot(aes(x = datetime, y = MWh, group= source, fill = source))+
  geom_bar(position = "stack", stat ="identity")+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)


KPX.trade.line<-KPX.datetime %>% 
  ggplot(aes(x = datetime, y = MWh, color=source))+
  geom_line()+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)




## plotly 라이브러리로 인터랙티브화




ggplotly(KPX.trade.2020.area)


ggplotly(KPX.trade.area)

p_interactive
head(KPX.demand)


KPX.demand <- KPX.demand.raw %>% 
  gather(hour, MWh, -date) 


KPX.demand <-KPX.demand %>% 
  mutate(datetime = ymd_h(str_c(date, hour, sep = " "))) %>% 
  select(datetime, MWh)

KPX.demand %>% 
  ggplot(aes(x = datetime, y = MWh))+
  geom_line()+
  scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)


############################### ESS 관련 ###################################################


## 지역별 태양광, 풍력, ESS with 태양광, ESS with 풍력

KPX.ESS.raw<-read.csv("211201_KPX_ESS.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 


head(KPX.ESS.raw)
View(KPX.ESS.raw)
str(KPX.ESS.raw)

KPX.ESS.raw$year <-as.numeric(KPX.ESS.raw$year)
KPX.ESS.raw$month <-as.numeric(KPX.ESS.raw$month)


KPX.ESS.tidy <-KPX.ESS.raw %>% 
  gather(type, MW, -year, -month, -region)


KPX.ESS.tidy



### combining year month into one data
library(zoo)

KPX.ESS.tidy<-transform(KPX.ESS.tidy, datetime = as.yearmon(paste(year, month, sep = "-")))

View(KPX.ESS.tidy)



KPX.ESS.tidy <-KPX.ESS.tidy %>%
  select( -year, -month)


#### 그래프 만들어보자 



KPX.ESS.tidy %>% 
  ggplot(aes(x = datetime, y = MW, group= type, fill = type))+
  geom_area(position = "stack", stat ="identity")+
  # scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)+
  facet_wrap2(~region, ncol = 5, axes = "all", remove_labels ="y")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = comma) +
  labs( x = NULL, y = "MWh",
        title = " 2018- 2021 신재생에너지연계 설비용량",
        caption = "데이터 출처 : 한국전력거래소, 제작 : KIER")+
  theme_minimal()




head(KPX.ESS.tidy)

#### 그래프 만들어보자- only 태양광 and ESS with 태양광
KPX.ESS.tidy %>% 
  filter(type == "pv" | type == "ESSwithpv") %>% 
  ggplot(aes(x = datetime, y = MW, color = type))+
  geom_line()+
# scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)+
  facet_wrap2(~region, ncol = 5, axes = "all", remove_labels ="y")+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(labels = comma) +
  labs( x = NULL, y = "MW",
        title = " 2018- 2021 신재생에너지연계 설비용량",
        caption = "데이터 출처 : 한국전력거래소, 제작 : KIER")+
  theme_minimal()

#### 그래프 만들어보자- only 풍력 and ESS with 풍력
KPX.ESS.tidy %>% 
  filter(type == "wind" | type == "ESSwithwind") %>% 
  ggplot(aes(x = datetime, y = MW,color = type))+
  geom_line()+
  # scale_y_continuous(breaks = seq(0,80000, 10000), labels = comma)+
  facet_wrap2(~region, ncol = 5, axes = "all", remove_labels ="y")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = comma) +
  labs( x = NULL, y = "MW",
        title = " 2018- 2021 신재생에너지연계 설비용량",
        caption = "데이터 출처 : 한국전력거래소, 제작 : KIER")+
  theme_minimal()


######################KEPCO 시군구별 전력판매량 정보






KEPCO.trade.raw<-read.csv("211202_KEPCO_powerconsumption.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 
### 수요량



str(KEPCO.trade.raw)

head(KEPCO.trade.raw)


### type '심 야', '합 계' 빈칸 gsub을 통해 없애기
KEPCO.trade.raw$type <-gsub(" ","", KEPCO.trade.raw$type)


KEPCO.trade.tidy<-KEPCO.trade.raw %>%
  gather(month, kWh, -year, -sido, -gungu, -type) %>% 
  filter(type != "합계")



View(KEPCO.trade.tidy)



########## combining year and month
library(zoo)


KEPCO.trade.tidy<-transform(KEPCO.trade.tidy, datetime = as.yearmon(paste(year, month, sep = "-"))) %>% 
  select(datetime, sido, gungu, type, kWh, -year, -month)

View(KEPCO.trade.tidy)


head(KEPCO.trade.tidy)


KEPCO.trade.sido.level <-KEPCO.trade.tidy %>% 
  select(-gungu) 

KEPCO.trade.sido.level

str(KEPCO.trade.sido.level)

KEPCO.trade.sido.level$kWh<-as.numeric(gsub(",","", KEPCO.trade.sido.level$kWh))

KEPCO.trade.sido.level %>% 
  group_by(datetime, sido, type) %>% 
  summarise(kWh = sum(kWh))

