
library(tidyverse)

##working directory 지정 Ctrl + shift + h
##check.names=FALSE 넣어야 함!! 년도 앞에 x 안나타나게


co2<-read.csv("211110_EPSIStotalGen.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE)


str(co2)
head(co2)

#datawrapper용 csv 뽑기

co2.total_datawrapper <- co2 %>% 
  filter(구분=="총배출량")

head(co2.total_datawrapper)


write.csv(co2.total_datawrapper, 'co2.total_datawrapper.csv')




##현재 wide data임
## gather() 함수를 이용해서 tidy data로 전환 필요


head(co2)

co2.tidy <-co2 %>% 
  gather(year, emission, -지역, -구분)

head(co2.tidy)

View(co2.tidy)

str(co2.tidy)




##numeric으로 설정!!!

co2.tidy$year <- as.numeric(co2.tidy$year)
co2.tidy$emission <- as.numeric(co2.tidy$emission)


head(co2.tidy)

View(co2.tidy)

##0인 값들을 NA로 설정. 그래프에 0값 안 뜨게

co2.tidy[co2.tidy == 0] <-NA

View(co2.tidy)





#datawrapper용 csv 뽑기

co2.total <- co2.tidy %>% 
  filter(구분=="총배출량" & 지역!="국가")



## 국가 제외하고, 총배출량만 필터터 
co2.total <- co2.tidy %>% 
  filter(구분=="총배출량" & 지역!="국가")


co2.total

head(co2.total)


class(co2.total)
str(co2.total)

## facet_wrap을 통해 각각 보여주기

co2.total %>% 
  ggplot()+
  geom_line(mapping= aes(x=year, y=emission))+
  facet_wrap(~지역)



## facet_wrap을 통해 각각 보여주기 (bar chart)

co2.total %>% 
  ggplot(mapping= aes(x=year, y=emission))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~지역)


## 한판에 보여주기

library(RColorBrewer)

library(gganimate)
library(gifski)



## gif 애니매이션 만들기

co2.total.result <-co2.total %>% 
  ggplot()+
  geom_line(mapping= aes(x=year, y=emission, color = 지역))+
  scale_fill_brewer(palette="Paired")+
  scale_x_continuous(breaks = seq(1990,2020,10))+
  transition_reveal(year)   # gif로 만드는 코드

## co2.total.result



## animate(co2.total.result, renderer = gifski_renderer())


## 지역별 순배출량 만들기

co2.net <- co2.tidy %>% 
  filter(구분=="순배출량" & 지역!="국가")

head(co2.net)


#### 총배출량, 순배출량 두 개 다 그리기

co2.kor <- ggplot()+
  geom_line(data=co2.total, mapping=aes(x=year, y=emission), color="red", alpha=0.6)+
  geom_line(data=co2.net, mapping =aes(x=year, y=emission), color="blue", alpha=0.6)+
  facet_wrap(~지역)+
  labs(title = "대한민국 17개 시도 온실가스 배출현황(1990-2018)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "온실가스 배출량 (Gt CO2eq.)",
       caption= "제작:KIER, 데이터 출처: 환경부 온실가스 종합 정보센터")

co2.kor




## ggthemes 테마

library(ggthemes)



### pdf 글꼴

if(!require(showtext)) {
  install.packages('showtext')
  library(showtext)
}
library(showtext)
showtext_auto()


############# 대한민국 온실가스 부문별로 구분(에너지/산업공정/농업/LULUCF/폐기물)
## 총배출량 : 에너지 + 산업공정 + 농업 + 폐기물
## 순배출량 : 에너지 + 산업공정 + 농업 + 폐기물 + LULUCF


co2_total<-read.csv("210818_co2_emission_total.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE)



View(co2_total)

str(co2_total)
head(co2_total, 10)

###co2_total_sector  <- co2_total %>% 
###  filter(구분 %in% c("에너지", "산업공정", "농업", "LULUCF", "폐기물"))



co2_total_tidy <-co2_total %>% 
  gather(year, emission, -구분)


co2_total_tidy



str(co2_total_tidy)

## 숫자로 세팅


co2_total_tidy$year <- as.numeric(co2_total_tidy$year)


co2_total_tidy <- as.data.frame(co2_total_tidy)

## emission  as.numeric 오류 나서 아래 코드로 해결...

co2_total_tidy$emission <- as.numeric(gsub(",","", co2_total_tidy$emission))




?palette

p1 <-co2_total_tidy %>% 
  filter(구분 %in% c("에너지", "산업공정", "농업", "LULUCF", "폐기물")) %>% 
  ggplot(aes(x=year, y=emission, fill = 구분, group= 구분))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  theme(plot.title = element_text(size=20, hjust=0.5),
        axis.title = element_text(size=35, face="bold"),
        plot.subtitle= element_blank()) +
  scale_x_discrete(breaks = seq(1990,2018, 5))


p1


p2 <- co2_total_tidy %>% 
  filter (구분 %in% c("총배출량", "순배출량")) %>% 
  ggplot(aes(x=year, y=emission, fill = 구분, group= 구분))+
  geom_line()+
  theme(plot.title = element_text(size=20, hjust=0.5),
        axis.title = element_text(size=35, face="bold"),
        plot.subtitle= element_blank()) +
  scale_x_discrete(breaks = seq(1990,2020, 5))


p2

###  patchwork 라이브러리를 통해 그래프 이어붙이기


library(patchwork)


p1+p2




p3 <-co2_total_tidy %>% 
  filter(구분 %in% c("에너지", "산업공정", "농업", "LULUCF", "폐기물")) %>% 
  ggplot(aes(x=year, y=emission, fill = 구분, group= 구분))+
  geom_col()+
  theme(plot.title = element_text(size=20, hjust=0.5),
        axis.title = element_text(size=35, face="bold"),
        plot.subtitle= element_blank()) +
  scale_x_discrete(breaks = seq(1990,2020, 5))+
  geom_line(aes(y=emission))

p3



head(co2_total_tidy)


### 210826 _17개 광역시 온실가스 배출 1990~2018  (에너지, 산업공정, 농업, LULUCF, 폐기물)

##working directory 지정 Ctrl + shift + h
##check.names=FALSE 넣어야 함!! 년도 앞에 x 안나타나게


CO2_17_sector <- read.csv("210901_CO2_17_net_included.csv", header=TRUE, check.names=FALSE)


View(CO2_17_sector)
str(CO2_17_sector)


##0인 값들을 NA로 설정. 그래프에 0값 안 뜨게

CO2.17[CO2.17 == 0] <-NA


##현재 wide data임
## gather() 함수를 이용해서 tidy data로 전환 필요

head(CO2_17_sector)

CO2.17<-CO2_17_sector %>% 
  gather(year, emission, -지역, -구분)

## 소수점 둘째 자리

options(digits=2)

##numeric으로 설정!!!

CO2.17$year <- as.numeric(CO2.17$year)

head(CO2.17, 10)




## 1990~ 2018 17개시도별 온실가스 배출 구분  (국가 뺴고) - 누적 바차트


CO2.17 %>% 
  filter ( 지역 != "국가" & 구분 != "순배출량") %>%  
  ggplot(mapping= aes(x=year, y=emission, group = 구분, fill = 구분))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~지역)



## 글꼴 꺠질떄 

library(showtext)
showtext_auto()


### 17개시도 분야별(에너지, 폐기물, 산업공정,  농업, lulucf) + Net line graph

CO2.17_reg_sec <- CO2.17 %>% 
  filter ( 지역 != "국가" & 구분 != "순배출량") 

head(CO2.17_reg_sec, 10)


CO2.17_reg_net <- CO2.17 %>% 
  filter ( 지역 != "국가" & 구분 == "순배출량")

head(CO2.17_reg_net)


ggplot() + 
  geom_area(data = CO2.17_reg_sec, mapping= aes(x=year, y=emission, fill = 구분, group = 구분))+
  geom_line(data = CO2.17_reg_net, mapping= aes(x=year, y=emission), 
            color = "blue", size =1, alpha = 0.7)+
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~지역, nrow = 2)+
  labs ( x = "년도",
         y = "온실가스 배출량")



## 1990~ 2018 17개시도별 온실가스 배출 구분  (누적 area 차트)  -> 인포그래픽 제작



## 잘 되는 버전 (완성)

CO2.17 %>% 
  filter ( 지역 != "국가" & 구분 != "순배출량") %>%  
  ggplot(mapping= aes(x=year, y=emission, fill = 구분, group = 구분))+
  geom_area()+
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~지역, nrow = 2)




CO2.17_heatmap <- CO2.17 %>% 
  filter ( 지역 != "국가" & 구분 == "순배출량" & year == 2018) %>% 
  select(지역, emission)

CO2.17_heatmap

class(CO2.17_heatmap)


str(CO2.17_heatmap)



####  지역별 온실가스 배출량 + KESIS 지역별 발전량 (테스트) Generation


powergen_raw <- read.csv("210915_powergeneration.csv",  fileEncoding = "UTF-8-BOM", header=TRUE, check.names=FALSE)

str(powergen_raw)

View(powergen_raw)

powergen_tidy <-powergen_raw %>% 
  gather('year', 'GWh', -지역)

View(powergen_tidy)

str(powergen_tidy)
powergen_tidy$year <-as.numeric(powergen_tidy$year)



## 쉼표가 포함된 숫자를 gsub을 통해 변형시켜야 as numeric 가능함  ->> 중요!!
### powergen_tidy$GWh <-as.numeric(powergen_tidy$GWh) 이거 안됌!!!

powergen_tidy$GWh <- as.numeric(gsub(",","", powergen_tidy$GWh))


library(scales)

ggplot() + 
  geom_area(data = CO2.17_reg_sec, mapping= aes(x=year, y=emission, fill = 구분, group = 구분))+
  geom_line(data = powergen_tidy, mapping= aes(x=year, y=GWh/1), 
            color = "blue", size =1, alpha = 0.7)+
  scale_y_continuous( labels = comma,
                      sec.axis = sec_axis(~.*1, name = "지역별 발전량(GWh)", labels = comma))+ 
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~지역, nrow = 2)+
  labs ( x = "년도",
         y = "온실가스 배출량")



####  지역별 온실가스 배출량 + KESIS 지역별 소비량(테스트) Consumption


powerconsump_raw <- read.csv("210915_powerconsumption.csv",  fileEncoding = "UTF-8-BOM", header=TRUE, check.names=FALSE)

str(powerconsump_raw)

View(powergen_raw)

head(powergen_raw)

powerconsump_tidy <-powerconsump_raw %>% 
  gather('지역', 'GWh', -year)

View(powerconsump_tidy)

str(powerconsump_tidy)



## 쉼표가 포함된 숫자를 gsub을 통해 변형시켜야 as numeric 가능함  ->> 중요!!
### powergen_tidy$GWh <-as.numeric(powergen_tidy$GWh) 이거 안됌!!!

powerconsump_tidy$GWh <- as.numeric(gsub(",","", powerconsump_tidy$GWh))


ggplot() + 
  geom_area(data = CO2.17_reg_sec, mapping= aes(x=year, y=emission, fill = 구분, group = 구분))+
  geom_line(data = powerconsump_tidy, mapping= aes(x=year, y=GWh/1), 
            color = "blue", size =1, alpha = 0.7)+
  scale_y_continuous( labels = comma,
                      sec.axis = sec_axis(~.*1, name = "지역별 전력소비량(GWh)", labels = comma))+ 
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~지역, nrow = 2)+
  labs ( x = "년도",
         y = "온실가스 배출량")



### 지역별 발전량 + 소비량   Power generation + consumption

ggplot() + 
  geom_line(data = powerconsump_tidy, mapping= aes(x=year, y=GWh), 
            color = "red", size =1, alpha = 0.7)+
  geom_line(data = powergen_tidy, mapping= aes(x=year, y=GWh), 
            color = "blue", size =1, alpha = 0.7)+
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~지역, nrow = 2)+
  theme(legend.title=element_text(size=10))+
  labs ( x = "년도",
         y = "온실가스 배출량")






## 지역별 온실가스 배출량 + 전력자립도  double axis (테스트)

ggplot() + 
  geom_area(data = CO2.17_reg_sec, mapping= aes(x=year, y=emission, fill = 구분, group = 구분))+
  geom_line(data = independence_tidy, mapping= aes(x=year, y=percent/0.001), 
            color = "blue", size =1, alpha = 0.7)+
  scale_y_continuous( labels = comma,
                      sec.axis = sec_axis(~. *0.001, name = "인당CO2 배출량(tCO2eq/인)", labels = comma))+ 
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~지역, nrow = 2)+
  labs ( x = "년도",
         y = "온실가스 배출량")



ggplot()

str(CO2.17)

independence_raw <- read.csv("210915_independence.csv",  fileEncoding = "UTF-8-BOM", header=TRUE, check.names=FALSE)

str(independence_raw)
View(independence_raw)



independence_tidy <-independence_raw %>% 
  gather('year', 'percent', -지역)

View(independence_tidy)
independence_tidy$year <-as.numeric(independence_tidy$year)
str(independence_tidy)

View(CO2.17)






## 1990~ 2018 국가 온실가스 배출 구분   (바 차트)

CO2.17 %>% 
  filter ( 지역 == "국가") %>%  
  ggplot(mapping= aes(x=year, y=emission, group = 구분, fill = 구분))+
  geom_bar(position="stack", stat="identity")+
  labs(title = "1990~ 2018 국가 온실가스 배출 구분",
       subtitle = "에너지, 산업공정, 농업, LULUCF 폐기물 5가지 분야",
       x = "년도",
       y = " 온실가스 배출량")+
  scale_fill_brewer(palette="Set3")


CO2.17 %>% 
  filter ( 지역 == "국가" & year == 2018) %>%  
  View()



## 1990~ 2018 국가 온실가스 배출 구분   (바 차트 눕혀서)

CO2.17 %>% 
  filter ( 지역 == "국가") %>%
  ggplot(mapping= aes(x=year, y=emission, group = 구분, fill = 구분))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  labs(title = "1990~ 2018 국가 온실가스 배출 구분",
       subtitle = "에너지, 산업공정, 농업, LULUCF 폐기물 5가지 분야",
       x = "년도",
       y = " 온실가스 배출량")+
  scale_fill_brewer(palette="Set3")+
  scale_x_reverse()



## 1990~ 2018 국가 온실가스 배출 구분   (누적 라인 차트) -> 인포그래픽 사용

CO2.17 %>% 
  filter ( 지역 == "국가") %>%  
  ggplot(mapping= aes(x=year, y=emission, fill = 구분))+
  geom_area() +
  geom_line(position="stack", size = 1, alpha = 0.4)+
  labs(title = "1990~ 2018 국가 온실가스 배출 구분",
       subtitle = "에너지, 산업공정, 농업, LULUCF 폐기물 5가지 분야",
       x = "년도",
       y = " 온실가스 배출량",
       color = "구분")+
  scale_fill_brewer(palette="Set3")+
  theme_minimal()

##### 인당 co2 배출 가져오기 ####


co2percapita_raw <- read.csv("210915_co2percapita.csv",  fileEncoding = "UTF-8-BOM", header=TRUE, check.names=FALSE)


View(co2percapita_raw)

str(co2percapita_raw)


co2percapita_tidy <-co2percapita_raw %>% 
  gather('year', 'tonpercapita', -process)



View(co2percapita_tidy)

str(co2percapita_tidy)

co2percapita_tidy$year <-as.numeric(co2percapita_tidy$year)




total <- CO2.17 %>% 
  filter ( 지역 == "국가") 

View(total)


total_co2percapita <-bind_rows(total, co2percapita_tidy)

str(total_co2percapita)

View(total_co2percapita)


####### 1990 ~ 2018 국가 온실가스 배출 구분 + 인당 CO2 배출 추이  (double y axis)

library(scales)

total_co2percapita %>% 
  ggplot(aes(x = year, group = 구분, fill = 구분))+
  geom_area(aes(y = emission))+
  geom_line(aes(y = tonpercapita/0.00003), size = 1, alpha = 0.5, color="red")+
  scale_y_continuous( labels = comma,
                      sec.axis = sec_axis(~. *0.00003, name = "인당CO2 배출량(tCO2eq/인)", labels = comma))+  
  scale_fill_brewer(palette="Paired")+
  labs(title = "시나리오별 발전량 비중 및 온실가스 배출",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  scale_x_continuous(breaks = seq(1990,2020, 5)) 




## 1990~ 2018 국가 온실가스 배출 구분   (누적 라인 차트 - 눕힌거)

CO2.17 %>% 
  filter ( 지역 == "국가") %>%  
  ggplot(mapping= aes(x=year, y=emission, fill = 구분))+
  geom_area() +
  geom_line(position="stack", size = 1, alpha = 0.4)+
  labs(title = "1990~ 2018 국가 온실가스 배출 구분",
       subtitle = "에너지, 산업공정, 농업, LULUCF 폐기물 5가지 분야",
       x = "년도",
       y = " 온실가스 배출량",
       color = "구분")+
  scale_fill_brewer(palette="Set3")+
  theme_minimal()+
  scale_x_reverse()+
  coord_flip()





#### 2018년 17개 시도별 온실가스 배출 구분 (국가 제외)

CO2.17 %>% 
  filter( year == 2018 & 지역 != "국가"& 구분 !="순배출량") %>% 
  ggplot(mapping= aes(x=year, y=emission, group = 구분, fill = 구분))+
  geom_bar(position="stack", stat="identity")+
  labs(title = "1990~ 2018 국가 온실가스 배출 구분",
       subtitle = "에너지, 산업공정, 농업, LULUCF 폐기물 5가지 분야",
       x = "년도",
       y = " 온실가스 배출량")+
  scale_fill_brewer(palette="Set3")+
  facet_grid(cols = vars(지역))+
  theme(axis.ticks.x=element_blank(),
        axis.text.x = element_blank())


#### 2018년 17개 시도별 온실가스 배출 구분 (국가 제외)  - > 순서 정렬??? 어떻게 하지????

head(CO2.17)


CO2.17 %>% 
  filter( year == 2018 & 지역 != "국가"& 구분 !="순배출량") %>% 
  ggplot(mapping= aes(x=reorder(year, emission), y=emission, group = 구분, fill = 구분))+
  geom_bar(position="stack", stat="identity")+
  labs(title = "1990~ 2018 국가 온실가스 배출 구분",
       subtitle = "에너지, 산업공정, 농업, LULUCF 폐기물 5가지 분야",
       x = "년도",
       y = " 온실가스 배출량")+
  scale_fill_brewer(palette="Set3")+
  facet_grid(cols = vars(지역))+
  theme(axis.ticks.x=element_blank(),
        axis.text.x = element_blank())



## 2018년 기준 순배출량 순서대로 나타내기 

CO2.17 %>% 
  relocate(year, .before =지역) %>%
  filter(year == 2018 & 구분 !="순배출량" & 지역 !="국가") %>% 
  group_by(지역) %>% 
  summarise(net_emission=sum(emission)) %>%
  arrange(desc(net_emission))


## 2018년 기준 총배출량 순서대로 나타내기 

CO2.17 %>% 
  relocate(year, .before =지역) %>%
  filter(year == 2018 & 구분 !="총배출량" & 지역 !="국가") %>% 
  group_by(지역) %>% 
  summarise(net_emission=sum(emission)) %>%
  arrange(desc(net_emission))


## 2018년 기준 순배출량 순서대로 나타내기 

CO2.17 %>% 
  relocate(year, .before =지역) %>%
  filter(year == 2018 & 구분 !="순배출량" & 지역 !="국가") %>% 
  group_by(지역) %>% 
  summarise(net_emission=sum(emission)) %>%
  ggplot(aes(x = 지역, y = net_emission, group = 지역))+
  geom_bar(stat="identity")





#### 심심해서 만들어본 EPSIS 데이터

EPSIS <- read.csv("210901_EPSIS_regional.csv", header=TRUE, check.names=FALSE)

str(EPSIS)

library(lubridate)


EPSIS$기간 <- as.Date(EPSIS$기간)

head(EPSIS)



EPSIS_tidy <-EPSIS %>% 
  gather('발전원', '발전설비', -기간, -지역)

head(EPSIS_tidy)

str(EPSIS_tidy)

View(EPSIS_tidy)



### 지역별 발전원/발전설비용량

EPSIS <- EPSIS_tidy %>% 
  filter ( 발전원 != "합계" & 지역 != "소계") %>%  
  ggplot(mapping= aes(x=기간, y=발전설비, group = 발전원, fill = 발전원))+
  geom_area()+
  facet_wrap(~지역, nrow=3)+
  scale_x_continuous(c(2001, 2005, 2010, 2015, 2020)) 

EPSIS


### 전국 발전원/발전설비용량

EPSIS_tidy %>% 
  filter ( 발전원 != "합계" & 지역 == "소계") %>%  
  ggplot(mapping= aes(x=기간, y=발전설비, group = 발전원, fill = 발전원))+
  geom_area()+
  scale_x_continuous(breaks = seq(2001,2020, 1)) 




### 경북(test)

EPSIS_tidy %>% 
  filter ( 발전원 != "합계" & 지역 != "소계" & 지역 == "경북") %>%  
  ggplot(mapping= aes(x=기간, y=발전설비, group = 발전원, fill = 발전원))+
  geom_area()+
  scale_x_continuous(breaks = seq(2001,2020, 1)) 



library(plotly)

EPSIS_interactive<- ggplotly(EPSIS)

EPSIS_interactive




### 한국전력 통계 바탕으로 지역별 전력 생산량 구하기

KEPCO <-  read.csv("210901_electricity_generation_kor.csv", header=T, check.names=FALSE)


head(KEPCO)
str(KEPCO)


KEPCO_tidy <-KEPCO %>% 
  gather(지역, 배출량, -연도)

head(KEPCO_tidy)


str(KEPCO_tidy)



## 쉼표가 포함된 숫자를 gsub을 통해 변형시켜야 as numeric 가능함

KEPCO_tidy$배출량 <- as.numeric(gsub(",","", KEPCO_tidy$배출량))


## 그래프 만들기

KEPCO_tidy %>% 
  filter ( 지역 != "전국") %>%  
  ggplot(mapping= aes(x=연도, y=배출량, fill = 지역, group = 지역))+
  geom_line()+
  facet_wrap(~지역, nrow = 2)

## 전국을 뺀 데이터 만들기
KEPCO_reg<- KEPCO_tidy %>% 
  filter ( 지역 != "전국")


library(scales)

ggplot() + 
  geom_area(data = CO2.17_reg_sec, mapping= aes(x=year, y=emission, fill = 구분, group = 구분))+
  geom_line(data = KEPCO_reg, mapping= aes(x=연도, y=배출량), 
            color = "blue", size =1, alpha = 0.7)+
  scale_y_continuous( labels = comma,
                      sec.axis = sec_axis(~., name = "CO2 배출량", labels = comma))+      scale_fill_brewer(palette="Set3")+
  facet_wrap(~지역, nrow = 2)+
  labs ( x = "년도",
         y = "온실가스 배출량")
