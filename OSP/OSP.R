library(tidyverse)
library(ggrepel)


# 데이터 불러오기

osp<-read.csv("osp.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) %>% 
  arrange(scenario)


head(osp)



##현재 wide data임
## gather() 함수를 이용해서 tidy data로 전환 필요


osp.tidy <-osp %>% 
  gather(year, GWh, -process, -scenario)

head(osp.tidy)

str(osp.tidy)

##dplyr를 활용해 sumif 로 추출######### KEY


osp.result<-osp.tidy %>% 
  group_by(process, scenario, year) %>% 
  summarise(GWh_sum = sum(GWh, na.rm=T))

View(osp.result)


## 2050년까지만 filter

osp.2050<- osp.result %>% filter(year <= 2050) %>% 
  arrange(scenario)

write.csv(osp.2050, 'osp.result.csv')



library(RColorBrewer)

osp.2050$year <-as.numeric(osp.2050$year)



osp.2050 %>%   
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_area()+
  geom_line(position="stack", size=0.5, alpha=0.5)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "테스트 테스트트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  scale_x_discrete(breaks = c(2020, 2030, 2040, 2050))+
  facet_wrap(~scenario, scales="free_x")
 

### plotly to interactive

p <- osp.2050 %>%   
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_area()+
  geom_line(position="stack", size=0.5, alpha=0.5)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "테스트 테스트트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  scale_x_discrete(breaks = c(2020, 2030, 2040, 2050))+
  facet_wrap(~scenario, scales="free_x")

p




## plotly 라이브러리로 인터랙티브화

install.packages("plotly")
library(plotly)

p_interactive<- ggplotly(p)

p_interactive

##CO2 배출량


CO2<-read.csv("CO2.csv", header=TRUE,  fileEncoding = "UTF-8-BOM", check.names=FALSE)


head(CO2)

CO2.tidy<-CO2 %>% 
  gather(year, GtCO2, -scenario) %>% 
  arrange(scenario)

CO2.tidy$year <- as.numeric(CO2.tidy$year)


View(CO2.tidy)

## 데이터 합치기 bind_rows !!

# test_result는 발전량과 CO2 배출량 다 있음

head(osp.2050)

head(CO2.tidy)

test_result <-bind_rows(osp.2050, CO2.tidy) 

View(test_result)




test_result %>% 
  filter(year=='2050')%>%
  filter(!is.na(process)) %>% 
  select(scenario, process, year, GWh_sum) %>% 
  arrange(scenario) %>% 
  group_by(scenario) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  head()


   
## 누적 차트


 test_result %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_area()+
  geom_line(position="stack", size=0.5, alpha=0.5)+
    scale_fill_brewer(palette="Paired")+
  labs(title = "테스트 테스트트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, scales="free_x", ncol=6)


#### 박상용 박사님 요청 시나리오별로 2017, 2050년만 '비중' 차이 나도록

 ## 2050년도 비중 (파이차트) geom_col + coord_polar

 test_result %>% 
  filter(year=='2050') %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  coord_polar("y", start=0)+
  facet_wrap(~scenario)+
  theme(axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


## 2050년도 비중 (파이차트) geom_col + coord_polar


 ## 소수점 자리 숫자 보여주는 함수  

 options(digits=1)
  


### 2017년 2050년 시나리오 비교 (y축 : 발전 비중 (100%)) -> position = "fill"

test_result %>% 
  filter(year=='2017' | year =='2050') %>% 
  filter(!is.na(process)) %>% 
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+

  
  ##  geom_text(aes=label(percent))+

    labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario)+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank())


# 2017년 2050년 시나리오 구분 (y축 :발전량)   -> position = "stack"

test_result %>% 
  filter(year=='2017' | year =='2050') %>% 
  filter(!is.na(process)) %>% 
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario)+
  theme(axis.text.x=element_blank())


##### # 2017년 2050년 시나리오 구분 (y축 :발전량)   -> position = "stack". 값 추가...


## 소수점 자리 숫자 보여주는 함수  

options(digits=1)

test_result %>% 
  filter(year=='2017' | year =='2050') %>% 
  filter(!is.na(process)) %>% 
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  geom_text(aes(label = percent), position = "fill")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


### 2050년 시나리오 순서대로 비중 보여주기 (발전비중 100%)

test_result %>% 
  filter(year =='2050') %>% 
  filter(!is.na(process)) %>% 
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  filter(year =='2050') %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process, 
             label=sprintf("%0.2f", round(percent, digits= 2))))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
    labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 비중 (%)",
       caption= "제작:KIER")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none")

test_result %>% 
  filter(year =='2050') %>% 
  filter(!is.na(process)) %>% 
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  filter(year =='2050') %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process, 
             label=sprintf("%0.2f", round(percent, digits= 2))))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_text(aes(label = process), position = "fill")+
  labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 비중 (%)",
       caption= "제작:KIER")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none")

#### label 을 비중으로

test_result %>% 
  filter(year =='2050') %>% 
  filter(!is.na(process)) %>% 
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process, 
             label=sprintf("%0.2f", round(percent, digits= 2))))+
  ##geom_text(aes(label = paste(round(percent, digits=0), "%")))+
  geom_bar(position="fill", stat = "identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 비중 (%)",
       caption= "제작:KIER")+
  facet_grid(~scenario)+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none")


library(forcats)

### 2050년 시나리오 순서대로 비중 보여주기 (발전량 GWh)

test_result %>% 
  filter(year =='2050') %>% 
  filter(!is.na(process)) %>% 
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  filter(year =='2050') %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process, 
             label=sprintf("%0.2f", round(percent, digits= 2))))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = process), position = "stack", vjust = 2)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 시나리오별 발전량(GWh)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_grid(cols = vars(scenario))+
  theme(axis.ticks.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")


### 2050년 시나리오 순서대로 비중 보여주기 (그래프 눕히기)

test_result %>% 
  filter(year =='2050') %>% 
  filter(!is.na(process)) %>% 
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  filter(year =='2050') %>% 
  ggplot(aes(x=scenario, y=GWh_sum, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  geom_text(aes(label = process), position = "fill", size = 3, hjust = 1)+
  labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "맨 위 막대 Baseline, 맨 아래 Net Zero 시나리오",
       x= "구분",
       y= "발전량 비중 (100%)",
       caption= "제작:KIER")+
  theme(axis.text.y=element_blank(),
        legend.position = "none")+
  scale_x_discrete(limits = c("OSP_NET_ZERO", "OSP_CN_REN_90", "OSP_CN_REN_80", "OSP_CN_REN_70", "OSP_CN_REN_60", "OSP_CN_REN_50", "OSP_CN_REN_40", "OSP_CN_REN_30", "OSP_CN_REN_20", "OSP_CN_REN_10", "OSP_BASE"))+
  coord_flip()
  


library(forcats)

## 베이스시나리오 2017년 비중


test_result %>% 
  filter(year=='2017' & scenario == "OSP_BASE") %>% 
  arrange(desc(GWh_sum)) %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process))+
  geom_bar(position = "fill", stat="identity")+
  scale_fill_brewer(palette= "Paired")+
  labs(title = "2017년 베이스시나리오 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (%)",
       caption= "제작:KIER")+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())




test_result %>% 
  filter(year =='2017' | year =='2050') %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_area()+
  geom_bar(position="fill", stat= "identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "테스트 테스트트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario)+
  scale_x_continuous( limits = c(2015,2055))



library(scales)  # labels = comma 사용


View(test_result)

## test 2 =>>>> 성공!!!!!

osp_result <- test_result %>% 
  ggplot(aes(x = year, group = process, fill = process))+
    geom_area(aes(y = GWh_sum))+
  geom_line(aes(y = GtCO2/3), size = 1, alpha = 0.5, color="blue")+
    scale_y_continuous( labels = comma,
    sec.axis = sec_axis(~. *3, name = "CO2 배출량", labels = comma))+  
  scale_fill_brewer(palette="Paired")+
  labs(title = "시나리오별 발전량 비중 및 온실가스 배출",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, scales = "free_x")

osp_result


# 인터랙티브화

library(plotly)

osp_result_interactive<- ggplotly(osp_result)

osp_result_interactive


# 저장

library(htmlwidgets) 

saveWidget(osp_result_interactive, "osp_result_interactive.html",
           selfcontained=FALSE, libdir ="lib/")




#gif 만들기

library(tidyverse)
library(gganimate)
library(ggthemes)


osp.result.animation = osp_result + 
  transition_reveal(year)+
  view_follow(fixed_y = TRUE)

### 시간 오래걸려서 막아 놓음   osp.result.animation

#gif 저장

### 시간 오래걸려서 막아 놓음 
### animate(osp.result.animation, height = 500, width = 800, fps =30, duration = 10,
###     end_pause = 60, res = 100)

### anim_save("osp result animation.gif")


# 베이스시나리오로 테스트
test_result_base <-test_result %>% 
  filter(scenario =="OSP_BASE")


test_result_base %>% 
  ggplot(aes(x = year, group = process, fill = process))+
  geom_area(aes(y = GWh_sum))+
  geom_line(aes(y=GtCO2/5, color = "blue"))
  scale_y_continuous(labels = comma,
    sec.axis = sec_axis(~. * 3, name ="CO2 배출"))




## CO2 배출량
test_result %>% 
  ggplot(aes(x = year, fill = process, group = process))+
  geom_line(aes(y = GtCO2))+
  scale_fill_brewer(palette="Paired")+
  labs(title = "시나리오별 발전량 비중 및 온실가스 배출",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, scales = "free_x")



  




library(scales)

result2<-CO2.tidy %>% 
  ggplot(mapping = aes(x=year, y=GtCO2))+
  facet_wrap(~scenario, scales="free_x")+
  geom_point()+
  geom_smooth()+
  scale_y_continuous(labels = comma)

result2

  
result1+result2

  
## gganimate

library(gganimate)





# 결측값이 들어있는 벡터는 통계함수 적용 시 NA 나옴. 이를 막기 위해 na.rm=TRUE 사용

osp.result<-osp.tidy %>% 
    group_by(process,scenario) %>% 
  summarise(sum_GWh = sum(GWh, na.rm=TRUE), year=year) 

head(osp.result, 20)

View(osp.result)

osp.base<-osp.result %>% 
  filter(scenario =="OSP_BASE")

head(osp.base)

view(osp.base)


## subset 을 이용하여 필요한 
osp.base<-subset(osp.base, select = c(process, sum_GWh, year))

head(osp.base)





View(osp.base)




head(osp_base)

# 결측값이 들어있는 벡터는 통계함수 적용 시 NA 나옴. 이를 막기 위해 na.rm=TRUE 사용

osp_base %>% 
  group_by(process, year) %>% 
  summarise(sum_GWh = sum(GWh, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=sum_GWh)) + 
  geom_point()
  


osp_base %>% 
  group_by(scenario, process)

  summarise(sum_GWh = sum(GWh, na.rm=TRUE), year=year)
  
  
osp_base
  
str(osp_base)

ggplot(data=osp_base, 
       aes(x=year, y=sum_GWh, group=process, color=process)) + 
  geom_point()




#태양광


osp.pv <-osp.2050 %>% 
  filter(process=="태양광")



osp.pv %>%  
  ggplot(mapping=aes(x=year, y=GWh_sum))+
  geom_point(aes(color=scenario), alpha=0.7)+
  geom_smooth()+
  facet_wrap(~scenario)+
  labs(title = "시나리오별 태양광 발전량",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")
osp.pv


#ESS

osp.ESS <-osp.2050 %>% 
  filter(process=="ESS")

head(osp.ESS)

osp.ESS %>%  
  ggplot(mapping=aes(x=year, y=GWh_sum))+
  geom_point(aes(color=scenario), alpha=0.7)+
  geom_line()+
  facet_wrap(~scenario)+
  labs(title = "시나리오별 태양광 발전량",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")


#석탄

osp.coal <-osp.2050 %>% 
  filter(process=="석탄")

head(osp.coal)

osp.coal %>%  
  ggplot(mapping=aes(x=year, y=GWh_sum))+
  geom_point(aes(color=scenario), alpha=0.7)+
  geom_line()+
  facet_wrap(~scenario)+
  labs(title = "시나리오별 석탄 발전량",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")


#원자력

osp.nuclear <-osp.2050 %>% 
  filter(process=="원자력")

head(osp.nuclear)

osp.nuclear %>%  
  ggplot(mapping=aes(x=year, y=GWh_sum))+
  geom_point(aes(color=scenario), alpha=0.7)+
  geom_line()+
  facet_wrap(~scenario)+
  labs(title = "시나리오별 원자력 발전량",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")







### 테스트

test1 <-data.frame(var1= c("태양광", "태양광", "풍력"),
                   var2= c("sce1", "sce1", "sce1"),
                   var3 = c(100, 200, 300))



test1 <-rename(test1, process=var1, scenario=var2, GWh=var3)

test1

test1 %>% 
  group_by(process) %>% 
  summarise(mean_GWh = mean(GWh), sum_GWh= sum(GWh), n=n())



## Stack-area 그래프 테스트

trade<-read.csv("trade_stat.csv", header=TRUE)

head(trade)

###년도 첫번쨰(1)에서 네번쨰(4) 까지 추출하기

trade_stat<-transform(trade, Year = substr(Time, 1,4))


head(trade_stat, 10)

sapply(trade_stat, class)

install.packages("sqldf")
library(sqldf)



trade_stat_Year <- sqldf('select Year, 
                         sum(export_amt)/100000 as exp_amt_Year, 
                         sum(import_amt)/100000 as imp_amt_Year 
                         from trade_stat
                         group by Year
                         order by Year ')

trade_stat_Year

library(reshape)

trade_stat_Year_melt <-melt(trade_stat_Year, idvars = c("Year"))

head(trade_stat_Year_melt, 20)

trade_stat_Year_melt <-rename(trade_stat_Year_melt, c(variable="trade_cd", value="amount_B"))

head(trade_stat_Year_melt)

ggplot(trade_stat_Year_melt, aes(x=Year, y=amount_B, fill=trade_cd, group=trade_cd))+
  geom_area(colour=NA, alpha=0.5)+
  scale_fill_brewer(palette="Blues")+
  geom_line(position="stack", size=0.5)+
  ggtitle("테스트 테스트 2007-2014")


##한글 폰트 나오게 하는 라이브러리(안되네;;)

if(!require(showtext)) {
  install.packages('showtext')
  library(showtext)
}

showtext_auto()



##########################################################################
##########################################################################
################# 210907  HHCH 시나리오 테스트 ############################
##########################################################################

library(tidyverse)



## UTF는 한글 깨지는 거 방지, check.names는 연도 앞에 X 오는거 방지

OSP_HHCH_result <-read.csv("210907_OSP_result.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names= FALSE)

head(OSP_HHCH_result)

str(OSP_HHCH_result)


## 불필요한 process_original 날리고, Dummy도 날리고

OSP_HHCH_filtered <- OSP_HHCH_result %>%  
  select(-process_original) %>% 
  filter (process != "Dummy")

View(OSP_HHCH_filtered)  

#



##현재 wide data임
## gather() 함수를 이용해서 tidy data로 전환  후 group_by를 통해 같은 process별로 GWh_sum 만들기


OSP_HHCH_tidy <- OSP_HHCH_filtered %>% 
  gather(year, GWh, -process, -scenario) %>% 
  group_by(scenario, process, year) %>% 
  summarise(GWh_sum = sum(GWh, na.rm=T)) %>% 
  mutate ( )




View(OSP_HHCH_tidy)



OSP_HHCH_tidy %>%   
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  filter(process == '양수') %>% 
  View()


OSP_HHCH_tidy %>%   
  filter(  process != '양수') %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_area()+
  geom_line(position="stack", size=0.5, alpha=0.5)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "테스트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, nrow= 2, scales="free_x")+
  theme_bw()


summarise(GWh_sum = sum(GWh, na.rm=T))






### 2050년 시나리오 순서대로 비중 보여주기 (발전비중 100%)




OSP_HHCH_tidy %>% 
  filter(year =='2050', process != '양수') %>% 
  select(scenario, process, year, GWh_sum) %>% 
  group_by(scenario, year) %>% 
  mutate(percent = GWh_sum/ sum(GWh_sum)*100) %>% 
  filter(year =='2050')%>%
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
  labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 비중 (%)",
       caption= "제작:KIER")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank())


### 2050년 시나리오 순서대로 비중 보여주기 (발전량)


OSP_HHCH_tidy %>%
  filter(year =='2050',  process != '양수') %>% 
  ggplot(aes(x=year, y=GWh_sum, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  #geom_text(aes(label = process), position = "stack", vjust = 2)+
  labs(title = "2050년 시나리오별 발전량 ",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  scale_fill_brewer(palette = "Paired")+
  facet_grid(cols = vars(scenario))+
  theme(axis.ticks.x=element_blank(),
        axis.text.x = element_blank())+
  scale_color_manual(values = colors)


library(ggthemes)
######## 수소 만들기



HDG_PROC_raw <-read.csv("210908_HDG_PROC.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names= FALSE)

head(HDG_PROC_raw)


HDG_filtered <- HDG_PROC_raw %>%  
  gather(year, toe, -process, -scenario)

View(HDG_filtered)

HDG_filtered$year <-as.numeric(HDG_filtered$year)

str(HDG_filtered)




# 
# 시나리오 제약조건 (2050년도 기준)
# a = 80,000
# b = 75,000
# c = 70,000
# d = 65,000
# e = 60,000
# f = 55,000
# g = 50,000
# h = 40,000
# i = 35,000
# j = 30,000


######### 수소 발전량 20217- 2060

HDG_filtered %>%   
  ggplot(aes(x=year, y=toe, fill=process, group=process))+
  geom_area()+
  geom_line(position="stack", size=0.5, alpha=0.5)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "테스트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, scales="free_x", nrow=2) +
  theme_bw()


##### 시나리오별/연도별 수소 발전 비중 (100%)

HDG_filtered %>%  
  group_by(scenario, year) %>% 
  mutate(percent = toe/ sum(toe)*100) %>%
  ggplot(aes(x=year, y=toe, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
  labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 비중 (%)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, nrow=2)+
  theme(axis.ticks.y=element_blank())




##### 2050년 수소 발전 비중 (100%)

HDG_filtered %>%  
  filter(year =='2050') %>% 
  group_by(scenario, year) %>% 
  mutate(percent = toe/ sum(toe)*100) %>%
  ggplot(aes(x=year, y=toe, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
  labs(title = "2050년 시나리오별 수소 생산 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 비중 (%)",
       caption= "제작:KIER")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank())


###### 수소 발전량 (toe)

HDG_filtered %>%  
  filter(year =='2050') %>% 
  group_by(scenario, year) %>% 
  ggplot(aes(x=year, y=toe, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 시나리오별 발전량",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량",
       caption= "제작:KIER")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank())




################# 210909 HHCH #########################################  


library(tidyverse)


## UTF는 한글 깨지는 거 방지, check.names는 연도 앞에 X 오는거 방지

HHCH_raw <-read.csv("210909_HHCH.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names= FALSE)

View(HHCH_raw)

str(HHCH_raw)


### Dummy 날리고 filtered로 저장

HHCH_filtered <- HHCH_raw %>%  
  filter (process != "Dummy")

View(HHCH_filtered)


##현재 wide data임
## gather() 함수를 이용해서 tidy data로 전환  후 group_by를 통해 같은 process별로 GWh_sum 만들기


HHCH_tidy <- HHCH_filtered %>% 
  gather(year, toe, -process, -scenario) %>% 
  group_by(scenario, process, year) %>% 
  summarise(toe_sum = sum(toe, na.rm=T)) %>% 
  mutate(percent = toe_sum/ sum(toe_sum)*100) 


View(HHCH_tidy)

### 양수가 차지하는 비중은 얼마나 될까?

HHCH_tidy %>%   
  group_by(scenario, year) %>% 
  filter(process == '양수') %>% 
  View()






### 시나리오별 2017- 2050년 발전량  



HHCH_tidy %>%   
  filter(process != '양수') %>% 
  ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_area()+
  geom_line(position="stack", size=0.5, alpha=0.5)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "테스트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, nrow= 2, scales="free_x")+
  theme_bw()




### 2050년 시나리오 순서대로 비중 보여주기 (발전비중 100%)



HHCH_tidy %>% 
  filter(year =='2050', process != '양수') %>% 
  select(scenario, process, year, toe_sum) %>% 
  ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
  labs(title = "2050년 시나리오별 발전 비중(100%)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전 비중 (%)",
       caption= "제작:한국에너지기술연구원 정책연구실")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank())


### 2050년 시나리오 순서대로 비중 보여주기 (발전량)    ->   박상용 


HHCH_tidy %>%
  filter(year =='2050',  process != '양수') %>% 
  ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  #geom_text(aes(label = process), position = "stack", vjust = 2)+
  labs(title = "2050년 CCUS 민감도 분석 - 발전믹스 ",
       x= "구분",
       y= "발전량 (1000toe)")+
  scale_fill_brewer(palette = "Paired")+
  facet_grid(cols =  vars(scenario))+
  theme(axis.ticks.x=element_blank(),
        axis.text.x = element_blank())+
  scale_color_manual(values = colors)


HHCH_tidy_new <- HHCH_tidy_new



HDG_PROC_raw <-read.csv("210909_HDG_PROC.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names= FALSE)

head(HDG_PROC_raw)




HDG_filtered <- HDG_PROC_raw %>%  
  gather(year, toe, -process, -scenario)





HDG_filtered$year <-as.numeric(HDG_filtered$year)

str(HDG_filtered)

######### 수소 발전량 20217- 2060

HDG_filtered %>%   
  filter( process !='BYP' & process != 'IMP') %>% 
  ggplot(aes(x=year, y=toe, fill=process, group=process))+
  geom_area()+
  geom_line(position="stack", size=0.5, alpha=0.5)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "테스트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, scales="free_x", nrow=2) +
  theme_bw()


##### 시나리오별/연도별 수소 발전 비중 (100%)

HDG_filtered %>%  
  filter( process !='BYP' & process != 'IMP') %>% 
  group_by(scenario, year) %>% 
  mutate(percent = toe/ sum(toe)*100) %>%
  ggplot(aes(x=year, y=toe, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
  labs(title = "2050년 시나리오별 발전량 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 비중 (%)",
       caption= "제작:한국에너지기술연구원 정책연구실")+
  facet_wrap(~scenario, nrow=2)+
  theme(axis.ticks.y=element_blank())




##### 2050년 수소 발전 비중 (100%)

HDG_filtered %>%    
  filter( process !='BYP' & process != 'IMP') %>% 
  filter(year =='2050') %>% 
  group_by(scenario, year) %>% 
  mutate(percent = toe/ sum(toe)*100) %>%
  ggplot(aes(x=year, y=toe, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
  labs(title = "2050년 시나리오별 수소 생산 비중",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 비중 (%)",
       caption= "제작:KIER")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank())


###### 수소 발전량 (toe)  -> 박상용

HDG_filtered %>%    
  filter( process !='BYP' & process != 'IMP') %>% 
  filter(year =='2050') %>% 
  group_by(scenario, year) %>% 
  ggplot(aes(x=year, y=toe, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 CCUS 민감도 분석 - 수소생산 믹스",
       x= "시나리오 구분",
       y= "수소생산량")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank())

















################# 210914 수소 제약에 따른 시나리오 #########################################  


library(tidyverse)


## UTF는 한글 깨지는 거 방지, check.names는 연도 앞에 X 오는거 방지

HDG_raw <-read.csv("210914_HDG_Scenario.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names= FALSE)


head(HDG_raw)

##현재 wide data임
## gather() 함수를 이용해서 tidy data로 전환  후 group_by를 통해 같은 process별로 GWh_sum 만들기


HDG_Scenario_tidy <- HDG_raw %>% 
  gather(year, toe, -process, -scenario) %>% 
  group_by(scenario, process, year) %>% 
  summarise(toe_sum = sum(toe, na.rm=T)) %>% 
  mutate(percent = toe_sum/ sum(toe_sum)*100) 


View(HDG_Scenario_tidy)



### 시나리오별 2017- 2050년 발전량  



HDG_Scenario_tidy %>%   
    ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_area()+
  geom_line(position="stack", size=0.5, alpha=0.5)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "테스트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "생산량 (1000toe)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, nrow= 2, scales="free_x")+
  theme_bw()




### 2017- 2050년 수소 제약 시나리오에 따른 생산 비중 보여주기(100%)



HDG_Scenario_tidy %>% 
  select(scenario, process, year, toe_sum) %>% 
  ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
  labs(title = "2050년 시나리오별 발전 비중(100%)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전 비중 (%)",
       caption= "제작:한국에너지기술연구원 정책연구실")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank())


### 2017-2050년 수소 제약 시나리오 따른 생산량 보여주기     


HDG_Scenario_tidy %>%
    ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  #geom_text(aes(label = process), position = "stack", vjust = 2)+
  labs(title = "2050년 CCUS 민감도 분석 - 발전믹스 ",
       x= "구분",
       y= "발전량 (1000toe)")+
  scale_fill_brewer(palette = "Paired")+
  facet_grid(cols =  vars(scenario))+
  theme(axis.ticks.x=element_blank(),
        axis.text.x = element_blank())



###### 2050년 수소 생산량 (1000toe)  

HDG_Scenario_tidy %>%    
  filter(year =='2050') %>% 
  group_by(scenario, year) %>% 
  ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년민감도 분석 - 수소생산 믹스",
       x= "시나리오 구분",
       y= "수소생산량")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank())




###  2050년 수소 생산 비중 (100%)



HDG_Scenario_tidy %>% 
  select(scenario, process, year, toe_sum) %>%
  filter(year == '2050') %>% 
  ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
  labs(title = "2050년 시나리오별 수소 생산 비중(100%)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "생산 비중 (%)",
       caption= "제작:한국에너지기술연구원 정책연구실")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank())





################# 210923 HDG with 박상용 #########################################  


library(tidyverse)


## UTF는 한글 깨지는 거 방지, check.names는 연도 앞에 X 오는거 방지

HHCH_gen <-read.csv("210923_HDG_generation.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names= FALSE)

View(HHCH_gen)




### Dummy 날리고 filtered로 저장

HDG_gen_nodummy<- HHCH_gen %>%  
  filter (process != "Dummy")

View(HDG_gen_nodummy)


##현재 wide data임
## gather() 함수를 이용해서 tidy data로 전환  후 group_by를 통해 같은 process별로 GWh_sum 만들기


HHCH_tidy <- HDG_gen_nodummy %>% 
  gather(year, toe, -process, -scenario) %>% 
  group_by(scenario, process, year) %>% 
  summarise(toe_sum = sum(toe, na.rm=T)) %>% 
  mutate(percent = toe_sum/ sum(toe_sum)*100) 


View(HHCH_tidy)



### 시나리오별 2017- 2050년 발전량  



HHCH_tidy %>%   
  filter(process != '양수') %>% 
  ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_area()+
  geom_line(position="stack", size=0.5, alpha=0.5)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "테스트",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:KIER")+
  facet_wrap(~scenario, nrow= 2, scales="free_x")+
  theme_bw()




### 2050년 시나리오 순서대로 비중 보여주기 (발전비중 100%)



HHCH_tidy %>% 
  filter(year =='2050', process != '양수') %>% 
  select(scenario, process, year, toe_sum) %>% 
  ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_text(aes(label = process), position = "fill", vjust = 3)+ ##vjust로 글자 위아래 이동
  labs(title = "2050년 시나리오별 발전 비중(100%)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전 비중 (%)",
       caption= "제작:한국에너지기술연구원 정책연구실")+
  facet_grid(cols = vars(scenario))+
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank())


### 2050년 시나리오 순서대로 비중 보여주기 (발전량)    ->   박상용 


HHCH_tidy %>%
  filter(year =='2050',  process != '양수') %>% 
  ggplot(aes(x=year, y=toe_sum, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  #geom_text(aes(label = process), position = "stack", vjust = 2)+
  labs(title = "2050년 CCUS 민감도 분석 - 발전믹스 ",
       x= "구분",
       y= "발전량 (1000toe)")+
  scale_fill_brewer(palette = "Paired")+
  facet_grid(cols =  vars(scenario))+
  theme(axis.ticks.x=element_blank(),
        axis.text.x = element_blank())+
  scale_color_manual(values = colors)




################# 210923 타임슬라이스별 발전량 정리 #########################################  


library(tidyverse)


## UTF는 한글 깨지는 거 방지, check.names는 연도 앞에 X 오는거 방지

timeseries_raw <-read.csv("210923_timeseries.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names= FALSE)


View(timeseries_raw)


### Dummy 날리고 filtered로 저장


timeseries_filtered<- timeseries_raw %>%  
  filter (process != "Dummy")


View(timeseries_filtered)


str(timeseries_filtered)
class(timeseries_filtered)

##현재 wide data임
## gather() 함수를 이용해서 tidy data로 전환  후 group_by를 통해 같은 process별로 GWh_sum 만들기


timeseries_tidy <- timeseries_filtered %>% 
  gather(time_series, gwh, -process, -scenario, -period) 


View(timeseries_tidy)


timeseries_tidy$gwh <-as.numeric(timeseries_tidy$gwh)


### NA 값 0으로 할당 해야함!!! 중요

timeseries_tidy[is.na(timeseries_tidy)] = 0


str(timeseries_tidy)

View(timeseries_tidy)


### 할당해줘야 하는 값 있는 컬럼 찾기


timeseries_tidy %>% 
  filter(time_series == 'S'|time_series == 'F' |time_series == 'W'| time_series == 'R')%>%
  filter(gwh != 0)%>%
  View()


timeseries_tidy %>% 
  filter(time_series == 'S'|time_series == 'F' |time_series == 'W'| time_series == 'R')%>%
  filter(gwh != 0)%>%
  group_by(scenario, period, process, time_series) %>% 
  summarise(gwh_sum = sum(gwh)) %>% 
  ungroup() %>% 
  select(time_series, gwh_sum) %>% 
  head()


## -> 아하, F, S W 의 원자력의 할당이 필요하구나~



####  str_sub를 통해서 시계열 값에서 SK, SE, FK 등등으로 분류!
## 원자력만 할당 필요하니까, 원자력만 filter 해서 nuclear로 저장

timeseries_adjusted <- timeseries_tidy %>%
  mutate(TS_length = str_sub(time_series, 1,2))
  
  

timeseries_nuclear_adjusted <- timeseries_adjusted %>% 
  filter(process =="원자력")


  View(timeseries_nuclear_adjusted)



### 할당 비율 dataset으로 지정하기

TS_length <- data.frame('SK' = 0.00947488584474886,
                        'SE' = 0.004337899543379,
                        'FK' = 0.00490867579908676,
                        'FE' = 0.00262557077625571,
                        'WK' = 0.00776255707762557,
                        'WE' = 0.00388127853881279,
                        'RK' = 0.00593607305936073,
                        'RE' = 0.00273972602739726)
                        
                                                  
View(TS_length)

TS_length_tidy <-TS_length %>% 
  gather(time_series, value)


View(TS_length_tidy)



### 리네임!!

TS_length_tidy <- rename(TS_length_tidy, TS_length=time_series)
  



TS_length_tidy[, "gwh_allocated"] <- c(33121, 33121, 17748, 17748, 27561, 27561, 0,0)


head(TS_length_tidy)


### full join으로 TS_length와 원 데이터 병합!!!

  timeseries_nuclear_final <- full_join(timeseries_nuclear_adjusted, TS_length_tidy, by='TS_length')
  

View(timeseries_nuclear_final)

head(timeseries_nuclear_final)

timeseries_nuclear_final<- timeseries_nuclear_final %>% 
  mutate(gwh_adjusted = value * gwh_allocated) %>% 
  select(-value, -gwh_allocated)


head(timeseries_nuclear_final)

## 원자력 아닌 것들만 뽑아내기 (rbind 하기 위해)

head(timeseries_adjusted)


timeseries_non_nuclear_adjusted <- timeseries_adjusted %>% 
  filter(process !='원자력') %>%
  mutate(gwh_adjusted = gwh)

View(timeseries_non_nuclear_adjusted)

timeseries_final <-rbind(timeseries_non_nuclear_adjusted, timeseries_nuclear_final)
           
View(timeseries_final)



## 그래프 만들어보기

timeseries_final %>% 
  select(process, time_series, gwh_adjusted) %>% 
  filter(time_series !='F' & time_series !='S' & time_series !='W' & time_series !='R') %>%
  ggplot(aes(x=time_series, y=gwh_adjusted, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 타임시리즈별 발전량 비중(GWh)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:한국에너지기술연구원 정책연구실")


str(timeseries_final)

timeseries_final$time_series <- as.factor(timeseries_final$time_series)

levels(timeseries_final$time_series)

View(timesereis_final)




















################# 210928 타임슬라이스별 발전량 정리 #########################################  


library(tidyverse)


## UTF는 한글 깨지는 거 방지, check.names는 연도 앞에 X 오는거 방지

TS_raw <-read.csv("210923_timeseries.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names= FALSE)


View(TS_raw)


### Dummy 날리고 filtered로 저장


TS_filtered<- TS_raw %>%  
  filter (process != "Dummy") %>% 
  select(-scenario, -period)


View(TS_filtered)


str(timeseries_filtered)

class(timeseries_filtered)

##현재 wide data임
## gather() 함수를 이용해서 tidy data로 전환  후 group_by를 통해 같은 process별로 GWh_sum 만들기


TS_tidy <- TS_filtered %>% 
  gather(time_series, gwh, -process)
  
  
TS_tidy<- rename(TS_tidy, TS_length = time_series)


head(TS_tidy)


TS_tidy$gwh <-as.numeric(TS_tidy$gwh)


### NA 값 0으로 할당 해야함!!! 중요

TS_tidy[is.na(TS_tidy)] = 0


str(TS_tidy)

View(TS_tidy)


head(TS_tidy)


### 할당해줘야 하는 값 있는 컬럼 찾기

TS_tidy %>% 
  distinct(process)


TS_join <-read.csv("210923_TS_join.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names= FALSE)


TS_join



####  str_sub를 통해서 시계열 값에서 SK, SE, FK 등등으로 분류!
## 원자력만 할당 필요하니까, 원자력만 filter 해서 nuclear로 저장

TS_adjusted <- TS_tidy %>%
  mutate(TS_length_adj = str_sub(TS_length, 1,2))

View(TS_adjusted)

head(TS_adjusted)



### full_join으로 두 데이터 병합!!!

TS_final <- full_join(TS_adjusted, TS_join, by= c("process", "TS_length_adj"))



View(TS_final)
head(TS_final)

TS_final


#### NA인 값들 0으로 설정해야함. 안 그러면 mutate에서 NA값들 때문에 계산 제대로 안됌

TS_final[is.na(TS_final)]<-0


TS_result<- TS_final %>% 
  mutate(gwh_adjusted = gwh +(gwh_nc*value)) 


View(TS_result)


TS_result %>% 
  distinct(process)

head(TS_result)

TS_result %>% 
  distinct(TS_length)




## process를 factor로 저장해서 stcking order 수정 !!! 발견!!!! 유레카!!!

TS_result$process <- as.factor(TS_result$process)

levels(as.factor(TS_result$process))

TS_result$process <- factor(TS_result$process, levels = c("ESS", "기타재생", "석탄",  "신에너지", "양수",  "천연가스CC", "태양광", "원자력","풍력","해상풍력"))



##
## 그래프 만들어보기  ######### 여기 filter 만져보다가 끝냄!!!! 여기 여기 여기

TS_result %>% 
  select(process, TS_length, gwh_adjusted) %>% 
  filter(TS_length !='F' & TS_length !='R' & TS_length !='S' & TS_length !='W'& TS_length !='FE'& TS_length !='FK' & TS_length !='RK' & TS_length !='RE' & TS_length !='SE'& TS_length !='SK' & TS_length !='WE' & TS_length !='WK') %>%
  ggplot(aes(x=TS_length, y=gwh_adjusted, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 타임시리즈별 발전량 비중(GWh)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:한국에너지기술연구원 정책연구실")


#### 원자력만 보기 Example

TS_result %>% 
  select(process, TS_length, gwh_adjusted) %>% 
  filter(TS_length !='F' & TS_length !='R' & TS_length !='S' & TS_length !='W'& TS_length !='FE'& TS_length !='FK' & TS_length !='RK' & TS_length !='RE' & TS_length !='SE'& TS_length !='SK' & TS_length !='WE' & TS_length !='WK' & process=="원자력") %>%
  ggplot(aes(x=TS_length, y=gwh_adjusted, fill=process, group=process))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 타임시리즈별 발전량 비중(GWh)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:한국에너지기술연구원 정책연구실")




######### 연습

TS_result %>% 
  select(process, TS_length, gwh_adjusted) %>% 
  filter(TS_length !='F' & TS_length !='R' & TS_length !='S' & TS_length !='W'& TS_length !='FE'& TS_length !='FK' & TS_length !='RK' & TS_length !='RE' & TS_length !='SE'& TS_length !='SK' & TS_length !='WE' & TS_length !='WK') %>%
  ggplot(aes(x=TS_length, y=gwh_adjusted, fill=factor(process, levels =c("해상풍력", "태양광", "천연가스CC", "원자력")), group=process))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 타임시리즈별 발전량 비중(GWh)",
       subtitle = "서브타이틀",
       x= "구분",
       y= "발전량 (GWh)",
       caption= "제작:한국에너지기술연구원 정책연구실")
