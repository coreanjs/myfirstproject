library(tidyverse)
library(RColorBrewer)
library(treemapify)



GIR_raw <- read.csv("211118_gir_raw.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE)


GIR_raw

str(GIR_raw)


View(GIR_raw)
head(GIR_raw)

GIR_tidy <-GIR_raw %>% 
  gather(year, emission, -type, -totalnet) 

head(GIR_tidy)



View(GIR_tidy)
str(GIR_tidy)


### character to numeric

GIR_tidy$year <- as.numeric(GIR_tidy$year)

##0인 값들을 NA로 설정. 그래프에 0값 안 뜨게

GIR_tidy$emission <- as.numeric(gsub(",","", GIR_tidy$emission))


head(GIR_tidy)




## 개별 line graph
GIR_tidy %>% 
  filter ( type != "온실가스" & totalnet =="순배출량") %>% 
  select(-totalnet) %>% 
  ggplot()+
  geom_line(aes(x= year, y = emission, group = type, color = type))+
  scale_x_continuous(limits= c(1990, 2020),
                     breaks = seq(1990, 2020, by=5))



GIR_tidy %>% 
  filter (type!="온실가스" &totalnet=="순배출량" &year == "2018") %>%
  arrange()
  

## type을 factor로 저장해서 stcking order 수정 

GIR_tidy$type <- as.factor(GIR_tidy$type)

levels(as.factor(GIR_tidy$type))

GIR_tidy$type <- factor(GIR_tidy$type, levels = c("CO2", "CH4", "N2O",  "HFCs", "PFCs",  "SF6", "온실가스"))

GIR_tidy$type

## 누적 column 그래프
GIR_tidy %>% 
  filter ( type != "온실가스" & totalnet =="순배출량") %>% 
  select(-totalnet) %>% 
  ggplot()+
  geom_col(aes(x= year, y = emission, group = type, fill = type))+
  scale_fill_brewer(palette="Paired")+
  scale_x_continuous(limits= c(1990, 2020),
                     breaks = seq(1990, 2020, by=5))



## 누적 area 그래프
GIR_tidy %>% 
  filter ( type != "온실가스" & totalnet =="순배출량") %>% 
  ggplot()+
  geom_area(aes(x= year, y = emission, group = type, fill = type))+
  scale_fill_brewer(palette="Paired")+
  scale_x_continuous(limits= c(1990, 2020),
                     breaks = seq(1990, 2020, by=5))


## 2018년 Treemap으로 나타내기

GIR_2018 <- GIR_tidy %>% 
  filter ( type != "온실가스" & totalnet =="순배출량" & year == "2018") %>% 
  mutate( percent = emission/sum(emission)*100) 

  
## 소수점 둘째 자리

options(digits=2)

head(GIR_2018)

a
b

sprintf("%0.2f", round(GIR_2018$percent, digits = 1))


library(glue)



## TEST 1
GIR_2018 %>% 
  ggplot(aes(fill = type, area = emission, label = paste0(type, "\n",sprintf("%0.1f", round(percent, digits = 2)))))+
  geom_treemap()+
  geom_treemap_text(color ="black", place = "centre")+
  scale_fill_brewer(palette="Set2")+
  labs(title = "대한민국 2018년 온실가스 배출량 구분",
       subtitle = "CO2, CH4, N20, HFCs, SF6, PFCs,")+
  theme(legend.position = "none")



### TEST 2 (성공)
GIR_2018 %>% 
  ggplot(aes(fill = type, area = emission, label = 
               paste0(type, "\n", round(percent, digits = 1), "%")))+
  geom_treemap()+
  geom_treemap_text(color ="black", place = "centre")+
  scale_fill_brewer(palette="Set2")+
  labs(title = "대한민국 2018년 온실가스 배출량 구분",
       subtitle = "CO2, CH4, N20, HFCs, SF6, PFCs,")+
  theme(legend.position = "none")

  

## 랜덤 컬러 뽑아주는 라이브러리
library(randomcoloR)

distinctColorPalette(3)
 
n <-6
jiseok <-distinctColorPalette(n)

jiseok

GIR_2018 %>% 
  ggplot(aes(fill = type, area = emission, label = paste0(type, "\n", round(percent, digits = 1), "%")))+
  geom_treemap()+
  geom_treemap_text(fontface = "italic", color ="black", place = "centre")+
  labs(title = "대한민국 2018년 온실가스 배출량 구분",
       subtitle = "CO2, CH4, N20, HFCs, SF6, PFCs,")+
  theme(legend.position = "none")+
  scale_fill_manual(values= jiseok)


GIR_a<-GIR_tidy %>% 
  filter ( type != "온실가스" & totalnet =="순배출량")

GIR_a

GIR_b<-GIR_tidy %>% 
  filter ( type == "온실가스" & totalnet =="순배출량")

head(GIR_tidy) %>% 
  distinct(type)

View(GIR_b)

head(GIR_b)

## type을 factor로 저장해서 stcking order 수정 

GIR_a$type <- as.factor(GIR_a$type)

levels(as.factor(GIR_a$type))

GIR_a$type <- factor(GIR_a$type, levels = c("CO2", "CH4", "N2O",  "HFCs", "PFCs",  "SF6", "온실가스"))

GIR_tidy$type


ggplot()+ 
  geom_area(data = GIR_a, aes(x= year, y = emission, group = type, fill = type))+
  geom_line(data = GIR_b, aes(x= year, y = emission),
            size = 1,
            color = "red",
            linetype = 2)+
  labs(title = "타이틀",
       subtitle = "서브타이틀")+
  scale_fill_brewer(palette="Paired")
  

ggplot()+ 
  geom_col(data = GIR_a, aes(x= year, y = emission, group = type, fill = type))+
  geom_line(data = GIR_b, aes(x= year, y = emission),
            size = 1,
            color = "red",
            linetype = 2)+
  labs(title = "타이틀",
       subtitle = "서브타이틀")+
  scale_fill_brewer(palette="Set3")



##### 트리맵 2탄 211122

Treemap2018_raw <- read.csv("211122_Treemap_GIR.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE)


View(Treemap_raw)
str(Treemap_raw)

## 현재 2018년도 배출량이 chr로 되어있음. 쉼표(,) 빼줘야 함
 
Treemap2018_raw$emission <-gsub(",","", Treemap_raw$emission)

## numeric으로 변경

Treemap2018_raw$emission <-as.numeric(Treemap2018_raw$emission)



View(Treemap_raw)


Treemap2018_total <- Treemap_raw %>% 
  filter(type !="LULUCF")




Treemap2018_total$emission <-gsub(",","", Treemap2018_total$emission)
Treemap2018_total$emission <-as.numeric(Treemap2018_total$emission)


head(Treemap2018_total)
str(Treemap2018_total)

View(Treemap2018_total)


library(treemap)




#폰트

library(showtext)

font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Poor Story", "poorstory")

showtext_auto()
showtext_end()

??showtext

windows() 

?treemap



### 대한민국 온실가스 총배출량 구분(2018년도 기준 )- 구분 1단계

treemap(Treemap2018_total,
        index=c("type"),  
        vSize = "emission",  
        type="index",
        palette = "Set3",  
        fontsize.labels = c(25),
        fontface.labels = c(1),
        title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
        fontsize.title = 30,
        align.labels = list(c("left", "top")),
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic")

### 대한민국 온실가스 총배출량 구분(2018년도 기준 )- 구분 2단계


treemap(Treemap2018_total, 
        index=c("type","sub1"),  
        vSize = "emission",  
        type="index", 
        palette = "Set3",  
        fontsize.labels = c(25,10),
        fontface.labels = c(4,3,2,1),
        title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
        fontsize.title = 30,
        align.labels = list(c("left", "top"),c("center", "center")),
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic")


### 대한민국 온실가스 총배출량 구분(2018년도 기준 )- 구분 3단계


treemap(Treemap2018_total, 
        index=c("type","sub1","sub2"),  
        vSize = "emission",  
        type="index",
        palette = "Set3", 
        fontsize.labels = c(25,10,10),
        fontface.labels = c(4,3,2,1),
        title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
        fontsize.title = 30,
        align.labels = list(c("left", "top"),c("center", "center"), c("center", "center"), c("right", "bottom")),
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic")


### 대한민국 온실가스 총배출량 구분(2018년도 기준 )- 구분 4단계

library(svglite)


svglite(width = 18, height = 30, file ='treemap3.svg')

treemap(Treemap2018_total, 
        index=c("type","sub1","sub2", "sub3"),  
        vSize = "emission",  
        type="index",
        palette = "Set3", 
        fontsize.labels = c(30,20,20, 20),
        fontcolor.labels = c("black", "purple", "blue", "white"),
        fontface.labels = c(2,3,1,1),
        title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
        fontsize.title = 30,
        align.labels = list(c("left", "top"),c("center", "center"), c("right", "center"), c("right", "bottom")),
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic",
        lowerbound.cex.labels = 0)

dev.off()


treemap(Treemap2018_energy, 
        index=c("type","sub1","sub2", "sub3"),  
        vSize = "emission",  
        type="index",
        fontsize.labels = c(25,20,20, 20),
        title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
        fontsize.title = 30,
        align.labels = list(c("left", "top"),c("center", "center"), c("center", "center"), c("right", "bottom")),
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic",
        lowerbound.cex.labels = 0)

#plotly

install.packages("plotly")
library(plotly)


#png로 저장하기

  png(width = 1366, height = 2008, file ='treemap2.png')
  
  treemap(Treemap2018_total, # your data frame object
          index=c("type","sub1","sub2", "sub3"),  #A list of your categorical variables
          vSize = "emission",  #This is your quantitative variable
          type="index", #Type sets the organization and color scheme of your treemap
          palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
          fontsize.labels = c(20,20,20,10),
          fontface.labels = c(4,3,2,1),
          title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
          fontsize.title = 30,
          align.labels = list(c("left", "top"),c("center", "center"), c("center", "center"), c("right", "bottom")),
          fontfamily.labels = "nanumgothic",
          fontfamily.legend = "nanumgothic",
          fontfamily.title = "nanumgothic")
  
  
  
  dev.off()


  #pdf로 저장하기
  
  pdf(width = 1366, height = 2008, file ='treemap.pdf')
  treemap(Treemap2018_total, # your data frame object
          index=c("type","sub1","sub2", "sub3"),  #A list of your categorical variables
          vSize = "emission",  #This is your quantitative variable
          type="index", #Type sets the organization and color scheme of your treemap
          palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
          fontsize.labels = c(80,40,20,10),
          fontface.labels = c(4,3,2,1),
          title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
          fontsize.title = 30,
          align.labels = list(c("left", "top"),c("center", "center"), c("center", "center"), c("right", "bottom")),
          fontfamily.labels = "nanumgothic",
          fontfamily.legend = "nanumgothic",
          fontfamily.title = "nanumgothic")
  dev.off()
  
  


Treemap2018_energy <- Treemap_raw %>% 
  filter(type =="에너지")


Treemap2018_energy$emission <-gsub(",","", Treemap2018_energy$emission)
Treemap2018_energy$emission <-as.numeric(Treemap2018_energy$emission)


str(Treemap2018_energy)
View(Treemap2018_energy)

treemap(Treemap2018_energy, 
        index=c("type","sub1","sub2", "sub3"),  
        vSize = "emission", 
        type="index", 
        palette = "Set2",
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic"
)


treemap(Treemap2018_energy,
        index=c("type","sub1","sub2", "sub3"),
        vSize = "emission",  
        type="index",
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic"
)





##plotly 테스트


labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura")
parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
values =  c(10, 14, 12, 10, 2, 6, 6, 1, 4)




plotly.test <- data.frame(labels, parents, values)

plotly.test


fig <- plot_ly(
  type='treemap',
  labels=plotly.test$labels,
  parents=plotly.test$parents,
  values= plotly.test$values,
  textinfo="label+value+percent parent+percent entry+percent root",
  domain=list(column=0))

fig

fig <- fig %>% add_trace(
  type='treemap',
  branchvalues="total",
  labels=labels,
  parents=parents,
  values=c(65, 14, 12, 10, 2, 6, 6, 1, 4),
  textinfo="label+value+percent parent+percent entry",
  outsidetextfont=list(size=20, color= "darkblue"),
  marker=list(line= list(width=2)),
  pathbar=list(visible= FALSE),
  domain=list(column=1))

fig

fig <- fig %>% layout(
  grid=list(columns=2, rows=1),
  margin=list(l=0, r=0, b=0, t=0))

fig




##################211129_ 트리맵 조금 더 정제해서 만든 것



Treemap2018v2_raw <- read.csv("211129_Treemap_GIR_v2.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE)


View(Treemap2018v2_raw)
str(Treemap2018v2_raw)

## 현재 2018년도 배출량이 chr로 되어있음. 쉼표(,) 빼줘야 함

Treemap2018v2_raw$emission <-gsub(",","", Treemap2018v2_raw$emission)

## numeric으로 변경

Treemap2018v2_raw$emission <-as.numeric(Treemap2018v2_raw$emission)



View(Treemap2018v2_raw)


Treemap2018v2_total <- Treemap2018v2_raw 




Treemap2018v2_total$emission <-gsub(",","", Treemap2018v2_total$emission)
Treemap2018v2_total$emission <-as.numeric(Treemap2018v2_total$emission)




library(treemap)



#폰트

library(showtext)

font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Poor Story", "poorstory")

showtext_auto()
showtext_end()

??showtext

windows() 

?treemap



### 대한민국 온실가스 총배출량 구분(2018년도 기준 )- 구분 4단계

library(svglite)


svglite(width = 18, height = 30, file ='treemap3.svg')



Treemap2018v2_total<- as.data.frame(Treemap2018v2_total)


head(Treemap2018v2_total)
str(Treemap2018v2_total)



svg(width = 15, height = 9, file ='jiseoktreemap_height9.svg')

treemap(Treemap2018v2_total, 
        index=c("type"),  
        vSize = "emission",  
        type="index",
        palette = "Set3", 
        fontsize.labels = c(30,20,20, 20),
        fontcolor.labels = c("black", "purple", "blue", "white"),
        fontface.labels = c(2,3,1,1),
        title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
        fontsize.title = 30,
        align.labels = list(c("left", "top"),c("center", "center"), c("right", "center"), c("right", "bottom")),
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic",
        lowerbound.cex.labels = 0)

dev.off()


# png로 저장하기 ####

png(width = 1400, height = 3008, file ='jiseoktreemap_height3000.png')

treemap(Treemap2018v2_total, 
        index=c("type","sub1","sub2", "sub3"),  
        vSize = "emission",  
        type="index",
        palette = "Set3", 
        fontsize.labels = c(30,20,20, 20),
        fontcolor.labels = c("black", "purple", "blue", "white"),
        fontface.labels = c(2,3,1,1),
        title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
        fontsize.title = 50,
        align.labels = list(c("left", "top"),c("center", "center"), c("right", "center"), c("right", "bottom")),
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic",
        lowerbound.cex.labels = 0)
dev.off()



# svg로 저장하기 #### ( 길게)

svg(width = 15, height = 80, file ='jiseoktreemap_height80_detail.svg')

treemap(Treemap2018v2_total, 
        index=c("type","sub1","sub2", "sub3"),  
        vSize = "emission",  
        type="index",
        palette = "Set3", 
        fontsize.labels = c(30,20,20, 20),
        fontcolor.labels = c("black", "purple", "blue", "white"),
        fontface.labels = c(2,3,1,1),
        title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
        fontsize.title = 50,
        align.labels = list(c("left", "top"),c("center", "center"), c("right", "center"), c("right", "bottom")),
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic",
        lowerbound.cex.labels = 0)
dev.off()



# svg로 저장하기 ####  (모니터에 꽉 차게)

svg(width = 15, height =9 , file ='jiseoktreemap_height9_detail.svg')

treemap(Treemap2018v2_total, 
        index=c("type","sub1","sub2", "sub3"),  
        vSize = "emission",  
        type="index",
        palette = "Set2", 
        fontsize.labels = c(30,20,20, 20),
        fontcolor.labels = c("black", "purple", "blue", "white"),
        fontface.labels = c(2,3,1,1),
        title = "대한민국 온실가스 총배출량 구분(2018년 기준)",
        fontsize.title = 50,
        align.labels = list(c("left", "top"),c("center", "center"), c("right", "center"), c("right", "bottom")),
        fontfamily.labels = "nanumgothic",
        fontfamily.legend = "nanumgothic",
        fontfamily.title = "nanumgothic",
        lowerbound.cex.labels = 0)

dev.off()


### voronoiTreemap 을 활용해서 만들어보자. 보로노이 트리맵 !! 실패!!!



library(voronoiTreemap)


head(ExampleGDP)
View(ExampleGDP)
gdp_json <- vt_export_json(vt_input_from_df(ExampleGDP))
vt_d3(gdp_json)

??voronoiTreemap




################## 새로 테스트



powergen.KOR.raw <- read.csv("211129_dumbbell.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE)




powergen.KOR.raw



str(powergen.KOR.raw)


powergen.KOR.tidy <-powergen.KOR.raw %>% 
  gather(year, emission, -type, -totalnet) 

head(GIR_tidy)



View(GIR_tidy)
str(GIR_tidy)


### character to numeric

GIR_tidy$year <- as.numeric(GIR_tidy$year)

##0인 값들을 NA로 설정. 그래프에 0값 안 뜨게

GIR_tidy$emission <- as.numeric(gsub(",","", GIR_tidy$emission))


head(GIR_tidy)





