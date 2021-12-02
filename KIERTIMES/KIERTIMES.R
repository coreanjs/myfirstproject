library(tidyverse)
library(RColorBrewer)
library(scales)


##  power generation

powergen.raw<-read.csv("211123_powergen_CCUS.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 


## 소수점 둘째 자리

options(digits=4)


view(powergen.raw)
str(powergen.raw)

head(powergen.raw)

powergen.tidy <-powergen.raw %>% 
  gather(year, GWh, -process, -scenario)

View(powergen.tidy)

head(powergen.tidy)


### techlist

techlist<-read.csv("techlist.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 


head(techlist)




## Force R not to use exponential notation

options(scipen = 999)

### full join으로 TS_length와 원 데이터 병합!!!

powergen.joined <- full_join(powergen.tidy, techlist, by='process') %>% 
  select(scenario, year, GWh, type1, type2, type3, type4, type5, type6) %>% 
  filter(scenario !='NA')
  

view(powergen.joined)

head(powergen.joined)


## process를 factor로 저장해서 그래프 순서 수정 !!! 중요

powergen.joined$scenario <- as.factor(powergen.joined$scenario)
levels(as.factor(powergen.joined$scenario))
powergen.joined$scenario <- factor(powergen.joined$scenario, 
                                   levels = c("CCUS55","CCUS60","CCUS65","CCUS70","CCUS75","CCUS80","CCUS85","CCUS90","CCUS95","CCUS100"))

### type 1 
powergen.type1 <- powergen.joined %>% 
  select(type1, scenario, year, GWh)



head(powergen.type1)
View(powergen.type1)

powergen.type1 %>% 
  ggplot(aes(x= year, y = GWh, group = type1, fill = type1))+
  geom_bar(position = "stack", stat ="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous( labels = comma)+
  facet_wrap(~scenario, nrow = 1)

    

### type 2 
powergen.type2 <- powergen.joined %>% 
  select(type2, scenario, year, GWh)


head(powergen.type2)
View(powergen.type2)

powergen.type2 %>% 
  ggplot(aes(x= year, y = GWh, group = type2, fill = type2))+
  geom_bar(position = "stack", stat ="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous( labels = comma)+
  facet_wrap(~scenario, nrow =1)



### type 3 
powergen.type3 <- powergen.joined %>% 
  select(type3, scenario, year, GWh)

powergen.type3 %>% 
  ggplot(aes(x= year, y = GWh, group = type3, fill = type3))+
  geom_bar(position = "stack", stat ="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous( labels = comma)+
  facet_wrap(~scenario, nrow =1)


### type 4 
powergen.type4 <- powergen.joined %>% 
  select(type4, scenario, year, GWh) 



powergen.type4 %>% 
  distinct(type4)

## 랜덤 컬러 뽑아주는 라이브러리
library(randomcoloR)

Random <-distinctColorPalette(15)

Random




powergen.type4 %>% 
  filter(type4!="Dummy") %>% 
  ggplot(aes(x= year, y = GWh, group = type4, fill = type4))+
  geom_bar(position = "stack", stat ="identity")+
  scale_fill_manual(values= Random)+
  scale_y_continuous( labels = comma)+
  facet_wrap(~scenario, nrow=1)



## Force R not to use exponential notation

options(scipen = 999)


### to 박상용



powergen.type4 %>% 
  filter(type4!="Dummy" & year == "2050") %>% 
  ggplot(aes(x= year, y = GWh, group = type4, fill = type4))+
  geom_bar(position = "stack", stat ="identity")+
  scale_fill_manual(values= Random)+
  scale_y_continuous( labels = comma)+
  facet_wrap(~scenario, nrow=1)


############### geom text에 들어가는 데이터

powergen.type4.text <-powergen.type4 %>% 
  filter(type4 !="Dummy" & year == "2050") %>%
  select(scenario, type4, GWh) %>%
  group_by(scenario,type4) %>%
  summarise(GWh = sum(GWh, na.rm=TRUE)) %>% 
  mutate(percent = GWh/sum(GWh, na.rm=TRUE )*100)%>%
  filter(type4 =="신에너지")
  

powergen.type4.text 


############################# 211130 -> 박상용 (CCUS 제약시나리오 - 발전량) #######################################
powergen.type4 %>% 
  filter(type4!= "Dummy" & year == "2050" & type4 != "집단에너지" & type4 !="천연가스CC") %>%
  select(scenario, type4, GWh) %>%
  group_by(scenario,type4) %>%
  summarise(GWh = sum(GWh, na.rm=TRUE)) %>% 
  mutate(percent = GWh/sum(GWh, na.rm=TRUE )*100)%>%
  ggplot(aes(x= scenario, y =GWh, group = type4, fill = type4))+
  geom_bar(position = "stack", stat ="identity")+
  geom_text(data = powergen.type4.text, aes(label =paste0(sprintf("%1.1f", percent), "%")),
            position =position_stack(vjust = 2.2))+
  scale_fill_brewer(palette = "Paired")+
  scale_y_continuous(breaks = seq(0,1500000, 500000), labels = comma)+
  theme(axis.ticks.y=element_blank())+
  labs(title = "2050년 CCUS 제약 시나리오별 발전량",
       x= "2050년 시나리오",
       y= "GWh",
       fill = "구분")+
  theme_minimal()






###### HDG_PROC



HDGPROC.raw<-read.csv("211123_hydrogen_CCUS.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 

view(HDGPROC.raw)

head(HDGPROC.raw)

HDGPROC.tidy <-HDGPROC.raw %>%
  select(-commodity) %>% 
  gather(year, kton, -process, -scenario)






## process를 factor로 저장해서 그래프 순서 수정 !!! 중요

HDGPROC.tidy$scenario <- as.factor(HDGPROC.tidy$scenario)
levels(as.factor(HDGPROC.tidy$scenario))
HDGPROC.tidy$scenario <- factor(HDGPROC.tidy$scenario, 
                                   levels = c("CCUS55","CCUS60","CCUS65","CCUS70","CCUS75","CCUS80","CCUS85","CCUS90","CCUS95","CCUS100"))



HDGPROC.tidy %>% 
  ggplot(aes(x= year, y = kton, group = process, fill = process))+
  geom_bar(position = "stack", stat ="identity")+
  scale_fill_brewer(palette="Paired")+
  facet_wrap(~scenario, ncol=9)


## to 박상용


HDGPROC.tidy

HDGPROC.tidy.2050 <- HDGPROC.tidy %>% 
  filter(year == "2050" & process != "HDG93PP-DSMR1N") %>% 
  select(scenario, process, kton) %>%
  group_by(scenario) %>% 
  mutate( percent = kton/sum(kton)*100)

head(HDGPROC.tidy.2050)

options(digit=1)
str(HDGPROC.tidy.2050)


library(glue)


HDGPROC.tidy.2050$percent <-as.numeric(HDGPROC.tidy.2050$percent)

############################# 211130 -> 박상용 (CCUS 제약 시나리오 - 수소 생산량) ###############################

library(ggrepel)

HDGPROC.tidy.2050$process <- as.factor(HDGPROC.tidy.2050$process)
levels(as.factor(HDGPROC.tidy.2050$process))
HDGPROC.tidy.2050$process <- factor(HDGPROC.tidy.2050$process, 
                                   levels = c("HDG93PP-IMP", "HDG93PP-BYP","HDG93PP-ELY1N",  "HDG93PP-DSMRCC1N"))

HDGPROC.tidy.2050

HDGPROC.tidy.2050 %>% 
  group_by(process) %>% 
  distinct(process)

View(HDGPROC.tidy.2050)

HDGPROC.tidy.2050.text<- HDGPROC.tidy.2050 %>% 
  group_by(process, scenario) %>% 
  summarise(kton = sum(kton),
            percent =sum(percent)) %>% 
  filter(process == "HDG93PP-DSMRCC1N" | process =="HDG93PP-ELY1N")


HDGPROC.tidy.2050.text


HDGPROC.tidy.2050%>%
  ggplot(aes(x= scenario, y = kton, group = process, fill = process))+
  geom_bar(position = "stack", stat ="identity")+
  geom_text(data = HDGPROC.tidy.2050.text, aes(label =paste0(sprintf("%1.1f", percent), "%")),
            position =position_dodge(0), vjust= 1)+
  scale_y_continuous(breaks = seq(0,30000, 10000), labels = comma)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 CCUS 제약 시나리오별 수소 생산량",
       x= "2050년 시나리오",
       y= "천톤",
       fill = "구분")+
  theme_minimal()



  





############################ 2021.11.25  수소 RND 변화하는 시나리오##################################################

##  power generation

powergen.HDG.raw<-read.csv("211125_powergen_HDGRND.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 


## 소수점 둘째 자리

options(digits=4)


## Force R not to use exponential notation

options(scipen = 999)


View(powergen.HDG.raw)
str(powergen.HDG.raw)


powergen.HDG.tidy <-powergen.HDG.raw %>% 
  gather(year, GWh, -process, -scenario)



### full join으로 TS_length와 원 데이터 병합!!!

powergen.HDG.joined <- full_join(powergen.HDG.tidy, techlist, by='process') %>% 
  select(scenario, year, GWh, type1, type2, type3, type4, type5, type6) %>% 
  filter(scenario !='NA')


view(powergen.HDG.joined)



## process를 factor로 저장해서 그래프 순서 수정 !!! 중요

powergen.joined$scenario <- as.factor(powergen.joined$scenario)
levels(as.factor(powergen.joined$scenario))
powergen.joined$scenario <- factor(powergen.joined$scenario, 
                                   levels = c("CCUS55","CCUS60","CCUS65","CCUS70","CCUS75","CCUS80","CCUS85","CCUS90","CCUS95","CCUS100"))


### type 4 
powergen.HDG.type4 <- powergen.HDG.joined %>% 
  select(type4, scenario, year, GWh)




############### geom text에 들어가는 데이터

powergen.HDG.joined.text <-powergen.HDG.joined %>% 
  filter(type4 !="Dummy" & year == "2050") %>%
  select(scenario, type4, GWh) %>%
  group_by(scenario,type4) %>%
  summarise(GWh = sum(GWh, na.rm=TRUE)) %>% 
  mutate(percent = GWh/sum(GWh, na.rm=TRUE )*100)%>%
  filter(type4 =="신에너지")


powergen.HDG.joined.text

############################# 211130 -> 박상용 ( 수소 R&D시나리오 - 발전량) #######################################

powergen.HDG.joined %>% 
  filter(type4!= "Dummy" & year == "2050" & type4 != "집단에너지" & type4 !="천연가스CC") %>%
  select(scenario, type4, GWh) %>%
  group_by(scenario,type4) %>%
  summarise(GWh = sum(GWh, na.rm=TRUE)) %>% 
  mutate(percent = GWh/sum(GWh, na.rm=TRUE )*100)%>%
  ggplot(aes(x= scenario, y =GWh, group = type4, fill = type4))+
  geom_bar(position = "stack", stat ="identity")+
  geom_text(data = powergen.HDG.joined.text, aes(label =paste0(sprintf("%1.1f", percent), "%")),
            position =position_stack(vjust = 2.2))+
  scale_fill_brewer(palette = "Paired")+
  scale_y_continuous(breaks = seq(0,1500000, 500000), labels = comma)+
  theme(axis.ticks.y=element_blank())+
  labs(title = "2050년 수소 R&D 시나리오별 발전량",
       x= "2050년 시나리오",
       y= "GWh",
       fill = "구분")+
  theme_minimal()









############################# 211130 -> 박상용   (수소 R&D 시나리오 - 수소 생산량)########################################

###### HDG_PROC



HDGRND.raw<-read.csv("211125_hydrogen_HDGRND.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 

view(HDGRND.raw)

head(HDGRND.raw)

HDGRND.tidy <-HDGRND.raw %>%
  select(-commodity) %>% 
  gather(year, kton, -process, -scenario)


HDGRND.tidy.2050 <- HDGRND.tidy %>% 
  filter(year=="2050") %>% 
  select(-year) 




HDGRND.tidy.2050 

HDGRND.tidy.2050 %>% 
  distinct(process)



head(HDGRND.tidy.2050)

options(digit=1)
str(HDGRND.tidy.2050)


library(glue)


HDGRND.tidy.2050$percent <-as.numeric(HDGRND.tidy.2050$percent)


############################# 211130 -> 박상용 (수소 R&D 시나리오  - 수소 생산량) ###############################


HDGRND.tidy.2050$process <- as.factor(HDGRND.tidy.2050$process)
levels(as.factor(HDGRND.tidy.2050$process))
HDGRND.tidy.2050$process <- factor(HDGRND.tidy.2050$process, 
                                    levels = c("HDG93PP-IMP", "HDG93PP-BYP","HDG93PP-ELY1N",  "HDG93PP-DSMRCC1N"))



## 텍스트




##0인 값들을 NA로 설정. 그래프에 0값 안 뜨게


HDGRND.tidy.2050<- HDGRND.tidy.2050 %>% 
  replace(is.na(.), 0)

head(HDGRND.tidy.2050)

View(HDGRND.tidy.2050)


HDGRND.tidy.2050.text <- HDGRND.tidy.2050 %>% 
  group_by(scenario) %>% 
  mutate( percent = kton/sum(kton)*100)%>%   
  filter(process == "HDG93PP-DSMRCC1N" | process =="HDG93PP-ELY1N")
  


View(HDGRND.tidy.2050.text)


HDGRND.tidy.2050.text



## 그래프


HDGRND.tidy.2050 %>% 
  select(process, scenario, kton) %>% 
  filter (process == "HDG93PP-CSMR1N")

HDGRND.tidy.2050%>%
  filter( process != "HDG93PP-DSMR1E" & process != "HDG93PP-CSMR1N") %>% 
  ggplot(aes(x= scenario, y = kton, group = process, fill = process))+
  geom_bar(position = "stack", stat ="identity")+
  geom_text(data = HDGRND.tidy.2050.text, aes(label =paste0(sprintf("%1.1f", percent), "%")),
            position =position_dodge(0.3), vjust= 1, size = 3.5)+
  scale_y_continuous(breaks = seq(0,30000, 10000), labels = comma)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 수소 R&D 시나리오별 수소 생산량",
       x= "2050년 시나리오",
       y= "천톤",
       fill = "구분")+
  theme_minimal()





##### 수소


























################# 수소 수입 시나리오#################################################################################

###### HDG_PROC





############## 211130 -> 박상용 (수소 수입량 제약 시나리오 - 발전량) #######################################################



##  power generation

powergen.HDGIMP.raw<-read.csv("211125_powergen_HDGIMP.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 


## 소수점 둘째 자리

options(digits=4)


## Force R not to use exponential notation

options(scipen = 999)


View(powergen.HDGIMP.raw)
str(powergen.HDGIMP.raw)


powergen.HDGIMP.tidy <-powergen.HDGIMP.raw %>% 
  gather(year, GWh, -process, -scenario)



### full join으로 TS_length와 원 데이터 병합!!!

powergen.HDGIMP.joined <- full_join(powergen.HDGIMP.tidy, techlist, by='process') %>% 
  select(scenario, year, GWh, type1, type2, type3, type4, type5, type6) %>% 
  filter(scenario !='NA')


view(powergen.HDGIMP.joined)



### type 4 
powergen.HDGIMP.type4 <- powergen.HDGIMP.joined %>% 
  select(type4, scenario, year, GWh)




### to 박상용

powergen.HDGIMP.type4 %>% 
  filter(type4!="Dummy" & year == "2050") %>% 
  ggplot(aes(x= year, y =GWh, group = type4, fill = type4))+
  geom_bar(position = "stack", stat ="identity")+
  scale_fill_brewer(palette = "Paired")+
  scale_y_continuous(breaks = seq(0,1500000, 500000),labels = comma) +
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = "2050년 수소 수입량 제약 시나리오별 발전량",
       x= "2050년 시나리오",
       y= "GWh",
       fill = "구분")+
  facet_wrap(~scenario, nrow=1)



############### geom text에 들어가는 데이터

powergen.HDGIMP.type4.text <-powergen.HDGIMP.type4 %>% 
  filter(type4 !="Dummy" & year == "2050") %>%
  select(scenario, type4, GWh) %>%
  group_by(scenario,type4) %>%
  summarise(GWh = sum(GWh, na.rm=TRUE)) %>% 
  mutate(percent = GWh/sum(GWh, na.rm=TRUE )*100)%>%
  filter(type4 =="신에너지")

powergen.HDGIMP.type4.text


############################# 211130 -> 박상용 (수소 수입량 제약 시나리오 - 발전량) #######################################
powergen.HDGIMP.type4 %>% 
  filter(type4!= "Dummy" & year == "2050" & type4 != "집단에너지" & type4 !="천연가스CC") %>%
  select(scenario, type4, GWh) %>%
  group_by(scenario,type4) %>%
  summarise(GWh = sum(GWh, na.rm=TRUE)) %>% 
  mutate(percent = GWh/sum(GWh, na.rm=TRUE )*100)%>%
  ggplot(aes(x= scenario, y =GWh, group = type4, fill = type4))+
  geom_bar(position = "stack", stat ="identity")+
  geom_text(data = powergen.HDGIMP.type4.text, aes(label =paste0(sprintf("%1.1f", percent), "%")),
            position =position_stack(vjust = 2))+
  scale_fill_brewer(palette = "Paired")+
  scale_y_continuous(breaks = seq(0,1500000, 500000), labels = comma)+
  theme(axis.ticks.y=element_blank())+
  labs(title = "2050년 수소 수입량 제약 시나리오별 수소 발전량",
       x= "2050년 시나리오",
       y= "GWh",
       fill = "구분")+
  theme_minimal()




############################# 211130 -> 박상용  (수소 수입량 제약 시나리오 - 수소 생산량)###############################

hydrogen.HDGIMP.raw<-read.csv("211125_hydrogen_HDGIMP.csv", header=TRUE, fileEncoding = "UTF-8-BOM", check.names=FALSE) 

view(hydrogen.HDGIMP.raw)

head(hydrogen.HDGIMP.raw)

hydrogen.HDGIMP.tidy <-hydrogen.HDGIMP.raw %>%
  select(-commodity) %>% 
  gather(year, kton, -process, -scenario)

head(hydrogen.HDGIMP.tidy)
str(hydrogen.HDG.tidy)



hydrogen.HDGIMP.2050 <- hydrogen.HDGIMP.tidy%>% 
  filter(year == "2050" & process != "HDG93PP-DSMR1N") %>% 
  select(scenario, process, kton) %>%
  group_by(scenario) %>% 
  mutate( percent = kton/sum(kton)*100)


hydrogen.HDGIMP.2050


hydrogen.HDGIMP.tidy %>% 
  ggplot(aes(x= year, y = kton, group = process, fill = process))+
  geom_bar(position = "stack", stat ="identity")+
  scale_fill_brewer(palette="Paired")+
  facet_wrap(~scenario, ncol=9)


## to 박상용


hydrogen.HDGIMP.tidy %>%
  filter(year =="2050") %>% 
  ggplot(aes(x= year, y = kton, group = process, fill = process))+
  geom_bar(position = "stack", stat ="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = comma) +
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = "2050년 수소 R&D 시나리오별 수소 생산량",
       x= "2050년 시나리오",
       y= "천톤",
       fill = "구분")+
  facet_wrap(~scenario, nrow=1)




hydrogen.HDGIMP.2050$process <- as.factor(hydrogen.HDGIMP.2050$process)
levels(as.factor(hydrogen.HDGIMP.2050$process))
hydrogen.HDGIMP.2050$process <- factor(hydrogen.HDGIMP.2050$process, 
                                       levels = c("HDG93PP-IMP", "HDG93PP-BYP","HDG93PP-ELY1N",  "HDG93PP-DSMRCC1N"))

hydrogen.HDGIMP.tidy%>% 
  group_by(process) %>% 
  distinct(process)



hydrogen.HDGIMP.2050


hydrogen.HDGIMP.text<- hydrogen.HDGIMP.2050 %>% 
  group_by(process, scenario) %>% 
  summarise(kton = sum(kton),
            percent =sum(percent)) %>% 
  filter(process == "HDG93PP-DSMRCC1N" | process =="HDG93PP-ELY1N")


hydrogen.HDGIMP.text

hydrogen.HDGIMP.2050%>%
  ggplot(aes(x= scenario, y = kton, group = process, fill = process))+
  geom_bar(position = "stack", stat ="identity")+
  geom_text(data =hydrogen.HDGIMP.text, aes(label =paste0(sprintf("%1.1f", percent), "%")),
            position =position_stack(vjust= 0.5))+
  scale_y_continuous(breaks = seq(0,30000, 10000), labels = comma)+
  scale_fill_brewer(palette="Paired")+
  labs(title = "2050년 수소 수입량 제약 시나리오별 수소 생산량",
       x= "2050년 시나리오",
       y= "천톤",
       fill = "구분")+
  theme_minimal()






























