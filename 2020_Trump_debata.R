setwd("C:/Users/User/Documents/2019 Datafit/2019 Blogovi i kolumne/2020_Trump_debata")
library(tidyverse)
library(rvest)
library(stringi)
library(hms)
library(naniar)
library(patchwork)
library(ggdark)
library(rtrek)
library(janitor)
library(scales)


debata<-"https://www.rev.com/blog/transcripts/donald-trump-joe-biden-1st-presidential-debate-transcript-2020"
d2<-read_html(debata) %>% 
  html_nodes(css = "#transcription > div:nth-child(1)") %>% 
  html_text()
d2

crosstalk<-stri_split_lines1(d2) %>% 
  stri_extract_all(d2, regex = "crosstalk|\\d\\d\\:\\d\\d\\:\\d\\d|\\d\\d\\:\\d\\d") %>% 
  as.data.frame() %>% 
  select(-V3,-V5) %>% 
  rename(Pitanje=V1,
         Ctalk1=V2,
         Ctalk2=V4,
         Ctalk3=V6) %>% 
  mutate(Ctalk1=str_replace(Ctalk1, "00:00:22","00:22:07"),
         Dio=c(rep("P1",times=361),rep("P2",1227)),
         Pitanje=paste0("00:",Pitanje)) %>% 
  replace_with_na(replace = list(Pitanje="crosstalk")) %>% 
  replace_with_na(replace = list(Pitanje="00:NA")) %>% 
  replace_with_na(replace = list(Pitanje="00:crosstalk"))

crosstalk2<-crosstalk %>% 
  slice(1455:1588) %>% 
  mutate(Pitanje=stri_sub(Pitanje,4,11))

crosstalk3<-rbind.data.frame(crosstalk, crosstalk2) %>% 
            slice(-(1455:1588)) %>%
  pivot_longer(cols = c(Ctalk1, Ctalk2, Ctalk3), names_to="Crosstalk", values_to="Ctalk_Vrijeme") %>% 
  mutate(Vrijeme=paste0(Pitanje, Ctalk_Vrijeme),
         Vrijeme=str_replace(Vrijeme, "NA",""),
         ctalk=ifelse( !is.na(Ctalk_Vrijeme),1,0)) %>% 
         replace_with_na(replace = list(Vrijeme="NA")) %>% 
  select(-Crosstalk) %>% 
  mutate(Pitanje=as_hms(Pitanje),
         Vrijeme=as_hms(Vrijeme),
         Ctalk_Vrijeme=as_hms(Ctalk_Vrijeme),
         Dio=as.factor(Dio),
         ctalk=as.numeric(ctalk))

prvi_dio_debate<-crosstalk3 %>% 
  filter(Dio=="P1") %>% 
  ggplot(aes(x=Vrijeme,y=ctalk))+
  geom_line()+
  labs(title = "Crosstalks between Trump and Biden at presidential debate",
       subtitle = "First part of the presidential debate held on September 29, 2020.",
       x = "Duration of the debate",
       y = "Crosstalks",
       caption = "Each peak denotes a crosstalk")+
  scale_y_continuous(labels = NULL)+
  theme(axis.ticks.y.left = element_blank(),
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y.left = element_text(size = 30),
        plot.caption = element_text(size = 20),
        axis.text.x.bottom = element_text(size = 20))
  
prvi_dio_debate

drugi_dio_debate<-crosstalk3 %>% 
  filter(Dio=="P2") %>% 
  ggplot(aes(x=Vrijeme,y=ctalk))+
  geom_line()+
  labs(title = "Crosstalks between Trump and Biden at presidential debate",
       subtitle = "Second part of the presidential debate held on September 29, 2020.",
       x = "Duration of the debate",
       y = "Crosstalks",
       caption = "Each peak denotes a crosstalk")+
  scale_y_continuous(labels = NULL)+
  theme(axis.ticks.y.left = element_blank(),
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y.left = element_text(size = 30),
        plot.caption = element_text(size = 20),
        axis.text.x.bottom = element_text(size = 20))

drugi_dio_debate

prvi_dio_debate/drugi_dio_debate
