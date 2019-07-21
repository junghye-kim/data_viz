# Data Viz Final Lab

library(tidyverse)
library(cowplot)
library(ggforce)
library(grid)
# Junghye Kim Theme 

theme_junghye<-function (base_size = 12, base_family = "Avenir Book") 
{
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      plot.title = element_text(color = "#1b417c", 
                                face = "italic",
                                size = rel(1.5),
                                hjust = 0,
                                margin=margin(t = 0, r = 0, b = 15, l = 0)),
      
      axis.text = element_text(size = rel(0.6), color="#9ea0a3"), 
      axis.text.y=element_text(margin=margin(t = 0, r = 10, b = 0, l = 0)),
      axis.ticks = element_line(colour = "#cecece"), 
      axis.title = element_text(color="#1b417c", 
                                size= rel(0.8)),
      
      legend.key = element_rect(colour = "white"), 
      legend.title = element_text(color="#1b417c",
                                  face = "italic",
                                  size= rel(0.85),
                                  margin=margin(t = 0, r = 0, b = 5, l = 0)),
      legend.text = element_text(color="#1b417c", 
                                 size= rel(0.6)),
      
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border = element_blank(), 
      panel.grid.major = element_line(colour = "#ededed", size = 0.2), 
      panel.grid.minor = element_blank() #,
      # 
      # strip.background = element_rect(fill = "grey80", colour = "grey50", 
      #                                 size = 0.2)
  )
}
measles<-read.csv("~/Data_Viz/measles.csv")
measles_post2002<-read.csv("~/Data_Viz/measles_post2002.csv")
str(measles)
View(measles)

ggplot(measles, aes(x=CountValue))+
  geom_histogram(bins=50)+
  xlim(0,10000)+
  ylim(0,5000)

range(measles$CountValue)

measles<-measles %>% 
  mutate(year=as.numeric(substr(PeriodStartDate, 1,4))) %>% 
  group_by(year) %>% 
  summarize(year_cases=sum(CountValue, na.rm=TRUE))

measles_post2002<-measles_post2002 %>% 
  filter(Years>=2002)
colnames(measles_post2002)<-c("year","year_cases")
measles_data<-rbind(measles, measles_post2002)
measles_data$year<-as.numeric(measles_data$year)
measles_data$year_cases<-as.numeric(measles_data$year_cases)
measles_data<-measles_data %>% 
  filter(year>=1890)

mumps<-read.csv("~/Data_Viz/mumps.csv")
View(mumps)
mumps<-mumps %>% 
  mutate(year=as.numeric(substr(PeriodStartDate, 1,4))) %>% 
  group_by(year) %>% 
  summarize(year_cases=sum(CountValue, na.rm=TRUE)) %>% 
  filter(year>=1967)

rubella<-read.csv("~/Data_Viz/rubella.csv")
View(rubella)
rubella<-rubella %>% 
  mutate(year=as.numeric(substr(PeriodStartDate, 1,4))) %>% 
  group_by(year) %>% 
  summarize(year_cases=sum(CountValue, na.rm=TRUE))

polio<-read.csv("~/Data_Viz/polio.csv") 
View(polio)
polio<- polio %>% 
  mutate(year=as.numeric(substr(PeriodStartDate, 1,4))) %>% 
  group_by(year) %>% 
  summarize(year_cases=sum(CountValue, na.rm=TRUE))
polio2_years<-c(1972:1979)
polio2_cases<-c(10,10,10,10,10,10,10,3)
polio2<-data.frame(polio2_years, polio2_cases)
colnames(polio2)<-c('year',"year_cases")
polio<-rbind(polio, polio2)

years<-c(seq(1890,2020, 10))
label_angle<-c(-5,-35,-65,-90, 60, 40, 15,-15,-40,-70,85, 60, 35, 2 )
label_df<-data.frame(years, label_angle)

disease_plot<- ggplot()+
  # disease cases
  geom_point(data=measles_data, aes(x=year, y=10, size=year_cases), alpha=0.4, color="#A01A7D")+
  geom_point(data=mumps, aes(x=year, y=.25, size=year_cases), alpha=0.4, color="#86BBD8")+
  geom_point(data=rubella, aes(x=year, y=4, size=year_cases), alpha=0.5, color="#F6AE2D")+
  geom_point(data=polio, aes(x=year, y=7, size=year_cases), alpha=0.5, color="#F26419")+
  
  #scaling 
  scale_x_continuous(breaks=seq(1890,2020, 10), expand = c(0,0), limits = c(1888,2021))+
  scale_y_continuous(expand = c(0,0),limits=c(-5,19))+
  scale_size(range=c(1,20))+
  theme_junghye()+
  
  # vaccines lines 
  geom_segment(aes(x = 1963,xend=1963, y=-5, yend=15), lwd=1, color="#A01A7D", alpha=0.5)+
  geom_segment(aes(x = 1967,xend=1967, y=-5, yend=15), lwd=1, color="#F6AE2D", alpha=0.5)+
  geom_segment(aes(x = 1969,xend=1969, y=-5, yend=15), lwd=1, color="#86BBD8", alpha=0.5)+
  geom_segment(aes(x = 1955,xend=1955, y=-5, yend=15), lwd=1, color="#F26419", alpha=0.5)+

  # missing data, label band
  geom_segment(aes(x=1890, xend=1967, y=1, yend=1), color="#264270", lwd=1, alpha=0.15)+
  geom_segment(aes(x=2001.5, xend=2014.5, y=4, yend=4), color="#264270", lwd=1, alpha=0.15)+
  geom_segment(aes(x=1890, xend=1965.5, y=4, yend=4), color="#264270", lwd=1, alpha=0.15)+
  geom_segment(aes(x=1890, xend=1911.5, y=7, yend=7), color="#264270", lwd=1, alpha=0.15)+
  geom_segment(aes(x=1888, xend=2021, y=13, yend=13), color="#264270", lwd=6, alpha=.95)+
  geom_text(data=label_df, aes(x=years, label=years, angle=label_angle), y=13, color="#fffdf4", size=2.5, vjust=0.5, hjust=0.5, family="Avenir Book")+
  
  #text annotations
  geom_text(aes(x=1963, y=16, label="1963: Measles \n vaccine introduced", angle=66), size=2.4, hjust=1, color="#A01A7D", alpha=0.5)+
  geom_text(aes(x=1967, y=16, label="1967: Rubella \n vaccine introduced", angle=60), size=2.4, hjust=1, color="#F6AE2D", alpha=0.85)+
  geom_text(aes(x=1970, y=16, label="1969: Mumps \n vaccine introduced", angle=51), size=2.4, hjust=1, color="#86BBD8", alpha=0.7)+
  geom_text(aes(x=1955, y=16, label="1955: Polio \n vaccine introduced", angle=90), size=2.4, hjust=1, color="#F26419", alpha=0.7)+
  geom_segment(aes(x = 1912,xend=1912, y=10, yend=18), lwd=.4, color="#b7b7b7", alpha=0.5)+
  geom_segment(aes(x = 1989,xend=1989, y=10, yend=19), lwd=.4, color="#b7b7b7", alpha=0.5)+
  geom_segment(aes(x = 2000,xend=2000, y=10, yend=19), lwd=.4, color="#b7b7b7", alpha=0.5)+
  geom_segment(aes(x = 2006,xend=2006, y=.25, yend=19), lwd=.4, color="#b7b7b7", alpha=0.5)+
  geom_segment(aes(x = 2019,xend=2019, y=.25, yend=16.5), lwd=.4, color="#b7b7b7", alpha=0.5)+
  geom_segment(aes(x = 2004,xend=2004, y=4, yend=19), lwd=.4, color="#b7b7b7",alpha=0.5)+
  geom_segment(aes(x = 1979,xend=1979, y=7, yend=19), lwd=.4, color="#b7b7b7",alpha=0.5)+
  geom_segment(aes(x = 1940,xend=1940, y=7, yend=19), lwd=.4, color="#b7b7b7",alpha=0.5)+
  geom_segment(aes(x = 2019,xend=2019, y=7, yend=10), lwd=.4, color="#b7b7b7",alpha=0.5)+
  
  # design
  coord_polar()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y= element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill="#fffdf4"),
        panel.spacing = unit(2, "cm"),
        plot.background = element_rect(fill="#fffdf4", color = NA),
        plot.subtitle = element_text(size=rel(0.75), color="#1b417c", vjust=3),
        plot.margin=unit(c(1, 3.5, 3, 3.5),"cm"))
disease_plot

disease_legend<- ggplot()+
  geom_point(aes(y=1.2,x=.4), alpha=0.6, color="#A01A7D", size=5)+
  geom_text(aes(y=1.2,x=.4, label="100"), alpha=1, color="#A01A7D", size=2, family="Avenir Book")+
  geom_text(aes(y=.7,x=.4, label="Measles"), alpha=0.4, color="#A01A7D", size=3, family="Avenir Book")+
  geom_point(aes(x=1.25, y=1.2), alpha=0.6, color="#86BBD8", size=9)+
  geom_text(aes(x=1.25, y=1.2, label="15k"), alpha=1, color="#7bacc6", size=2)+
  geom_text(aes(x=1.25, y=.7, label="Mumps"), alpha=0.6, color="#86BBD8", size=3, family="Avenir Book")+
  geom_point(aes(x=2.37,y=1.2), alpha=0.7, color="#F6AE2D", size=13)+
  geom_text(aes(x=2.37,y=1.2, label="70k"), alpha=1, color="#ce9325", size=2, family="Avenir Book")+
  geom_text(aes(x=2.37, y=.7, label="Rubella"), alpha=0.5, color="#F6AE2D",size=3, family="Avenir Book")+
  geom_point(aes(x=3.65, y=1.2), alpha=0.7, color="#F26419", size=18)+
  geom_text(aes(x=3.65, y=1.2, label="3m"), alpha=1, color="#bf5015", size=2, family="Avenir Book")+
  geom_text(aes(x=3.65, y=.68, label="Polio"), alpha=0.5, color="#F26419", size=3, family="Avenir Book")+
  geom_text(aes(x=2.1, y=1.95, label="Circle size corresponds to the number of annual cases"),size=2.5, family="Avenir Book", color="#a3a3a3", alpha=1)+
  geom_segment(aes(x=0.4, xend=3.8, y=1.75, yend=1.75), arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), color="#b5b5b5", alpha=0.4)+
  ylim(.6,2)+
  xlim(0,4)+
  coord_fixed()+
  theme_junghye()+
  theme(plot.background = element_rect(fill="#fffdf4", color = NA),
        panel.background =element_rect(fill="#fffdf4", color = NA),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())
disease_legend

ggdraw() +
  draw_plot(disease_plot) +
  draw_plot(disease_legend, x = .22, y = -.43, scale = 0.25)

#annotations
grid.text("1912: Measles becomes a nationally \nnotifiable disease. In the next decade, an \naverage of 6k measles related deaths \nwere reported each year.",
          x = .83,
          y = .68,
          just = "right",
          gp=gpar(fontsize=7,
                  col="#848383",
                  alpha=0.8,
                  font="Avenir Book"))
grid.text("1989: A measles outbreak among \nschool children prompts the \nrecommendation for a second dose \nof the vaccines for all children.",
          x = .17,
          y = .57,
          just = "left",
          gp=gpar(fontsize=7,
                  col="#848383",
                  alpha=0.8,
                  font="Avenir Book"))
grid.text("2000: Measles are declared officially \neliminated from the US.",
          x = .2,
          y = .73,
          just = "left",
          gp=gpar(fontsize=7,
                  col="#848383",
                  alpha=0.8,
                  font="Avenir Book"))
grid.text("2006: US Mumps cases begin to increase \nand outbreaks occur more frequently.",
          x = .24,
          y = .83,
          just = "left",
          gp=gpar(fontsize=7,
                  col="#848383",
                  alpha=0.8,
                  font="Avenir Book"))
grid.text("Present: Mumps are not yet \nconsidered eliminated.",
          x = .48,
          y = .86,
          just = "center",
          gp=gpar(fontsize=7,
                  col="#848383",
                  alpha=0.85,
                  font="Avenir Book"))
grid.text("2004: Rubella is eliminated from the US.",
          x = .284,
          y = .775,
          just = "center",
          gp=gpar(fontsize=7,
                  col="#848383",
                  alpha=0.8,
                  font="Avenir Book"))
grid.text("1979: Since 1979, no new cases of polio \nhave originated in the US.",
          x =.17,
          y = .42,
          just = "left",
          gp=gpar(fontsize=7,
                  col="#848383",
                  alpha=0.8,
                  font="Avenir Book"))
grid.text("1940: In the 1940s, polio outbreaks caused more \nthan 15k cases of paralysis a year.",
          x =.81,
          y = .29,
          just = "right",
          gp=gpar(fontsize=7,
                  col="#848383",
                  alpha=0.8,
                  font="Avenir Book")) 
# titles
grid.text("Infectious Diseases in the US",
          x =.18,
          y = .96,
          just = "left",
          gp=gpar(fontsize=20,
                  col="#1b417c",
                  fontface="italic",
                  alpha=0.85)) 
grid.text("A history of disease and vaccination from 1888 to the present.* Grey bands represent missing or unavailable data.",
          x =.18,
          y = .93,
          just = "left",
          gp=gpar(fontsize=8,
                  col="#1b417c",
                  fontface="italic",
                  alpha=0.85))
grid.text("Sources:\nThe Center for Disease Control, Disease History pages, University of Pittsburgh Tycho Institute Datasets, The World Health Organization",
          x =.17,
          y = .03,
          just = "left",
          gp=gpar(fontsize=6,
                  col="#848383",
                  fontface="italic",
                  alpha=0.55))

