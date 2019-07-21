# Junghye Kim - Final

wiid<-read.csv("~/Data_Viz/dv_final_datasets/WIID_Dec2018.csv")
crime<-read.csv("~/Data_Viz/dv_final_datasets/us_crimes_1860_1920.csv")
sat<-read.csv("~/Data_Viz/dv_final_datasets/sat_2016.csv")
kites<-read.csv("~/Data_Viz/dv_final_datasets/black_kites/black_kites.csv")

library(tidyverse)
library(rgdal)
library(sf)
library(sp)
library(openintro)
library(rvest)

# Plot 1
View(sat)

counties <- map_data("county")
ca_county <- subset(counties, region == "california")
sat <-sat %>% 
  mutate(subregion=tolower(cname),
         avgscrmath=as.numeric(as.character(avgscrmath)),
         avgscrread=as.numeric(as.character(avgscrread)))
county_data<- left_join(ca_county,sat, by="subregion") 

CA_math<-ggplot()+
  geom_polygon(data=county_data, aes(x=long, y=lat, group=group, fill=avgscrmath))+
  coord_map()+
  ylab("Latitude")+
  xlab("Longitude")+
  ggtitle("Average Math SAT Scores")+
  scale_fill_gradient(low="#ffaa00",high="#4106a8", limits=c(450,575), name="Score", na.value = "#ffaa00")+
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(size=8),
        plot.title = element_text(hjust=0),
        legend.text = element_text(size=8),
        panel.border = element_blank(),
        panel.grid=element_blank())

CA_math

# text legend 

CA_read<-ggplot()+
  geom_polygon(data=county_data, aes(x=long, y=lat, group=group, fill=avgscrread))+
  coord_map()+
  ylab("Latitude")+
  xlab("Longitude")+
  ggtitle("Average Reading SAT Scores")+
  labs(caption="Source: California Department of Education")+
  scale_fill_gradient(low="#7cc151",high="#040ac9", limits=c(450,575), name="Score", na.value = "#7cc151")+
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(size=8),
        plot.caption = element_text(hjust=0.5, size=8, vjust=0),
        plot.title = element_text(hjust=0),
        legend.text = element_text(size=8),
        panel.border = element_blank(),
        panel.grid=element_blank())

CA_read

grid.arrange(CA_math, CA_read, nrow=1, top="California 2015-2016 SAT Scores by County")


# plot 2

ivy<-read_html("https://en.wikipedia.org/wiki/Ivy_League") %>% 
  html_nodes("table") %>%
  .[2:2] %>%
  html_table()
ivy<-ivy[[1]]
ivy$Undergraduates<-as.numeric(gsub(",","",ivy$Undergraduates)) # get rid of commas

NCAA<- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/111") %>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
NCAA<-NCAA[[1]]

NCAA2<- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p2") %>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
NCAA2<-NCAA2[[1]]

NCAA3<- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p3") %>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
NCAA3<-NCAA3[[1]]

NCAA4<- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p4") %>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
NCAA4<-NCAA4[[1]]

NCAA4<- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p4") %>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
NCAA4<-NCAA4[[1]]

NCAA5<- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p5") %>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
NCAA5<-NCAA5[[1]]

NCAA6<- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p6") %>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
NCAA6<-NCAA6[[1]]

NCAA7<- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p7") %>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
NCAA7<-NCAA7[[1]]

NCAA_full<-rbind(NCAA, NCAA2, NCAA3, NCAA4, NCAA5, NCAA6, NCAA7)

wins1<-read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/169")%>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
wins1<-wins1[[1]]

wins2<-read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/169/p2")%>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
wins2<-wins2[[1]]

wins3<-read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/169/p3")%>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
wins3<-wins3[[1]]

wins4<-read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/169/p4")%>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
wins4<-wins4[[1]]

wins5<-read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/169/p5")%>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
wins5<-wins5[[1]]

wins6<-read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/169/p6")%>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
wins6<-wins6[[1]]

wins7<-read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/169/p7")%>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
wins7<-wins7[[1]]

wins_full<-rbind(wins1, wins2, wins3, wins4, wins5, wins6, wins7)

wins_pts<-left_join(NCAA_full, wins_full, by="Team") %>% 
  mutate(pts_per_game=PTS/GM)

plot2<-ggplot(wins_pts, aes(x=pts_per_game, y=Pct))+
  geom_point(alpha=0.5, size=2)+
  ylab("Win Percentage")+
  xlab("Offensive Points per Game")+
  ggtitle("NCAA Women's Basketball")+
  labs(caption="Source: https://www.ncaa.com/stats/basketball-women/d1")+
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust=0),
        plot.caption = element_text(size=7),
        panel.border=element_blank(),
        panel.grid=element_blank())

plot2

# plot 3
View(crime)

plot3<-ggplot(crime, aes(x=year, y=arrests/1000, group=city))+
  geom_line(color="#6b6b67")+
  geom_smooth(inherit.aes = FALSE, data=crime,aes(x=year, y=arrests/1000))+
  ylab("Total Arrests (in Thousands)")+
  xlab("Year")+
  labs(title="U.S. Arrests, 1860-1820",
       subtitle = "23 Major Cities, Unadjusted for Population")+
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust=0, face=NULL),
        axis.text=element_text(size=9),
        panel.border = element_blank(),
        panel.grid = element_blank())
plot3

# plot 4
View(wiid)
asia_mean<-mean(wiid$gini_reported[wiid$year==2010 & wiid$region_un=="Asia"], na.rm=TRUE)
am_mean<-mean(wiid$gini_reported[wiid$year==2010 & wiid$region_un=="Americas"], na.rm=TRUE)
wiid_asia<-wiid %>% 
  filter(year==2010, region_un=="Asia") %>% 
  group_by(country) %>% 
  mutate(centered=mean(gini_reported,na.rm=TRUE)-asia_mean)

wiid_am<-wiid %>% 
  filter(year==2010, region_un=="Americas")%>% 
  group_by(country) %>% 
  mutate(centered=mean(gini_reported,na.rm=TRUE)-am_mean)

asia_plot<-ggplot(wiid_asia, aes(x=country, y=centered))+
  geom_bar(stat="identity", fill="#3d7ee5", width = .8, alpha=0.9)+
  ylim(-10,10)+
  coord_flip()+
  xlab("")+
  ylab("")+
  ggtitle("Asian Gini Index Scores")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_text(size=8.5),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=13, hjust=0.5))
asia_plot

am_plot<-ggplot(wiid_am, aes(x=country, y=centered))+
  geom_bar(stat="identity", fill="#1a681b", width = .8, alpha=0.9)+
  ylim(-10,10)+
  coord_flip()+
  xlab("")+
  ylab("")+
  ggtitle("Americas Gini Index Scores")+
  labs(caption = "Source: World Income Inequality Database")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_text(size=8.5),
        plot.caption = element_text(hjust=0.5, size = 9),
        plot.title = element_text(size=13, hjust=0.5),
        panel.grid = element_blank(),
        panel.border = element_blank())
am_plot

grid.arrange(asia_plot, am_plot, nrow=1, top="Centered Gini Index Scores by Region")


# plot 5
library(ggrepel) 

ivy<-read_html("https://en.wikipedia.org/wiki/Ivy_League") %>% 
  html_nodes("table") %>%
  .[2:2] %>%
  html_table()
ivy<-ivy[[1]]

cars<-read_html("https://en.wikipedia.org/wiki/Production_car_speed_record")%>% 
  html_nodes("table") %>%
  .[1:1] %>%
  html_table()
cars<-cars[[1]]
cars<-cars %>% 
  mutate(speed=str_extract_all(`Top speed of production car`, ".+km/h"),
         speed=substr(speed, 1, nchar(speed)-4),
         speed=as.numeric(speed))
plot5<-ggplot(cars, aes(x=Year, y=speed))+
  geom_point(color="blue", alpha=0.8)+
  geom_text_repel(aes(label=`Make and model`))+
  geom_smooth(color="red")+
  ggtitle("Street Legal Car Speed Records")+
  ylab("Speed in Kilometers Per Hour (KPH)")+
  labs(caption="Sources:https://en.wikipedia.org/wiki/Production_car_speed_record")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0),
        axis.text = element_text(size=8.5),
        plot.caption = element_text(size=9.5),
        panel.border = element_blank(),
        panel.grid = element_blank())
plot5

# plot 6

AUS<-readOGR(dsn="~/Data_Viz/dv_final_datasets/aus_spatial_dat", layer="Aussie")
cope<-read.csv("~/Data_Viz/dv_final_datasets/aus_spatial_dat/copepods_standardised.csv")
View(cope)
ggplot()+
  stat_density_2d(geom="polygon",data=cope, aes(fill=..level.., x=longitude, y=latitude), alpha=0.8)+
  scale_fill_gradientn(colors=c("#c1e9ff", "#1a93f2","#0073cc"))+
  geom_sf(data=st_as_sf(AUS), fill="#0e6611", color="#1dba23")+
  coord_sf()+
  ylab("Longitude")+
  xlab("Latitude")+
  labs(title="Australian Copepods",
       subtitle = "Plankton and Crustacean Concentrations")+
  theme_bw()+
  theme(panel.background = element_rect(fill="#c1e9ff"),
        panel.grid.major = element_line(color="#ffffff"),
        plot.title = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0),
        axis.line = element_blank(),
        panel.border = element_blank())

# plot 7
US<-readOGR(dsn="~/Data_Viz/dv_final_datasets/US_Climate", layer="US_Climate")
states<-map_data("state")

june<-ggplot()+
  geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="#c5c6cc", color="#ffffff")+
  geom_sf(data=st_as_sf(US), aes(color=T06))+
  scale_color_gradientn(colors=c("#0922ba","#179316","#fbff26","#f79800"), name="Degrees")+
  ylim(25,50)+
  xlim(-130,-65)+
  labs(title = "U.S. Climate Station Data", 
       subtitle = "June Temperatures")+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = "white"),
        axis.line = element_blank(),
        plot.title = element_text(hjust=0),
        axis.text = element_text(size=8.5),
        plot.subtitle = element_text(hjust=0),
        panel.border = element_blank())
june

january<-ggplot()+
  geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="#c5c6cc", color="#ffffff")+
  geom_sf(data=st_as_sf(US), aes(color=T01) )+
  ylim(25,50)+
  xlim(-130,-65)+
  labs(title = "U.S. Climate Station Data", 
       subtitle = "June Temperatures")+
  scale_color_gradientn(colors=c("#adecf7","#0c0ca0","#1daf22"), name="Degrees")+
  coord_sf()+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = "white"),
        axis.line = element_blank(),
        plot.title = element_text(hjust=0),
        axis.text = element_text(size=8.5),
        plot.subtitle = element_text(hjust=0),
        panel.border = element_blank())
january

grid.arrange(june, january, nrow=1)


