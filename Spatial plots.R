## Data Visualization (GOVT16-QSS17) Spring 2019
## Spatial 
##
## Name: Junghye Kim
## Date: May 9, 2019

library(tidyverse)
library(USAboundaries)
library(rgdal)
library(dplyr)
library(sp)
library(sf)
library(ggspatial)
library(raster)
library(choroplethr)
library(usmap)
library(animation)
library(ggmap)
library(maps)
library(maptools)

theme_junghye<-function (base_size = 12, base_family = "Avenir Book") 
{
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      plot.title = element_text(color = "#1b417c", 
                                face = "italic",
                                size = rel(1.35),
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
      panel.grid.major = element_line(colour = "grey90", size = 0.2), 
      panel.grid.minor = element_blank(),
      
      strip.background = element_rect(fill = "grey80", colour = "grey50", 
                                      size = 0.2))
}
# 1.
# Animating Plots
# Go to Voteview.com and download the entire House Member Ideology file. The final 
# product of this plot should be the following: an animated U.S. map of the median 
# ideology (1st dimension NOMINATE score) in the House for each state from the 
# beginning of the FDR Administration to the most recent full Congress. Adjust to 
# colors that are both theoretically appropriate and visually appealing. Again, you 
# may need some additional code to get gganimate or gg animate to work. In that case, 
# you will need either the three scripts posted or the devtools installation of 
# gganimate. IF your computer is giving you issues, try running the animation on a 
# subset of the data ()
house<-read.csv("~/Data_Viz/HSall_members.csv")
house<-house %>% 
  filter(chamber=="House", congress>=73) %>% 
  group_by(state_abbrev, congress) %>% 
  summarize(median_nom=median(nominate_dim1, na.rm=TRUE))
  

fips<-read.csv("~/Data_Viz/FIPS.csv")
colnames(fips)<-c("state_abbrev", "FIPS", "name")
house<-merge(house, fips, by="state_abbrev", all.x=TRUE)
house<-house %>% 
  mutate(fips=FIPS)

saveGIF({
  for(i in sort(unique(house$congress))){
    data_house<-subset(house, congress==i)
    plot<-plot_usmap(regions="states", data=data_house, values="median_nom")+
      ggtitle(paste("Median DWNominate by State: Congress No.",as.character(i)))+
      scale_fill_gradientn(colors=c("#3748b7","#eedefc","#a80000"), name="DW Nominate Score")+
      theme_junghye()+
      theme(axis.text=element_blank(),
                           axis.ticks = element_blank(),
                           axis.title = element_blank())
    print(plot)
  }
}, movie.name = "median.gif", interval=0.5)



# Mapping Data
# For your first problem, make a map of Vermont and New Hampshire. 
# Add New Hampshire and Vermont cities to the map, including Hanover. Adjust the 
# size of the points according to the population of the cities. Change the colors 
# and adjust all titles. Note: if you use one of the pre-loaded cities functions 
# available, you may see a city in Massachusetts plotted as well.
VT<-map_data("state",region="Vermont")
NH<-map_data("state",region="New Hampshire")

cities<-us_cities()



VT_cities<-cities %>% filter(state_abbr=="VT") %>% mutate(city_name=paste0(city,",VT"))
VT_geocode<-VT_cities$city_name %>% geocode()
VT_points<-cbind(as.data.frame(VT_cities), as.data.frame(VT_geocode))

NH_cities<-cities %>% filter(state_abbr=="NH") %>% mutate(city_name=paste0(city,",NH"))
NH_geocode<-NH_cities$city_name %>% geocode()
NH_points<-cbind(as.data.frame(NH_cities), as.data.frame(NH_geocode))
ggplot()+
  geom_polygon(data=VT, aes(x=long, y=lat), fill=NA, color="#ccf99a")+
  geom_polygon(data=NH, aes(x=long, y=lat), fill=NA, color="#ffc53f")+
  geom_point(data=VT_points, aes(x=lon, y=lat, size=population), color="#ccf99a", alpha=0.6)+
  geom_point(data=NH_points, aes(x=lon, y=lat, size=population), color="#ffc53f", alpha=0.6)+
  coord_map()+
  ggtitle("Cities of NH and VT, by population")+
  theme_junghye()
  
# Let’s take a look at some historical data. Show me a map of the United States just 
# after the Louisiana Purchase. For full points, make the Louisiana Territory its own 
# color. Plot territory labels as well.
data3<-us_boundaries(map_date="1804-01-01")
lp<-data3 %>% filter(abbr_name=="LA Pur")
data3<-data3 %>% filter(abbr_name!="LA Pur")
ggplot()+
  geom_sf(data=data3, mapping=aes(), fill=alpha("#1b407c",0.7))+
  geom_sf(data=lp, mapping=aes(), fill=alpha("#92B4F4",0.7))+
  geom_sf_label(data=data3, aes(label=name), label.size = 0.07, size=2, label.padding = unit(0.1,"lines"),position=position_jitter(width=1,height=1))+
  geom_sf_label(data=lp, aes(label=name), label.size = 0.07,size=2)+
  ggtitle("United States - Post Louisiana Purchase")+
  theme_junghye()+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


# Let’s look at a plot of the Trail of Tears. One way to honor people who have 
# suffered greatly is to know their story. One way we can get to know the plight of
# Native Americans is to make a map of their journey from the East to the Oklahoma 
# Territory. The files I have provided you in a folder are all needed for this plot,
# but you only need to read in one file. Read in the trht...shp file through the 
# readOGR function. Depending on whether you use ggplot2 or tmap to map the trail, 
# you might need to transform the object into another type (data frame, sf, or sp).
map4<-readOGR(dsn="~/Data_Viz/trail", layer="trte_nht_100k_line")
background<-us_boundaries(map_date="1830-01-01")
summary(map4)
ggplot()+
  geom_sf(data=background, fill=alpha("#687ea3", 0.25), lwd=0.1)+
  geom_sf(data=st_as_sf(map4), color="#ad0c05")+
  ggtitle("The Trail of Tears")+
  ylim(c(30,45))+
  xlim(c(-100,-75))+
  theme_junghye()+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# 4. Remember the 2016 election? Any one of a number of swing states could have been
# counted as the difference-maker in the Electoral College delegate count. I have 
# given you a dataset from the MIT Election Data & Science Lab for election results
# by county. Map the data from the winner in each county to counties for the state of
# Florida. Use a red-white-blue “diverging” palette.
elections<-read.csv("~/Data_Viz/presbycounty.csv")
election_2016<-elections %>% 
  filter(year==2016, state_po=="FL", party=="democrat") %>% 
  mutate(subregion=tolower(county)) %>% 
  group_by(subregion) %>% 
  summarize(clinton_vote_share=candidatevotes/totalvotes)
FL<- map_data("county", region="Florida")
counties <- map_data("county")
fl_county <- subset(counties, region == "florida")
county_data<- left_join(fl_county,election_2016, by="subregion")

ggplot()+geom_polygon(data=fl_county, aes(x=long, y=lat, group=group))
FL_map<-ggplot()+
  geom_polygon(data=county_data, aes(x=long, y=lat, group=group, fill=clinton_vote_share))+
  coord_map()+
  scale_fill_gradientn(colors=c("#3748b7","#ffffff","#a80000"), name="Clinton Vote %")+
  ggtitle("2016 Election Results by County, Florida")+
  theme_junghye()+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
FL_map
