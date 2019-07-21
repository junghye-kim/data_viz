## Data Visualization (GOVT16-QSS17) Spring 2019
## Tidyverse
##
## Name: Junghye Kim
## Date: April 25, 2019

library(tidyverse)
library(hexbin)
wiid<-read.csv("~/Data_Viz/WIID.csv")
# 1. Using the wiid dataset, show the proportion of observations in the dataset by UN region 
# (region_un) that are classified as the highest income group in a pie chart. 
# Remove the axes and make the theme minimal. Fix the ugly colors and change the label for the 
# legend.
data1<-wiid %>% 
  filter(incomegroup=="High income") %>% 
  group_by(region_un)
plot1<-ggplot(data1, aes(x=1, fill=region_un))+
  geom_bar()+
  xlab("")+
  ylab("")+
  coord_polar(theta="y")+
  theme_minimal()+
  scale_fill_brewer(palette="Blues", name="UN Region")+
  theme(axis.ticks.x = element_blank(),
        axis.text = element_blank())+
  ggtitle("High Income Countries by UN Region")
  
plot1
# 2. Do the same, but show me a pie chart of the low income groups by region. This pie chart
# should look remarkably different from the previous.
data2<- wiid %>% 
  filter(incomegroup=="Low income") %>% 
  group_by(region_un)

plot2<-ggplot(data2, aes(x=1, fill=region_un))+
  geom_bar()+
  xlab("")+
  ylab("")+
  coord_polar(theta="y")+
  theme_minimal()+
  scale_fill_brewer(palette="Blues", name="UN Region")+
  theme(axis.ticks.x = element_blank(),
        axis.text = element_blank())+
  ggtitle("Low Income Countries by UN Region")
plot2
# 3. Create a subset of European countries called nordic that take the Nordic coun- tries
# (Denmark, Finland, Iceland, Norway, and Sweden) and plot their mean Gini index values over 
# time. Add a smoother and adjust its sensitivity, reduc- ing the span argument to 0.6. 
# Adjust the background theme to dark. Change the colors so that you can see them better 
# given the new background theme. There seems to be one country that is a bit of an outlier; 
# make sure your col- ors reflect that by highlighting the outlier country relative to the 
# other Nordic countries. Adjust all labels.

nordic<-wiid %>% 
  filter(country %in% c("Denmark", "Finland", "Iceland","Norway", "Sweden")) %>% 
  group_by(country, year) %>% 
  summarize(mean_gini=mean(gini_reported, na.rm=TRUE))

plot3<-ggplot(nordic, aes(x=year, y=mean_gini, color=country))+
  geom_line()+
  geom_smooth(span=0.6)+
  theme_dark()+
  scale_color_manual(values=c("#f2c2bc","#bbe2f1","#e2f0ba", "#eec7f9", "#2207f2"), name="Nordic Country")+
  ylab("Mean Gini Index")+
  xlab("Year")+
  ggtitle("Gini Index over Time, Nordic Countries")
plot3
# 4. Do the same plot as above but for the “Southern Cone” countries including Brazil. This 
# time I would like you to create your own theme from scratch. Ad- just legend specifications,
#background (even the color), labels, axes (including ticks, etc.). Save it as its own object 
#(named something like theme_studentname) before you plot and then simply add theme_studentname 
# at the end of the plot.
data4<- wiid %>% 
  filter(country %in% c("Argentina", "Chile", "Brazil", "Paraguay", "Uruguay")) %>% 
  group_by(country, year) %>% 
  summarize(mean_gini=mean(gini_reported, na.rm=TRUE))

theme_junghye<-function (base_size = 12, base_family = "Georgia") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8), color="#cecece"), 
          axis.ticks = element_line(colour = "#cecece"), 
          legend.key = element_rect(colour = "white"), 
          panel.background = element_rect(fill = "#efefef", colour = NA), 
          panel.border = element_blank(), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80", colour = "grey50", 
                                          size = 0.2))
}

plot4<-ggplot(data4, aes(x=year, y=mean_gini, color=country))+
  geom_line(alpha=0.6)+
  geom_smooth(span=0.6)+
  theme_dark()+
  ylab("Mean Gini Index")+
  xlab("Year")+
  ggtitle("Gini Index over Time, Southern Cone Countries")+
  scale_color_brewer(name="Southern Cone \nCountries", palette="YlOrRd")+
  theme_junghye()
plot4
# 5. I am curious about whether countries with lower GDPPC have higher or lower Gini index values
# across the history of these estimates. If I were you, I would probably log the income-based 
# variable GDPPC before I tried to plot it. Also, try a new geom called geom_hex. Change 
# the number of bins to a much higher number than the default, something like 50. 
# Change colors (using color hex codes) to your own three-color scale in an attempt to highlight 
# the variation. Change all labels to look nice.
plot5<-ggplot(wiid,(aes(x=log(gdp_ppp_pc_usd2011), y=gini_reported)))+
  geom_hex(bins=50)+
  scale_fill_gradientn(colors=c("#ede2ff","#d3bafc","#3b1577"), na.value = "white", name="Count")+
  xlab("GDPPC Values")+
  ylab("Gini Index")+
  ggtitle("Gini Index vs. GDPPC Values")+
  theme_bw()
plot5

# 6. I am curious about how each region looks as compared to the other regarding Gini index 
# scores. Facet a plot of individual Gini index scores by UN region. Cut out the years where 
# only a few countries have observations; perhaps start around 1940. Change all labels to look 
# nice and professional. Use the classic theme this time. Here’s a slightly tricky addition: 
# add ALL the points to each facet in a light gray color. Adjust the regional colors so they 
# stand out a bit from the gray.
data6<- wiid %>% 
  filter(year>=1940) 
#inherit.aes=false 

plot6<-ggplot(data6, aes(y=gini_reported, x=year,color=region_un))+
  geom_point(data=wiid[,c("gini_reported", "year")], aes(y=gini_reported, x=year),color="grey", alpha=0.5, size=0.3)+
  geom_point(alpha=0.5, size=0.3)+
  facet_grid(.~region_un)+
  xlim(c(1940,2017))+
  xlab("Year")+
  ylab("Gini Index")+
  ggtitle("Gini Index by Region, 1940-2017")+
  labs(color=guide_legend("UN Region"))+
  theme_classic()

plot6
# 7. Repeat the plot from above, but this time facet the data by both income group and UN
# region. Make sure the income groups are in the correct order, from high to low.
wiid$ordered_income<-ordered(wiid$incomegroup, levels=c("High income", "Upper middle income", "Lower middle income","Low income"))
data7<- wiid %>% 
  filter(year>=1940) 
#inherit.aes=false 

plot7<-ggplot(data7, aes(y=gini_reported, x=year,color=region_un))+
  geom_point(data=wiid[,c("gini_reported", "year")], aes(y=gini_reported, x=year),color="grey", alpha=0.5, size=0.3)+
  geom_point(alpha=0.5, size=0.3)+
  facet_grid(ordered_income~region_un)+
  xlim(c(1940,2017))+
  xlab("Year")+
  ylab("Gini Index")+
  ggtitle("Gini Index by Region, 1940-2017")+
  labs(color=guide_legend("UN Region"))+
  theme_classic()
plot7
