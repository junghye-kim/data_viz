## Data Visualization (GOVT16-QSS17) Spring 2019
## Tidyverse
##
## Name: Junghye Kim
## Date: May 2, 2019

library(tidyverse)
library(ggtern)
library(cowplot)
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

# 1. Using the WIID dataset,create a combination density-histogram of the gini_reported. Add a gray “rug” to the 
# plot.  Provide a title and change colors so that it presents nicely. I want to see the separate bins of 
# the histogram, but subtly, and I want the bins to be filled with a good color.
wiid<-read.csv("~/Data_Viz/WIID.csv")
plot1<-ggplot(wiid, aes(x=gini_reported))+
  geom_histogram(aes(y=..density..),fill=alpha("#b0befc", 0.75), color="#b0befc")+
  geom_density(color="#132577")+
  geom_rug(colour = "#9ea0a3")+
  xlab("Gini Index")+
  ylab("Density")+
  ggtitle("WIID Gini Indeces (Density)")+
  theme_junghye()
plot1
# 2. This problem is an exercise in comparing visualizations. First, I want you to simply go into the wiid
# dataset and count the observations by incomegroup. Now, go make a histogram of gini_reported that distinguishes
# incomegroup with a fill aesthetic. We have done this before, and it isn’t quite right. If you lower the 
# alpha, you’ll see that the groups don’t overlap; they are being stacked. Now, instead, make a group of 
# overlapping density plots of gini_reported with a low alpha. Does this look like it accurately represents 
# the data from the count? Not to me. Adjust the data to account for the size of each group, making larger
# densities larger and smaller densities smaller using a weight aesthetic. This is the final plot for this 
# problem. DO EACH STEP.
wiid$incomegroup<-ordered(wiid$incomegroup, levels=c("High income", "Upper middle income", "Lower middle income", "Low income"))
plot2a<-ggplot(wiid, aes(x=gini_reported))+
  geom_histogram(aes(y=..density.., fill=incomegroup), color="#b0befc", lwd=0.1, alpha=0.75)+
  xlab("Gini Index")+
  ylab("Density")+
  ggtitle("WIID Gini Indeces by Income Group (Density)")+
  scale_fill_manual(values=c("#1b417c","#94958B","#B7B6C1", "#D5CFE1"), name="Income Group")+
  theme_junghye()
plot2b<-ggplot(wiid, aes(x=gini_reported, group=incomegroup))+
  geom_density(aes(fill=incomegroup), color="#b0befc", lwd=0.1, alpha=0.5)+
  xlab("Gini Index")+
  ylab("Density")+
  ggtitle("WIID Gini Indeces by Income Group (Density)")+
  scale_fill_manual(values=c("#1b417c","#94958B","#B7B6C1", "#D5CFE1"), name="Income Group")+
  theme_junghye()
plot2a
plot2b

wiid2<- wiid %>% 
  group_by(incomegroup) %>% 
  mutate(weight_counts=n()/nrow(wiid))

plot2c<-ggplot(wiid2, aes(x=gini_reported, group=incomegroup,weight=weight_counts))+
  geom_density(aes(fill=incomegroup ), color="#b0befc", lwd=0.1, alpha=0.5)+
  xlab("Gini Index")+
  ylab("Density")+
  ggtitle("WIID Gini Indeces by Income Group (Density)")+
  scale_fill_manual(values=c("#1b417c","#94958B","#B7B6C1", "#D5CFE1"), name="Income Group")+
  theme_junghye()
plot2c
# 3. Create a two-dimensional density plot of the Gini index scores and GDP per capita of countries. Make
# the fill the levels of the density plot. Add the points themselves in gray. Create your own color scale
# (I suggest three colors) with which to fill in the density levels. Don’t color the lines; color the area
# between the density lines. Change all labels to look nice.
plot3<- ggplot(wiid, aes(x=gini_reported, y=gdp_ppp_pc_usd2011))+
  geom_point(color="#9ea0a3", size=0.5, alpha=0.5)+
  stat_density_2d(geom="polygon", aes(fill=..level..), alpha=0.8)+
  xlab("Gini Index")+
  ylab("GDP Per Capita")+
  scale_fill_gradientn(colors=c("#ede2ff","#d3bafc","#3b1577"), name="Density Level")+
  ggtitle("Gini Index vs. GDP Per Capita (Density)")+
  theme_junghye()
plot3
# 4. Ternary plots are most useful when they are comparing variables that add up to 1 (in other words, 
# most often when they split up a variable into parts, especially categorical variables). Go back to the
# WIID dataset. Recode the income decile variables into low_d, middle_d, and high_d, where low_d is 
# deciles 1-3, mid_d is deciles 4-7, and high.is deciles 8-10. Then recode the deciles again, calling them 
# low2_d, mid2_d, and high_d, where they are deciles 1-6, 7-9, and 10, respectively. Plot two density 
#ternary plots side by side comparing the groupings using the gridExtra package. Make all labels nice. 
#Add color to the points and make them somewhat transparent. Make appropriate titles. This plot should 
# be depressing.
data4a<-wiid %>% 
  group_by(id) %>% 
  mutate(low_d=sum(d1,d2,d3),
         middle_d=sum(d4,d5,d6,d7),
         high_d=sum(d8,d9,d10))
data4b<-wiid %>% 
  group_by(id) %>% 
  mutate( low_2d=sum(d1,d2,d3,d4,d5,d6),
          mid_2d=sum(d7,d8,d9),
          high_2d=sum(d10))
        
 plot4a<-ggtern(data4a,aes(low_d, middle_d, high_d) )+
   geom_point(color="#9ea0a3", alpha=0.4, size=0.2)+
   geom_density_tern(alpha=0.7)+
   ggtitle("Ternary Plots by Decile Groupings")+
   Tlab("Middle Deciles")+
   Llab("Lower Deciles")+
   Rlab("High Deciles")+
   theme(axis.title = element_blank())+
   theme_junghye()
plot4a

plot4b<-ggtern(data4b,aes(low_2d, mid_2d, high_2d) )+
  geom_point(color="#9ea0a3", alpha=0.4, size=0.2)+
  geom_density_tern(alpha=0.7)+
  Tlab("Middle Deciles")+
  Llab("Lower Deciles")+
  Rlab("High Deciles")+
  theme(axis.title = element_blank())+
  theme_junghye()
  
plot4b

grid.arrange(plot4a, plot4b, nrow=1)

# 5. Make a violin plot that shows Africa’s Gini Index values by UN subregions.
data5<-wiid %>% 
  filter(region_un=="Africa") %>% 
  group_by(region_un_sub)

plot5<- ggplot(data5, aes(x=region_un_sub, y=gini_reported, fill=region_un_sub))+
  geom_violin(color=NA, alpha=0.8)+
  scale_fill_manual(values=c("#33658A","#86BBD8", "#2F4858", "#F6AE2D","#C94226"))+
  xlab("UN Sub Region")+
  ylab("Gini Index")+
  ggtitle("Gini Index by UN Subregion, Africa")+
  theme_junghye()+
  theme(legend.position = "none")
plot5
