## Data Visualization (GOVT16-QSS17) Spring 2019
## Web Data and Scraping in R
##
## Name: Robert Cooper
## Date: May 21, 2019

library(tidyverse)
library(rvest)
library(usmap)
library(likert)
library(grid)
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
      plot.margin=unit(c(1,1,1,1),"cm"),
      
      strip.background = element_rect(fill = "grey80", colour = "grey50", 
                                      size = 0.2))
}
# Scraping (and Plotting) Data from the Web

# Go to the Wikipedia page for the Ivy League. Scrape the first table of data, which includes the name of 
# the school, its location, its nickname/mascot, and its undergraduate enrollment, among other things. I 
# want you to scrape these data and put them into a data frame. Then make a bar plot of the undergraduate
# enrollment of each school. Make the color of each bar match the school’s colors.
ivy<-read_html("https://en.wikipedia.org/wiki/Ivy_League") %>% 
  html_nodes("table") %>% 
  .[2:2] %>% 
  html_table()
ivy<-ivy[[1]] 
ivy$Undergraduates<-as.numeric(gsub(",","",ivy$Undergraduates)) # COMMAS 
ivy_plot<- ggplot(ivy, aes(x=Institution, y=Undergraduates, fill=Institution))+
  geom_col(alpha=0.8)+
  theme_junghye()+
  ggtitle("Undergraduates by Ivy League Instituion")+
  scale_fill_manual(values=c("#63451a","#b1d3ef","#aa382e","#427a40","#7a0f03","#f7b259","#32346b","#1a1b47"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
ivy_plot

# Next, go to the Wikipedia page for the highest grossing films of all time. Scrape the first table, which 
# has the top 50 films, their worldwide gross earnings, year, etc. Make a simple scatterplot of the top 50 
# films where the x-axis is the year of the film and the y-axis is the world gross earnings. Clean up all 
# necessary plotting elements, titles, etc. If I were you, I would also transform the y-axis scale.
movies<-read_html("https://en.wikipedia.org/wiki/List_of_highest-grossing_films") %>% 
  html_nodes("table") %>% 
  .[1:1] %>% 
  html_table()
movies<-movies[[1]]
movies$`Worldwide gross`<-movies$`Worldwide gross` %>% 
  str_remove_all(',') %>% 
  substr(2,nchar(movies$`Worldwide gross`)) %>% 
  as.numeric()

movie_plot<-ggplot(movies, aes(x=Year, y=log(`Worldwide gross`)))+
  geom_point()+
  ylab("Worldwide Gross-Log Scale")+
  ggtitle("Top 50 Highest Grossing Films over Time")+
  theme_junghye()
movie_plot


# Go to the Wikipedia page of the Global Fortune 500. Make a bar plot for these companies. This time, 
# highlight the petroleum companies on the list with one color. Change titles and themes.
fortune<-read_html("https://en.wikipedia.org/wiki/Fortune_Global_500") %>% 
  html_nodes("table") %>% 
  .[1:1] %>% 
  html_table()
fortune<-fortune[[1]] %>%
  tbl_df() %>% 
  mutate(`Revenue in USD`=substr(`Revenue in USD`,2,5),
         petroleum=ifelse(Industry=='Petroleum',1,0)) 

fortune_plot<-ggplot(fortune, aes(x=Company, y=`Revenue in USD`, fill=petroleum))+
  geom_col(alpha=0.9)+
  ylab("Revenue in USD, Billions")+
  ggtitle("Revenue of Top 10 Fortune 500 Companies")+
  scale_fill_continuous(low="#ffe0bc",high="#4F5D75")+
  theme_junghye()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
fortune_plot

# Go to Wikipedia and look up the “List of U.S. States by Carbon Emissions.” Scrape the table and recreate
# the map you see in the top right of the page. If you want to do it exactly, you’ll need the fiftystater
# package to inset Alaska and Hawaii. Also, add a title and change the color scheme.
carbon<-read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_carbon_dioxide_emissions") %>% 
  html_nodes("table") %>% 
  .[1:1] %>% 
  html_table
carbon<-carbon[[1]]
carbon<-carbon[-c(1),]
colnames(carbon)<-c("Rank", "state","C02_emissions", "Percent_of_all_emissions","Population","Percent_of_total_pop","Carbon_per_cap")
carbon<-carbon %>% 
  tbl_df() %>% 
  mutate(carbon_factor=cut(Carbon_per_cap, breaks=c(-Inf, 10,15,20,25,30,50,Inf), labels=c("5-10","10-15","15-20","20-25","25-30","30-50","50+")),
         carbon_factor=reverse.levels(carbon_factor))


carbon_plot<-plot_usmap(regions = "states", data=carbon, values="carbon_factor")+
  scale_fill_manual(values=c("#2C0204","#640206","#95060C","#CA0813","#F41018","#FB6867","#FFCBCC"))+
  theme(legend.position = c(1,.3),
        legend.title = element_blank())
carbon_plot
grid.text("C02 emission per capita per state \nin 2011.",
          x =.8,
          y = .24,
          just = "left",
          gp=gpar(fontsize=6,
                  col="#000000"))  
grid.text("Millions of metric tons",
          x =.9,
          y = .3,
          just = "left",
          gp=gpar(fontsize=4.5,
                  col="#000000"))  

