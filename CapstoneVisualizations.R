
library(maps)
library(tidyverse)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(extrafont)
loadfonts(device = "win")

# Capstone Visualizations 

data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Johns Hopkins /Capstone/Data Files/WomensHealth5.0.csv") %>% as.data.frame() 
data <- subset(data, select = -c(X))

maternmort_noNA <- data %>% drop_na(maternmort) # remove NA from maternal mortality

######## Control Vars ##########

poorhealth_bar <- ggplot(data, aes(x = reorder(state, - poorhealth), y = poorhealth, fill = factor(implemented))) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("#CC6666", "#9999CC")) +
  coord_flip() +
  ggtitle("Percentage of Women Reporting Fair/Poor Health by State")

uninsured_bar <- ggplot(data, aes(x = reorder(state, - uninsured), y = uninsured, fill = factor(implemented))) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("#CC6666", "#9999CC")) +
  coord_flip() +
  ggtitle("Percentage of Women 19-64 Uninsured by State")

######### Maps ###########
data$state <- tolower(data$state)

states_map <- map_data("state")
pfl_map <- merge(states_map, data, by.x = "region", by.y = "state")

maternmort_noNA$state <- tolower(maternmort_noNA$state)
states_map <- map_data("state")
pfl_map_noNA <- merge(states_map, maternmort_noNA, by.x = "region", by.y = "state")

# infant mortality
ggplot(pfl_map, aes(x = long, y = lat, group = group, fill = infantmort)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic")

# map of maternal mortality- really shows how many states are missing data
matern_mort_map <- ggplot(pfl_map, aes(long, lat, group = group, fill = maternmort)) + 
  geom_polygon(colour = "black") +
  scale_fill_continuous(low = "white", high = "darkblue") +
  ggtitle(label = "Figure 1. Maternal Mortality per 100,000 Live Births", subtitle = "2018-2020") +
  coord_map("polyconic")


matern_mort_map2 <- matern_mort_map +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        text=element_text(size=15,family="Times")) +
  labs(fill = "Mortality Rate") 
matern_mort_map2

?theme()

#poorhealth 
poorhealth_map <- ggplot(pfl_map, aes(x = long, y = lat, group = group, fill = poorhealth)) + 
  geom_polygon(colour = "black") +
  scale_fill_continuous(low = "white", high = "darkblue", name = "Percent", labels = c("12.5%", "15%", "17.5%", "20%", "22.5%", "25%")) +
  ggtitle(label = "Figure 2. Percentage of Women Reporting Fair or Poor Health", subtitle = "2021") +
  coord_map("polyconic")

poorhealth_2 <- poorhealth_map +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        text=element_text(size=15,family="Times")) 
poorhealth_2

## uninsured

uninsured_map <- ggplot(pfl_map, aes(x = long, y = lat, group = group, fill = uninsured)) + 
  geom_polygon(colour = "black") +
  scale_fill_continuous(low = "white", high = "darkblue", name = "Percent", breaks = c(0.05, 0.10, 0.15, 0.20), labels = c("5%", "10%", "15%", "20%")) +
  ggtitle(label = "Figure 3. Percentage of Women 19-64 Uninsured", subtitle = "2019") +
  coord_map("polyconic")

uninsured_2 <- uninsured_map +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        text=element_text(size=15,family="Times"),
        ) 
uninsured_2

####### Bar Plots of Dependent Vars by State #######

maternmort_noNA_bar <-ggplot(data=maternmort_noNA, aes(x=state, y=maternmort)) +
  geom_bar(stat="identity")

matnermort_withNA_bar <- ggplot(data=data, aes(x=state, y=maternmort)) +
  geom_bar(stat="identity")

materndeath_bar <- ggplot(data=data, aes(x=state, y=nummaterndeath)) +
  geom_bar(stat="identity")

infantmort_bar <- ggplot(data=data, aes(x=state, y=infantmort)) +
  geom_bar(stat="identity")

everbreast_bar <- ggplot(data=data, aes(x=state, y=everybreast)) +
  geom_bar(stat="identity")

bf6mo_bar <- ggplot(data=data, aes(x=state, y=breastfed6mo)) +
  geom_bar(stat="identity")
  
bf12mo_bar <- ggplot(data=data, aes(x=state, y=breastfed12mo)) +
  geom_bar(stat="identity")
  
immunized_bar <- ggplot(data=data, aes(x=state, y=immunized)) +
  geom_bar(stat="identity")

######### Relationships #########

infantmort_birthrate <- ggplot(data, aes(x=birthrate15_44, y=infantmort)) +
  geom_point()

pfl_immunized <- ggplot(data, aes(x=implemented, y = infantmort)) +
  geom_line()





