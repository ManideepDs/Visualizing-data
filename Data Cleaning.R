# IST719 M800
# Information Visualization
#Final Project
#Group_1
######################################

#Setting up workspace
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

#Loading required libraries
library(tidyverse)
library(lubridate)
library(ggmap)
library(ggpubr)
library(rgdal)
library(broom)
library(httr)
library(geojsonio)

#importing dataset
filepath <- file.choose()

raw_data <- read.csv(filepath, header = T, stringsAsFactors = FALSE) %>% 
  janitor::clean_names() 


###############################
#Data cleaning -begin
data <- raw_data %>%
  mutate(inspection_date = mdy(inspection_date),
         grade_date = mdy(grade_date)) %>%
  filter(latitude !=0 & longitude !=0 &
           (grade %in% c("A","B","C")) & 
           critical_flag %in% c("Y","N")) %>%
  select(-phone, -record_date, -community_board, 
         -census_tract,-council_district,
         -bin, -bbl,-nta, -grade_date) %>%
  mutate(grade = factor(grade),
         boro = factor(boro),
         violation_code = factor(violation_code),
         action = factor(action),
         critical_flag = factor(critical_flag)) %>%
  na.omit()
  
  

#summary(data$action)
#DAta cleaning -end
#####################################


#Unique inspection grading
points <- data %>%
  group_by(camis,inspection_date) %>%
  select(camis,boro,cuisine_description,inspection_date,grade,score)%>%
  unique() %>%
  ungroup()


#####################################
#Grading History year wise _ begin
grading_history <- points %>%
  mutate(year = year(inspection_date)) %>%
  group_by(year,grade) %>%
  summarise(count = n())

total_inspections <- points %>%
  mutate(year = year(inspection_date)) %>%
  group_by(year) %>%
  summarise(count = n())

ggplot(data = grading_history)+
  geom_line(aes(x=year, 
                y=count, 
                group =grade,
                col = grade),
            size =1.2)+
  geom_line(data = total_inspections,
            mapping = aes(x=year, y=count),
            size=1.2,
            col = "#E67E22")+
  scale_x_continuous(limits = c(2016,2019),expand = c(0, 0.1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "None",
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "grey50", linetype = "dashed"))+
  labs(x ="Year",
       y= "Number of Inspection",
       title = "New York restaurants grading history across years",
       subtitle = "They always strive for best grade",
       caption = "Source: Tidy Tuesday R | Manideep")

#dev.print(pdf, "plot1_1.pdf", width = 8,height =6)
#Grading History year wise _ Ends
##################################

############################################
#constant grade A _begin
gradeBC <- data %>%
  filter(grade != 'A') %>%
  select(camis,boro) %>%
  unique() %>%
  group_by(boro) %>%
  summarise(count =  n()) %>%
  mutate(grade = "bc") 

gradeA <- data %>%
  filter(! camis %in% gradeBC$camis ) %>%
  select(camis,boro) %>%
  unique() %>%
  group_by(boro) %>%
  summarise(count =  n()) %>%
  mutate(grade = "a") 

grades <- rbind(gradeA,gradeBC)

ggplot()+
  geom_col(data = grades,
           mapping = aes(x=boro,
                         y=count,
                         fill = grade),
           position = "dodge")
#dev.print(pdf, "grades.pdf", width = 8,height =8)
#constant grade A _ End
#########################################################

#########################################################
#Final and intial gradings- begin
final_grading <- points %>%
  group_by(camis) %>%
  filter(inspection_date == max(inspection_date)) %>%
  unique() %>%
  mutate(inspection = "final")

initial_grading <- points %>%
  group_by(camis) %>%
  filter(inspection_date == min(inspection_date)) %>%
  unique() %>%
  mutate(inspection = "initial")

gradings_fi <- rbind(final_grading, initial_grading)

ggplot(data = gradings_fi)+
  geom_bar(mapping = aes(x=inspection, fill = fct_rev(grade)),
           position = "fill",
           width = 0.3,
           color = "grey50")+
  coord_flip()+
  scale_fill_manual(values = c("#C0392B", "#E7B800","#3498DB"))+
  theme(legend.position = "None",
        axis.title = element_blank(),
        #axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey50", linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey50", linetype = "dashed"))
#dev.print(pdf, "plot3.pdf", width = 8,height =6)
#Final and intial gradings -end
########################################

###########################################
#Distribution of scores - Begin
ggplot(data = points)+
  geom_bar(aes(x=score, fill = grade))+
  scale_fill_manual(values = c("#2e86c1","#16a085","#e74c3c"))+
  scale_x_continuous(limits = c(0,55),expand = c(0, 0.1)) +
  scale_y_continuous(expand = c(0, 0))+
  theme(legend.position = "None",
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"))+
  coord_cartesian(ylim = c(0,10000))
#dev.print(pdf, "plot2.pdf", width = 8,height =6)

#Distribution of scores -end
##############################################

##############################################
#Top violations -begin
violations <- data %>% 
  group_by(violation_code,critical_flag,violation_description) %>%
  summarise(count = n()) %>%
  arrange(-count)

ggdotchart(violations[1:25,], x = "violation_code", y = "count",
           color = "critical_flag",                                # Color by groups
           palette = c("#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 2,                                 # Large dot size
           y.text.col = TRUE,                            # Color y text by groups
           ggtheme = theme_pubr()                        # ggplot2 theme
)+
  theme_cleveland()                                      # Add dashed grids
#dev.print(pdf, "pviolations_dot.pdf", width = 8,height =6)
#Top violations - end
##############################################

##########################
#Cuisine wise grading distribution -begin
d<- final_grading %>% 
  group_by(cuisine_description, grade ) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(cuisine_description) %>%
  mutate(total = sum(count),pct =  100* count/sum(count)) %>%
  filter(total >=250)

d$gradeM <- "BC"
d$gradeM[d$grade == "A"] <- "A"

d2 <- d %>%
  group_by(cuisine_description,gradeM) %>%
  summarise(newpct = sum(pct))

d2$newpct[d2$gradeM == "BC"] <- (-1)*(d2$newpct[d2$gradeM == "BC"])

d2$cuisine_description[d2$cuisine_description == "Latin (Cuban, Dominican, Puerto Rican, South & Central American)"] <- "Latin"

d2 <- d2 %>%
  arrange(-newpct)

ggdotchart(d2, x = "cuisine_description", y = "newpct",
           color = "gradeM",
           palette = c("#2e86c1", "#FC4E07"),
           add= "segments",
           add.params = list(color = "lightgray", size = 3),
           rotate = TRUE,             
           dot.size = 10,       
           font.label = list(color = "white", size = 7,vjust = 0.5), 
           label = round(d2$newpct,1),
           ggtheme = theme_pubr())+
  geom_hline(yintercept = 0, linetype = 1, color = "black")
#Cuisine wise grading distribution -end
###################################################


restaurants <- data %>%
  group_by(camis, inspection_date) %>%
  filter(inspection_date == max(inspection_date)) %>%
  ungroup() %>%
  select(camis, boro, zipcode, latitude, longitude) %>%
  unique()

bn <- data %>%
  group_by(camis, inspection_date) %>%
  filter(inspection_date == max(inspection_date)) %>%
  ungroup() %>%
  select(camis, boro, zipcode, grade, latitude, longitude) %>%
  unique() %>%
  group_by(boro,grade,zipcode) %>%
  summarise(count =n()) %>%
  ungroup()%>%
  group_by(boro, grade) %>%
  filter(count == max(count) & grade == 'A')


###############################
#New york city map -begin
spdf <- GET("https://data.beta.nyc/dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson")

nyc_city <- readOGR(content(spdf,'text'), 'OGRGeoJSON', verbose = F)
nyc_city_df <- tidy(nyc_city)

ggplot() + 
  geom_polygon(data=nyc_city_df, aes(x=long, y=lat, group=group),
               fill = "grey50",
               show.legend = F)+
  geom_point(data=restaurants[restaurants$zipcode %in% bn$zipcode,], 
             aes(x=longitude, y=latitude,
                 color = boro))+
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line())
#New york city map - end
#########################################