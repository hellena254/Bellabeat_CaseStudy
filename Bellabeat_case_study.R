library(tidyverse)
library(lubridate)
library(readr)
library(skimr)
library(janitor)
#ggrepel and ggpubr

#importing the csv files
daily_activity <- read.csv('E:/H workstation/Data Analytics/Projects/Case_study_2_Bellabeat/Datasets/dailyActivity_merged.csv')
hourly_steps <- read.csv('E:/H workstation/Data Analytics/Projects/Case_study_2_Bellabeat/Datasets/hourlySteps_merged.csv')
daily_sleep <- read.csv('E:/H workstation/Data Analytics/Projects/Case_study_2_Bellabeat/Datasets/sleepDay_merged.csv')

#summary of data frame
view(hourly_steps)
view(daily_sleep)
view(daily_activity)

head(daily_activity)
head(hourly_steps)
head(daily_sleep)

str(daily_activity)
str(hourly_steps)
str(daily_sleep)

nrow(daily_activity)
nrow(daily_sleep)
nrow(hourly_steps)


#DATA CLEANNG PROCESS 
#unique entries
# n_unique needs the skimr package
#Here we check to ensure that our data has only unique entries. To do that we will first verify the number of unique entries there are per dataset.
n_unique(daily_activity$Id)
n_unique(hourly_steps$Id)
n_unique(daily_sleep$Id)



#check for duplicates
#duplicated shows if there is any duplicate set of data.
#!duplicated removes any duplicated value - vector

anyDuplicated(daily_activity)
anyDuplicated(hourly_steps)
sum(anyDuplicated(daily_sleep))#gives 162

sum(duplicated(daily_activity))
sum(duplicated(hourly_steps))
sum(duplicated(daily_sleep)) #gives 3


#removing duplicates and n/a
daily_activity <- daily_activity%>%
  distinct()%>%
  drop_na()

hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()

daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()

sum(duplicated(daily_sleep)) #gives 0

#COLUMNS
#clean col names
#use janitor package to use clean_names function
clean_names(daily_activity)
clean_names(hourly_steps)
clean_names(daily_sleep)

daily_activity <- rename_with(daily_activity, tolower)
hourly_steps <- rename_with(hourly_steps, tolower)
daily_sleep <- rename_with(daily_sleep, tolower)

daily_activity <- rename(daily_activity, date = activitydate)
hourly_steps <- rename(hourly_steps, date_time = activityhour)
daily_sleep <- rename(daily_sleep, date = sleepday)

head(daily_activity)
head(hourly_steps)
head(daily_sleep)
 
#date column consistent
daily_activity <- mutate(daily_activity, date = as_date(date, format = "%m/%d/%Y"))
daily_sleep <- mutate(daily_sleep, date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
hourly_steps <- mutate(hourly_steps, date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone()))


#MERGE THE DATA SETS
daily_activity_sleep <- merge(daily_activity, daily_sleep, by=c ("id", "date" ))
glimpse(daily_activity_sleep)


#ANAYSIS AND SHARE PHASE 
#type of users
daily_average_steps <- daily_activity_sleep %>%
  group_by(id) %>%
  summarise(average_steps = mean(totalsteps))

head(daily_average_steps)

#classify users
user_type <- daily_average_steps %>%
  mutate(user_type = case_when(average_steps < 5000 ~ 'sedentary',
                               average_steps >= 5000 & average_steps < 7499 ~ 'lightly active',
                               average_steps >= 7499 & average_steps < 9999 ~ 'fairly active',
                               average_steps > 10000 ~ 'very active'))

head(user_type)
  
#percentages
usertype_percent <- user_type %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(total_users = sum(total)) %>% 
  group_by(user_type) %>%
  summarise(total_percent = total / total_users) %>% 
  mutate(percentage = scales::percent(total_percent)) 

usertype_percent$user_type <- factor(usertype_percent$user_type, levels = c('sedentary', 'lightly active', 'fairly active', 'very active'))
  
head(usertype_percent)


#pie chart
usertype_percent %>%
  #we first create a basic bar chart
  ggplot(aes(x = '' , y = total_percent, fill = user_type)) +
  geom_bar(stat = 'identity' , width = 1) +
  # we then convert to pie (polar coordinates) and add labels
  coord_polar('y' , start = 0) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5)) +
  #we add color scales
  scale_fill_manual(values = c('#55dde0', '#33658a',  '#f26419', '#f6ae2d')) +
  #remove labels and add title
  labs(x = NULL, y = NULL, fill = NULL, title = 'Percentage of user type' ) + 
  theme_classic() + #tidy up the theme
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,   face = 'bold'))
  
           
##Weekday sleep and steps
weekday_steps_sleep <- daily_activity_sleep %>%
  mutate(weekday = weekdays(date))

weekday_steps_sleep$weekday <- ordered(weekday_steps_sleep$weekday, levels = c("Monday", "Tuesday", 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

weekday_steps_sleep <- weekday_steps_sleep %>%
  group_by(weekday) %>%
  summarise(average_steps = mean(totalsteps), average_sleep = mean(totalminutesasleep))

head(weekday_steps_sleep)

#Visualization of average steps and average sleep
ggplot(data = weekday_steps_sleep) +
  geom_col(mapping =aes(x = weekday, y = average_steps), fill = '#33658a') +
  geom_hline(yintercept = 7500) +
  labs(title = 'Average steps in a week', x = '', y = '') +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = .5))
  

ggplot(data = weekday_steps_sleep) +
  geom_col(mapping =aes(x = weekday, y = average_sleep), fill = '#55de80') +
  geom_hline(yintercept = 480) +
  labs(title = 'Average sleep in minutes in a week', x ='', y='') +
  theme(axis.text.x = element_text(angle = 30, vjust = .5, hjust = .5))

#Hourly steps
hourly_steps <- hourly_steps %>%
  separate(date_time, into = c("date", "time"), sep = " ") %>%
  mutate(date = ymd(date))

head(hourly_steps)

#viz of hourly steps
hourly_steps %>%
  group_by(time) %>%
  summarise(average_steps = mean(steptotal)) %>%
  ggplot(aes(x = time, y = average_steps, fill = average_steps)) +
  geom_col() +
  labs(title = "Hourly average steps in a day", x = "", y = "" ) +
  scale_fill_gradient(low = "green", high = "orange") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = .5)) 


#correlation
ggplot(data = daily_activity_sleep, mapping = aes(x = totalsteps, y = totalminutesasleep)) +
  geom_jitter() +
  geom_smooth(color = "blue") +
  labs(title = "Daily steps vs Minutes asleep", x = 'Daily steps' , y = 'Minutes asleep') +
  theme_bw()
  #theme(panel.background = element_blank(), plot.title = element_text(size = 14))


ggplot(data = daily_activity_sleep, mapping = aes(x = totalsteps, y = calories)) +
  geom_jitter() +
  geom_smooth(color = "blue") +
  labs(title = "Daily steps vs Calories burnt", x = 'Daily steps' , y = 'calories') +
  theme_bw()  


#usage of smart device.
## How long the smart devices are used.
daily_usage <- daily_activity_sleep %>%
  group_by(id) %>%
  summarise(days_used = sum(n())) %>%
  mutate(usage = case_when(days_used >= 21 & days_used <= 31 ~ "high use",
                           days_used >= 11 & days_used <= 20 ~ "moderate use",
                           days_used >= 1 & days_used <= 10 ~ "low use"))

head(daily_usage)

#percentage of daily usage
usage_percent <- daily_usage %>%
  group_by(usage) %>%
  summarise(total = n()) %>%
  mutate(total_sum = sum(total)) %>%
  group_by(usage) %>%
  summarise(total_percent = total / total_sum) %>%
  mutate(percentage = scales::percent(total_percent))

usage_percent$usage <- factor(usage_percent$usage, levels = c("high use", "moderate use", "low use"))
head(usage_percent)

#viz of percentage of usage
usage_percent %>%
  ggplot(aes(x = '' , y = total_percent, fill = usage)) +
  geom_bar(stat = 'identity' , width = 1) +
  coord_polar('y' , start = 0) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "", fill = NULL, title = 'Percentage usage of smart devices' ) + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,   face = 'bold')) +
  scale_fill_manual(values = c("#006699", "#00e688", "#85e0e0"),
                    labels = c("High use - 21 to 31 days",
                               "Moderate use - 11 to 20 days",
                               "Low use - 1 to 10 days")) 

##Time spent on smart devices.
daily_usage_activity <- merge(daily_activity, daily_usage, by = c('id'))
glimpse(daily_usage_activity)
head(daily_usage_activity)

#minutes worn
active_duration <- daily_usage_activity %>%
  mutate(total_active_duration = veryactiveminutes + lightlyactiveminutes + fairlyactiveminutes + sedentaryminutes) %>%
  mutate(active_duration_percent = (total_active_duration / 1440) * 100) %>%
  mutate(active = case_when(active_duration_percent == 100 ~ "All day",
                            active_duration_percent >= 50 & active_duration_percent < 100 ~ "More than half a day",
                            active_duration_percent >0 & active_duration_percent < 50 ~ "Less than half a day"))


active_duration$active <- factor(active_duration$active, levels = c("All day" , "More than half a day" , "Less than half a day"))
head(active_duration)

#Total time active
active_percent <- active_duration %>%
  group_by(active) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(active) %>%
  summarise(total_percent = total / totals) %>%
  mutate(percentage = scales::percent(total_percent))


highly_active <- active_duration %>%
  filter(usage == "high use") %>%
  group_by(active) %>% 
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(active) %>%
  summarise(total_percent = total / totals) %>%
  mutate(percentage = scales::percent(total_percent))
  

moderately_active <- active_duration %>%
  filter(usage == "moderate use") %>%
  group_by(active) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(active) %>%
  summarise(total_percent = total / totals) %>%
  mutate(percentage = scales::percent(total_percent))


low_active <- active_duration %>%
  filter(usage == "low use") %>%
  group_by(active) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(active) %>%
  summarise(total_percent = total / totals) %>%
  mutate(percentage = scales::percent(total_percent))


head(active_percent)
head(highly_active)
head(moderately_active)
head(low_active)


#Visualization
active_percent %>%
  ggplot(aes(x = "" , y = total_percent , fill = active)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y" , start = 0) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c('#55dde0', '#33658a',  '#f26419')) +
  labs(title = "Time worn in a day", x = "" , y = "") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,   face = 'bold'))


highly_active %>%
  ggplot(aes(x = "" , y = total_percent , fill = active)) +
  geom_bar(stat = "identity" , width = 1) +
  coord_polar("y" , start = 0) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c('#55dde0', '#33658a',  '#f26419')) +
  labs(title = "Daily duration worn by high active users" , x = "" , y = "") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))
  
  
moderately_active %>%
  ggplot(aes(x = "", y = total_percent, fill = active)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y" , start = 0) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c('#55dde0', '#33658a',  '#f26419')) +
  labs(title = "Daily duration worn by moderate users" , x = "" , y = "") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))


low_active %>%
  ggplot(aes(x = "", y = total_percent, fill = active)) +
  geom_bar(stat = "identity" , width = 1) +
  coord_polar("y" , start = 0) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c('#55dde0', '#33658a',  '#f26419')) +
  labs(title = "Daily duration worn by low users") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))












#viz
ggarrange(
  ggplot(active_percent, aes(x="",y=total_percent, fill= active)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start=0)+
    scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
    geom_text(aes(label = percentage),position = position_stack(vjust = 0.5), size = 3.5)+
    labs(title="Time worn per day", subtitle = "Total Users")+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)),
   
  
  ggarrange(
    ggplot(highly_active, aes(x="",y=total_percent, fill= active)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text_repel(aes(label = percentage) , position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "High use - Users") +
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.position = "none"),
    
      
    ggplot(moderately_active, aes(x="",y=total_percent, fill= active)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = percentage), position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Moderate use - Users") +
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none"),
            
     
    ggplot(low_active, aes(x="",y=total_percent, fill= active)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = percentage), position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Low use - Users")+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") ,
      ncol = 3),
      nrow = 2)
  
  




















