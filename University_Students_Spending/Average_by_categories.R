#install packages
remove.packages("ggplot2")
install.packages("tidyverse")
install.packages(tidyr)
install.packages("colorspace")
install.packages("dplyr") # data trim
install.packages("skimr") # data cleaning
install.packages("janitor") # data cleaning
install.packages("rmarkdown") # R marksdown
install.packages("knitr")# R marksdown

#import and read dataframe
uni_df <- read.csv("University Students Monthly Expenses.csv")
str(data1)

# trim df
trimmed_df <- uni_df %>% 
  select("Gender","Age",'Monthly_expenses_.')


df_drinks <- uni_df %>% 
  select('Monthly_expenses_.','Drinks')

df_schlorship <- uni_df %>% select('Monthly_expenses_.','Scholarship')

# clean, change data
data<-replace(uni_df, uni_df=='', "No") # replace NULL value to 'No'
data1 <- data %>% drop_na(Study_year) # remove data that omitted the year
data1[is.na(data1)] <- 0 # resign null Monthly_expenses to 0
data2<-data %>% drop(Living ) # replace NULL value to 'No'
#Summarize
df_summary <- 
  data %>%
  group_by(Study_year) %>%
  summarise(average_spending=mean(Monthly_expenses_.))

df_summary_sch <- 
  df_schlorship %>%
  group_by(Scholarship) %>%
  summarise(average_spending=mean(Monthly_expenses_.))

           

# ggplot
  

ggplot(data = df_summary, aes(x = Age, y = average_spending)) + 
  geom_bar(stat='identity')




ggplot(data = uni_df, mapping = aes(Monthly_expenses_.)) + 
  geom_bar() +
  facet_wrap(~Part_time_job) +
  labs(title="new title")

ggplot(data = trimmed_df, mapping = aes(Monthly_expenses_.)) + 
  geom_bar() +
  facet_wrap(~Gender) +
  labs(title="new title")

ggplot(data = df_drinks, mapping = aes(Monthly_expenses_.)) + 
  geom_bar() +
  facet_wrap(~Drinks) +
  labs(title="Drinks vs spending")

#By Ages
df_ages <- 
  data1 %>%
  group_by(Age) %>%
  summarise(average_spending=mean(Monthly_expenses_.))

plot_avg_ages <- ggplot(data = df_ages, aes(x = Age, y = average_spending)) + 
  geom_bar(stat='identity')+
  labs(title="Average Spending(Group by ages)")

#By Gender
df_gen <- 
  data1 %>%
  group_by(Gender) %>%
  summarise(average_spending=mean(Monthly_expenses_.))

plot_avg_gen <-ggplot(data = df_gen, aes(x = Gender, y = average_spending)) + 
  geom_bar(stat='identity')+
  labs(title="Average Spending(Group by Gender)")

#By Year
df_yr <- 
  data1 %>%
  group_by(Study_year) %>%
  summarise(average_spending=mean(Monthly_expenses_.))

plot_avg_yr <-ggplot(data = df_yr, aes(x = Study_year, y = average_spending)) + 
  geom_bar(stat='identity')+
  labs(title="Average Spending(Group by Study year)")