library(dplyr)
library(tidyverse)
library(rvest)
library(stringr)

url_grants <- "http://www.hdb.gov.sg/cs/infoweb/residential/buying-a-flat/new/first-timer-and-second-timer-couple-applicants"
webpage_grants <- read_html(url_grants)
grant_amount <- html_nodes(webpage_grants, 'table')[2] %>% html_node("tbody") %>% html_nodes("tr") %>% html_nodes("td:last-child") %>% html_text() 
grant_amount <- grant_amount[-c(1)]
grant_amount <- str_replace_all(grant_amount, "[$,]", "") %>% as.numeric()
income_level <- html_nodes(webpage_grants, 'table')[2] %>% html_node("tbody") %>% html_nodes("tr") %>% html_nodes("td:first-child") %>% html_text() 
income_level <- income_level[-c(1)]
income_level <- str_replace_all(income_level, "[\t\n\r\v\f]", "") 
df_grants <- data.frame(income_level=income_level, grant_amount=grant_amount)
df_grants$income_level <- factor(df_grants$income_level, levels = df_grants$income_level)
df_ageSpecificMarriageRate <- data.frame(year=csv_femaleAgeSpecificMarriageRate$year, 
                                         value=((csv_femaleAgeSpecificMarriageRate$value+csv_maleAgeSpecificMarriageRate$value)/2), 
                                         age_group=csv_femaleAgeSpecificMarriageRate$level_2
                                         )
df_numMarriagesFemaleByAge <- group_by(csv_femaleAgeSpecificMarriageRate, age_group=level_2) %>% 
  summarise(number=sum(value))
df_numMarriagesMaleByAge <- group_by(csv_maleAgeSpecificMarriageRate, age_group=level_2) %>% 
  summarise(number=sum(value))
df_numMarriagesByAge <- data.frame(age_group=df_numMarriagesFemaleByAge$age_group,
                                   number=((df_numMarriagesFemaleByAge$number+df_numMarriagesMaleByAge$number)/2)
                                   )
df_flatsConstructedByHDB <- subset(csv_flatsConstructedByHDB, year>=1980)
df_ageSpecificMarriageRate_f <- subset(df_ageSpecificMarriageRate, 
                                       (age_group==('25 - 29 Years') | age_group==('30 - 34 Years'))
                                       )
df_numMarriagesByYear_f <- group_by(df_ageSpecificMarriageRate_f, year=year) %>% 
  summarise(number=sum(value))
df_numDivorces_f <- subset(csv_totalDivorcesByDurationOfMarriage, 
                           (level_2==('5-9 Years'))
)

