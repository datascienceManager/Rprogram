
library(readr)
library(data.table)

library(dplyr)

data2 = read.csv("https://raw.githubusercontent.com/PatMartin/Dex/master/data/CrimeStatebyState.csv",header=TRUE)

head(data2)


data3 = fread("https://raw.githubusercontent.com/PatMartin/Dex/master/data/CrimeStatebyState.csv",header=TRUE)


# Structure Details
str(data3)

data3 = data.frame(data3)

# Summary details
summary(data3)

head(data3)

data3_Crime2005 = data3%>% filter(.,Year=='2005',State=='New York')


# Arrange 


 Crime2005 = data3_Crime2005 %>% arrange(.,desc(Count))


# Group by and taking proportion based on the 

GroupProport = data3_Crime2005 %>% group_by(.,Type.of.Crime) %>% select(.,Type.of.Crime,Count)%>% mutate(.,Total = sum(Count))%>%
                    mutate(.,GroupProportion = round(Count/Total,3))%>%
                    select(.,Type.of.Crime,Count,GroupProportion)%>% data.frame()




TotalProportion = data3_Crime2005 %>% mutate(.,TotalProportion = round(Count/sum(Count),3))%>%
                  select(.,Type.of.Crime,Count,TotalProportion)               
                  
# str(TotalProportion)
Final = left_join(Crime2005,GroupProport,by=c("Type.of.Crime","Count"))%>% 
        left_join(.,TotalProportion,by=c("Type.of.Crime","Count"))


# Summarising the data based on the group

