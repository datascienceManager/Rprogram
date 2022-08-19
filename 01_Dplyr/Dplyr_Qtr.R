

library(dplyr)
library(curl)
library(data.table)

# install.packages('curl')

data = curl("https://raw.githubusercontent.com/deepanshu88/data/master/sampledata.csv")

# Converting data into csv files

data2 = read.table(data,sep=',')

# str(data1)
# data1 = readLines(data)
# data1 = do.call('rbind',strsplit(data1,","))


head(data2) # since the first row is the column name , so need to change this into column name, below is the process to do so

colnames(data2) = data2[1,]

data2 = data2[-1,]


str(data2) # since all were in character need to convert into numeric

data3 = type.convert(data2,as.is=TRUE)

str(data3)


# Above can also be read in following manner 

data = read.csv("https://raw.githubusercontent.com/deepanshu88/data/master/sampledata.csv")

head(data)

setwd('M:\\09_R\\R\\R Programs\\dplyr usage')

fwrite(data,'Data.csv') # data is as same as data3


# DPLYR

mydata = data


# Selecting Random Fraction of Rows
sample_frac(mydata,0.1)

# Remove Duplicate Rows based on all the variables

Unique_data = distinct(mydata)

# Removing duplicate value based on the one particular variable 

        # .keep_all function is used to retain all other variables

Unique_SingleVariable = distinct(mydata,Index,.keep_all = TRUE)


# Remove Duplicates Rows based on multiple variables

Unique_MultipleVariable = distinct(mydata,Index,Y2012,.keep_all = TRUE)


# select( ) Function

mydata4 = mydata %>% select(.,Index,State,Y2002)

# Dropping Variables

mydata5 =mydata %>% select(.,-Index,-State)

    #  Same can be written in following manner

mydata6 = mydata %>% select(., -c(Index,State))



# starts_with() function

mydata7 = mydata %>% select(starts_with('Y'))

# Dropping anything starting with Y

mydata8 = mydata %>% select(.,-starts_with('Y'))

# Selecting Variables contain 'I' in their names

mydata9 = mydata %>% select(.,contains("I"))

# Reorder Variables

mydata10 = mydata %>% select(.,State,everything())

mydata10

# rename( ) Function


# rename() syntax : rename(data , new_name = old_name)


mydata11 = mydata %>% rename(.,c(INDEX = Index,STATE = State))

mydata11


# Filter function

mydata12 = mydata%>% filter(.,Index =='A')


# Multiple Selection Criteria


mydata13 = mydata%>% filter(.,Index %in% c('A','C'))

# AND  Condition


mydata14 = mydata %>% filter(.,Index %in% c('A','C') & Y2008 >= 1300000)


#  OR condition 

mydata15 = mydata %>% filter(.,Index %in% c('A','F') | Y2002 >=1900000)


# NOT condition

mydata16 = mydata %>% filter(.,!Index %in% c('A','F'))


# Contains Condition

mydata17 = mydata %>% filter(.,grepl('Ar',State))


# Summarize selected variables

mydata18 = mydata %>% summarise(Y2002Mean = mean(Y2002),Y2002Median = median(Y2002))


# Summarize Multiple Variables

mydata19 = mydata %>% summarise_at(.,vars(Y2002,Y2006),funs(n(),sum(.,na.rm=TRUE),mean(.,na.rm=TRUE),median(.,na.rm=TRUE)))

        # funs( ) has been soft-deprecated (dropped) from dplyr 0.8.0. Instead we should use list. The equivalent code is stated below

# mydata19_1 = mydata %>% summarise_at(.,vars(Y2002,Y2003),list(n=~n(),mean = mean, median = median))

# #  Another method is as follows 

# summarise_at(mydata, vars(Y2005, Y2006), list(~n(), ~mean(.), ~median(.)))



# Custom NON Standard Function

  dummmyy <- data.frame(X1=sample(1:100,100), X2=runif(100))
  summarise_at(dummmyy,vars(X1,X2), function(x) var(x - mean(x)))


# Summarize only if the variables are numeric 

mydata %>% summarise_if(.,is.numeric,funs(n(),mean(.,na.rm=TRUE),median(.,na.rm=TRUE)))


# Summarize all Numeric data


mydata %>% select_if(.,is.numeric) %>%summarise_all(.,funs(n(),mean(.,na.rm=TRUE),median(.,na.rm=TRUE)))

# Summarizing only functional data

mydata%>% select(.,Index) %>% summarise_all(., funs(nlevels(.), nmiss=sum(is.na(.))))


# Summarizing based on the group by for factor or character levels

x=c("A","A","A","A","A","B","B","B","B","C","C","C","D","D","D","E","E")
y=c("a","b","c","a","b","a","b","c","d","c","b","e","b","d","f","a","b")
z=c("x","x","x","y","y","p","p","p","p","t","v","v","m","m","n","o","o")
d=data.frame(x,y,z)
 


d %>% group_by(.,x) %>% summarise_all(., funs(n_distinct(.)))

# Summarizing  the levels of distinct levels of each caretorgy variable

d %>% summarise_if(., is.character, funs(n_distinct(.)))

#  Arrange Function

mydata %>% arrange(.,desc(Index),Y2002)

# To suppress this warning you can use the following command.

options(dplyr.summarise.inform=F)



# do() function
# Use : Compute within groups
# Syntax :
# do(data_frame, expressions_to_apply_to_each_group)

# Suppose you need to pull top 3 rows from 'A', 'C' and 'F' categories of variable Index

mydata  %>% filter(.,Index %in% c('A','C','F'))%>% 
            group_by(.,Index)%>%
            do(head(.,3))%>% 
            data.frame()


# Selecting 3rd Maximum Value by Categorical Variable



mydata %>% select(.,Index,Y2007)%>%
           filter(.,Index %in% c('A','C','N'))%>%group_by(.,Index)%>%
           do(arrange(.,desc(Y2007),Index))%>%
           slice(3)


# ------------ Another method 

mydata %>% select(.,Index,Y2007)%>%
           filter(.,Index %in% c('A','C','N'))%>%
           group_by(.,Index)%>%
           filter(min_rank(desc(Y2007))==3)
           
str(mydata)


#  Mutating all the variables 
# ---- Multiplying with 100


mydata %>% mutate_if(.,is.numeric,funs("new"=.*1000))

# Above can be performed in differently fashion too

mydata%>% select_if(.,is.numeric)%>%mutate_all(.,funs("new"=.*100))


# Select State that generated highest income among the variable 'Index'

mydata %>% select(.,Index,Y2015)%>%
           group_by(.,Index)%>%
           filter(.,Index %in% c('A','C','D'),min_rank(desc(Y2015))==1)

          
# Cummulative Sum 

mydata %>% select(.,Index,Y2015)%>%group_by(.,Index)%>%mutate(.,Total = cumsum(Y2015))


# Joins

df1 = data.frame(ID = c(1, 2, 3, 4, 5),
w = c('a', 'b', 'c', 'd', 'e'),
x = c(1, 1, 0, 0, 1),
y=rnorm(5),
z=letters[1:5])

df2 = data.frame(ID = c(1, 7, 3, 6, 8),
a = c('z', 'b', 'k', 'd', 'l'),
b = c(1, 2, 3, 0, 4),
c =rnorm(5),
d =letters[2:6])

df3 = data.frame(IDD = c(1, 7, 3, 6, 8),
a = c('z', 'b', 'k', 'd', 'l'),
b = c(1, 2, 3, 0, 4),
c =rnorm(5),
d =letters[2:6])

# Inner Join
InnJoinsA = inner_join(df1,df2,by= 'ID')

# Inner Join
# https://www.statology.org/dplyr-join-on-multiple-columns/
# https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti

InnJoinsB = inner_join(df1,df3,by=c('ID'='IDD','w'='a'))

library(dplyr)

# 

mtcars$model <- rownames(mtcars)

# Eliminating the rownames

rownames(mtcars) = NULL

first <- mtcars[1:20, ]
second <- mtcars[15:32, ]


# Intersection happens when their is common values

dplyr::intersect(first, second)


# Applying UNION
# UNION displays all rows from both the tables and removes 
# duplicate records from the combined dataset. By using union_all function, it allows duplicate rows in the combined dataset

dplyr::union(first, second)

# Rows appear in one table but not in other table
dplyr::setdiff(first, second)

dplyr::setdiff(second, first)

# Combines all the values including duplicate values
union_all(first, second)


# Handling of duplicates:
a <- data.frame(column = c(1:10, 10))
b <- data.frame(column = c(1:5, 5))

# intersection is 1 to 5, duplicates removed (5)
intersect(a, b)

# union is 1 to 10, duplicates removed (5 and 10)
union(a, b)

# set difference, duplicates removed (10)
setdiff(a, b)


# union all does not remove duplicates
union_all(a, b)


# 
startwars = data.frame(starwars)

# Choose rows using their position with slice()

starwars %>% slice(5:7)


starwars %>% slice_head(n = 3)


# Slice with Sample and proportion

# Use replace = TRUE to perform a bootstrap sample. If needed, you can weight the sample with the weight argument.

starwars %>% slice_sample(prop = 0.1, replace=TRUE)


starwars %>% slice_sample(n = 5)

# slice_min() and slice_max() select rows with highest or lowest values of a variable. 
# Note that we first must choose only the values which are not NA.

starwars = data.frame(starwars[1:10,])


# Top 3 best from respective column 
starwars %>% filter(.,!is.na(height))%>%slice_max(.,height,n=3)

# similarly for minimum 

starwars %>% filter(.,!is.na(height))%>%slice_min(.,height,n=3)

# across() function
# across( ) function was added starting dplyr version 1.0. It helps analyst to perform same operation on multiple columns

starwars %>% 
  select(.,species,height,mass) %>%
  group_by(.,species) %>%
  summarise(across(height:mass, mean))

starwars %>% select(.,species,height,mass) %>%
             group_by(.,species) %>% 
             summarise(across(where(is.numeric),mean))

# Here we are using two summary statistics - mean and no. of distinct values in two different set of variables.
  mtcars %>% 
  group_by(carb) %>% 
  summarise(across(mpg:qsec, mean), across(vs:gear, n_distinct))

# across() can also be applied with mutate function

mtcars %>% group_by(carb)%>%mutate(across(where(is.numeric),mean))

mtcars%>% group_by(.,carb,model) %>% mutate(across(everything(), mean, na.rm = TRUE))

# rowwise()

df <- data.frame(id= seq(1,5,1),x = round(runif(5,1,10),0), y = round(runif(5,1,10),0), z = round(runif(5,1,10),0))

df %>%mutate(MinVal = do.call(pmin,.), MaxVal = do.call(pmax,.))


df%>%rowwise(.,id)%>%mutate(.,TotalSum = sum(c_across(x:z)),MinV = min(c_across(x:z)))


df%>%rowwise(.,id)%>%mutate(.,TotalSum = sum(c_across(is.numeric)))

# How to deal with Quotation









