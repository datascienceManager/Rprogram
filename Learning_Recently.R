# Clearning Terminal in VS code 

# Per comments, in later versions of VSCode (1.29 and above) this shortcut is missing / needs to be created manually.

# Navigate: File > Preferences > Keyboard Shortcuts
# search for workbench.action.terminal.clear
# If it has no mapping or you wish to change the mapping, continue; otherwise note & use the 
# existing mapping
# Double click on this entry & you'll be prompted for a key binding. Hold CTRL and tap K. 
# Ctrl + K should now be listed. Press enter to save this mapping
# Right click the entry and select Change when expression. Type terminalFocus then press enter.
# That's it. Now, when the terminal is in focus and you press Ctrl+K you'll get the behaviour 
# you'd have expected to get from running clear/cls.




# ============ Reading data from Git hut

# first need to click on RAW and then copy the URL and use read.csv to access the file

# https://github.com/PatMartin/Dex/blob/master/data/CrimeStatebyState.csv

# https://raw.githubusercontent.com/PatMartin/Dex/master/data/CrimeStatebyState.csv

data2 = read.csv("https://raw.githubusercontent.com/PatMartin/Dex/master/data/CrimeStatebyState.csv",header=TRUE)

head(data2)
# ----- OR 

data3 = fread("https://raw.githubusercontent.com/PatMartin/Dex/master/data/CrimeStatebyState.csv",header=TRUE)


# ============ To get the list of Packages

ip = as.data.frame(installed.packages()[,c(1,3:4)])

ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
ip



# ======== Implementing function in R


# install.packages('lazyeval')

library(lazyeval)

set.seed(100)
ds <- data.frame(group=c(rep("a",100), rep("b",100), rep("c",100)), 
                 x=rnorm(n = 300, mean = 3, sd = 2), y=rnorm(n = 300, mean = 2, sd = 2))


my_fun <- function(x, y){
  res_x = mean(x) + 2
  res_y = mean(y) * 5 
  return(data.frame(res_x, res_y))
}

fun = function(data, x_var_name, y_var_name, group_var_name){
  # group_by_() .dots argument
  group_dots = interp(~ group_var_name, group_var_name = as.name(group_var_name))
  # do_() .dots argument
  do_dots = interp( ~ my_fun(x = .[[x_var_name]], y = .[[y_var_name]]))
  # Operations
  out = data %>%
    group_by_(.dots = group_dots) %>%
    do_(.dots = do_dots)
  return(out)
}

library(dplyr)

fun(data=ds, x_var_name='x', y_var_name="y", group_var_name='group')





dt <- data.frame(Product Name.May = c("Lettuce", "Beetroot", "Spinach", "Kale", "Carrot"),
                 Count ry = c("CA", "FR", "FR", "CA", "CA"),
                 Q1 = c(NA, 61, 40, 54, NA), Q2 = c(22,  8, NA,  5, NA),
                 Q3 = c(51, NA, NA, 16, NA), Q4 = c(79, 10, 49, NA, NA))



# ======= getting minimum,max and avg , range value in R



library(matrixStats)
library(data.table)
library(dplyr)

dt <- data.frame(ProductName = c("Lettuce", "Beetroot", "Spinach", "Kale", "Carrot"),
                 Country = c("CA", "FR", "FR", "CA", "CA"),
                 Q1 = c(NA, 61, 40, 54, NA), Q2 = c(22,  8, NA,  5, NA),
                 Q3 = c(51, NA, NA, 16, NA), Q4 = c(79, 10, 49, NA, NA))
#install.packages('matrixStats')

n_dt = names(dt)[3:6]

# 
# dt = as.matrix(dt)
# 
# dt = as.data.frame(dt)

str(dt)

Inf_NA_NAN_BGMP = function (x) {
  x = as.numeric(as.character(x))
  x[is.na(x)] = 0
  x[is.infinite(x)] = 0
  x[is.nan(x)] = 0
  x
}

dt[3:ncol(dt)] = apply(dt[3:ncol(dt)],2,Inf_NA_NAN_BGMP)

dt_1=dt%>%dplyr::select(.,starts_with('Q'))%>%dplyr::mutate(Minm = do.call(pmin,(.))) %>%
  dplyr::select(Minm) %>% cbind(dt,.)

dt_2=dt_1%>%dplyr::select(.,starts_with('Q'))%>%dplyr::mutate(Maxm = do.call(pmax,(.))) %>%
  dplyr::select(Maxm) %>% cbind(dt_1,.)%>% rowwise()%>%dplyr::mutate(.,Rn_g =(Maxm-Minm))


dt  = data.frame(dt_2)

# dt_1 = dt %>% mutate_at(.,vars(Q1:Q4),funs('Normal_'=1(x-MIN)/Rn_g))

Col_n = c('Q1N','Q2N','Q3N','Q4N')

dt_n = data.frame(Sr.No = seq(1:nrow(dt)))

for(i in 1 : 4){
  # i=2
    ni = 2+i
  mi = grep("Minm",colnames(dt))
  rn_g =grep("Rn_g",colnames(dt))
  Avt_Min = (dt[ni]-dt[mi])
  rngg = dt[rn_g]
  divd = (Avt_Min/rngg)
  dt_n[paste(Col_n[i],sep='')]= 1-divd
}

dt_n = data.frame(apply(dt_n,2,Inf_NA_NAN_BGMP))



rm('dt_n')

# ========== Displaying exponential as number 

options("scipen" = 10)


#====================Getting Sequence on groupby variables






# ---------rowwise sum for only numeric variables

Usage1=Usage%>% select_if(.,is.numeric)%>% mutate(.,Row_su=rowSums(.))%>%select(.,Row_su)%>%bind_cols(Usage,.)



# ---------- Excluding one of the data frame from spark context 

dplyr::db_drop_table(sc, "batting")


dplyr::db_drop_table(sc, 'iris')
tbl_cache(sc, "iris")
src_tbls(sc)

tbl_name <- "impmatrx"
DBI::dbGetQuery(sc, paste("DROP TABLE", tbl_name))

#------------ Extracting the first row 

library(dplyr)
data <- data.frame(personal_id = c("111-111-111", "999-999-999", "222-222-222", "111-111-111",'999-999-999','111-11-111'),
                   gender = c("M", "F", "M", "M",'F','M'),
                   temperature = c(99.6, 98.2, 97.8, 95.5,98.3,98.9))

str(data)


data2<- data %>% group_by(.,personal_id)%>%mutate(.,Sequence_num = row_number())%>%arrange(.,Sequence_num)%>%filter(.,Sequence_num==1)



#================ Extract only coefficients whose p values are significant from a logistic model



data.frame(summary(score)$coef[summary(score)$coef[,4] <= .05, 4])


# ================= Creating Model 

Model_dep = colnames(Dep_Freq[2:ncol(Dep_Freq)])


for (i in 1:length(Model_dep)) {
  
  Model = paste("Model",i,sep="_")
  
  Final_Independent_data_ModelData = Final_Independent_data %>% select(.,-starts_with("Dep_"),-MSISDN) %>% names %>%unlist
  
  m = glm(as.formula(paste(Model_dep[i],paste(Final_Independent_data_ModelData,collapse = "+"),sep="~ ")),data=Final_Independent_data)
  
  New_model_Significant_coef = step(m,direction = "both")
  
  assign(Model,New_model_Significant_coef)
  
  
}

# ============= Dynamically Reading respective file pattern 
# from the folder 


MainDir = "/R_Model/Data"

setwd(file.path(MainDir))

Data_Envirnmt_ = new.env()

temp = list.files(pattern="*.csv")

for (i in 1:length(temp)) assign(temp[i],envir = Data_Envirnmt_, fread(temp[i]))

List_dataset = mget(ls(envir = Data_Envirnmt_),envir = Data_Envirnmt_)

list2env(List_dataset, .GlobalEnv)


# =========== Dynamically changing the column name along with 

List_dataset_name = names(List_dataset)

Old_names = colnames(List_dataset[[1]])

dataset_name = data.frame()

for(DN in 1:length(List_dataset_name)){
  dataset_name[DN,paste("Col")]= sub(".c.*","",List_dataset_name[DN])
}

for(l in 1:length(List_dataset_name)){
  for (i in 1:length(Old_names)){
    if(substr(List_dataset_name[l],1,3)=="Las"){colnames(List_dataset[[l]])[which(colnames(List_dataset[[l]]) %in% Old_names[i] )] <- paste(Old_names[i],"Last",sep = "_")
    }else (colnames(List_dataset[[l]])[which(colnames(List_dataset[[l]]) %in% Old_names[i] )] <- paste(Old_names[i],"Previous",sep = "_")
    )
  }}





#str(List_dataset)





# ======= Dynamica Model Building activity 

Model_dep = colnames(Dep_Freq[2:ncol(Dep_Freq)])


for (i in 1:length(Model_dep)) {
  
  Model = paste("Model",i,sep="_")
  
  Final_Independent_data_ModelData = Final_Independent_data %>% select(.,-starts_with("Dep_"),-MSISDN) %>% names %>%unlist
  
  m = glm(as.formula(paste(Model_dep[i],paste(Final_Independent_data_ModelData,collapse = "+"),sep="~ ")),data=Final_Independent_data)
  
  New_model_Significant_coef = step(m,direction = "both")
  
  assign(Model,New_model_Significant_coef)
  
}



# ======== Using dplyr summarise in R with dynamic variable
# https://stackoverflow.com/questions/39252405/using-dplyr-summarise-in-r-with-dynamic-variable


library(lazyeval)
GraphVar <- "dist"

head(cars)

cars %>%
  group_by_("speed") %>%
  summarise_(Sum = interp(~sum(var, na.rm = TRUE), var = as.name(GraphVar)), 
             Count = ~n())


# ---------- 2 nd method

ccccc = colnames(iris[1:2])

col_index <- which(sapply(colnames(iris), function(x) any(x == "Sepal.Length")))

df2= iris%>%
  group_by(.[[col_index]]) %>%
  summarise(count = n()) %>%
  as.data.frame()


# -----------3 

iris

df1 = iris%>%
  group_by_at(vars(-starts_with("Sepal"))) %>% 
  summarise(mySum = sum(Petal.Length))


# ------------- 4

df4 = iris %>%
  group_by_(.dots = iris %>% select(contains("Sepal")) %>% colnames()) %>%
  summarise(mySum = sum(Petal.Length))

# =================== Stratified 



install.packages("splitstackshape")

library(splitstackshape)



# Stratified Sample


# Uses a subset of the Iris data set with different proportions of the Species factor
set.seed(42)
iris_subset <- iris[c(1:50, 51:80, 101:120), ]

stratified_sample <- iris_subset %>%
  group_by(Species) %>%
  mutate(num_rows=n()) %>%
  sample_frac(0.4, weight=num_rows) %>%
  ungroup

# These results should be equal
table(iris_subset$Species) / nrow(iris_subset)
table(stratified_sample$Species) / nrow(stratified_sample)



# ========= Method two

set.seed(42)
iris_subset <- iris[c(1:50, 51:80, 101:120), ]

stratified_sample <- iris[,1:4] %>%
  # group_by(Species) %>%
  # mutate(num_rows=n()) %>%
  # sample_frac(0.1, weight=num_rows)%>%
  sample_frac(0.1, replace=TRUE)%>%
  ungroup



View(stratified_sample)

summary(iris[,1:4])
summary(stratified_sample[,1:4])

# These results should be equal
table(iris) / nrow(iris)
table(stratified_sample) / nrow(stratified_sample)

#======================== Linear Regression loop for each independent variable individually against dependent

# https://stackoverflow.com/questions/25036007/linear-regression-loop-for-each-independent-variable-individually-against-depend

library(data.table)
Fits <- as.data.table(mtcars)[, list(MyFits = lapply(.SD[, -1, with = F], function(x) summary(lm(mpg ~ x))))]

Fits$MyFits


Fits[, lapply(MyFits, coef)]

Fits[, lapply(MyFits, function(x) x$r.squared)]

#  2nd method 

models <- lapply(paste("mpg", names(mtcars)[-1], sep = "~"), formula)
res.models <- lapply(models, FUN = function(x) {summary(lm(formula = x, data = mtcars))})
names(res.models) <- paste("mpg", names(mtcars)[-1], sep = "~")
res.models[["mpg~disp"]]


# ====================== Using R to do a regression with multiple dependent and multiple independent variables



head(mtcars)

for (i in 1:dim(mtcars)){
  print(paste("This is", i, "regression", "with dependent var",gsub("~","",myvar[i,1])))
  k[[i]]<-lm(as.formula(paste(myvar[i,1],paste(myvar[i,2:3],collapse="+"))),mydata)
  print(k[[i]])
}

# ---- Creating dummy var

# =========================  Check existence of directory and create if doesn't exist



ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)




# ===================== Reshape2======================================================

library(reshape2)

# require(reshape2)
x = data.frame(subject = c("John", "Mary"), 
               time = c(1,1),
               age = c(33,NA),
               weight = c(90, NA),
               height = c(2,2))

# --------- converting variable name into seperate column and all the values into separate column 

molten = melt(x, id = c("subject", "time"))

# ---------- Excluding na values using below query 

molten2 = melt(x,id=c("subject", "time"),na.rm = TRUE)


# dat44 = dcast(melt(norm44, id.vars = "rowname"), variable ~ rowname)

# --------- Reshaping data 

dcast(molten, formula = variable ~ time + subject )

#     time subject age weight height
# 1    1    John  33     90      2
# 2    1    Mary  NA     NA      2


dcast(molten, formula = subject + time  ~ variable)

#     subject time age weight height
# 1    John    1  33     90      2
# 2    Mary    1  NA     NA      2


dcast(molten, formula =   subject ~ variable)

dcast(molten, formula =  variable ~ subject)

dcast(molten, formula = time + subject ~ variable)
dcast(molten, formula = subject + time  ~ variable)
dcast(molten, formula = subject  ~ variable)
dcast(molten, formula = ...  ~ variable)

# It is also possible to create higher dimension arrays by using more than one ~ in the formula. For example,

acast(molten, formula = subject  ~ time ~ variable)



# ==================== Include most of the dependent packages ========================

install.packages("tidyverse")
install.packages("tidytext")


# =================== Class interval frequency  ======================================


# The same cuts for separate variables
# 
# If the distributions of your variables are very similar you could extract the exact cutpoints by setting the argument onlycuts = T and reuse them for the other variables. In case the distributions are different though, you will end up with few cases in some intervals.
# 
# Using your data:

# install.packages("Hmisc")

library(magrittr)
library(Hmisc)

df1 = runif(100,25,75)

df1 = data.frame(x = df1)

colnames(df1)[1]= "x"

head(df1)

cuts <- cut2(df1$x, g = 30, onlycuts = T) # determine cuts based on df1

ccc = cut2(df1$x, cuts = cuts) %>% table


# -------------- 2nd Method 



# Break the range into non-overlapping sub-intervals by defining a sequence of equal distance break points. If we round the endpoints of the interval [1.6, 5.1] to the closest half-integers, we come up with the interval [1.5, 5.5]. Hence we set the break points to be the half-integer sequence { 1.5, 2.0, 2.5, ... }.
breaks = seq(0, 75, by=10)    # half-integer sequence 
breaks 


# Classify the eruption durations according to the half-unit-length sub-intervals 
# with cut. As the intervals are to be closed on the left, and open on the right, 
# we set the right argument as FALSE.

duration.cut = cut(df1$x, breaks, right=FALSE)

# Compute the frequency of eruptions in each sub-interval with the table function.

duration.freq = table(duration.cut)

duration.freq1 = data.frame(duration.freq)

str(duration.freq1)

# ========================== Stratified sample =================================




# I would suggest using either stratified from my "splitstackshape" package, 
# or sample_n from the "dplyr" package:

## Sample data
set.seed(1)
n <- 1e4
d <- data.table(age = sample(1:5, n, T), 
                lc = rbinom(n, 1 , .5),
                ants = rbinom(n, 1, .7))
# table(d$age, d$lc)

library(data.table)

# For stratified, you basically specify the dataset, the stratifying columns, 
# and an integer representing the size you want from each group OR a decimal representing
# the fraction you want returned (for example, .1 represents 10% from each group).

library(splitstackshape)
set.seed(1)
out <- stratified(d, c("age", "lc"), 30)
head(out)
#    age lc ants
# 1:   1  0    1
# 2:   1  0    0
# 3:   1  0    1
# 4:   1  0    1
# 5:   1  0    0
# 6:   1  0    1

table(out$age, out$lc)
#    
#      0  1
#   1 30 30
#   2 30 30
#   3 30 30
#   4 30 30
#   5 30 30

# For sample_n you first create a grouped table (using group_by) and then specify the number of observations you want. If you wanted proportional sampling instead, you should use sample_frac.

library(dplyr)
set.seed(1)
out2 <- d %>%
  group_by(age, lc) %>%
  sample_n(30)

# table(out2$age, out2$lc)

# ================== Split a large dataframe into a list of data frames 
#                   based on common value in column



# If you want individual object with the group g names you could 
# assign the elements of X from split to objects of those names, 
# though this seems like extra work when you can just index 
# the data frames from the list split creates.

#I used lapply just to drop the third column g which is no longer needed.
Y <- lapply(seq_along(X), function(x) as.data.frame(X[[x]])[, 1:2]) 

#Assign the dataframes in the list Y to individual objects
A <- Y[[1]]
B <- Y[[2]]
C <- Y[[3]]
D <- Y[[4]]
E <- Y[[5]]

#Or use lapply with assign to assign each piece to an object all at once
lapply(seq_along(Y), function(x) {
  assign(c("A", "B", "C", "D", "E")[x], Y[[x]], envir=.GlobalEnv)
}
)

# Edit Or even better than using lapply to assign to the global environment use list2env:

names(Y) <- c("A", "B", "C", "D", "E")

list2env(Y, envir = .GlobalEnv)


# -------- 3
(iris)


Product_Subset = split(iris,iris$Species)

a = lapply(Product_Subset,FUN=function(i)sapply(i[,1:4],mean))


library(tibble)

new_one = data.frame()

for(i in 1:3){
  a1= data.frame(a[i])
  
  a2 = data.frame(t(a1))
  
  a3 = rownames_to_column(a2,var="Stype")
  
  new_one = rbind(new_one,a3)
  
}





# ======== List of Package ================

for (package_name in sort(loadedNamespaces())) {print(paste(package_name, packageVersion(package_name)))}


# ======= To find size of  data.frame =============
 
object.size(Model_data)
#120017224 bytes
print(object.size(Model_data),units="Gb")



# ======== using contains like condition using filter in dplyr ====

attach(mtcars)

mtcars$type <- rownames(mtcars)


library(dplyr)
View(mtcars)

aa= filter(mtcars, grepl('Toyota|Mazda', type))


# ======== How can I remove all objects except one from the workspace in R?---


rm(list=setdiff(ls(), "x"))


# ========== Using DPLYR to sum across the row and column=======


# --- sum down each column---

df %>%
  replace(is.na(.), 0) %>%
  summarise_each(funs(sum))

# ----sum up each row----

df %>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(.[1:5]))



# ===== Standardization using following function =====

Standardization = function(x){
  return((x-mean(x))/sd(x))
}

data_zstd = as.data.frame(laddply(data,Standardization))


# ==== Multiple replace of string =======

mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}


# -------------Change the Column Names ----------
# of all the Data Frames in the Global Environment from the following list

df1 <- data.frame(A = 1, B = 2, C = 3)
df2 <- data.frame(X = 1, Y = 2, Z = 3)
dfList <- list(df1,df2)
colnames <- c("U","W","Y")

# Then, lapply over the list using setNames and supply the vector of new column 
# names as second argument to setNames:
  
  lapply(dfList, setNames, colnames)


  # If you want to assign the data.frames back to the global environment,
  # you can modify the code like this:
  
  dfList <- list(df1 = df1, df2 = df2)
  
  list2env(lapply(dfList, setNames, colnames), .GlobalEnv)
  
  
# ---------- Deleting the column with NA values ----

DatasetNAme[,colSums(is.na(DatasetNAme))!=nrow(DatasetNAme)]


# - Creating difference between lastcolumn with previous column of the dataframe in list ====


Diff_btw_LastWk_lastbut1Wk = new.env()


for (i in seq_along(Non_rev)){
  num_colm_Non_rev = ncol(Non_rev[[i]])# Gives number of column in respective objective
  lastbutone = num_colm_Non_rev-1
  assign(paste("Diff",Name_NonRev[i],sep="_"),envir =Diff_btw_LastWk_lastbut1Wk,(Non_rev[[i]][num_colm_Non_rev]-Non_rev[[i]][lastbutone])/Non_rev[[i]][lastbutone]  )
}



Diff_LastWk_LastbutWk_Drop = mget(ls(envir=Diff_btw_LastWk_lastbut1Wk),envir = Diff_btw_LastWk_lastbut1Wk)




# ====== Change the Last column name ==================

Rename_ATR = names(ATR_Avg_Values)



for(i in 1:length(Rename_ATR)){
  names(ATR_Avg_Values[[i]])[1] = paste("ATR",Rename_ATR[i],sep = "_")
}



# ================ Changing the Column Name ===========

# If you need to rename not all but multiple column at once when you only know the old column names you can use colnames function and %in% operator. Example:
  
  # df = data.frame(bad=1:3, worse=rnorm(3), worst=LETTERS[1:3])
  
  # bad      worse    worst
  # 1   1 -0.77915455       A
  # 2   2  0.06717385       B
  # 3   3 -0.02827242       C
  # Now you want to change "bad" and "worst" to "good" and "best". You can use
  
  colnames(df)[which(colnames(df) %in% c("bad","worst") )] <- c("good","best")
  
  
  # This results in
  
  # good      worse  best
  # 1    1 -0.6010363    A
  # 2    2  0.7336155    B
  # 3    3  0.9435469    C



# ======================== Good web page ids ============

# http://www.statmethods.net/graphs/scatterplot.html

# https://rpubs.com/nishantsbi/92510
# TExt mining 


# http://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html

# https://sebastianbarfort.github.io/sds/slides/lecture3.html#5

# http://walkerke.github.io/

# http://www.r-datacollection.com/materials/



# =================== Using ggplot to plot categorical verses continous


library(broom)
library(ggplot2)
library(tidyverse)


glimpse(Final)

grp = Final

grp$Accepted_Rejected=factor(Final$Accepted_AccptNot)

grp$Accepted_AccptNot = NULL

colnames_1 <- dimnames(grp)[[2]]

for ( i in 1:(ncol(grp)-1)){
  # pdf(file=paste("Category_Continous",1,".pdf",sep=""))
  ptl1 = ggplot(grp,aes(x=Accepted_Rejected,y=grp[,i]))+geom_boxplot()+labs(x="Accepted_Rejected",y=colnames_1[i])
  ggsave(paste("Category_Continous",i,".pdf",sep=""))
}

# ======= Changing the column name in loop for multiple data frame

df1 <- data.frame(A = 1, B = 2, C = 3)
df2 <- data.frame(X = 1, Y = 2, Z = 3)
dfList <- list(df1,df2)
colnames <- c("USAF","WBAN","YR--MODAHRMN")


lapply(dfList, setNames, colnames)



# ========= Find the path for R  library and bin in computer ========

gsub("library", "bin", .libPaths())


# ========== To find the previous or lag value=========

library(dplyr)

d = data.frame(name=rep(c("r","y",""),5))


d %>% 
  mutate(previous = if_else(name=="",lag(d$name),name))

# ----------- 2 version of it----------

input=data.frame(x=c('m','m','m','m',"","",'n','',"",'m')) 
input[input$x == "", "x"] <- NA
library(zoo)
input$x <- na.locf(input$x) #  NA value get replace by previous value

# --------- using "apply" function to convert all the columns with 2 decimal=========

Diff_Dependent_Round = apply(Diff_Dependent,2,function(x)round(x,digits =2)) 


# -------- Replacing 'NaN' value into 0

x <- data.frame(X1=sample(c(1:3,NaN), 200, replace=TRUE), X2=sample(c(4:6,NaN), 200, replace=TRUE))
head(x)
x <- replace(x, is.na(x), 0)
head(x)




# Dynamically spliting based on the variable pattern

x=data.frame(`20TT`=c(1,2),`30TT`=c(2,3),`m3G`=c(2,3),`N3G`=c(3,5))

a=split.default(x,sub(".*3","",names(x)))





# ==================== Handling error related to Hmisc
library(Hmisc)

# Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  # there is no package called 'data.table'
# Error: package or namespace load failed for 'Hmisc'

# 

# remove.packages(c("Hmisc"))

install.packages('Hmisc', dependencies = TRUE)

# ========================== Selecting only numerical Columns

x <- data.frame(v1=1:20,v2=1:20,v3=1:20,v4=letters[1:20])

# Since a data frame is a list we can use the list-apply functions:
  
  nums <- sapply(x, is.numeric)
  # Then standard subsetting
  
  x[ , nums]


# ========================= Contingency Table 

# install.packages("tidyr")

bevs <- data.frame(cbind(name = c("Bill", "Llib"),
                         drink = c("coffee", "tea", "cocoa", "water"), 
                         cost = seq(1:8)))

bevs = bevs[,-3]

# ----------------------- 1st 
library(tidyr)

#library(tidyr)
hj <- bevs %>% gather() %>% table() %>% t()

# ------------------------ 2nd 

bnm<- apply(bevs, 2, function(x) table(factor(x, levels = unique(unlist(bevs)))))


# ---------- 3rd method


Table_count_Matrix_format = table(bevs[,c('name','drink')])

# ================================== Diagonal Matrix 

size <- 6
mat <- matrix(seq_len(size ^ 2), ncol = size)


low <- 0
high <- 3

delta <- rep(seq_len(ncol(mat)), nrow(mat)) - 
  rep(seq_len(nrow(mat)), each = ncol(mat))
#or Ben Bolker's better alternative
delta <- row(mat) - col(mat)
mat[delta < low | delta > high] <- NA
mat




# ================================ Counting the number of elements with the values of x 
# ==============================   in a vector or getting frequency of the values

numbers <- c(4,23,4,23,5,43,54,56,657,67,67,435,
             453,435,324,34,456,56,567,65,34,435)

a <- table(numbers)

as.data.frame(table(numbers))


#====================== Building Model and sink this information to respective folder

mainDir <-"G:\\sample data\\Zeppelin Using\\Final"
subDir <- "final_4"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))
dir()
getwd()



# ======================= To Create the automatic list with data.frame

# ===================== Assigning the data frame to the list

# ------------------ Method 1


e1 <- new.env()

mmm= data.frame(c(1,2,3,4))

colnames(mmm)=c('Name')

for ( i in 1:4)
{
  # assign(paste("Modl",i,sep="_"),envir = e1, rpart (lm[,i]~., data=lm, control=rpart.control(maxdepth=7,cp=0.0002), method="class",parms=list(split="gini")))
  assign(paste("Modl",i,sep="_"),envir = e1, mmm[i,])
  
  }

mod <- mget(ls(envir=e1), envir = e1)


# ------------------- Method 2

EOG2006 = data.frame()
EOG2007 = data.frame()
EOG2008 = data.frame()



lsEOG<-list()

for (j in 2006:2008){
  z <- j
  sEOG <- paste("EOG", z, sep="")
  dEOG <- get(paste("EOG",z, sep=""))
  lsEOG[[sEOG]] <-dEOG
}


#============================ Used to find the maximum value in the column

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colSort <- function(data, ...) sapply(data, sort, ...)

Max_mum_values = colMax(Monthly_IM_Summary)

# ======================== Rearrange of the column names
ordered_columns_leftside=c('var10','var34','var8')
df=df[c(ordered_columns_leftside, setdiff(names(df),ordered_columns_leftside))]

# -------------------------

Move_target_last <- function(data,move){
  data[c(setdiff(names(data),move),move)]
}

Pre_sec_sim_only_alt <- Move_target_last(Pre_sec_sim_only_alt,c("SIM_TYPE"))

# ------------------ sorting or ording the columns which as character with numeric

library(gtools)


df[mixedorder(names(df))]


# ====================================

library(lubridate)

# M_ = as.numeric(format(now(),"%m"))
# 
# D_ = as.numeric(format(now(),"%d"))
# 
# Y_ = as.numeric(format(now(),"%Y"))
# 
# Last_1_Week = now()- days(7)
# 
# Last_1_Week


# =========================================
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
system("unzip world_shape_file.zip")


# ================ Automating the models



for ( i in 8:ncol(lm[8:31]))
{
  assign(paste("Modl",i,sep="_"),rpart (lm[,i]~., data=lm, control=rpart.control(maxdepth=7,cp=0.0002), method="class",parms=list(split="gini")))
}


# ============================ Having 50:50 Ratio of Data 

#  ---------------- Sample Indexing

indexes = sample(1:nrow(Pri_sec_sim_only), size = 0.25 * nrow(Pri_sec_sim_only))



# ----------------- Split data 


Test_data = Pri_sec_sim_only[indexes,]

Train_data = Pri_sec_sim_only[-indexes,]

dim(Test_data)

dim(Train_data)

table(Train_data$SIM_TYPE==1,Train_data$SIM_TYPE==0)

str(Train_data)




#  ---------------- Modifying Sample Indexing into 50 :50 of 0 and 1

Having_1 <- subset(Pri_sec_sim_only,Pri_sec_sim_only$SIM_TYPE==1,select=Age:SIM_TYPE)

Having_0 <- subset(Pri_sec_sim_only,Pri_sec_sim_only$SIM_TYPE==0,select=Age:SIM_TYPE)

dim(Having_1)

dim(Having_0)


Train_data <- rbind(Having_1[1:140,],Having_0[1:140,])

Test_data <- rbind(Having_1[141:175,],Having_0[141:175,])

#  Randomizing the data or reshuffling the data

Train_data <- Train_data[sample(1:nrow(Train_data)),]
head(Train_data)



dim(Train_data)
dim(Test_data)

table(Test_data$SIM_TYPE==1,Test_data$SIM_TYPE==0)

table(Train_data$SIM_TYPE==1,Train_data$SIM_TYPE==0)



# =================================== Calculating the percentile for each column value

Assign_Perce_value = data.frame(seq(1,nrow(Difference_IndepVar)))

colnames(Assign_Perce_value) = c("Num_obs")

for ( col_ in 1 : ncol(Difference_IndepVar))

  {
b=col_
Assign_Perce_value[paste("Percet",Per_Indep[col_],sep = "_")]=
    ifelse(Difference_IndepVar[b]<Cal_Percentile[1,b],Cal_Percentile[1,11],
    ifelse(Difference_IndepVar[b]>=Cal_Percentile[1,b]&Difference_IndepVar[b]<Cal_Percentile[2,b],Cal_Percentile[1,11],
    ifelse(Difference_IndepVar[b]>=Cal_Percentile[2,b]&Difference_IndepVar[b]<Cal_Percentile[3,b],Cal_Percentile[2,11],
    ifelse(Difference_IndepVar[b]>=Cal_Percentile[3,b]&Difference_IndepVar[b]<Cal_Percentile[4,b],Cal_Percentile[3,11],                 
    ifelse(Difference_IndepVar[b]>=Cal_Percentile[4,b]&Difference_IndepVar[b]<Cal_Percentile[5,b],Cal_Percentile[4,11], 
    ifelse(Difference_IndepVar[b]>=Cal_Percentile[5,b]&Difference_IndepVar[b]<Cal_Percentile[6,b],Cal_Percentile[5,11],
    ifelse(Difference_IndepVar[b]>=Cal_Percentile[6,b]&Difference_IndepVar[b]<Cal_Percentile[7,b],Cal_Percentile[6,11],
    ifelse(Difference_IndepVar[b]>=Cal_Percentile[7,b]&Difference_IndepVar[b]<Cal_Percentile[8,b],Cal_Percentile[7,11],
    ifelse(Difference_IndepVar[b]>=Cal_Percentile[8,b]&Difference_IndepVar[b]<Cal_Percentile[9,b],Cal_Percentile[8,11],
           Cal_Percentile[9,11])))))))))     
           
  }
  

           
           
           
# ========================== IF ELSE statement


if(client=='private'){
  tot.price <- net.price * 1.12
} else if(client=='public'){
  tot.price <- net.price * 1.06
} else {
  tot.price <- net.price
}




# ======================R - Assign column value based on closest match in second data frame 
# ======================= Lookup value from the two table to get the nearest values

logger <- data.frame(
  time = c(1280248354:1280248413),
  temp = runif(60,min=18,max=24.5)
)

df <- data.frame(
  obs = c(1:10),
  time1 = runif(10,min=1280248354,max=1280248413),
  temp = NA
)

logger
df

#  Load package
require( data.table )

#  Make data.frames into data.tables with a key column
ldt <- data.table( logger , key = "time" )
dt <- data.table( df , key = "time1" )

#  Join based on the key column of the two tables (time & time1)
#  roll = "nearest" gives the desired behaviour
#  list( obs , time1 , temp ) gives the columns you want to return from dt
fin = ldt[ dt , list( obs , time1 , temp ) , roll = "nearest" ]
#          time obs      time1     temp

fin = fin[order(obs),]


# ======================Calculating percentile of dataset column

set.seed(123)
xxc <- rnorm(100)
xxc= data.frame(xxc)
quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1)) # quartile
quantile(x, probs = seq(0, 1, by= 0.1)) # decile


Target_Table= data.frame(seq(from=1,to=nrow(Row_Total_Lastmonth)))

qartile_val = data.frame(seq(from=1,to=nrow(xxc)))

qqqqq = function(x)
{
  qartile_val[paste0("D",i,sep="_")]
}


# ====================== Percentile Rank for the group of variable 

perc.rank <- function(x) trunc(rank(x))/length(x)

set.seed(123)
my<- data.frame(x=rnorm(200),y=rnorm(200))
my_ <- within(my, xr <- perc.rank(x))

head(my_)

#======================== how to calculate 95th percentile of values with grouping variable in R or Excel

DF <- data.frame('watershed'=sample(c('a','b','c','d'), 1000, replace=T), wq=rnorm(1000))

head(DF)

with(DF, tapply(wq, watershed, quantile, probs=0.95))


#========================  Converting to factor


lm[,1:15] <- lapply(lm[,1:15] , factor)


# =========== Creating Dummy Variable 
#Generate example dataframe with character column
example <- as.data.frame(c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))
names(example) <- "strcol"

#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(example$strcol)){
  example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
}


#===================== Pivot to find the summary data

# install.packages("devtools")
# install.packages("rpivotTable")

library(devtools)

# install_github("ramnathv/htmlwidgets") 
# install_github("smartinsightsfromdata/rpivotTable")

library(rpivotTable)

Depend_var_Lastmonth

rpivotTable(mtcars, rows="gear", col="cyl", aggregatorName="Average", 
            vals="mpg", rendererName="Table")

# =====================
# sorting examples using the mtcars dataset
attach(mtcars)

# sort by mpg
newdata <- mtcars[order(mpg),] 

# sort by mpg and cyl
newdata <- mtcars[order(mpg, cyl),]

#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),] 

detach(mtcars)


# ================================= Converting Number to Month name or month abbrevation


set.seed(1)

df <- data.frame(A = runif(10), Month = sample(12, 10, replace = TRUE))

with(df, month.abb[Month])

df <- transform(df, MonthAbb = month.abb[Month])

# ============================


today <- Sys.Date()

aaa = cat(sprintf('title: NYC Weekly %s\n', today))

as.symbol(paste0("aaa",today))


# Convert type of multiple columns of a dataframe at once


foo<-data.frame( 
                y=c("red", "red", "red", "blue", "blue", 
                    "blue", "yellow", "yellow", "yellow", 
                    "green")
                )

str(foo)

convert.magic <- function(obj,types){
  for (i in 1:length(obj)){
    FUN <- switch(types[i],character = as.character, 
                  numeric = as.numeric, 
                  factor = as.factor)
    obj[,i] <- FUN(obj[,i])
  }
  obj
}

for (i in 1:1)
{
assign(paste0("out",i),convert.magic(paste0("foo"),c('character')))
}
str(out)




# This helps in seperating character from numeric values in a column

# Converting Character into Numeric and extracting only character value

Distinct_Columns$numeric_1 = Distinct_Columns$Distinct_Var

Distinct_Columns$numeric=as.numeric(as.character(Distinct_Columns$numeric))


Distinct_Columns$numeric_1 = as.character(Distinct_Columns$numeric_1)

new_DF <- Distinct_Columns[is.na(Distinct_Columns$numeric),]

str(new_DF)


# ========================= Dynamically creating data frame

for (i in 1:5) {
  assign(paste0("DF", i), data.frame(A=rnorm(10), B=rnorm(10)))
}


# ======================== using formula function

listoffactors <- c("factor1","factor2")
as.formula(paste("y~",paste(listoffactors,collapse="+")))

# ======================= finding difference between rows

set.seed(4871)
m = matrix(sample(1:5,50,TRUE),nrow=10,ncol=5)
m
t(apply(m,1,diff))



#======================= using 


Else in different format

quadratic.formula = function (a, b, c)
{
  rad <- b^2 - a * 1 * c
  if(is.complex(rad) || all(rad >= 0)) {
    rad <- sqrt(rad)
  } else {
    rad <- sqrt(as.complex(rad))
  }
  cbind(-b - rad, -b + rad) / (2 * a)
}



quadratic.formula(1,-5,6)

# ======================= Website link for quantstrategy

# https://www.linkedin.com/pulse/20141124133216-78438968-list-of-r-package-for-back-testing-quantitative-trading-strategies


# ======================== Sprintf function

sprintf("%s is %f feet tall", "Sven", 7.1) 

sprintf("%s is %i feet tall", "Sven", 7 ) 

sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%1.0f", pi)
sprintf("%5.1f", pi)
sprintf("%05.1f", pi)
sprintf("%+f", pi)
sprintf("% f", pi)
sprintf("%-10f", pi) # left justified
sprintf("%e", pi)
sprintf("%E", pi)
sprintf("%g", pi)
sprintf("%g",   1e6 * pi) # -> exponential
sprintf("%.9g", 1e6 * pi) # -> "fixed"
sprintf("%G", 1e-6 * pi)

## no truncation:
sprintf("%1.f", 101)

n <- 1:18
sprintf(paste0("e with %2d digits = %.", n, "g"), n, exp(1))


## Using arguments out of order
sprintf("second %2$1.0f, first %1$5.3f, third %3$1.0f", pi, 2, 3)

## Using asterisk for width or precision
sprintf("precision %.*f, width '%*.3f'", 3, pi, 8, pi)

## Asterisk and argument re-use, 'e' example reiterated:
sprintf("e with %1$2d digits = %2$.*1$g", n, exp(1))

## re-cycle arguments
sprintf("%s %d", "test", 1:3)



# ------- Best on Book on String handleing ----- Handling and Processing String in R


# Using grepl to find the 

extra_data_1 <- subset(a_1,grepl("MSISDN",a_1$`START.OF.EXECUTION`, ignore.case = TRUE))


aaa = grep("C", c("b","A","C"),ignore.case = FALSE, fixed=TRUE)

#  how to find the position of the character with in the string in r

x <- "1234_5" 
a = regexpr("_",x) 
# a = as.numeric(a[1]) ------ is used to convert integer to numerical
# nchar(c("1234_5"))   ------- is used to convert 

fiinal = substr(x, as.numeric(a[1]), nchar(c("1234_5")))

using_sub = sub(fiinal,"",x)

# ============= seperating the second string

# I had a variable in a melted table that had _ as a separator and made two 
# separate variables for the prefix and suffix based on @Grothendieck answer: 
# prefix <- sub("_.*", "", variable) 
# and suffix <- sub(".*_", "", variable) - swihart Nov 13 '15 at 19:45

prefix <- sub("_.*", "", x) 
suffix <- sub(".*_", "", x)


# ---------------- 2nd Method like gsub or sub

string = c("G1:E001", "G2:E002", "G3:E003")

fir = sub(".*:", "", string)

# --------------3rd method to extract or split the character

string = c("G1:E001", "G2:E002", "G3:E003")

split_1 = strsplit(string,":")

# output [1] "G1"   "E001"  


# ------------------- Best method to find specific character from the string 

unlist(gregexpr(pattern ='2',"the2quickbrownfoxeswere2tired"))

# ---------------------4th method to split

library(stringi)
str = stri_locate_all(pattern = '2', "the2quickbrownfoxeswere2tired", fixed = TRUE)


# Box plot 

# for (Treshold in seq(from=0.01,to=0.5,by=.01))

Data_for_analysis = KEL_Churn_Raw[,4:91]

str(Data_for_analysis)

Selected_columns = colnames(KEL_Churn_Raw[,4:91])

# To find the whether their are even or odd number of variables in the dataset

colnumber_= ifelse(length(Selected_columns)%%2==0,length(Selected_columns),length(Selected_columns)-1)


# Find the number of prameters in chart


Numberofcolumns=4

while(Numberofcolumns < 50)
{
  Numberofcolumns <- Numberofcolumns+1;
  Even = colnumber_ %% Numberofcolumns;
  numberofvariable = colnumber_/Numberofcolumns
  print(Numberofcolumns);
  if(Even==0) break;
}


setwd("D:\\BOBR\\Data\\Box Plot")
for (i in seq(from=1,to=colnumber_,by = numberofvariable))
{
  
  pdf(file = paste("boxplot",i,".pdf",sep = ""))
  a=i
  b= (a+numberofvariable)-1
  plot = boxplot(Data_for_analysis[,a:b],las=2,par(mar=c(12,5,4,2)+0.1))
  dev.off()
}

setwd("D:\\BOBR\\Data")



# To look at the Distribution of the data



#  To find the boundary values or "mar" values and previous values were 5.1 4.1 4.1 2.1
# par(mar=c(5.1,4.1,4.1,2.1))

par("mar")


# Automating the Density plot


setwd("D:\\BOBR\\Data\\DensityPlot")


for (i in seq(from=1,to=colnumber_,by = numberofvariable))
{
  pdf(file = paste("Density",i,".pdf",sep = ""))
  
  a=i
  b= (a+numberofvariable)-1
  
  par(mar=c(2,2,2,2))
  par(mfrow=c(4, 4))
  colnames <- dimnames(Data_for_analysis)[[2]]
  
  for (i in a:b) {
    d <- density(Data_for_analysis[,i])
    plot(d, type="n", main=colnames[i])
    polygon(d, col="red", border="red")
  }
  dev.off()
}  




# ========================Normalizing the data

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

Data_for_analysis_1 <- as.data.frame(lapply(Data_for_analysis,normalize))




# ====================== while loop


# Break example in While loop
x <- 1
while(x < 5) {x <- x+1; if (x == 3) break; print(x); }


# Next example in While loop
x <- 1
while(x < 5) {x <- x+1; if (x == 3) next; print(x);}


# ====================== To findout which column has highest  and lowest value

DF <- data.frame(TT_50=c(2,8,1),TT_51=c(7,3,5),FTT_53=c(9,6,4))

Highest <- colnames(DF)[apply(DF,1,which.max)]

Lowest <- colnames(DF)[apply(DF,1,which.min)]

Highest_n <- apply(DF,1,which.max)

Lowest_n <- apply(DF,1,which.min)


Final_df <- cbind(DF,Highest_n,Highest,Lowest,Lowest_n)

# --------------- Finding column with 1 and 0 

DF_1_0 <- data.frame(TT_50=c(0,0,1),TT_51=c(1,0,1),FTT_53=c(1,1,1))

find_1 = colnames(DF_1_0)[apply(DF_1_0,1,which.max)]

Final_DF_1 = cbind(DF_1_0,find_1)





#======================= To find the path of the libarary

gsub("library", "bin", .libPaths())



#======================  Use the optimal number of variables selected at each split and run random forest again

rf_again = randomForest(Target~.,data=Pri_sec_sim_only_1,mtry=best.m,importance=TRUE,ntree=200)

print(rf_again)


# Below is the code to save the fitted formula for futures use

saveRDS(rf_again,"Randome_forest_Delhi_circle.rds")


# To use the code next time just load it and use it on the new set of data

rf_Delhi_MultiSIM = readRDS("Randome_forest_Delhi_circle.rds")



#========================= Moving dependent or target variable to last 

Move_target_last <- function(data,move){
  data[c(setdiff(names(data),move),move)]
}


Pre_sec_sim_only_alt <- Move_target_last(Pre_sec_sim_only_alt,c("SIM_TYPE"))

#======================== Code for normalization 


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



#  Log transformation 


signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}



# I have to assume you meant to say that you wanted a mean of 0 and a 
# standard deviation of 1. 
# If your data is in a dataframe and all the columns are numeric you can 
# simply call the scale 
# function on the data to do what you want.

dat <- data.frame(x = rnorm(10, 30, .2), y = runif(10, 3, 5))

scaled.dat <- scale(dat)

# check that we get mean of 0 and sd of 1
colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.dat, 2, sd)




# -----------

library(plyr)

US_NYC_Term_Logistic$Marketcode_lk <- US_NYC_Term_Logistic$Marketcode

US_NYC_Term_Logistic$Programcode_lk <- US_NYC_Term_Logistic$Programcode

US_NYC_Term_Logistic$Productcode_lk <- US_NYC_Term_Logistic$Productcode




Market_numeric <- unique(US_NYC_Term_Logistic$Marketcode)

Program_numeric <- unique(US_NYC_Term_Logistic$Programcode)

Product_numeric <- unique(US_NYC_Term_Logistic$Productcode)


# Unique_Market_code <- unique(US_NYC_Term_Logistic$Marketcode)
# 
# Unique_Program_code <- unique(US_NYC_Term_Logistic$Programcode)


#  Converting data to 



length(Market_numeric)

str(Market_numeric)

str(Program_numeric)

str(Product_numeric)


US_NYC_Term_Logistic$Marketcode <- mapvalues(US_NYC_Term_Logistic$Marketcode , from = Market_numeric , to= c(1:length(Market_numeric)) )

US_NYC_Term_Logistic$Programcode <- mapvalues(US_NYC_Term_Logistic$Programcode, from = Program_numeric , to = c(1:length(Program_numeric)))

US_NYC_Term_Logistic$Productcode <- mapvalues(US_NYC_Term_Logistic$Productcode, from = Product_numeric , to = c(1:length(Product_numeric)))



str(US_NYC_Term_Logistic)


US_NYC_Term_Logistic$Marketcode <- as.integer(US_NYC_Term_Logistic$Marketcode)
US_NYC_Term_Logistic$Programcode<- as.integer (US_NYC_Term_Logistic$Programcode)
US_NYC_Term_Logistic$Productcode<- as.integer (US_NYC_Term_Logistic$Productcode)




#======================= Sample Indexing

indexes = sample(1:nrow(Pri_sec_sim_only), size = 0.25 * nrow(Pri_sec_sim_only))



# ====================== Split data 


Test_data = Pri_sec_sim_only[indexes,]

Train_data = Pri_sec_sim_only[-indexes,]

dim(Test_data)

dim(Train_data)

str(Train_data)

table(Primary=Train_data$SIM_TYPE==1,Secondary=Train_data$SIM_TYPE==0)







# ====================== Detail information on all the variables

library(Hmisc)

sink("Describe_DEL_3.txt",append = TRUE,split = TRUE)

describe(Pri_sec_sim_only)

sink()



#======================= Replacing outlier value to .95 percentile value

for(i in c(1:7)) {
  
  
  q= Pri_sec_sim_only[,i]
  
  Percentile_ <- quantile(q,c(.95))
  
  Pri_sec_sim_only[,i][Pri_sec_sim_only[,i] >Percentile_] <- Percentile_
}





#======================= Missing Value in Age is changed with the median value 

f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}

length(Pri_sec_sim)

Pri_sec_sim_only_1 <- data.frame(apply(Pri_sec_sim[,2:26],2,f))



#======================== Taking only those columns which are integer


Ext_US_NYC_INT_v1 <- sapply(Ext_US_NYC, is.integer)



Ext_US_NYC_INT_v1 <- Ext_US_NYC[,Ext_US_NYC_INT_v1]





#====================== Sum product


a=data.frame(c_1=c(1,2,3),c_2=c(10,20,30))
b=data.frame(c2_1=c(1,1,1),c2_2=c(2,2,2))

a
b

data.frame(rowSums(a*b))



#======================== How to calculate harmonic Mean

# install.packages("mmod")

library(mmod)


a <- runif(25,min = 1,max=45)
print(a)

harmonic_mean(a)
mean(a)


# ====================== Assigning dataframe to specific parameters

#  Creating separate data frame for separate classified variables
# Applying Split funtion along with seperate value of the column 

head(iris)

str(iris)

# First split 
iris_split <- split(iris, iris$Species)

new_names <- as.character(unique(iris$Species))

for (i in 1:length(iris_split)) {
  assign(new_names[i], iris_split[[i]])
}





#  restricting the number of digits


size <- readline("how many digits do you want?")

options(digits = size)


pi   # pi the scientific value 

size

scan()
2 3 4 5
4 4 9 0


#  To see the list of packages

library()



#  XGBOOST coding 
# Data Frames and Transactions

data("AdultUCI")

str(AdultUCI)

library("arules");

data("AdultUCI");

Adult = as(AdultUCI, "transactions");


my_data = paste("1,2","1","2,3", sep="\n")

write(my_data, file = "my_basket");

trans = read.transactions("my_basket", format = "basket", sep=",");

inspect(trans);


## example 4: creating transactions from a data.frame with transaction IDs and items
a_df3 <- data.frame(
  TID = c(1,1,2,2,2,3),
  item=c("a","a","a","b","c", "b")
)

a <- split(a_df3[,"item"], a_df3[,"TID"])

trans4 <- as(split(a_df3[,], a_df3[,"TID"]), "transactions")

str(a_df3)




# 

trans = read.transactions("some_data.csv", format = "single", sep = ",", cols = c("transactionID", "productID"))


#  Ranking or to find the max, min ,small, large

ran3 <-data.frame()

a <- runif(15,min = 1,max = 10)
b <- runif(15,min=5, max=25)
c <- runif(15,min = -5,max = 5)

a<- data.frame(a)
b<- data.frame(b)
c<- data.frame(c)

d <- cbind(a,b,c)

str(d)


# Ranking the row based on the values
Ranking_lowest_rowvalues= data.frame(d,t(apply(d,1,rank)))

# Ranking the row based on the values [if we add minus to the name of the data frame the
#                                     then the rank would be based on the highest value]
Ranking_highest_rowvalues = data.frame(d,t(apply(-d,1,rank)))




# ================== Second method find 

df = data.frame( car = c (2,1,1,1,0), bus = c (0,2,0,1,0),
                 walk = c (0,3,2,0,0), bike = c(0,4,0,0,1))

# df$max = max.col(df,ties.method="first")

# df$val = apply(df[ ,1:4], 1, max)


# a function that returns the position of n-th largest
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]


 # position of the largest
  apply(df, 1, maxn(1))

  # position of the 2nd largest
  apply(df, 1, maxn(2))
  
  # value of the largest #colnames(df) 
dd= apply(df, 1, function(x)x[maxn(1)(x)])
dd= apply(df, 1, function(x)x[maxn(2)(x)])


#  Indexing the column name or getting the column name for the highest value
dd = colnames(df)[apply(df, 1, maxn(1))]
dd_2 =colnames(df)[apply(df,1,maxn(2))]


dd = t(dd)
dd_2 = t(dd_2)
# dd = data.frame(dd)

dd = as.data.frame(dd)
dd_2=as.data.frame(dd_2)

f =cbind(df,dd,dd_2)
  
 
# ========================

 final_Data_KEL <- data.frame()

# for ( i in (sample$Column_no))
 for ( i in (sample$X))
{

  ch <- a[i:(i+15),]
  ch1 <- data.frame(ch)
  final_Data_KEL <- rbind(final_Data_KEL,ch1)
}


# ---------- Loopsing and finding Treshold for GLM classifier
 
 
 
 
 #  To determine the treshold 
 
 Final_result <- data.frame(rbind())
 
 for (Treshold in seq(from=0.01,to=0.5,by=.01))
 {
   
   pred$Predicted_Class <- ifelse(pred$predict_reg > Treshold,1,0)
   
   Cross_table <- table(Actual_termination=pred$Actual_term,Predicted_termination=pred$Predicted_Class)
   
   Cross_Matrix <- matrix(Cross_table,nrow=2,ncol=2)
   
   Actual_terminated <- Cross_Matrix[2,1]+Cross_Matrix[2,2]
   
   Actual_Nonterminated <-Cross_Matrix[1,1]+Cross_Matrix[1,2]
   
   True_positive <- Cross_Matrix[2,2]
   
   True_Negative <- Cross_Matrix[1,1]
   
   False_Positive <- Cross_Matrix[1,2]
   
   False_Negative <- Cross_Matrix[2,1]
   
   
   TotalObservation = sum(Cross_Matrix[1:4])
   
   True_positive_Rate <- Cross_Matrix[2,2]/Actual_terminated
   
   True_Negative_Rate  <- Cross_Matrix[1,1]/Actual_Nonterminated
   
   False_Positive_Rate  <- Cross_Matrix[1,2]/Actual_Nonterminated
   
   False_Negative_Rate  <- Cross_Matrix[2,1]/Actual_terminated   
   
   Overall_accuracy = (True_positive+True_Negative)/TotalObservation
   
   Error_rate <- (False_Positive+False_Negative)/TotalObservation
   
   Sensitivity = True_positive_Rate
   
   Specifycity = True_Negative_Rate
   
   Overall_Predit_true = True_positive +False_Positive
   
   Per_PredTrue = True_positive / Overall_Predit_true
   
   Per_PredFalse = False_Positive/Overall_Predit_true
   
   a = data.frame(cbind(Treshold,True_positive,True_Negative,False_Positive,False_Negative,TotalObservation
                        ,Actual_terminated,Actual_Nonterminated,True_positive_Rate,True_Negative_Rate,False_Positive_Rate,False_Negative_Rate
                        ,Overall_accuracy,Error_rate,Sensitivity,Specifycity,Per_PredTrue,Per_PredFalse))
   
   Final_result = rbind(Final_result,a)
   
   
 }
 
 

