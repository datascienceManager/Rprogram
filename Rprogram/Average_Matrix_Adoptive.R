


##################################### Uniform Matrix ##########################################
#---------------- Removing old Dataset from the environment ------------------------#
rm(list=ls())

#-----------------Loading respective library file ----------------------------------#
library(kernlab)
library(stats)
#library(plyr)
library(dplyr)
library(data.table)
#library(proxy)
#library(tibble)

#----------------- Setting working directiry --------------------------------------#
setwd("/sftp/Adaptive_Cognizance")
#dir()

#----------------Reading usr usage Dataset ----------------------------------------#
kl= fread("50k_stratified/KPI_DUMP_50K_2ndlist_NUMBER.csv", integer64 = "numeric")

#-------------- Converting from Data.table data.frame to data.frame ----------------#
kl2 = data.frame(kl)

#--------------Getting column name -------------------#
#colnames(kl2)

#--------------Selecting only column name with the respective month name -----------#
kl2 = kl2[grepl("_Oct", names(kl2),ignore.case=TRUE)]

#colnames(kl2)

#---------------- Creating new Dataset for MSISDN --------------------#
MSISDN = kl[,1]

colnames(MSISDN) = "CONSUMER_ID"

MSISDN$CONSUMER_ID = as.numeric(MSISDN$CONSUMER_ID)

#str(MSISDN)

#-----------Combining  MSISDN and kl2 --------------------------------#

kl4 = cbind(MSISDN,kl2)

kl4 <- data.frame(kl4)
#colnames(kl4)

#str(kl4)

#-------------Loading Subscription or Recharge Dataset -------------------------#


kl9 = fread("50k_stratified_recharges_kol/50k_strat_rech_oct.csv",verbose = T, integer64 = "numeric")

#colnames(kl9)

#head(kl9)
#-------------Converting from Data.table data.frame to data.frame -----------#
kl9 = data.frame(kl9)


#------------- Renaming column name -------------------------#
colnames(kl9) = c("CONSUMER_ID","TimeStamp","ProductID")
#str(kl9)


#--------------- Creating new Dataset for unique CONSUMER_ID -----------------#
unq_kl9 = unique(kl9$CONSUMER_ID)

unq_kl9 = data.frame(unq_kl9)

unq_kl9$unq_kl9 = as.numeric(unq_kl9$unq_kl9)

colnames(unq_kl9) = c("CONSUMER_ID")

#-------------- Adding new column FSA in unq_k19 dataset ---------------------#
unq_kl9$FSA = 1

#str(unq_kl9)

#-------------Creating new dataset by merging  MSISDN, KPI and FSA Dataset ---------------------#

matrx_fsa = merge(kl4,unq_kl9, by = "CONSUMER_ID", all.x = T)

#summary(matrx_fsa$FSA)

#-------------Replacing missing value of matrix_fsa dataset with 0----------------------#
matrx_fsa[is.na(matrx_fsa)] = 0

#table(matrx_fsa$FSA)

# 0      1 
# 28124   21856 
# class imbalance

#------------ Creting new dataset with equal numer of 0 & 1---------------------#

matrx_fsa_1 = matrx_fsa[matrx_fsa$FSA == 1,]

matrx_fsa_0 = matrx_fsa[matrx_fsa$FSA == 0,]

num <- count(matrx_fsa_1)

str(num)

matrx_fsa_0_new = matrx_fsa_0[1:nrow(matrx_fsa_0),]

#----------- Combining both the matrix with 0 and 1 ---------------------------#
matrx_fsa_2 = rbind(matrx_fsa_1,matrx_fsa_0_new)

#class(matrx_fsa_2)
matrx_fsa_2 <- data.frame(matrx_fsa_2)
#str(matrx_fsa_2)

#table(matrx_fsa_2$FSA)


#------------ Replacing missing value with 0 ------------------------------------#

matrx_fsa_2[is.na(matrx_fsa_2)] = 0

#-------FSA (Feature selection algorithm) Algorithm---------------------------#

#--------------------Polycor correlation ---------------------------------#

lm = matrx_fsa_2[,-1]

#------------Remove columns with zero values from a dataframe ------------------------#
lm = lm[, colSums(lm != 0) > 0] 

#str(lm)

corl = cor(lm)

corl2 = corl[-16,16]

#----------- Creating correlation matrix --------------------------------------------#
corl2 = as.matrix(corl2)


#--------Selecting correlaion variable having value more than  50 percentile of its sample ------------#
corl4 = corl2[which(corl2[,1]>quantile(corl2[,1], 0.5)),]

corl4 = as.matrix(corl4)

#----------------Random Forest---------------#
library(randomForest)

lm$FSA = as.factor(lm$FSA)

#-------- Building Random Forest algorithm ----------------#
mod2<-randomForest(FSA~., data=lm, ntree=50,mtry=3,oob.error=0.4601)

#-----------Getting important feature from random forest algorithm ---------------#
rfvar = importance(mod2)

#--------- Creating Feature imp matrix -------------------------------#
rfvar = as.data.frame.matrix(rfvar)

str(rfvar)

colsumrf = colSums(rfvar)

rfvar$gini = (rfvar$MeanDecreaseGini/colsumrf)

rfvar = as.matrix(rfvar)

ginirf = rfvar[,-1]

ginirf = as.matrix(ginirf)

#--------Selecting random forest feature variable having value more than  50 percentile value of its sample ------------#
ginirf2 =ginirf[which(ginirf[,1]>quantile(ginirf[,1], 0.50)),]


ginirf2 = as.matrix(ginirf2)

# #PCA-------------
# 
# lm$FSA=as.numeric(levels(lm$FSA))[lm$FSA]
# 
# str(lm)
# 
# pca =prcomp(lm, scale. = T) # scale = T for correlation and F for covariance
# 
# #summary(pca)
# 
# #pca$sdev
# 
# pcavar = as.matrix(pca$rotation)
# 
# pcavar
# 
# pcavar1 = as.matrix(pcavar[,1])
# 
# pcavar1 = pcavar1[!rownames(pcavar1) %in% c("FSA"), ] #remove specific row by name
# 
# pcavar1 = as.matrix(pcavar1)
# 
# #pcavar2 = pcavar1[which(pcavar1[,1]>mean(pcavar1[,1])),]
# pcavar2 = pcavar1[which(pcavar1[,1]>quantile(pcavar1[,1], 0.5)),]
# 
# pcavar2 = as.matrix(pcavar2)

#########################Creating Master FSA Table############################
#---------Find max length vector and put it as first vector in list----------------#

list_data = list(corl4, ginirf2)

list_of_data = list_data[order(-sapply(list_data, nrow))]

common_names = Reduce(intersect, lapply(list_of_data, row.names))

#------------ Make list with only common_names --------------------------------#
list_of_data2 = lapply(list_of_data, function(x) { x[row.names(x) %in% common_names,] })

#----------------combine both the dataset --------------------------------#

a = list_data[[1]]

b= list_data[[2]]

#c=list_data[[3]]


a = data.frame(a)

b = data.frame(b)

#c= data.frame(c)

library(data.table)
setDT(a, keep.rownames = TRUE)[]
setDT(b, keep.rownames = TRUE)[]
#setDT(c, keep.rownames = TRUE)[]

#-------- Merge multiple data frames ------------------#
jkl = list(a,b) %>% Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="rn"), .)

jkl[is.na(jkl)] <- 0

jkl$comb2 = apply(jkl[2:3], 1, max)

comb3 <- jkl[order(-jkl$comb2),]

combnames = comb3$rn
class(combnames)

#-------- Creating not common names list -------------------#
desc_order_list = combnames[!combnames %in% c(common_names)]

cnt = length(common_names)

min_list = ifelse(cnt<15, 15-cnt,0)

cnt_2 = length(desc_order_list)

min_list_2 = ifelse(cnt_2<min_list,cnt_2,min_list)

#-------------min_list = 15 - cnt ---------------------------#

fnl_desc = desc_order_list[0:min_list_2]

msisdn = "CONSUMER_ID"

#---------------tchpnt = "FSA"--------------------------#

fnl_15_list = append(common_names, fnl_desc)

fnl_15_list2 = append(msisdn, fnl_15_list)

drops2 <- c(fnl_15_list2)

fnl_dataset = kl4[ , (names(kl4) %in% drops2)]

colnames(fnl_dataset)

#------------------ Saving final dataset --------------------#
# write.csv(fnl_dataset, "fnl_KPI_dataset_FSA_new_oct_v2.csv", row.names = FALSE)

comb4 = comb3
colnames(comb4) = c("Attributes", "Corrletaion", "Random_Forest", "Max_Value")

#-------------------- Saving the combine matrxi --------------------------#
# write.csv(comb4, "FSA_Combined_Rank_new_oct_v2.csv", row.names = FALSE)

#########################################################################################

#--------------------Adaptive cognizant algoritm code ------------------------#

#########################################################################################

#------------- Removing unwanted Data.frame -------------------------------#
rm(list=setdiff(ls(), c("comb4", "fnl_dataset", "kl9", "MSISDN")))

kl4 <- fnl_dataset

# head(fnl_dataset)

#-------------- Creating Seperate Dataset for MSISDN for kl4 -------------------#
MSISDN <- data.frame(kl4$CONSUMER_ID)
colnames(MSISDN) <- "CONSUMER_ID"

# kl= fread("KPI_DUMP_50K_NUMBER.csv", integer64 = "numeric")
# 
# kl2 = data.frame(kl)
# 
# #Segregating May month-----------------------------
# 
# kl2 = kl2[grepl("_Oct", names(kl2),ignore.case=TRUE)]
# 
# colnames(kl2)
# 
# #Create MSISDN-------------------
# 
# MSISDN = kl[,1]
# 
# MSISDN = data.frame(MSISDN)
# 
# colnames(MSISDN) = "CONSUMER_ID"
# 
# MSISDN$CONSUMER_ID = as.numeric(MSISDN$CONSUMER_ID)
# 
# str(MSISDN)
# 
# #Combine MSISDN and kl2------------------------
# 
# kl4 = cbind(MSISDN,kl2)
# 
# colnames(kl4)
# 
# str(kl4)
# 
# fwrite(kl4,"kl4.csv")
# 
# #Subscription -------------------------
# 
# kl9 = fread("oct_rech_final.csv")
# 
# str(kl9)
# 
# kl9 = data.frame(kl9)

#subsetting 38 products-------------------------------

# setwd("/dfs/Model_Files/Adap R script")


# prod38 = fread("prod38.csv")
#
# head(prod38)
#
# prod38_list = prod38$`Product ID`
#
# kl9 = kl9[which(kl9$ProductID %in% prod38_list),]

#----------------------reading timestamp---------------------------#
str(kl9)

#-------------- Creating new timestaamp from older one -------------#
kl9$newtimestamp <- strptime(kl9$TimeStamp,format="%Y-%m-%d %H:%M:%S")

#------------- Renaming column name -------------------------#
colnames(kl9)=c("CONSUMER_ID","TimeStamp","ProductID","newtimestamp")

kl11 = kl9[,c("CONSUMER_ID","ProductID" ,"newtimestamp")]


colnames(kl11)[3] = "Timestamp"

#-----------------sorting timestamp--------------------------#

kl12 <- kl11[order(kl11$Timestamp),]

# #-------------------Keeping latest recharge as per timestamp---------------------#
# 
# kl22 = kl12[!rev(duplicated(rev(kl12$CONSUMER_ID))),]
# 
# kl22 <- kl22[order(kl22$Timestamp),]
# 
# kl22$CONSUMER_ID = as.numeric(kl22$CONSUMER_ID)
# 
# str(kl22)
# 
# str(kl4)
# 
# #-------------------Creating Main dataframe by Merging kl4 with kl22------------------#
# 
# kl24 = merge(kl4,kl22, by = "CONSUMER_ID", all.x = T)
# 
# colnames(kl24)
# 
# summary(kl24)
# 
# str(kl24)
# 
# #--------This is only for those who recharge------------------------#
# 
# kl24 = kl24[complete.cases(kl24$ProductID),]

#============= Merging Usage details along with Product Recharge 

Usage_Details = kl4

kl11$CONSUMER_ID = as.numeric(kl11$CONSUMER_ID)

kl11$Date = as.Date(kl11$Timestamp,format = "%m-%d-%Y")

Product_list = select(kl11,CONSUMER_ID,ProductID,Date)

Recharge_List_Avg = left_join(Product_list,kl4,by=c("CONSUMER_ID"))%>% 
  select(.,-c(Date,CONSUMER_ID ))


str(Recharge_List_Avg)


# ============== Calculating the Average per Recharge Type 

Product_Subset = split(Recharge_List_Avg,Recharge_List_Avg$ProductID)

a = lapply(Product_Subset,FUN=function(i)sapply(i[,1:length(Recharge_List_Avg)],mean))

library(tibble)

RechargeType_Avg_List = data.frame()

for(i in 1:length(a)){
  a1= data.frame(a[i])
  
  a2 = data.frame(t(a1))
  
  a3 = rownames_to_column(a2,var="Stype")
  
  RechargeType_Avg_List = rbind(RechargeType_Avg_List,a3)
  
}


# str(RechargeType_Avg_List)

kl24 = RechargeType_Avg_List %>% select(.,-c(Stype))


#Creating Impmatrix----------------------

DayDiff = seq(1,90, by=1) 
matimp = seq(0,1, by=0.01)
remove = c(0.05,0.16,0.20,0.26,0.36,0.46,0.56,0.66,0.76,0.86,0.96)
ImportanceIndex = setdiff(matimp,remove)

impmatrx = data.frame(DayDiff,ImportanceIndex)

#Current date-------------


lm = Sys.Date()

kl25 = kl12

str(kl25)

#arrange in decreasing order of timestamp------------------
kl25 <- kl25[order(kl25$Timestamp,decreasing = T),]

#remove duplicate as per MSISDN & Product both--------------------
kl25 = kl25[!duplicated(kl25[1:2]),]

str(kl25)

kl25$Date = format(kl25$Timestamp, format = "%m-%d-%Y")

kl25$Date = as.Date(kl25$Date,format = "%m-%d-%Y")

kl25$DayDiff = lm - kl25$Date

kl25$DayDiff = as.numeric(kl25$DayDiff)

str(kl25)

#Replace DayDiff > 90 as 90-------------------

kl25$DayDiff[kl25$DayDiff > 90] <- 90

#Merging kl18 with impmatrx---------------------

kl26 = merge(kl25,impmatrx, by = "DayDiff", all.x = T)

# summary(kl26)

#Product as row, MSISDN as col, and value as Impindex-------------------

kl27 = kl26[,c("CONSUMER_ID","ProductID","ImportanceIndex")]

kl27 <- as.data.table(kl27)

kl28 = data.table::dcast(kl27, ProductID  ~ CONSUMER_ID, value.var="ImportanceIndex", fill = 0)

kl28 <- data.frame(kl28)

# str(kl28)

kl28 = kl28[!(kl28$ProductID=="0"),]

#kl28[is.na(kl28)] = 0

#Make matriximp---------------------- 
norm88 = kl28
colnames(norm88)[1] = "rowname"

#Creating Product Matrix--------------------------------

kl32 = kl24

zx = kl32

zx = data.frame(zx)

# zx = zx[!(zx$ProductID=="0"),]

str(zx)

ProductID_names = zx$ProductID 

#Creating Usage Matrix--------------------------------

nm = kl4


library(proxy)

m1 = as.matrix(nm[,-1])

m2 = as.matrix(zx[,-1])

m4 = simil(m1,m2,method = "cosine",by_rows = TRUE)

m5 = dist(m1,m2,method = "Euclidean",by_rows = TRUE)

m6 = dist(m1,m2,method = "Manhattan",by_rows = TRUE)

#Coverting back to dataframe---------------------

str(m4)

m4 = as.data.frame.matrix(m4)
m5 = as.data.frame.matrix(m5)
m6 = as.data.frame.matrix(m6)


#Adding colnames and MSISDN----------------------

colnames(m4) = ProductID_names
m44 = cbind(MSISDN,m4)

colnames(m5) = ProductID_names
m55 = cbind(MSISDN,m5)

colnames(m6) = ProductID_names
m66 = cbind(MSISDN,m6)

#Transponse matrices------------------------

m444 = t(m44[,2:ncol(m44)])
colnames(m444) <- as.character(MSISDN$CONSUMER_ID)

m555 = t(m55[,2:ncol(m55)])
colnames(m555) <- as.character(MSISDN$CONSUMER_ID)

m666 = t(m66[,2:ncol(m66)])
colnames(m666) <- as.character(MSISDN$CONSUMER_ID)

#Normalization of matrices--------------------

norm44 = apply(m444, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

norm55 = apply(m555, MARGIN = 2, FUN = function(X) 1 - (X - min(X))/diff(range(X)))#why 1 minus with Norm

norm66 = apply(m666, MARGIN = 2, FUN = function(X) 1 - (X - min(X))/diff(range(X)))

#Replacing NAN value to 0--------------------

norm44[is.na(norm44)] = 0
norm55[is.na(norm55)] = 0
norm66[is.na(norm66)] = 0


#Coverting to dataframe---------------------

norm44 = as.data.frame.matrix(norm44)

norm55 = as.data.frame.matrix(norm55)

norm66 = as.data.frame.matrix(norm66)

head(norm44[1:5,1:5])

head(norm55[1:5,1:5])

library(tibble)

norm44 = rownames_to_column(norm44)
norm55 = rownames_to_column(norm55)
norm66 = rownames_to_column(norm66)

# View(head(norm44))

#Transpose matrcies---------------------

norm44 <- data.table(norm44)
norm55 <- data.table(norm55)
norm66 <- data.table(norm66)
norm88 <- data.table(norm88)

head(norm44[1:10,1:10])

str(norm44)
str(dat44)

head(dat88[1:10,1:10])

dat44 = dcast(melt(norm44, id.vars = "rowname"), variable ~ rowname)
dat55 = dcast(melt(norm55, id.vars = "rowname"), variable ~ rowname)
dat66 = dcast(melt(norm66, id.vars = "rowname"), variable ~ rowname)
dat88 = dcast(melt(norm88, id.vars = "rowname"), variable ~ rowname)

dat44 <- as.data.frame.matrix(dat44)
dat55 <- as.data.frame.matrix(dat55)
dat66 <- as.data.frame.matrix(dat66)
dat88 <- as.data.frame.matrix(dat88)


dat44_1 = dat44
dat55_1 = dat55
dat66_1 = dat66
dat88_1 = dat88



str(dat8)

dat88$Variable2 = substr(dat88$variable,2,length(dat88$variable))

vvv = c("Variable2")

dat88 = dat88[c(vvv,setdiff(names(dat88),"Variable2"))]

dat88$variable = NULL

colnames(dat88)[1]= c("variable")

#Rbind all matrices and incluse new col as per MSISDN and use those val as NA--------------

rm(list=setdiff(ls(),c("dat44","dat55","dat66","dat88", "kl4")))


List_Names = setdiff(names(dat44),c("variable"))


Average_Matrix = data.frame(ID= seq(1:nrow(dat44)))

for ( i in seq(1:length(List_Names))){
  
  a = List_Names[i]
  
  finl_= left_join(dat44,dat55,by=c("variable"))%>% 
    left_join(.,dat66,by=c("variable"))%>%
    left_join(.,dat88,by=c("variable"))%>%
    select(.,starts_with(a))%>%
    mutate(.,Average_Name= rowMeans(.,na.rm = TRUE))%>%
    select(.,Average_Name)
  
  finl_ = data.frame(finl_)
  
  colnames(finl_)[1] = paste("Average",List_Names[i],sep="_")
  
  Average_Matrix = cbind(Average_Matrix,finl_)
  
}

Average_Matrix$variable = dat44$variable

Average_Matrix$ID = NULL

variable = c("variable")

# --------------- Moving Variable column front 

Average_Matrix = Average_Matrix[c(variable,setdiff(names(Average_Matrix),variable))]

str(Average_Matrix)



setwd("/sftp/Adaptive_Cognizance/Average_Recharge_Comparison")


fwrite(Average_Matrix,"Average_Matrix.csv")

# 

head(Average_Matrix)

(0.8789412+0.9061478+0+0)/3

head(dat88[1:5,1:5])





# ================= Validated Average ==================================


dat4 = arrange(dat44[1:5,1:5],variable)
dat5 = arrange(dat55[1:5,1:5],variable)
dat6 = arrange(dat66[1:5,1:5],variable)
dat8 = arrange(dat88[1:5,1:5],variable)


str(dat8)

dat88$Variable2 = substr(dat88$variable,2,length(dat88$variable))

vvv = c("Variable2")

dat88 = dat88[c(vvv,setdiff(names(dat88),"Variable2"))]

dat88$variable = NULL

colnames(dat88)[1]= c("variable")

#Rbind all matrices and incluse new col as per MSISDN and use those val as NA--------------

rm(list=setdiff(ls(),c("dat44","dat55","dat66","dat88", "kl4")))


List_Names = setdiff(names(dat44),c("variable"))


Average_Matrix = data.frame(ID= seq(1:nrow(dat4)))

for ( i in seq(1:length(List_Names))){
  
  a = List_Names[i]
  
  finl_= left_join(dat4,dat5,by=c("variable"))%>% 
    left_join(.,dat6,by=c("variable"))%>%
    left_join(.,dat8,by=c("variable"))%>%
    select(.,starts_with(a))%>%
    mutate(.,Average_Name= rowMeans(.,na.rm = TRUE))%>%
    select(.,Average_Name)
  
  finl_ = data.frame(finl_)
  
  colnames(finl_)[1] = paste("Average",List_Names[i],sep="_")
  
  Average_Matrix = cbind(Average_Matrix,finl_)
  
}





# 

head(Average_Matrix)

(0.8789412+0.9061478+0+0)/3





matrx = rbindlist(list(dat44,dat55,dat66,dat88), use.names=TRUE, fill=TRUE)

matrx[is.na(matrx)] = 0

colnames(matrx)[1] = "CONSUMER_ID"
