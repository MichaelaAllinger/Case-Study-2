

###########################################################
# Packages
###########################################################

#load required packages
require(dplyr)
require(ggplot2)
library(randomForest)
library(Metrics)
library(microbenchmark)
library(dummies)

###########################################################
# Data Preparation
###########################################################

# Read csv files
dim_customer = read.csv("Customers.csv", header=TRUE, sep=",")
dim_geography = read.csv("DimGeography.csv", header=TRUE, sep=";")
dim_sales = read.csv("store_sales_One_Region_Per_sheet.csv", header=TRUE, sep=";", dec=",")
dim_product = read.csv("DimProductUnique_wirklichunique.csv", header=TRUE, sep=";")

#Rename colomnames (as a preparation of joining the data)
colnames(dim_sales) <- c("rowid", "orderid","orderdate", "shipdate", "shipmode", "customerid", "postalcode", "productid", "sales", "quantity", "discount", "profit", "store")
colnames(dim_product) <- c("productid", "productname", "subcategory", "category")
str(dim_sales)
str(dim_product)

#Join data
df1 = merge(dim_sales, dim_customer)
df2 = merge(df1, dim_geography)
data = merge(df2, dim_product)

#Overview of data
str(data)
head(data)
dim(data)

#Are there any duplicates?
anyDuplicated(data) 

#change data type
data$postalcode = as.factor(data$postalcode)
data$sales = as.numeric(data$sales)
data$discount = as.numeric(data$discount)
data$quantity = as.numeric(data$quantity)

data$profit = as.numeric(data$profit)
data$orderdate = as.Date(data$orderdate, format="%d.%m.%Y")
data$shipdate = as.Date(data$shipdate, format="%d.%m.%Y")

#calculate month, year,etc. for orderdate and shipdate
data$orderdate_day = as.numeric(format(as.Date(data$orderdate,format="%d.%m.%Y"), "%d"))
data$orderdate_weekday = as.factor(format(as.Date(data$orderdate,format="%d.%m.%Y"), "%A"))
data$orderdate_month = as.numeric(format(as.Date(data$orderdate, format="%d.%m.%Y"), "%m"))
data$orderdate_year = as.numeric(format(as.Date(data$orderdate,format="%d.%m.%Y"), "%Y"))

data$shipdate_day = as.numeric(format(as.Date(data$shipdate,format="%d.%m.%Y"), "%d"))
data$shipdate_weekday = as.factor(format(as.Date(data$shipdate,format="%d.%m.%Y"), "%A"))
data$shipdate_month = as.numeric(format(as.Date(data$shipdate, format="%d.%m.%Y"), "%m"))
data$shipdate_year = as.numeric(format(as.Date(data$shipdate,format="%d.%m.%Y"), "%Y"))

#calculate preparationtime(Time Difference in Days)
data$preparationtime = as.numeric(data$shipdate - data$orderdate)

#Overview of data
str(data)
head(data)
dim(data)
summary(data)

#data for prediction
data_p = data[,c("shipmode", "sales", "quantity", "discount", "profit",
                 "segment", "region", "state", "subcategory", "category",
                 "orderdate_day", "orderdate_weekday", "orderdate_month",
                 "orderdate_year", "shipdate_day", "shipdate_weekday", "shipdate_month",
                 "shipdate_year", "preparationtime")]

str(data_p) #no factor with more than 53 levels

###########################################################
# EDA (Exploratory Data Analysis)
###########################################################

## label (preparationtime)
summary(data_p$preparationtime)
jpeg('preparationtime.jpeg')
ggplot(data_p, aes(preparationtime)) +
  geom_histogram(bins=7) +
  theme_minimal()
dev.off()

## features
#pairwise correlation matrix 
#jpeg('matrix.jpeg')
pairs(data_p[,2:5], pch = 21) #pch defines the shape of visualization - in our case circles 
#dev.off()

#sales
#jpeg('sales.jpeg')
boxplot(data_p$sales ~ data_p$preparationtime, xlab="preparation time", ylab="sales")
means <- tapply(data_p$sales,data_p$preparationtime,mean)
points(means,col="red")
#dev.off()

#profit
#jpeg('profit.jpeg')
boxplot(data_p$profit ~ data_p$preparationtime, xlab="preparation time", ylab="profit")
means <- tapply(data_p$profit,data_p$preparationtime,mean)
points(means,col="red")
#dev.off()

#discount -> interessant!!
#jpeg('discount.jpeg')
boxplot(data_p$preparationtime ~ data_p$discount, xlab="discount", ylab="preparation time")
means <- tapply(data_p$preparationtime,data_p$discount,mean)
points(means,col="red")
#dev.off()

#quantity -> interessant!!
#jpeg('quantity.jpeg')
boxplot(data_p$preparationtime ~ data_p$quantity, xlab="quantity", ylab="preparation time")
means <- tapply(data_p$preparationtime,data_p$quantity,mean)
points(means,col="red")
#dev.off()

#shipmode -> interessant!!
#jpeg('shipmode.jpeg')
boxplot(data_p$preparationtime ~ factor(data_p$shipmode,c("Same Day","First Class","Second Class", "Standard Class")), xlab="shipmode", ylab="preparation time")
means <- tapply(data_p$preparationtime,data_p$shipmode,mean)
points(means,col="red")
#dev.off()

#state -> interessant!!
#jpeg('state.jpeg')
boxplot(data_p$preparationtime ~ data_p$state, xlab="state", ylab="preparation time")
means <- tapply(data_p$preparationtime,data_p$state,mean)
points(means,col="red")
#dev.off()

#region --> Median preparationtime über alle regions gleich
#jpeg('region.jpeg')
boxplot(data_p$preparationtime ~ factor(data_p$region), xlab="region", ylab="preparation time") #order descending
means <- tapply(data_p$preparationtime,data_p$region,mean)
points(means,col="red")
#dev.off()

#segment --> Median preprationtime über alle segmente gleich
#jpeg('segment.jpeg')
boxplot(data_p$preparationtime ~ data_p$segment, xlab="segment", ylab="preparation time")
means <- tapply(data_p$preparationtime,data_p$segment,mean)
points(means,col="red")
#dev.off()

#category --> Median preprationtime über alle categories gleich
#jpeg('category.jpeg')
boxplot(data_p$preparationtime ~ data_p$category, xlab="category", ylab="preparation time")
means <- tapply(data_p$preparationtime,data_p$category,mean)
points(means,col="red")
#dev.off()

#subcategory
#jpeg('subcategory.jpeg')
boxplot(data_p$preparationtime ~ data_p$subcategory, xlab="subcategory", ylab="preparation time")
means <- tapply(data_p$preparationtime,data_p$subcategory,mean)
points(means,col="red")
#dev.off()

#orderdate_day -> interessant!!
#jpeg('orderdate_day.jpeg')
boxplot(data_p$preparationtime ~ data_p$orderdate_day, xlab="orderdate_day", ylab="preparation time")
means <- tapply(data_p$preparationtime,data_p$orderdate_day,mean)
points(means,col="red")
#dev.off()

#orderdate_weekday --> Median preparationtime über alle weekdays fast gleich
#jpeg('orderdate_weekday.jpeg')
boxplot(data_p$preparationtime ~ factor(data_p$orderdate_weekday, c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")), xlab="orderdate_weekday", ylab="preparation time")
means <- tapply(data_p$preparationtime,factor(data_p$orderdate_weekday, c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")),mean)
points(means,col="red")
#dev.off()

#orderdate_month -> interessant!!
jpeg('orderdate_month.jpeg')
boxplot(data_p$preparationtime ~ factor(data_p$orderdate_month, c("Januar", "Februar", "Maerz", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")), xlab="orderdate_month", ylab="preparation time")
means <- tapply(data_p$preparationtime,factor(data_p$orderdate_month, c("Januar", "Februar", "Maerz", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")),mean)
points(means,col="red")
dev.off()

#orderdate_year
#jpeg('orderdate_year.jpeg')
boxplot(data_p$preparationtime ~ data_p$orderdate_year, xlab="orderdate_year", ylab="preparation time")
means <- tapply(data_p$preparationtime, data_p$orderdate_year,mean)
points(means,col="red")
#dev.off()

#shipdate_day -> interessant!!
#jpeg('shipdate_day.jpeg')
boxplot(data_p$preparationtime ~ data_p$shipdate_day, xlab="shipdate_day", ylab="preparation time")
means <- tapply(data_p$preparationtime, data_p$shipdate_day,mean)
points(means,col="red")
#dev.off()

#shipdate_weekday -> interessant!!
#jpeg('shipdate_weekday.jpeg')
boxplot(data_p$preparationtime ~ factor(data_p$shipdate_weekday, c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")), xlab="shipdate_weekday", ylab="preparation time")
means <- tapply(data_p$preparationtime,factor(data_p$shipdate_weekday, c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")),mean)
points(means,col="red")
#dev.off()

#shipdate_month -> interessant!!
jpeg('shipdate_month.jpeg')
boxplot(data_p$preparationtime ~ factor(data_p$shipdate_month, c("Januar", "Februar", "M?rz", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")), xlab="shipdate_month", ylab="preparation time")
means <- tapply(data_p$preparationtime,factor(data_p$shipdate_month, c("Januar", "Februar", "M?rz", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")),mean)
points(means,col="red")
dev.off()

#shipdate_year
#jpeg('shipdate_year.jpeg')
boxplot(data_p$preparationtime ~ data_p$shipdate_year, xlab="shipdate_year", ylab="preparation time")
means <- tapply(data_p$preparationtime, data_p$shipdate_year,mean)
points(means,col="red")
#dev.off()

###########################################################
#Random Forests
###########################################################

# create train and test set
LEN <- nrow(data_p)
set.seed(256) #ensures same random result
trn <- sample(LEN,trunc(LEN*.7) , replace = F) #takes randomly 70% of the data

traindata <- data_p[trn,]
testdata <- data_p[-trn,]

# parameter tuning

ntree = c(100,500,1000,2000)
mtry = c(5,8,13,17)

metrics = NULL
row = NULL

for (n in ntree){
  for(m in mtry){
    set.seed(123)
    rf = randomForest(preparationtime~., data=traindata, ntree=n, mtry=m,  do.trace=TRUE)
    preds = predict(rf, newdata=testdata)
    mse = mse(testdata$preparationtime, preds)
    rmse = rmse(testdata$preparationtime, preds)
    row = c(n,m,mse,rmse)
    metrics = rbind(metrics, row)
  }
}

row.names(metrics) <- NULL
colnames(metrics) <- c("n", "m", "mse", "rmse")
metrics = data.frame(metrics)

#write.csv(metrics, file = "metrics.csv")

#jpeg('metrics.jpeg')
ggplot(metrics, aes(x=factor(m), y=rmse)) +
         geom_line(aes(group=factor(n), color=factor(n))) +
          xlab("mtry") +
          labs(color="ntree") +
         geom_line()
#dev.off()

#--> m=17, n=1000 

#final RF model
set.seed(123)
randomf = randomForest(preparationtime~., data=traindata, ntree=1000, mtry=17,  do.trace=TRUE, importance = TRUE)
randomf

#microbenchmark
microbenchmark(randomf, times=5)

#test and evaluate model
randomf.pred.all <- predict(randomf, newdata=testdata)
act.pt <- testdata$preparationtime
mse(act.pt,randomf.pred.all)
rmse(act.pt, randomf.pred.all)

# variable importance
varImpPlot(randomf)

###########################################################
# XGBoost
###########################################################

#see Python code

###########################################################
# Linear Regression
###########################################################

##numerische Spalten separat als dataframe "numericCols" abspeichern
nums <- sapply(data_p, is.numeric)
numericCols = data_p[ , nums]
str(numericCols)



dumNamConvert <- function(x) {
  pos <- regexpr(')', x)
  nmLen <- nchar(x)
  newNm <- substr(x,pos+1,nmLen)
  make.names(newNm)
}

# mit dieser for-Schleife werden die factor variablen dummy kodiert und dann wird der Name ge�ndert

for (i in names(data_p)) {
  
  dum <- data_p[,i]
  if (is.factor(dum)) {
    dum <- as.data.frame(dummy(dum))
    nms <- dumNamConvert(names(dum))
    colnames(dum) <- paste0(i,".",nms)
    
    if (exists("dummyCols")) {
      dummyCols <- cbind(dummyCols,dum )
    } else {
      dummyCols <- dum
    }
  } 
}

str(dummyCols)

#-->dummyCols ist der Dataframe mit den dummy kodierten urspr�nglich factor variablen

# dann: binds the dummy variables with the numerical and the boolean columns

df_linreg = cbind( dummyCols, numericCols)

###train and test (the same as for random forest)
LEN <- nrow(df_linreg)
set.seed(256) #ensures same random result

trn1 <- sample(LEN,trunc(LEN*.7) , replace = F)

df_linreg.trn <- df_linreg[trn1,]
df_linreg.tst <- df_linreg[-trn1,]


reg<- lm(preparationtime~., data = df_linreg.trn)
summary(reg)

reg_mod = lm(preparationtime ~ shipmode.First.Class + shipmode.Same.Day + shipmode.Second.Class  +
               state.Florida + state.Iowa  + state.South.Carolina  + state.Tennessee + shipdate_weekday.Mittwoch  +
               shipdate_weekday.Montag + shipdate_weekday.Samstag + orderdate_day + orderdate_month + orderdate_year +
               shipdate_day + shipdate_month + shipdate_year, data=df_linreg.trn)

summary(reg_mod)


reg_mod_1 = lm(preparationtime ~ shipmode.First.Class + shipmode.Same.Day + shipmode.Second.Class  
               + state.Iowa  + state.South.Carolina  + state.Tennessee + shipdate_weekday.Mittwoch  +
                 shipdate_weekday.Montag  + orderdate_day + orderdate_month + orderdate_year +
                 shipdate_day + shipdate_month + shipdate_year, data=df_linreg.trn)

summary(reg_mod_1)

reg_mod_2 = lm(preparationtime ~ shipmode.First.Class + shipmode.Same.Day + shipmode.Second.Class  
               + state.Iowa   + state.Tennessee   +
                 shipdate_weekday.Montag  + orderdate_day + orderdate_month + orderdate_year +
                 shipdate_day + shipdate_month + shipdate_year, data=df_linreg.trn)

summary(reg_mod_2)
ptm <- proc.time() #to measure the time the code is running

reg_mod_3= lm(preparationtime ~ shipmode.First.Class + shipmode.Same.Day + shipmode.Second.Class  
              + state.Iowa   + orderdate_day + orderdate_month + orderdate_year +
                shipdate_day + shipdate_month + shipdate_year, data=df_linreg.trn)
proc.time() - ptm
summary(reg_mod_3)

#microbenchmark to measure time
microbenchmark(reg_mod_3, times=5)

#test the model
prediction_reg<- predict(reg_mod_3, newdata= df_linreg.tst)
rmse(df_linreg.tst$preparationtime,prediction_reg)
