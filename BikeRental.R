rm(list=ls(all=T))

setwd("C:/Users/DELL/Desktop/edwisor/project2")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"Metrics","psych","party",
      "knitr","kableExtra" , "dummies", "mice")

lapply(x, require, character.only = TRUE)

rm(x)

## Read the data
df = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))

###########################################Explore the data##########################################
str(df)

#######plot histograms for continuous variables#############
multi.hist(df[,c(10:16)], main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")

####### BoxPlots - Distribution and Outlier Check################
cnames = colnames(df[,-c(1:9)])
print(cnames)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(df))+
           stat_boxplot(geom = "errorbar", width = 0.2) +
           geom_boxplot(outlier.colour="red", fill = "green" ,outlier.shape=16,
                        outlier.size=1.5, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste(cnames[i])))
}

# ## Plotting plots together###########
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,ncol=1)

# #Replace all outliers with NA and impute###########
for(i in cnames){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print(length(val))
  df[,i][df[,i] %in% val] = NA
}

#######impute NAs with mice library######
imputer_data=mice(df[,-c(1:9)],m=5,maxit=50,method='pmm',seed=50)
summary(imputer_data)

df[,-c(1:9)]=complete(imputer_data,2)


##################################Feature Selection################################################
## Correlation Plot 
corrgram(df[,-c(1:9)], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

######dummy variables for season and weather########
df=cbind(df,dummy(df$season,sep="_season"))
df=cbind(df,dummy(df$weathersit,sep="_weathersit"))


#Drop ID and correlated column as it wont be of any requirement
df=df[,-c(1,2,3,9,10,14,15)]
str(df)

write.csv(df, "BikeRent.csv", row.names = F)

#Divide data into train and test using stratified sampling method
set.seed(123)
train.index = createDataPartition(df$cnt, p = .80, list = FALSE)
train = df[ train.index,]
test  = df[-train.index,]


###Random Forest
RF_model = randomForest(cnt ~., train, importance = TRUE, ntree = 500)
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-9])
rmsle(test[,9],RF_Predictions)

#Develop Linear Regression model
LM_model = lm(cnt ~., train)

#predict on test cases #raw
LM_Predictions = predict(LM_model, test[,c(1:8,10:16)])
rmse(test[,9],LM_Predictions)
summary(LM_model)
anova(LM_model)
LM_model2 = update(LM_model,. ~ . - df_season2-workingday)
summary(LM_model2)

