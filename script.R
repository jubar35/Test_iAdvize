########################
### 0. LOAD PACKAGES ###
########################

library(WDI)
library(tidyr)
library(dplyr)
library(FactoMineR)
library(ggplot2)
library(caret)
library(rattle)



#######################
### 1. PREPARE DATA ###
#######################

## Load data
data <- WDI(country="all", indicator=c("EN.ATM.CO2E.KT","EN.ATM.CO2E.PC"), # EN.ATM.CO2E.KT : CO2 KT /  EN.ATM.CO2E.KT : CO2 TONS PER CAPITA
            start=1964, end=2014, extra=TRUE, cache=NULL)

## Rename EN.ATM.CO2E.KT to CO2
data <- rename(data, CO2.KT=EN.ATM.CO2E.KT, CO2.PC=EN.ATM.CO2E.PC)


## Delete aggregate data
data <- data[which(data$region!="Aggregates"),]

## Country, Year, CO2, region
data <- select(data,"country", "CO2.KT", "CO2.PC", "year", "region", "income","lending") 

## Delete individuals with NA between 1964 and 2014 (57 countries)
data <- data %>% group_by(country) %>%
  filter(!any(is.na(CO2.KT)) & !any(is.na(CO2.PC))) %>%
  ungroup






######################
### 2. EXPLORATION ###
######################


## Overall

#CO2 KT
ggplot(data, aes(x=year, y=CO2.KT)) + 
  stat_summary(fun.y = sum, na.rm = TRUE, geom ='line', size=2) + 
  scale_x_continuous(name="Time",breaks=seq(1964,2014,10))+
  scale_y_continuous(name="CO2 emimission (KT)", labels=scales::comma)+
  ggtitle("Total CO2 emissions (KT) from 1964 to 2014") 


#all
ggplot(data, aes(x=year, y=CO2.KT)) + 
  geom_line(aes(group=country), col="grey")+
  guides(fill=FALSE) + #remove factor's legend
  stat_summary(fun.y = mean, na.rm = TRUE, color = 'red', geom ='line') + 
  stat_summary(fun.y = median, na.rm = TRUE, color = 'green', geom ='line') +
  scale_x_continuous(name="Time",breaks=seq(1964,2014,10))+
  scale_y_continuous(name="CO2 emimission (KT)",breaks=seq(0,1000000,500000), limits=c(0,1000000),labels=scales::comma)+
  ggtitle("Total CO2 emissions from 1964 to 2014")

#comparison 1964 and 2014
sum(data[data$year==1964,"CO2.KT"], na.rm=TRUE) #CO2 emission in 1964
sum(data[data$year==2014,"CO2.KT"], na.rm=TRUE) #CO2 emission in 2014



## Total CO2 emmission by country
ggplot(data %>% group_by(country) %>% summarise(CO2.KT=sum(CO2.KT)) %>% arrange(desc(CO2.KT)) %>% slice(1:10)
       , aes(country)) + 
  geom_bar(aes(weight=CO2.KT))+
  scale_x_discrete(name="Country")+
  scale_y_continuous(name="CO2 emimission (KT)",labels=scales::comma)+
  ggtitle("The 10 countries with the highest CO2 emissions sum since 1964")


## Total CO2 emmission by country (PC)
ggplot(data %>% group_by(country) %>% summarise(CO2.PC=sum(CO2.PC)) %>% arrange(desc(CO2.PC)) %>% slice(1:10)
       , aes(country)) + 
  geom_bar(aes(weight=CO2.PC))+
  scale_x_discrete(name="Country")+
  scale_y_continuous(name="CO2 emimission (PC)",labels=scales::comma)+
  ggtitle("The 10 countries with the highest CO2 emissions PC sum since 1964")



## By region

ggplot(data %>% group_by(year,region) %>% summarise(CO2.KT=sum(CO2.KT)), aes(x=year, y=CO2.KT)) + 
  geom_line(aes(group=region, colour=region))+
  scale_x_continuous(name="Time",breaks=seq(1964,2014,10))+
  scale_y_continuous(name="CO2 emimission (KT)",breaks=seq(0,50000000,2000000),labels=scales::comma)+
  ggtitle("CO2 emissions per region")



## By income

ggplot(data %>% group_by(year,income) %>% summarise(CO2.KT=sum(CO2.KT)), aes(x=year, y=CO2.KT)) + 
  geom_line(aes(group=income, colour=income))+
  scale_x_continuous(name="Time",breaks=seq(1964,2014,10))+
  scale_y_continuous(name="CO2 emimission (KT)",breaks=seq(0,50000000,2000000),labels=scales::comma)+
  ggtitle("CO2 emissions per income")



## By lending

ggplot(data %>% group_by(year,lending) %>% summarise(CO2.KT=sum(CO2.KT)), aes(x=year, y=CO2.KT)) + 
  geom_line(aes(group=lending, colour=lending))+
  scale_x_continuous(name="Time",breaks=seq(1964,2014,10))+
  scale_y_continuous(name="CO2 emimission (KT)",breaks=seq(0,50000000,2000000),labels=scales::comma)+
  ggtitle("CO2 emissions per lending")



## Boxplot by year

ggplot(data, aes(x=year, y=CO2.KT, group=year)) + 
  geom_boxplot()+
  scale_x_continuous(name="Time",breaks=seq(1964,2014,5))+
  scale_y_continuous(name="CO2 emimission (KT)",labels=scales::comma)+
  ggtitle("CO2 emission boxplot by year")

#zoom
ggplot(data, aes(x=year, y=CO2.KT, group=year)) + 
  geom_boxplot()+
  scale_x_continuous(name="Time",breaks=seq(1964,2014,5))+
  scale_y_continuous(name="CO2 emimission (KT)",limits=c(0,250000),labels=scales::comma)+
  ggtitle("CO2 emission boxplot by year")

# net difference of variability between years
# variance and average of CO2 emission increase since 1964
# lots of countries's level of CO2 emission are considerably superior 



## Boxplot by country

ggplot(data, aes(x=country, y=CO2.KT, group=country)) + 
  geom_boxplot()+
  scale_x_discrete(name="Country")+
  scale_y_continuous(name="CO2 emimission (KT)",labels=scales::comma)+
  ggtitle("CO2 emission boxplot by country")

#zoom
ggplot(data, aes(x=country, y=CO2.KT, group=country)) + 
  geom_boxplot()+
  scale_x_discrete(name="Time")+
  scale_y_continuous(name="CO2 emimission (KT)",labels=scales::comma)+
  coord_cartesian(ylim = c(0, 1000000))+
  ggtitle("CO2 emission boxplot by country")

#zoom China, Japan, India, US, France
ggplot(subset(data, country %in% c("China","Japan","United States","India","France")), aes(x=country, y=CO2.KT, group=country)) + 
  geom_boxplot()+
  scale_x_discrete(name="Time")+
  scale_y_continuous(name="CO2 emimission (KT)",labels=scales::comma)+
  ggtitle("CO2 emission boxplot by country")

# net difference of variability between countries
# 4 countries stand out from the others : 
# - China : strong variance and high average
# - Japan : high average
# - India : high average
# - United States : very high average







##############
### 3. PCA ###
##############

# supplementary individuals : United States and China
# (CTR : 72% & 19%)

## Prepare data

#Countries & CO2
data_pca <- data %>% select("country", "CO2.KT", "year") %>%
  spread("year", "CO2.KT")

data_pca <- as.data.frame(data_pca)

#Row names
row.names(data_pca) <- data_pca$country
data_pca$country <- NULL



## ACP

pca <- PCA(data_pca, ncp = 2, scale.unit = TRUE, ind.sup = c(29,148))
barplot(pca$eig[,2], main="eigenvalues (percentage)") 
sum(pca$eig[2,3]) #2 axis explain 98,69% of total inertia


## Representations

# individuals
plot(pca, axes=c(1,2), choix="ind") 
plot(pca,choix="ind",select="contrib 10", title="Individuals factor map",sub="the 10 individuals that gave the highest contribution", xlim=c(0,50),ylim=c(-20,30))
plot(pca,choix="ind",select="cos2 10", title="Individuals factor map",sub="the 10 individuals that gave the highest cos2")

# variables
plot(pca, axes=c(1,2), choix="var") 
plot(pca,choix="var",select="contrib 10", title="Variables factor map",sub="the 10 variables that gave the highest contribution")
plot(pca,choix="var",select="cos2 10", title="Variables factor map",sub="the 10 variables that gave the highest cos2")



## Interpretation of axis 1

#variables
var_table_dim1 <- cbind(pca$var$coord[,1],pca$var$cos2[,1],pca$var$contrib[,1])
colnames(var_table_dim1) <- c("coord","cos2","contrib")
var_ctr_avg <- 1/ncol(data_pca)*100 #variables contribution average
pca$var$contrib[which(pca$var$contrib[,1]>=var_ctr_avg),1] #variables which are a ctr higher than the average


#individuals
ind_table_dim1 <- cbind(pca$ind$coord[,1],pca$ind$cos2[,1],pca$ind$contrib[,1])
colnames(ind_table_dim1) <- c("coord","cos2","contrib")
ind_ctr_avg <- 1/(nrow(data_pca)-2)*100 #individuals contribution average
pca$ind$contrib[which(pca$ind$contrib[,1]>=ind_ctr_avg),1] #individuals which are a ctr higher than the average


#illustration
cat <- c("Japan","India","United Kingdom","Canada","France","Italy","Poland","South Africa","Mexico","Australia","Korea, Rep.","Iran, Islamic Rep.","Brazil","Spain","Saudi Arabia")
ggplot(subset(data, country %in% cat), aes(x=year, y=CO2.KT)) + 
  geom_line(aes(group=country), col="red")+
  scale_x_continuous(name="Time",breaks=seq(1964,2014,10))+
  scale_y_continuous(name="CO2 emimission (KT)",labels=scales::comma)+
  ggtitle("Total CO2 emissions from 1964 to 2014")+
  geom_line(data=subset(data, !country %in% cat & !country %in% c("China","United States")), aes(x=year, y=CO2.KT, group=country))

#mean per year for each country on the group
apply(data_pca[which(rownames(data_pca) %in% cat),11:42],1,mean)
sum(data_pca[which(!rownames(data_pca) %in% c("China","United States")),11:42]) / 7803



## Interpretation of axis 2

#variables
var_table_dim2 <- cbind(pca$var$coord[,2],pca$var$cos2[,2],pca$var$contrib[,2])
colnames(var_table_dim2) <- c("coord","cos2","contrib")
pca$var$contrib[which(pca$var$contrib[,2]>=var_ctr_avg),2] #variables which are a ctr higher than the average


#individuals
ind_table_dim2 <- cbind(pca$ind$coord[,2],pca$ind$cos2[,2],pca$ind$contrib[,2])
colnames(ind_table_dim1) <- c("coord","cos2","contrib")
pca$ind$contrib[which(pca$ind$contrib[,2]>=ind_ctr_avg),2] #individuals which are a ctr higher than the average

#illustration
cat1 <- c("India","Korea, Rep.","Indonesia","Iran","Saudi Arabia","Thailand","Turkey")
cat2 <- c("United Kingdom","France","Poland","Romania","Belgium")
ggplot(subset(data, country %in% cat1), aes(x=year, y=CO2.KT)) + 
  geom_line(aes(group=country), col="red")+
  scale_x_continuous(name="Time",breaks=seq(1964,2014,10))+
  scale_y_continuous(name="CO2 emimission (KT)",labels=scales::comma)+
  ggtitle("Total CO2 emissions from 1964 to 2014")+
  geom_line(data=subset(data, country %in% cat2), aes(x=year, y=CO2.KT, group=country))





#################
### 4. KMEANS ###
#################

## Kmeans, 3 clusters
km <- kmeans(data_pca, center=3)
cluster <- data.frame(cluster=km$cluster)
cluster$country <- rownames(cluster)
km$withinss #variability within clusters
km$betweenss #variability between clusters


## Illustration - all countries
ggplot(data %>% left_join(cluster, by="country") , aes(x=year, y=CO2.KT)) + 
  geom_line(aes(group=country, colour=cluster))+
  scale_x_continuous(name="Time",breaks=seq(1964,2014,10))+
  scale_y_continuous(name="CO2 emimission (KT)",labels=scales::comma)+
  ggtitle("Total CO2 emissions from 1964 to 2014")


## Illustration - mean oer group & per year
ggplot(data %>% left_join(cluster, by="country") %>% group_by(year,cluster) %>% summarise(CO2.KT=mean(CO2.KT)) , aes(x=year, y=CO2.KT)) + 
  geom_line(aes(group=cluster, colour=cluster))+
  scale_x_continuous(name="Time",breaks=seq(1964,2014,10))+
  scale_y_continuous(name="CO2 emimission (KT)",labels=scales::comma)+
  ggtitle("Total CO2 emissions from 1964 to 2014")






###################################
### 5. VARIABLES TO BE CONSIDER ###
###################################


## Load data
data <- WDI(country="all", indicator=c("EN.ATM.CO2E.KT",
                                       "SP.POP.TOTL",
                                       "EG.ELC.COAL.ZS",
                                       "NV.AGR.TOTL.KD",
                                       "NV.IND.TOTL.CD",
                                       "SP.URB.TOTL",
                                       "NE.EXP.GNFS.CD",
                                       "NE.IMP.GNFS.CD"
), 
start=1964, end=2014, extra=TRUE, cache=NULL)


## Rename EN.ATM.CO2E.KT to CO2
data <- rename(data, CO2=EN.ATM.CO2E.KT)


## World data
data <- data[which(data$country=="World"),]


## Year, CO2, variables
data <- data[,-c(1,2,13:18)] 


## Summary
summary(data)
#much NA for NV.IND.TOTL.CD -> delete this variable
data <- data[,-c(6)] 


## Correlations
for(var in 3:8){
  print(cor.test(data$CO2, data[,var], method="pearson", alternative="greater"))
}


## Missing data -> mean
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

## Decision Tree
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) #train method 3 x 10 folds
tree <- train(CO2 ~., data = data[,2:8], method = "rpart",  #train
              trControl = trctrl,
              tuneLength = 10)
fancyRpartPlot(tree$finalModel) #plot decision tree
varImp(tree) #variable importance
