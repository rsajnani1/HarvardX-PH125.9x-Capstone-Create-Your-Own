if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "https://cran.r-project.org/package=rpart")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "https://CRAN.R-project.org")
if(!require(repr)) install.packages("repr", repos = "http://cran.us.r-project.org")
if(!require(xts)) install.packages("xts", repos = "http://cran.us.r-project.org")
if(!require(sp)) install.packages("sp", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(modelr)) install.packages("modelr", repos = "http://cran.us.r-project.org")
install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
if(!require(AER)) install.packages("AER", repos = "http://cran.us.r-project.org")
library("knitr")
library("tidyverse")
library("dplyr")
library("lattice")
library("caret")
library("rpart")
library("rpart.plot")
library("repr")
library("modelr")
library("CASdatasets")
library("AER")


#load the two data sets and convert into tibbles
data(freMTPL2freq)
data <- as_tibble(freMTPL2freq)
data(freMTPL2sev)
sev<- as_tibble(freMTPL2sev)


#inspecting dimensions of first data set
cat("Dimensions", dim(data))




#inspecting dimensions of second data set
cat("Dimensions", dim(sev))

#change class of VehGas vector and double precision ClaimNb vector

data <- data %>% mutate(VehGas = factor(VehGas, levels = c("Diesel","Regular")), ClaimNb = as.double(ClaimNb))

#inspecting frequency data set after modification
cat("This is what the frequency data set looks like after changing the class of VehGas vector  and double precision ClaimNb vector")

head(data)


#Showcase claim numbers in Severity data set instead of claim amounts
sev$ClaimNb <- 1
sev_agg <- aggregate(sev,by=list(IDpol=sev$IDpol),FUN = sum)[c("IDpol","ClaimNb")]
cat("Updated severity data set with claim frequencies:")
head(sev_agg)

#join the two data sets
joint <- left_join(data[,-2],sev_agg,by="IDpol")

#replace all NA with a 0
joint[is.na(joint)] <- 0

cat("Sample View of the Joint data set after taking claim frequencies from the severity data set:")
head(joint)


#see what percentage of policies have had claims filed by number of claims

claimpercent <- joint %>% group_by(ClaimNb) %>% summarize(count = n()) %>% mutate(percent = count / sum(count) * 100)
claimpercent



#inspecting the Exposures column
hist(joint$Exposure,main = "Number of Claims by Exposure Duration", xlab = "Exposure Duration (years)",ylab = "Number of Claims", breaks=40, col="black", xlim=c(0,2.05), ylim=c(0,170000))


#inspecting the Driver Age column
hist(joint$DrivAge, main = "Number of Claims by Driver Age", xlab = "Driver Age (years)",ylab = "Number of Claims",breaks=15, col="black", xlim=c(0,90), ylim=c(0,120000))


#inspecting the Vehicle Age column
hist(joint$VehAge,main = "Number of Claims by Vehicle Age", xlab = "Vehicle Age (years)",ylab = "Number of Claims", breaks=40, col="black", xlim=c(0,100), ylim=c(0,200000))


#removing outliers

joint$ClaimNb  <- pmin(joint$ClaimNb,4)  
joint$Exposure <- pmin(joint$Exposure,1) 
joint$DrivAge  <- pmin(joint$DrivAge,90)   
joint$VehAge  <- pmin(joint$VehAge,40)

cat("After making the above changes, the data set looks something like this:")
sample(joint)


#let's see the relationship between some vectors and claims frequency

#create a new version of the data set with Frequency column
new_joint <- joint %>% mutate(Freq=ClaimNb/Exposure)

cat("the New Joint data set looks something like this:")
sample(new_joint)




DAge_corr <- new_joint %>% group_by(DrivAge) %>% summarize(claim_frequency=mean(ClaimNb)/mean(Exposure))

cat("These are the top 5 age groups with highest average claim rates:")
head(DAge_corr,5)


DAge_corr %>% ggplot(aes(DrivAge, claim_frequency))+ geom_area(fill="grey")+ labs(title = "Average Frequency by Age Level", x = "Driver Age",y = "Average Claim Frequency")



#Checking dependency on Fuel Type and Vehicle Brand

fuelagebrand <- new_joint %>% mutate(VehAgeS=as.factor(pmin(VehAge, 4))) %>% group_by(VehBrand,VehAgeS,VehGas) %>% summarize(claim_frequency=mean(ClaimNb)/mean(Exposure), expo=sum(Exposure)) %>%  ggplot(aes(x=VehBrand, y=claim_frequency, color=VehAgeS)) + facet_grid(. ~ VehGas) + geom_point(size=2) + labs(title = "Average Frequency by Vehicle Brand, Age and Fuel Type", x = "Vehicle Brand",y = "Average Claim Frequency")


fuelagebrand 



#checking impact of Area

new_joint %>% ggplot(aes(Area, Freq))+  geom_boxplot() + scale_y_log10() +labs(title = "Average Frequency by Area", x = "Area",y = "Average Claim Frequency")



#checking impact of Region

regionimp <- new_joint %>% group_by(Region) %>% summarize(claim_frequency=mean(ClaimNb)/mean(Exposure)) %>%  arrange(desc(claim_frequency))

regionimp %>% ggplot(aes(Region, claim_frequency))+geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ labs(title = "Average Frequency by Region", x = "Area",y = "Average Claim Frequency")




#Check Distribution type before going into Regression Analysis

hist(log10(new_joint$Freq),main = "Frequency Distribution", xlab = "Frequency of Claim",ylab = "Number of Claims",)




#we start by creating test and training data sets
set.seed(0)
test_index <- createDataPartition(y = new_joint$ClaimNb, times = 1, p = 0.3, list = FALSE)
train_set <- new_joint[-test_index,]
test_set <- new_joint[test_index,]


#Fitting the GLM  model

train_glm_offset <- glm(ClaimNb ~ VehPower+VehAge+DrivAge+BonusMalus+VehBrand+VehGas+Area+Density+Region + offset(log(Exposure)), family="poisson", data = train_set)

#Checking which predictors are significant
summary(train_glm_offset)




dispersiontest(train_glm_offset,trafo=1)



round(exp(coef(train_glm_offset)),3)

pred_glm_offset <- predict(train_glm_offset, test_set, type = "response", newoffset=log(Exposure))

round(sample(pred_glm_offset,6),3)


mae_glm <- sum( abs(pred_glm_offset - test_set$ClaimNb) ) / nrow(test_set)
mae_glm

tree_train <- rpart(cbind(Exposure,ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, train_set,  method="poisson", control=rpart.control(maxdepth=3,cp=0.001))   

rpart.plot(tree_train)


pred_tree <- predict(tree_train, test_set, type = "vector", newoffset=log(Exposure))

round(sample(pred_tree,6),3)


mae_tree <- sum( abs(pred_tree - test_set$ClaimNb) ) / nrow(test_set)
mae_tree
