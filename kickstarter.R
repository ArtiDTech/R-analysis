setwd("D:/ML/ArtiD/r-project")
data_k <- read.csv("train.csv")

dim(data_k)
summary(data_k)
colSums(sapply(data_k, is.na)) #Find number of missing in each column

#data_k$outlier_goal<-(data_k$goal>(mean(data_k$goal)+3*sd(data_k$goal)))*1 
#data_k$outlier_pledged<-(data_k$pledged>(mean(data_k$pledged)+3*sd(data_k$pledged)))*1 #Multiply by 1 to change 'logical' to 'numeric'
#data_k$outlier_backers<-(data_k$backers>(mean(data_k$backers)+3*sd(data_k$backers)))*1

data_k <- data_k[as.character(data_k$state) == "successful" | as.character(data_k$state) == "failed",] #left only data with success or fail
#unique(is.na(data_k$usd_pledged))
dim(data_k)
summary(data_k)

data_k$state_bin <- as.numeric(data_k$state == "successful")
data_k$avg_bid = data_k$usd_goal_real / data_k$backers
clean_data <- data_k[is.finite(data_k$avg_bid),]
summary(clean_data)
dim(clean_data)

train <- clean_data[1:200000,]
test <- clean_data[200000:292820,]
summary(train)
summary(test)


linearModel <- glm(state_bin ~ avg_bid, data=train, family = binomial())
summary(linearModel)

fitted.results <- predict(linearModel,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$state_bin)
print(paste('Accuracy',1-misClasificError))


data_k$launched <- as.Date(data_k$launched)
data_k <- data_k[as.numeric(format(data_k$launched, "%Y"))>=2009 & as.numeric(format(data_k$launched, "%Y"))<=2017,] #left only data from 2009 till 2017
sort(unique(as.numeric(format(data_k$launched, "%Y"))))
