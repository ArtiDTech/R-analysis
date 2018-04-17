loadmanylibs<-c('dummies', 'corrplot', 'ggplot2') 
lapply(loadmanylibs, require, character.only = TRUE) 

setwd("C:/Users/Ann/Documents/project")
data_k <- read.csv("train.csv")

#left only data with success or fail
data_k <- data_k[as.character(data_k$state) == "successful" | as.character(data_k$state) == "failed",] 
unique(data_k$state)
sort(unique(as.numeric(format(as.Date(data_k$launched), "%Y"))))

data_k$state_bin <- as.numeric(data_k$state == "successful")
data_k$avg_bid = data_k$usd_goal_real / data_k$backers
data_k$duration = as.POSIXlt(data_k$deadline) - as.POSIXlt(data_k$launched)
data_k$goal_dur =  data_k$usd_goal_real / as.numeric(data_k$duration) #money per day

#Find number of missing in each column
colSums(sapply(data_k, is.na)) 
#Changing all NA's on 0
data_k[is.na(data_k)] <- 0

clean_data <- data_k[is.finite(data_k$avg_bid),]
summary(clean_data)
dim(clean_data)
summary(clean_data$avg_bid)

########################################
# data_cor <- as.numeric(clean_data$ID)
# data_cor$goal <- as.numeric(clean_data$goal)
# is.numeric(clean_data$goal)

# clean_data_cor = cor(clean_data)
# par(mfrow=c(2,2)) #Define 2-by-2 graph
# corrplot(clean_data_cor, method = c("number"))
##########################################

train <- clean_data[1:200000,]
test <- clean_data[200000:292820,]
summary(train)
summary(test)

linearModel <- glm(state_bin ~ avg_bid  + duration + goal_dur, data=train, family = binomial())
summary(linearModel)

fitted.results <- predict(linearModel,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

plot(clean_data$state_bin~clean_data$avg_bid + clean_data$duration + clean_data$goal_dur, xlab="usd_goal_real / data_k$backers", ylab="state")
abline(linearModel)

misClasificError <- mean(fitted.results != test$state_bin)
print(paste('Accuracy',1-misClasificError))


###########################################################

#Create dummies
# data_k$outlier_goal<-(data_k$goal>(mean(data_k$goal)+3*sd(data_k$goal)))*1 
# data_k$outlier_pledged<-(data_k$pledged>(mean(data_k$pledged)+3*sd(data_k$pledged)))*1 #Multiply by 1 to change 'logical' to 'numeric'
# data_k$outlier_backers<-(data_k$backers>(mean(data_k$backers)+3*sd(data_k$backers)))*1

##########################################################

# divide 'categories' into different columns
unique(data_k$category)
# data_k <- merge(data_k, dummy(data_k$category, sep=":"))

main_categories <- sort(as.character(unique(clean_data$main_category)))
main_categories[7] <- "Film_Video"
main_categories

print(paste('data_',main_categories[1], sep = ""))

# creating dataframes for each category
for (i in 1:length(main_categories)) {
  assign(paste('data_',main_categories[i], sep = ""),clean_data[as.character(clean_data$main_category) == main_categories[i],])
}

data_Film_Video <- clean_data[as.character(clean_data$main_category) == "Film & Video",]

medians <- c(median(data_Art$avg_bid), median(data_Comics$avg_bid), median(data_Crafts$avg_bid), 
             median(data_Dance$avg_bid), median(data_Design$avg_bid), median(data_Fashion$avg_bid),
             median(data_Film_Video$avg_bid), median(data_Food$avg_bid),median(data_Games$avg_bid),
             median(data_Journalism$avg_bid), median(data_Music$avg_bid), median(data_Photography$avg_bid),
             median(data_Publishing$avg_bid), median(data_Technology$avg_bid), median(data_Theater$avg_bid)) 
avg_bid <- data.frame(category = main_categories, median = medians)

success <- c(nrow(data_Art[data_Art$avg_bid < median(data_Art$avg_bid) & data_Art$state_bin == 1,]),
             nrow(data_Comics[data_Comics$avg_bid < median(data_Comics$avg_bid) & data_Comics$state_bin == 1,]),
             nrow(data_Crafts[data_Crafts$avg_bid < median(data_Crafts$avg_bid) & data_Crafts$state_bin == 1,]),
             nrow(data_Dance[data_Dance$avg_bid < median(data_Dance$avg_bid) & data_Dance$state_bin == 1,]),
             nrow(data_Design[data_Design$avg_bid < median(data_Design$avg_bid) & data_Design$state_bin == 1,]),
             nrow(data_Fashion[data_Fashion$avg_bid < median(data_Fashion$avg_bid) & data_Fashion$state_bin == 1,]),
             nrow(data_Film_Video[data_Film_Video$avg_bid < median(data_Film_Video$avg_bid) & data_Film_Video$state_bin == 1,]),
             nrow(data_Food[data_Food$avg_bid < median(data_Food$avg_bid) & data_Food$state_bin == 1,]),
             nrow(data_Games[data_Games$avg_bid < median(data_Games$avg_bid) & data_Games$state_bin == 1,]),
             nrow(data_Journalism[data_Journalism$avg_bid < median(data_Journalism$avg_bid) & data_Journalism$state_bin == 1,]),
             nrow(data_Music[data_Music$avg_bid < median(data_Music$avg_bid) & data_Music$state_bin == 1,]),
             nrow(data_Photography[data_Photography$avg_bid < median(data_Photography$avg_bid) & data_Photography$state_bin == 1,]),
             nrow(data_Publishing[data_Publishing$avg_bid < median(data_Publishing$avg_bid) & data_Publishing$state_bin == 1,]),
             nrow(data_Technology[data_Technology$avg_bid < median(data_Technology$avg_bid) & data_Technology$state_bin == 1,]),
             nrow(data_Theater[data_Theater$avg_bid < median(data_Theater$avg_bid) & data_Theater$state_bin == 1,]))

failed <- c(nrow(data_Art[data_Art$avg_bid < median(data_Art$avg_bid) & data_Art$state_bin == 0,]),
             nrow(data_Comics[data_Comics$avg_bid < median(data_Comics$avg_bid) & data_Comics$state_bin == 0,]),
             nrow(data_Crafts[data_Crafts$avg_bid < median(data_Crafts$avg_bid) & data_Crafts$state_bin == 0,]),
             nrow(data_Dance[data_Dance$avg_bid < median(data_Dance$avg_bid) & data_Dance$state_bin == 0,]),
             nrow(data_Design[data_Design$avg_bid < median(data_Design$avg_bid) & data_Design$state_bin == 0,]),
             nrow(data_Fashion[data_Fashion$avg_bid < median(data_Fashion$avg_bid) & data_Fashion$state_bin == 0,]),
             nrow(data_Film_Video[data_Film_Video$avg_bid < median(data_Film_Video$avg_bid) & data_Film_Video$state_bin == 0,]),
             nrow(data_Food[data_Food$avg_bid < median(data_Food$avg_bid) & data_Food$state_bin == 0,]),
             nrow(data_Games[data_Games$avg_bid < median(data_Games$avg_bid) & data_Games$state_bin == 0,]),
             nrow(data_Journalism[data_Journalism$avg_bid < median(data_Journalism$avg_bid) & data_Journalism$state_bin == 0,]),
             nrow(data_Music[data_Music$avg_bid < median(data_Music$avg_bid) & data_Music$state_bin == 0,]),
             nrow(data_Photography[data_Photography$avg_bid < median(data_Photography$avg_bid) & data_Photography$state_bin == 0,]),
             nrow(data_Publishing[data_Publishing$avg_bid < median(data_Publishing$avg_bid) & data_Publishing$state_bin == 0,]),
             nrow(data_Technology[data_Technology$avg_bid < median(data_Technology$avg_bid) & data_Technology$state_bin == 0,]),
             nrow(data_Theater[data_Theater$avg_bid < median(data_Theater$avg_bid) & data_Theater$state_bin == 0,]))

avg_bid_success <- data.frame(category = main_categories, success = success)
s_f <- cbind(success,failed)
avg_bid <- cbind(avg_bid,s_f)

ggplot(data = avg_bid, aes(x = avg_bid$category, y = avg_bid$success+avg_bid$failed, fill = Stage.of.Change)) + 
  geom_bar() + coord_flip()
