loadmanylibs<-c('dummies', 'corrplot', 'ggplot2') 
lapply(loadmanylibs, require, character.only = TRUE) 

setwd("D:/ML/ArtiD/r-project")
data_k <- read.csv("train.csv")

#left only data with success or fail
data_k <- data_k[as.character(data_k$state) == "successful" | as.character(data_k$state) == "failed",] 
unique(data_k$state)
sort(unique(as.numeric(format(as.Date(data_k$launched), "%Y"))))

data_k$state_bin <- as.numeric(data_k$state == "successful")
data_k$goal_to_backers = data_k$usd_goal_real / data_k$backers
data_k$duration = as.POSIXlt(data_k$deadline) - as.POSIXlt(data_k$launched)
data_k$goal_dur =  data_k$usd_goal_real / as.numeric(data_k$duration) #money per day

#Find number of missing in each column
colSums(sapply(data_k, is.na)) 
#Changing all NA's on 0
data_k[is.na(data_k)] <- 0

clean_data <- data_k[is.finite(data_k$goal_to_backers) & data_k$goal_to_backers < 1000,]
summary(clean_data)
dim(clean_data)
summary(clean_data$goal_to_backers)

successful <- clean_data[clean_data$state_bin == 1,]
failed <- clean_data[clean_data$state_bin == 0,]

summary(dta$goal_to_backers)
hist(dta$goal_to_backers)

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

summary(data_Art$goal_to_backers)
hist(data_Art$goal_to_backers)
hist(data_Art$goal_dur)
summary(data_Comics$goal_to_backers)
hist(data_Comics$goal_to_backers)
summary(data_Crafts$goal_to_backers)
hist(data_Crafts$goal_to_backers)
summary(data_Dance$goal_to_backers)
hist(data_Dance$goal_to_backers)
summary(data_Design$goal_to_backers)
hist(data_Design$goal_to_backers)
summary(data_Fashion$goal_to_backers)
hist(data_Fashion$goal_to_backers)
summary(data_Film_Video$goal_to_backers)
hist(data_Film_Video$goal_to_backers)
summary(data_Food$goal_to_backers)
hist(data_Food$goal_to_backers)
summary(data_Games$goal_to_backers)
hist(data_Games$goal_to_backers)
summary(data_Journalism$goal_to_backers)
hist(data_Journalism$goal_to_backers)
summary(data_Music$goal_to_backers)
hist(data_Music$goal_to_backers)
summary(data_Photography$goal_to_backers)
hist(data_Photography$goal_to_backers)
summary(data_Publishing$goal_to_backers)
hist(data_Publishing$goal_to_backers)
summary(data_Technology$goal_to_backers)
hist(data_Technology$goal_to_backers)
summary(data_Theater$goal_to_backers)
hist(data_Theater$goal_to_backers)

# creating dataframes for each category
for (i in 1:length(main_categories)) {
  assign(paste('data_successful_',main_categories[i], sep = ""),successful[as.character(successful$main_category) == main_categories[i],])
}
for (i in 1:length(main_categories)) {
  assign(paste('data_failed_',main_categories[i], sep = ""),failed[as.character(failed$main_category) == main_categories[i],])
}

data_successful_Film_Video <- successful[as.character(successful$main_category) == "Film & Video",]
data_failed_Film_Video <- failed[as.character(failed$main_category) == "Film & Video",]

summary(data_successful_Art$goal_to_backers)
hist(data_successful_Art$goal_to_backers)
summary(data_failed_Art$goal_to_backers)
hist(data_failed_Art$goal_to_backers)
summary(data_successful_Art$goal_to_backers)
hist(data_successful_Art$goal_dur)
hist(data_failed_Art$goal_dur)
summary(data_successful_Comics$goal_to_backers)
hist(data_successful_Comics$goal_to_backers)
hist(data_failed_Comics$goal_to_backers)
summary(data_successful_Crafts$goal_to_backers)
hist(data_successful_Crafts$goal_to_backers)
hist(data_failed_Crafts$goal_to_backers)
summary(data_Dance$goal_to_backers)
hist(data_successful_Dance$goal_to_backers)
hist(data_failed_Dance$goal_to_backers)
summary(data_successful_Design$goal_to_backers)
hist(data_successful_Design$goal_to_backers)
hist(data_failed_Design$goal_to_backers)
summary(data_successful_Fashion$goal_to_backers)
hist(data_successful_Fashion$goal_to_backers)
hist(data_failed_Fashion$goal_to_backers)
summary(data_successful_Film_Video$goal_to_backers)
hist(data_successful_Film_Video$goal_to_backers)
hist(data_failed_Film_Video$goal_to_backers)
summary(data_successful_Food$goal_to_backers)
hist(data_successful_Food$goal_to_backers)
hist(data_failed_Food$goal_to_backers)
summary(data_successful_Games$goal_to_backers)
hist(data_successful_Games$goal_to_backers)
hist(data_failed_Games$goal_to_backers)
summary(data_successful_Journalism$goal_to_backers)
hist(data_successful_Journalism$goal_to_backers)
hist(data_failed_Journalism$goal_to_backers)
summary(data_successful_Music$goal_to_backers)
hist(data_successful_Music$goal_to_backers)
hist(data_failed_Music$goal_to_backers)
summary(data_successful_Photography$goal_to_backers)
hist(data_successful_Photography$goal_to_backers)
hist(data_failed_Photography$goal_to_backers)
summary(data_successful_Publishing$goal_to_backers)
hist(data_successful_Publishing$goal_to_backers)
hist(data_failed_Publishing$goal_to_backers)
summary(data_successful_Technology$goal_to_backers)
hist(data_successful_Technology$goal_to_backers)
hist(data_failed_Technology$goal_to_backers)
summary(data_successful_Theater$goal_to_backers)
hist(data_successful_Theater$goal_to_backers)
hist(data_failed_Theater$goal_to_backers)

medians <- c(median(data_Art$goal_to_backers), median(data_Comics$goal_to_backers), median(data_Crafts$goal_to_backers), 
             median(data_Dance$goal_to_backers), median(data_Design$goal_to_backers), median(data_Fashion$goal_to_backers),
             median(data_Film_Video$goal_to_backers), median(data_Food$goal_to_backers),median(data_Games$goal_to_backers),
             median(data_Journalism$goal_to_backers), median(data_Music$goal_to_backers), median(data_Photography$goal_to_backers),
             median(data_Publishing$goal_to_backers), median(data_Technology$goal_to_backers), median(data_Theater$goal_to_backers)) 
goal_to_backers <- data.frame(category = main_categories, median = medians)

success <- c(nrow(data_Art[data_Art$goal_to_backers < median(data_Art$goal_to_backers) & data_Art$state_bin == 1,]),
             nrow(data_Comics[data_Comics$goal_to_backers < median(data_Comics$goal_to_backers) & data_Comics$state_bin == 1,]),
             nrow(data_Crafts[data_Crafts$goal_to_backers < median(data_Crafts$goal_to_backers) & data_Crafts$state_bin == 1,]),
             nrow(data_Dance[data_Dance$goal_to_backers < median(data_Dance$goal_to_backers) & data_Dance$state_bin == 1,]),
             nrow(data_Design[data_Design$goal_to_backers < median(data_Design$goal_to_backers) & data_Design$state_bin == 1,]),
             nrow(data_Fashion[data_Fashion$goal_to_backers < median(data_Fashion$goal_to_backers) & data_Fashion$state_bin == 1,]),
             nrow(data_Film_Video[data_Film_Video$goal_to_backers < median(data_Film_Video$goal_to_backers) & data_Film_Video$state_bin == 1,]),
             nrow(data_Food[data_Food$goal_to_backers < median(data_Food$goal_to_backers) & data_Food$state_bin == 1,]),
             nrow(data_Games[data_Games$goal_to_backers < median(data_Games$goal_to_backers) & data_Games$state_bin == 1,]),
             nrow(data_Journalism[data_Journalism$goal_to_backers < median(data_Journalism$goal_to_backers) & data_Journalism$state_bin == 1,]),
             nrow(data_Music[data_Music$goal_to_backers < median(data_Music$goal_to_backers) & data_Music$state_bin == 1,]),
             nrow(data_Photography[data_Photography$goal_to_backers < median(data_Photography$goal_to_backers) & data_Photography$state_bin == 1,]),
             nrow(data_Publishing[data_Publishing$goal_to_backers < median(data_Publishing$goal_to_backers) & data_Publishing$state_bin == 1,]),
             nrow(data_Technology[data_Technology$goal_to_backers < median(data_Technology$goal_to_backers) & data_Technology$state_bin == 1,]),
             nrow(data_Theater[data_Theater$goal_to_backers < median(data_Theater$goal_to_backers) & data_Theater$state_bin == 1,]))

failed <- c(nrow(data_Art[data_Art$goal_to_backers < median(data_Art$goal_to_backers) & data_Art$state_bin == 0,]),
             nrow(data_Comics[data_Comics$goal_to_backers < median(data_Comics$goal_to_backers) & data_Comics$state_bin == 0,]),
             nrow(data_Crafts[data_Crafts$goal_to_backers < median(data_Crafts$goal_to_backers) & data_Crafts$state_bin == 0,]),
             nrow(data_Dance[data_Dance$goal_to_backers < median(data_Dance$goal_to_backers) & data_Dance$state_bin == 0,]),
             nrow(data_Design[data_Design$goal_to_backers < median(data_Design$goal_to_backers) & data_Design$state_bin == 0,]),
             nrow(data_Fashion[data_Fashion$goal_to_backers < median(data_Fashion$goal_to_backers) & data_Fashion$state_bin == 0,]),
             nrow(data_Film_Video[data_Film_Video$goal_to_backers < median(data_Film_Video$goal_to_backers) & data_Film_Video$state_bin == 0,]),
             nrow(data_Food[data_Food$goal_to_backers < median(data_Food$goal_to_backers) & data_Food$state_bin == 0,]),
             nrow(data_Games[data_Games$goal_to_backers < median(data_Games$goal_to_backers) & data_Games$state_bin == 0,]),
             nrow(data_Journalism[data_Journalism$goal_to_backers < median(data_Journalism$goal_to_backers) & data_Journalism$state_bin == 0,]),
             nrow(data_Music[data_Music$goal_to_backers < median(data_Music$goal_to_backers) & data_Music$state_bin == 0,]),
             nrow(data_Photography[data_Photography$goal_to_backers < median(data_Photography$goal_to_backers) & data_Photography$state_bin == 0,]),
             nrow(data_Publishing[data_Publishing$goal_to_backers < median(data_Publishing$goal_to_backers) & data_Publishing$state_bin == 0,]),
             nrow(data_Technology[data_Technology$goal_to_backers < median(data_Technology$goal_to_backers) & data_Technology$state_bin == 0,]),
             nrow(data_Theater[data_Theater$goal_to_backers < median(data_Theater$goal_to_backers) & data_Theater$state_bin == 0,]))

goal_to_backers_success <- data.frame(category = main_categories, success = success)
s_f <- cbind(success,failed)
goal_to_backers <- cbind(goal_to_backers,s_f)

