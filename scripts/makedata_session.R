setwd("~/Documents/kaggle/airbnb")

library(lubridate)


sessions <- read.csv("./data/sessions.csv", stringsAsFactors = FALSE)

sessions$secs_elapsed[is.na(sessions$secs_elapsed)] <- 0
sessions$action[is.na(sessions$action)] <- "BLANK"
sessions$action_type[is.na(sessions$action_type)] <- "BLANK"
sessions$action_detail[is.na(sessions$action_detail)] <- "BLANK"

sessions$action[sessions$action == ""] <- "BLANK"
sessions$action_type[sessions$action_type == ""] <- "BLANK"
sessions$action_detail[sessions$action_detail == ""] <- "BLANK"

sessions$action <- factor(sessions$action, levels = sort(unique(sessions$action)))
sessions$action_type <- factor(sessions$action_type, levels = sort(unique(sessions$action_type)))
sessions$action_detail <- factor(sessions$action_detail, levels = sort(unique(sessions$action_detail)))
sessions$device_type <- factor(sessions$device_type, levels = sort(unique(sessions$device_type)))

sessions$count <- 1
sessions$secs_elapsed <- NULL

imax <- nrow(sessions) 
batchSize <- 200000
i1 <- 1
i2 <- batchSize

ss2 <- cbind(sessions[i1:i2,1, drop=FALSE], model.matrix(~ -1 + ., sessions[i1:i2,-1]))
ss2[, 2:(ncol(ss2)-1)] <- ss2[,2:(ncol(ss2)-1)] * ss2$count
sessions2 <- aggregate(.~user_id, data = ss2, FUN=sum)
print(paste("Done with ...", i1, "to", i2))
print(dim(sessions2))

while (i2 < imax) {  
    i1 <- i2+1
    i2 <- i1 + (batchSize-1)
    if (i2 > imax) i2 <- imax
    
    ss2 <- cbind(sessions[i1:i2, 1, drop=FALSE], model.matrix(~ -1 + ., sessions[i1:i2, -1]))
    ss2[, 2:(ncol(ss2)-1)] <- ss2[,2:(ncol(ss2)-1)] * ss2$count
    print(dim(ss2))
    sessions2 <- aggregate(.~user_id, data=rbind(sessions2, ss2), FUN=sum)
    print(dim(sessions2))
    
    print(paste("Done with ...", i1, "to", i2))
}

write.csv(sessions2, "./data/sessions2.csv", row.names = FALSE)

