## based on u23a
## based on u21a without lang simplification
## based on u11, 
## same as u11a/9a, country vs all, add some more features & 3x4 folds
setwd("~/Documents/kaggle/airbnb")

library(lubridate)
library(xgboost)
library(caret)

train_usrs <- read.csv("./data/train_users_2.csv", stringsAsFactors = FALSE)
test_usrs <- read.csv("./data/test_users.csv", stringsAsFactors = FALSE)
#age_gender_bkts <- read.csv("./data/age_gender_bkts.csv", stringsAsFactors = FALSE)
#countries <- read.csv("./data/countries.csv", stringsAsFactors = FALSE)

#train_usrs <- train_usrs[train_usrs$date_account_created >= "2013-01-01",]

age_groups <- matrix(0, nrow = 21, ncol=2)
age_groups[,1] <- seq(0, 100, 5)
age_groups[,2] <- seq(4, 104, 5)
age_groups[21,2] <- 150
age_groups <- data.frame(age_groups)
names(age_groups) <- c("Lower", "Upper")

getCharRep <- function(ii) {
    paste(age_groups[ii,1], "-", age_groups[ii,2], sep = "")
}
age_groups$CharRep <- sapply(1:nrow(age_groups), getCharRep)
age_groups$CharRep[21] <- "100+"

test_usr_ids <- test_usrs$id
test_usrs$country_destination <- "NDF"

all_usrs <- rbind(train_usrs, test_usrs)
all_usrs <- all_usrs[, c(1, 16, 2:15)]

all_usrs$date_first_booking <- NULL

all_usrs$date_account_created <- ymd(all_usrs$date_account_created)
all_usrs$ACYear <- year(all_usrs$date_account_created)
all_usrs$ACMonth <- month(all_usrs$date_account_created)
all_usrs$ACDay <- day(all_usrs$date_account_created)
all_usrs$date_account_created <- NULL

all_usrs$timestamp_first_active <- as.character(all_usrs$timestamp_first_active)
all_usrs$FAYear <- as.integer(substring(all_usrs$timestamp_first_active, 1, 4))
all_usrs$FAMonth <- as.integer(substring(all_usrs$timestamp_first_active, 5, 6))
all_usrs$FADay <- as.integer(substring(all_usrs$timestamp_first_active, 7, 8))
all_usrs$timestamp_first_active <- NULL

all_usrs$age[is.na(all_usrs$age)] <- -1
all_usrs$age[all_usrs$age > 1900] <- 2015 - all_usrs$age[all_usrs$age > 1900]
all_usrs$age[all_usrs$age > 100] <- 100
all_usrs$age[(all_usrs$age < 14) & (all_usrs$age >= 0)] <- 14

ageGrpIdx <- ceiling((all_usrs$age + 1)/5)
ageGrpIdx[ageGrpIdx > 21] <- 21
all_usrs$AgeGrp <- "UNKN"
all_usrs$AgeGrp[ageGrpIdx > 0] <- age_groups$CharRep[ageGrpIdx]
#> sort(unique(all_usrs$AgeGrp))
#[1] "0-4"   "100+"  "15-19" "20-24" "25-29" "30-34" "35-39" "40-44" "45-49" "50-54"
#[11] "55-59" "5-9"   "60-64" "65-69" "70-74" "75-79" "80-84" "85-89" "90-94" "95-99"
#[21] "UNKN" 
all_usrs$AgeGrp <- factor(all_usrs$AgeGrp, levels = sort(unique(all_usrs$AgeGrp)))

all_usrs$gender <- as.factor(all_usrs$gender)

all_usrs$signup_method <- as.factor(all_usrs$signup_method)
all_usrs$signup_flow <- as.factor(all_usrs$signup_flow)
all_usrs$language <- as.factor(all_usrs$language)
all_usrs$affiliate_channel <- as.factor(all_usrs$affiliate_channel)
all_usrs$affiliate_provider <- as.factor(all_usrs$affiliate_provider)
all_usrs$first_affiliate_tracked <- as.factor(all_usrs$first_affiliate_tracked)

all_usrs$signup_app <- as.factor(all_usrs$signup_app)
all_usrs$first_device_type <- as.factor(all_usrs$first_device_type)
all_usrs$first_browser <- as.factor(all_usrs$first_browser)

## sorted in decreasing levels is prevelance
countryLevels <- c("NDF","US","other","FR","IT","GB","ES","CA","DE","NL","AU","PT")

## using sequence in kaggle scripts.
#countryLevels <- c("NDF","US","other","FR","CA","GB","ES","IT","PT","NL","DE","AU")

all_usrs <- cbind(all_usrs[,c(1,2), drop=FALSE], model.matrix(~ -1 + ., all_usrs[,-c(1,2)]))
print(dim(all_usrs))

### do this after expansion
all_usrs$age[all_usrs$age == -1] <- NA

session_info <- read.csv("./data/sessions2.csv", stringsAsFactors = FALSE)

colsForBooking <- c("actionapply_coupon_click_success", "actionapply_reservation", "actionbooking",  
                    "actionchange_availability", "actionchange_currency", "actioncoupon_code_click", 
                    "actionpay", "actionpayment_methods", "actionprint_confirmation", "actionrate", 
                    "actionreceipt", "actionrecent_reservations", "action_detailapply_coupon", 
                    "action_detailapply_coupon_click", "action_detailapply_coupon_click_success",
                    "action_detailapply_coupon_error", "action_detailbooking", "action_detailbook_it",
                    "action_detailchange_availability", "action_detailchange_or_alter", 
                    "action_detailcreate_payment_instrument", "action_detailmodify_reservations")

colForNonEnglish <- c("actionajax_google_translate", "actionajax_google_translate_description", 
                      "actionajax_google_translate_reviews","actionchange_currency",
                      "actioncountry_options", "actionsouth.america", "actionsouthern.europe", 
                      "actionspoken_languages", "action_detailtranslate_listing_reviews", 
                      "action_detailtranslations","actionlanguages_multiselect","actionspoken_languages",
                      "action_detailuser_languages")

session_info$BookingDone <- rowSums(session_info[, colsForBooking], na.rm = TRUE)
session_info$OtherThanEnglish <- rowSums(session_info[, colForNonEnglish], na.rm = TRUE)


all_usrs <- merge(all_usrs, session_info, all.x = TRUE, by.x = "id", by.y = "user_id")

nonZeros <- function(xx) {
    xx[is.na(xx)] <- 0
    xx[xx > 1] <- 1
    return (sum(xx))
}
countInstances <- sapply(all_usrs[,-c(1,2)], nonZeros)
colToRemove <- which(countInstances <= 10)
length(colToRemove)
colToRemove <- colToRemove + 2 ### we had removed first two cols from summation
all_usrs <- all_usrs[,-colToRemove]
dim(all_usrs)

all_usrs[is.na(all_usrs)] <- -99999

test_idx <- which(all_usrs$id %in% test_usr_ids)
test <- all_usrs[test_idx,]
test$country_destination <- NULL
train <- all_usrs[-test_idx,]

###############################################################
## do binary classification to create layers for each country

getBinaryOutcome <- function(train, test, countryName) {
    numFolds <- 2
    numRepeats <- 2
    numRounds <- numFolds*numRepeats
    
    label.country <- rep(0, nrow(train))
    label.country[train$country_destination == countryName] <- 1
    folds <- createMultiFolds(label.country, k = numFolds, times = numRepeats)
    
    train.pred <- rep(0, nrow(train))
    test.pred <- rep(0, nrow(test))
    
    test.DM  <- xgb.DMatrix(data = data.matrix(test[, -1]), missing = NA)
    
    for (ii in seq(1, length(folds)) ) {
        other.ids <- setdiff(1:nrow(train), folds[[ii]])
        print(length(other.ids))
        print(names(folds)[ii])
        
        train.DM  <- xgb.DMatrix(data = data.matrix(train[-other.ids,-c(1,2)]), 
                                 label = label.country[-other.ids], missing = NA)
        other.DM  <- xgb.DMatrix(data = data.matrix(train[other.ids,-c(1,2)]), 
                               label = label.country[other.ids], missing = NA)
        
        wlist <- list(val = other.DM, train = train.DM)
        param <- list( max.depth = 6, eta = 0.03, booster = "gbtree",
                       subsample = 0.7, colsample_bytree = 0.7,
                       objective = "binary:logistic")#, eval_metric = "mlogloss") ## mlogloss, merror
        
        set.seed(6745)
        model1 <- xgb.train(params = param, data = train.DM, nrounds = 2000,
                            early.stop.round = 50,
                            nthread = 4, verbose = 1, print.every.n = 20,
                            missing = NA, watchlist = wlist, maximize = FALSE)
        
        bestIter <- model1$bestInd
        train.pred[other.ids] <- train.pred[other.ids] + predict(model1, newdata = other.DM, ntreelimit = bestIter)
        test.pred <- test.pred + predict(model1, newdata = test.DM, ntreelimit = bestIter)
    }
    
    test.pred <- test.pred/numRounds
    train.pred <- train.pred/numRepeats
    
    #train.pred <- ifelse(train.pred < 0.5, 0, 1)
    #test.pred <- ifelse(test.pred < 0.5, 0, 1)
    
    retval = list(trainpred = train.pred, testpred = test.pred)
    return (retval)
}

##   NDF     US  other     FR     IT     GB     ES     CA     DE     NL     AU     PT 
# 124543  62376  10094   5023   2835   2324   2249   1428   1061    762    539    217 

NDFlist <- getBinaryOutcome(train, test, "NDF")
USlist <- getBinaryOutcome(train, test, "US")
Otherlist <- getBinaryOutcome(train, test, "other")
FRlist <- getBinaryOutcome(train, test, "FR")
ITlist <- getBinaryOutcome(train, test, "IT")
GBlist <- getBinaryOutcome(train, test, "GB")
ESlist <- getBinaryOutcome(train, test, "ES")
CAlist <- getBinaryOutcome(train, test, "CA")
DElist <- getBinaryOutcome(train, test, "DE")
NLlist <- getBinaryOutcome(train, test, "NL")
AUlist <- getBinaryOutcome(train, test, "AU")
PTlist <- getBinaryOutcome(train, test, "PT")

train$PredNDF <- NDFlist$trainpred
test$PredNDF <- NDFlist$testpred

train$PredUS <- USlist$trainpred
test$PredUS <- USlist$testpred

train$PredOther <- Otherlist$trainpred
test$PredOther <- Otherlist$testpred

train$PredFR <- FRlist$trainpred
test$PredFR <- FRlist$testpred

train$PredIT <- ITlist$trainpred
test$PredIT <- ITlist$testpred

train$PredGB <- GBlist$trainpred
test$PredGB <- GBlist$testpred

train$PredES <- ESlist$trainpred
test$PredES <- ESlist$testpred

train$PredCA <- CAlist$trainpred
test$PredCA <- CAlist$testpred

train$PredDE <- DElist$trainpred
test$PredDE <- DElist$testpred

train$PredNL <- NLlist$trainpred
test$PredNL <- NLlist$testpred

train$PredAU <- AUlist$trainpred
test$PredAU <- AUlist$testpred

train$PredPT <- PTlist$trainpred
test$PredPT <- PTlist$testpred


#################################################

evalNDCG5 <- function(preds, dtrain) {
    labels <- getinfo(dtrain,"label")
    pred <- matrix(preds, nrow = 12)
    top5 <- apply(pred, 2, function(x1) order(x1, decreasing = TRUE)[1:5]-1)
    retval <- computeNDCG5(labels, t(top5))
    return (list(metric = "ndcg5", value = retval))
}

computeNDCG5 <- function(actual, predicted) {
    xx <- ifelse(predicted == actual, 1, 0)
    dcg <- function(yy) sum((2^yy - 1)/log(2:(length(yy)+1), base = 2))
    ndcg <- mean(apply(xx, 1, dcg))
    return (ndcg)
}

getRank <- function(arr) {
    retval <- rep(0, 12)
    retval[order(arr, decreasing = TRUE)] <- seq(1,12,1)
    return (retval)
}

set.seed(200)
train <- train[sample(nrow(train)),]
train <- train[sample(nrow(train)),]

numFolds <- 5
numRepeats <- 3
numRounds <- numFolds*numRepeats

label.country <- as.integer(factor(train$country_destination, levels = countryLevels))
label.country <- label.country - 1
numClasses <- max(label.country) + 1

folds <- createMultiFolds(train$country_destination, k = numFolds, times = numRepeats)

val.pred <- matrix(0, nrow=nrow(train), ncol=numClasses)
val.rank <- matrix(0, nrow=nrow(train), ncol=numClasses)
test.pred <- matrix(0, nrow = nrow(test), ncol = numClasses)
test.rank <- matrix(0, nrow = nrow(test), ncol = numClasses)

test <- test[order(test$id),]
test.DM  <- xgb.DMatrix(data = data.matrix(test[, -1]), missing = NA)

bestIters <- rep(0, length(folds))

for (ii in seq(1, length(folds)) ) {
    val.ids <- setdiff(1:nrow(train), folds[[ii]])
    print(length(val.ids))
    print(names(folds)[ii])
    
    train.DM  <- xgb.DMatrix(data = data.matrix(train[-val.ids,-c(1,2)]), 
                             label = label.country[-val.ids],
                             missing = NA)
    val.DM  <- xgb.DMatrix(data = data.matrix(train[val.ids,-c(1,2)]), 
                           label = label.country[val.ids],
                           missing = NA)
    
    wlist <- list(val = val.DM, train = train.DM)
    param <- list( max.depth = 6, eta = 0.22, booster = "gbtree",
                   num_class = numClasses, # gamma = 0.1,
                   subsample = 0.6, colsample_bytree = 0.6,
                   objective = "multi:softprob")#, eval_metric = "mlogloss") ## mlogloss, merror
    
    set.seed(1)
    model1 <- xgb.train(params = param, data = train.DM, nrounds = 200,
                        early.stop.round = 30,
                        nthread = 4, verbose = 1, print.every.n = 2,
                        missing = NA,
                        feval =  evalNDCG5,
                        watchlist = wlist, maximize = TRUE)
    
    bestIter <- model1$bestInd
    bestIters[ii] <- bestIter
    
    val.pred.fold <- predict(model1, newdata = val.DM, ntreelimit = bestIter)
    val.pred.fold <- matrix(data = val.pred.fold, nrow = numClasses, ncol = length(val.pred.fold) /numClasses)
    val.pred.fold <- t(val.pred.fold)
    val.pred[val.ids,] <- val.pred[val.ids,] + val.pred.fold
    
    val.fold.rank <- t(apply(val.pred.fold, 1, getRank))
    val.rank[val.ids,] <- val.rank[val.ids,] + val.fold.rank 
    
    pred1 <- data.frame(val.pred.fold)
    names(pred1) <- countryLevels
    val_top5 <- apply(pred1, 1, function(xx) names(sort(xx, decreasing = TRUE)[1:5]))
    print(paste("prob based = ", computeNDCG5(train$country_destination[val.ids], t(val_top5))))
    
    test.pred.fold <- predict(model1, newdata = test.DM, ntreelimit = bestIter)
    test.pred.fold <- matrix(data = test.pred.fold, nrow = numClasses, ncol = nrow(test))
    test.pred.fold <- t(test.pred.fold)
    test.pred <- test.pred + test.pred.fold
    
    test.fold.rank <- t(apply(test.pred.fold, 1, getRank))
    test.rank <- test.rank + test.fold.rank 
}

test.pred <- test.pred/numRounds
val.pred <- val.pred/numRepeats

print(bestIters) ## mean = 
## [1] 21  8 24 22 18 24 33 23 14 18 16 30 19 37 43

### probability based
predictions <- data.frame(val.pred)
names(predictions) <- countryLevels
val_top5 <- apply(predictions, 1, function(xx) names(sort(xx, decreasing = TRUE)[1:5]))
print(computeNDCG5(train$country_destination, t(val_top5)))
## CV = 0.8331714

### rank based
predictions <- data.frame(val.rank)
names(predictions) <- countryLevels
val_top5 <- apply(predictions, 1, function(xx) names(sort(xx)[1:5]))
print(computeNDCG5(train$country_destination, t(val_top5)))
## CV = 0.8333672

############## (based on probability)
### make test prediction and submission 
predictions <- data.frame(test.pred)
names(predictions) <- countryLevels
test_top5 <- apply(predictions, 1, function(xx) names(sort(xx, decreasing = TRUE)[1:5]))
test_top5 <- as.vector(test_top5)

## save predictions for later
predictions$TestId <- test$id
predictions <- predictions[,c("TestId", names(predictions)[1:12])]
write.csv(predictions, "./submissions/sub_u26a_raw_prob.csv", quote=FALSE, row.names = FALSE)

submission <- NULL
idx = test$id
id_mtx <-  matrix(idx, 1)[rep(1,5), ]
ids <- c(id_mtx)
submission$id <- ids

submission$country <- test_top5
submission <- as.data.frame(submission)
head(submission)

write.csv(submission, "./submissions/sub_u26a_prob.csv", quote=FALSE, row.names = FALSE)
### public LB = 0.88111 //// private LB = 0.88676

###############
### make test prediction and submission (based on ranks)
predictions <- data.frame(test.rank)
names(predictions) <- countryLevels
test_top5 <- apply(predictions, 1, function(xx) names(sort(xx)[1:5]))
test_top5 <- as.vector(test_top5)

## save predictions for later
predictions$TestId <- test$id
predictions <- predictions[,c("TestId", names(predictions)[1:12])]
write.csv(predictions, "./submissions/sub_u26a_raw_rank.csv", quote=FALSE, row.names = FALSE)

submission <- NULL
idx = test$id
id_mtx <-  matrix(idx, 1)[rep(1,5), ]
ids <- c(id_mtx)
submission$id <- ids

submission$country <- test_top5
submission <- as.data.frame(submission)
head(submission)

write.csv(submission, "./submissions/sub_u26a_rank.csv", quote=FALSE, row.names = FALSE)
### public LB = 0.88121 //// private LB = 0.88691


