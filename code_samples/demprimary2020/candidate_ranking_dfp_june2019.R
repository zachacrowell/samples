# Config
setwd('~/Downloads/data//dfp/')
require(data.table)
require(dplyr)
require(Hmisc)
require(caret)

# Helper function:
wrmse <- function(actual, predicted, weight){
  sqrt(sum((predicted-actual)^2*weight)/sum(weight))
}

#Load Data
#df4 <- fread('dfp_omni_4.19_data.csv', data.table = F)
#df5 <- fread('dfp_may19.csv', data.table = F)
df6 <- fread('dfp_RV_omni_062019.csv', data.table = F)


# Combine Top 5 & Bottom 5 Rankings:
cand <- c("biden", "sanders", "harris", "orourke", "booker", "klobuchar", "warren",
          "hickenlooper", "gillibrand", "delaney", "castro", "deblasio",
          "gabbard", "buttigieg", "inslee", "ryan", "moulton", "swalwell", "yang",
          "williamson", "gravel", "bullock", "bennet", "messam")
for (i in 1:24){
  df6[, cand[i]] <-  ifelse(df6[, c(paste0('TOP5_RANK_', i))]< 6,
                            df6[, c(paste0('TOP5_RANK_', i))],
                            ifelse(df6[, c(paste0('TOP5_20_', i))] == 1, 3,
                                   ifelse(df6[, c(paste0('BOTTOM5_RANK_', i))] < 6,
                                          25-df6[, c(paste0('BOTTOM5_RANK_', i))],
                                          ifelse(df6[, c(paste0('BOTTOM5_20_', i))]== 1,
                                                 23, 12.5)
                                   )
                            )
  )
}

# Create Weighted Ranks to test Spearman Correlations
# N.B. I ommitted this portion after exploratory testing
for (i in 1:24){
  df6[, paste0('rank_', cand[i])] <-  wtd.rank(df6[, cand[i]], df6$weight)/nrow(df6)
}

# Create Base Correlation Matrixes
df_cor <- cov.wt(df6[, cand], wt = df6$weight, cor = T)$cor
df_cor1 <- cov.wt(df6[, paste0('rank_', cand)], wt = df6$weight, cor = T)$cor


# Make Synthetics
df <- df6[, c(2:4, 7:16, 18:54, 180:360, 362, 363, 365, 367:376, 395:398, 400:402, 404:411, 413, 414)]
df <- df %>%  mutate_if(., is.integer, as.factor)
df$birthyr <- df6$birthyr ##make numeric

# After intital testing, set Hyper Params for Models
# #GBM 
grid <- expand.grid(n.trees=c(10, 20, 50),
                    shrinkage=c(0.05, 0.1, .2),
                    n.minobsinnode = c(10, 15, 20),
                    interaction.depth=c(2))
## LASSO
grid2 <-expand.grid(alpha=1, lambda=seq(0, 10, by = 0.1))


#Train Ensemble Models for all 24 caniddates:
for (i in 1:24){
  # Set y
  df$support <- df6[, cand[i]]
  
  # Run models
  gbm1 <- caret::train(support ~ ., data = df,
                       distribution="gaussian", method="gbm",
                       weights = df6$weight,
                       metric = "RMSE",
                       trControl =  trainControl(method = 'repeatedcv', 
                                                 number = 5),
                       tuneGrid = grid)
  mod1 <- train(support ~ ., data = df,
                family = "gaussian", method="glmnet",
                weights = df6$weight,
                trControl =  trainControl(method = 'repeatedcv',  number = 2),
                tuneGrid = grid2,
                preProc = c("center", "scale"))
  
  # Create Pred
  df6$pred1 <- predict(gbm1) %>% unlist(.)
  df6$pred2 <- predict(mod1) %>% unlist(.)
  df6$support <- df6[, cand[i]]
  # Combines 2 Models and Create Synethic Score
  reg1 <- lm(support ~ pred1*pred2, data = df6, weight = weight)
  df6$pred <- predict(reg1) %>% unlist(.)
  
  df6[, paste0('syn_', cand[i])] <- df6$pred
}


# Create full support variable based on nvl(rank, rescaled(syn_rank)):
for (i in 1:24){
  #rescale: using scales, but custom is (maxy-miny)*[(X-minx)/(maxx-minx)] + miny.
  # Start off with Orginial Rank
  df6[, c(paste0('support_', cand[i]))] <- df6[, cand[i]]
  # Fill Missing in TOP 5
  df6[, c(paste0('support_', cand[i]))][df6[, c(paste0('TOP5_20_', i))] == 1  &
                                          df6[, c(paste0('TOP5_RANK_', i))] > 5] <- 
    scales::rescale(df6[, c(paste0('syn_', cand[i]))][df6[, c(paste0('TOP5_20_', i))] == 1  &
                                                        df6[, c(paste0('TOP5_RANK_', i))] > 5], c(1, 5))
  # Fill in MIssing for non-top5, non-bottom5 ranks:
  df6[, c(paste0('support_', cand[i]))][df6[, c(paste0('TOP5_20_', i))] != 1  
                                        & df6[, c(paste0('BOTTOM5_20_', i))] != 1] <-
    scales::rescale(df6[, c(paste0('syn_', cand[i]))][df6[, c(paste0('TOP5_20_', i))] != 1  & 
                                                        df6[, c(paste0('BOTTOM5_20_', i))] != 1],
                    c(6, 19))
  # FInally Fill in Missing Bottom 5 Ranks
  df6[, c(paste0('support_', cand[i]))][df6[, c(paste0('BOTTOM5_20_', i))] == 1  &
                                          df6[, c(paste0('BOTTOM5_RANK_', i))] > 5] <-
    scales::rescale(df6[, c(paste0('syn_', cand[i]))][df6[, c(paste0('BOTTOM5_20_', i))] == 1  &
                                                        df6[, c(paste0('BOTTOM5_RANK_', i))] > 5],
                    c(19, 24))
}

# Create Final Correlation Matrix
df_cor2 <- cov.wt(df6[, c(paste0('support_', cand))],
                  wt = df6$weight, cor = T)$cor


# Validation checks
# Which Supporters are most easily predicted?
for(i in 1:24){
  wr <- wrmse(df6[, c(paste0('support_', cand[i]))], df6[, cand[i]], df6$weight)
  print(cand[i])
  print(wr)    
}



# Checks for "Case Study: Harriss"
# Set y
df$support <- df6$syn_harris
# Run models on Final Pred
gbm1 <- caret::train(support ~ ., data = df,
                     distribution="gaussian", method="gbm",
                     weights = df6$weight,
                     metric = "RMSE",
                     trControl =  trainControl(method = 'repeatedcv', 
                                               number = 5),
                     tuneGrid = grid)
# Print Results
print(summary(gbm1$finalModel))

