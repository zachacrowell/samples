# Config
setwd('~/Downloads/data//dfp/')
require(data.table)
require(dplyr)
require(Hmisc)
require(caret)
require(ggcorrplot)

#Load Data
df8 <- fread('FIRSTDEBATE_DATA.csv', data.table = F)
df8 <- fread('july_alldata.csv', data.table = F)

cand <- c("bennet", "biden", "booker","bullock", "buttigieg","castro", 
          "deblasio", "delaney","gabbard", "gillibrand", "gravel", 
         "harris", "hickenlooper","inslee",  "klobuchar","messam", "moulton",
         "orourke",  "ryan", "sanders", "warren","williamson", "yang", "sestak")
cand1 <- c("bennet", "biden", "booker","bullock", "buttigieg","castro", 
          "deblasio", "delaney","gabbard", "gillibrand", "gravel", 
          "harris", "hickenlooper","inslee",  "klobuchar","messam", "moulton",
          "orourke",  "ryan", "sanders", "swalwell", "warren","williamson", "yang", "sestak")

#Remove Swall from rankings
nums <- c(1:20, 22:25)

#swall
for (i in nums){
  df8[, c(paste0('RANKCONSIDERING20_', i))] <-  
    ifelse(!is.na(df8$RANKCONSIDERING20_21) & 
             !is.na(df8[, c(paste0('RANKCONSIDERING20_', i))]) & 
             df8$RANKCONSIDERING20_21 <= df8[, c(paste0('RANKCONSIDERING20_', i))],
           df8[, c(paste0('RANKCONSIDERING20_', i))] - 1,
           df8[, c(paste0('RANKCONSIDERING20_', i))]
    )
}

# Convert NA to 0
for (i in 1:25){
  df8[, c(paste0('CONSIDERING20_', i))] <-  ifelse(!is.na(df8[, c(paste0('CONSIDERING20_', i))]),
                            df8[, c(paste0('CONSIDERING20_', i))],
                            0
  )
}
# Create intitial Rank
for (i in 1:24){
  df8[, cand[i]] <-  ifelse(!is.na(df8[, c(paste0('RANKCONSIDERING20_', nums[i]))]),
                            df8[, c(paste0('RANKCONSIDERING20_', nums[i]))],
                            NA
  )
}

# Get cols for rowmeans
df_rank <- df8 %>% select(., starts_with('RANKCONSIDERING')) 
df_rank$RANKCONSIDERING20_25 <- NULL
col_rank <- colnames(df_rank)
df_con <- df8 %>% select(., starts_with('CONSIDERING')) 
df_con$CONSIDERING20_25 <- NULL
df_con$CONSIDERING20_97 <- NULL
col_con <- colnames(df_con)

# Create Considering Counts
df8$con <- rowSums(df8[, colnames(df8) %in% col_con], na.rm = T)
df8$rank <- rowSums(!is.na(df8[, colnames(df8) %in% col_rank]), na.rm = T)


# Fill in Consideirng but UnRanked
for (i in 1:24){
df8[, cand[i]] <- ifelse(is.na(df8[, cand[i]]) & df8[, c(paste0('CONSIDERING20_', nums[i]))] == 1,
                         (df8$con + df8$rank+1)/2, 
                         df8[, cand[i]]
                         )
}

#Get Ranks for Not Considering

df_notcon <- df8 %>% select(., starts_with('NOTCONSIDERING')) 
df_notcon$NOTCONSIDERING20_21 <- NULL
df_notcon$NOTCONSIDERING20_97 <- NULL
col_notcon <- colnames(df_notcon)

df8$notcon <- rowSums(df8[, colnames(df8) %in% col_notcon], na.rm = T)

for (i in 1:24){
  df8[, cand[i]] <- ifelse(is.na(df8[, cand[i]]) & df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 1,
                           (49-df8$notcon)/2, 
                           df8[, cand[i]]
  )
}

# Fill in Rest
for (i in 1:24){
  df8[, cand[i]] <- ifelse(is.na(df8[, cand[i]]),
                           (25+df8$con-df8$notcon)/2, 
                           df8[, cand[i]]
  )
}


# New WEights
df8$weight_top <-  df8$con- df8$rank
df8$weight_top2 <- NA
df8$weight_top2[df8$weight_top<2] <- 1
df8$weight_top2[df8$weight_top>=2] <-  (1/4)*as.numeric(lapply(df8$weight_top[df8$weight_top>=2],
                                                               function(x) 1/var(seq(to = x))))
table(df8$weight_top2)
#middle weight
df8$weight_mid <- 25 -  df8$con - df8$notcon 
df8$weight_mid2 <- NA
df8$weight_mid2[df8$weight_mid<2] <- 1
df8$weight_mid2[df8$weight_mid>=2] <- 
  (1/4)*as.numeric(lapply(df8$weight_mid[df8$weight_mid>=2],  function(x) 1/var(seq(to = x))))

#bottom weight
df8$weight_bot <- df8$notcon
df8$weight_bot2 <- NA
df8$weight_bot2[df8$weight_bot<2] <- 1
df8$weight_bot2[df8$weight_bot>=2] <- 
  (1/4)*as.numeric(lapply(df8$weight_bot[df8$weight_bot>=2],  function(x) 1/var(seq(to = x))))



for (i in 1:24){
  df8[, paste0(cand[i], '_var')] <- ifelse(!is.na(df8[, paste0('RANKCONSIDERING20_', nums[i])]), 1, 
                                           ifelse(df8[, paste0('CONSIDERING20_', nums[i])] == 1,
                                                  df8$weight_top2, 
                                                  ifelse(df8[, paste0('NOTCONSIDERING20_', nums[i])] ==1,
                                                         df8$weight_bot2, df8$weight_mid2)
                                                  )
                                           )
  df8[is.na(df8[, paste0(cand[i], '_var')]), paste0(cand[i], '_var')] <- 0
  }

# Get COls for Training:
df <- df8[, c(3:12, 14:18, 20:25, 28:53, 166, 192:241, 243:395, 397, 398, 400:407, 409:416, 418)]
df$postdebate <- ifelse(!is.na(df8$weight_postdebate), 1, 0) %>% as.factor
classes <- lapply(df, class)

lapply(df, class)



# Make Synthetics
min(sapply(df, function(x) sum(!duplicated(x))))

df <- df %>%  mutate_if(., is.integer, as.character)

df <- df %>%  mutate_if(., function(x) sum(!duplicated(x)) > 17, as.numeric)
classes <- lapply(df, class)
nonnum <- colnames(df[, classes != "numeric"])
df[, classes != "numeric"] <- df %>% select_if(colnames(.) %in% nonnum) %>% 
  mutate %>% replace(., is.na(.), "unknown")
df$inputregstate <- as.factor(df$inputregstate)
df <- df %>%  mutate_if(., is.character, as.factor)
classes <- lapply(df, class)
max(sapply(df[, classes == "factor" & colnames(df) != "inputregstate"], function(x) sum(!duplicated(x))))

##make numeric
meanx <-function(x){
  mean(x, na.rm = T)
}
cols <- df %>% select(., starts_with('cand_alloc')) %>% colnames(.)
df_mean <- df[, colnames(df) %in% cols] %>% summarise_all(funs(meanx))
for (i in 1:17) (
  df[, cols[i]] <- ifelse(is.na(df[, cols[i]]), 
                           df_mean[, cols[i]],
                           df[, cols[i]])
)



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
  st <- Sys.time()
  # Set y and weight
  df$support <- df8[, cand[i]]
  df8$weight <- df8$weight_fullsample*df8[, paste0(cand[i], "_var")]
  
  # Run models
  gbm1 <- caret::train(support ~ ., data = df,
                       distribution="gaussian", method="gbm",
                       weights = df8$weight,
                       metric = "RMSE",
                       trControl =  trainControl(method = 'repeatedcv', 
                                                 number = 5),
                       tuneGrid = grid)
  mod1 <- train(support ~ ., data = df,
                family = "gaussian", method="glmnet",
                weights = df8$weight,
                trControl =  trainControl(method = 'repeatedcv',  number = 2),
                tuneGrid = grid2,
                preProc = c("center", "scale"))
  
  # Create Pred
  df8$pred1 <- predict(gbm1) %>% unlist(.)
  df8$pred2 <- predict(mod1) %>% unlist(.)
  df8$support <- df8[, cand[i]]
  # Combines 2 Models and Create Synethic Score
  
  reg1 <- lm(support ~ pred1*pred2, data = df8, weight = weight)
  df8$pred <- predict(reg1) %>% unlist(.)
  
  df8[, paste0('syn_', cand[i])] <- df8$pred
  
  # Save Features
  df_feat <- summary(gbm1$finalModel)
  df_feat <- df_feat[df_feat$rel.inf > 0,]
  write.csv(df_feat, paste0('feat_', cand[i]), row.names = F)
  print(cand[i])
  fn <- Sys.time()
  print(fn-st)
}

 # Create SUpport Model
for (i in 1:24){
  #rescale: using scales, but custom is (maxy-miny)*[(X-minx)/(maxx-minx)] + miny.
  # Start off with Orginial Rank
  df8[, c(paste0('support_', cand[i]))] <- df8[, cand[i]]
  # Replace Considering but Unranked
  df8[, c(paste0('support_', cand[i]))][is.na(df8[, c(paste0('RANKCONSIDERING20_', nums[i]))]) &
                                            df8[, c(paste0('CONSIDERING20_', nums[i]))] == 1 &
                                            df8$con > 1] <- 
    df8[, c(paste0('syn_', cand[i]))][is.na(df8[, c(paste0('RANKCONSIDERING20_', nums[i]))]) &
                                            df8[, c(paste0('CONSIDERING20_', nums[i]))] == 1 &
                                            df8$con > 1]
  # Rescale (with tiebreaker adjustment)
  df8[, c(paste0('support_', cand[i]))][is.na(df8[, c(paste0('RANKCONSIDERING20_', nums[i]))]) &
                                          df8[, c(paste0('CONSIDERING20_', nums[i]))] == 1 &
                                          df8$con > 1 & 
                                          df8[, c(paste0('syn_', cand[i]))] > df8$con] <- 
    df8$con[is.na(df8[, c(paste0('RANKCONSIDERING20_', nums[i]))]) &
              df8[, c(paste0('CONSIDERING20_', nums[i]))] == 1 &  df8$con > 1 & 
              df8[, c(paste0('syn_', cand[i]))] > df8$con] - .05 + 
    .001*df8[is.na(df8[, c(paste0('RANKCONSIDERING20_', nums[i]))]) &
               df8[, c(paste0('CONSIDERING20_', nums[i]))] == 1 &  df8$con > 1 & 
               df8[, c(paste0('syn_', cand[i]))] > df8$con, c(paste0('syn_', cand[i]))]
  # Replace Not Considering
 df8[, c(paste0('support_', cand[i]))][!is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) & 
                                         df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 1 &
                                          df8$notcon > 1] <- 
    df8[, c(paste0('syn_', cand[i]))][!is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) &
                                        df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 1 &
                                            df8$notcon > 1]
  #Rescale w tiebreaker
 df8[, c(paste0('support_', cand[i]))][!is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) & 
                                         df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 1 &
                                         df8$notcon > 1 & 
                                         df8[, c(paste0('syn_', cand[i]))] < 24-df8$notcon] <-
   24-df8$notcon[!is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) & 
                                           df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 1 &
                                           df8$notcon > 1 & 
                                           df8[, c(paste0('syn_', cand[i]))] < 24-df8$notcon] + .05 -
   .001*df8[!is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) & 
                     df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 1 &
                     df8$notcon > 1 & 
                     df8[, c(paste0('syn_', cand[i]))] < 24-df8$notcon,
            c(paste0('syn_', cand[i]))]
 # Replace InBetweens
 df8[, c(paste0('support_', cand[i]))][(is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) | 
                                         df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 0) &
                                         (is.na(df8[, c(paste0('CONSIDERING20_', nums[i]))]) | 
                                         df8[, c(paste0('CONSIDERING20_', nums[i]))] == 0) &
                                         df8$con+df8$notcon >= 0 & df8$con+df8$notcon <24] <- 
   df8[, c(paste0('syn_', cand[i]))][(is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) | 
                                        df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 0) &
                                       (is.na(df8[, c(paste0('CONSIDERING20_', nums[i]))]) | 
                                          df8[, c(paste0('CONSIDERING20_', nums[i]))] == 0) &
                                       df8$con+df8$notcon >= 0 & df8$con+df8$notcon <24]
 #Rescale from top w tiebreaker
 df8[, c(paste0('support_', cand[i]))][(is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) | 
                                          df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 0) &
                                         (is.na(df8[, c(paste0('CONSIDERING20_', nums[i]))]) | 
                                            df8[, c(paste0('CONSIDERING20_', nums[i]))] == 0) &
                                         df8$con+df8$notcon <24 &
                                         df8$con+df8$notcon >= 0 &
                                         df8[, c(paste0('syn_', cand[i]))] > 24-df8$notcon] <-
   24-df8$notcon[(is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) | 
                    df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 0) &
                   (is.na(df8[, c(paste0('CONSIDERING20_', nums[i]))]) | 
                      df8[, c(paste0('CONSIDERING20_', nums[i]))] == 0) &
                   df8$con+df8$notcon <24 &
                   df8$con+df8$notcon >= 0 &
                   df8[, c(paste0('syn_', cand[i]))] > 24- df8$notcon] - .05 +
   .001*df8[(is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) | 
               df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 0) &
              (is.na(df8[, c(paste0('CONSIDERING20_', nums[i]))]) | 
                 df8[, c(paste0('CONSIDERING20_', nums[i]))] == 0) &
              df8$con+df8$notcon <24 &
              df8$con+df8$notcon >= 0 &
              df8[, c(paste0('syn_', cand[i]))]  > 24- df8$notcon,
            c(paste0('syn_', cand[i]))]
 
 # Rescale from bottom 
 df8[, c(paste0('support_', cand[i]))][(is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) | 
                                          df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 0) &
                                         (is.na(df8[, c(paste0('CONSIDERING20_', nums[i]))]) | 
                                            df8[, c(paste0('CONSIDERING20_', nums[i]))] == 0) &
                                         df8$con+df8$notcon <24 &
                                         df8$con+df8$notcon >= 0 &
                                         df8[, c(paste0('syn_', cand[i]))] < df8$con] <-
   24-df8$notcon[(is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) | 
                    df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 0) &
                   (is.na(df8[, c(paste0('CONSIDERING20_', nums[i]))]) | 
                      df8[, c(paste0('CONSIDERING20_', nums[i]))] == 0) &
                   df8$con+df8$notcon <24 &
                   df8$con+df8$notcon >= 0 &
                   df8[, c(paste0('syn_', cand[i]))] < df8$con] + .05 -
   .001*df8[(is.na(df8[, c(paste0('NOTCONSIDERING20_', nums[i]))]) | 
               df8[, c(paste0('NOTCONSIDERING20_', nums[i]))] == 0) &
              (is.na(df8[, c(paste0('CONSIDERING20_', nums[i]))]) | 
                 df8[, c(paste0('CONSIDERING20_', nums[i]))] == 0) &
              df8$con+df8$notcon <24 &
              df8$con+df8$notcon >= 0 &
              df8[, c(paste0('syn_', cand[i]))] < df8$con,
            c(paste0('syn_', cand[i]))]
 #enforce global min of 1
 df8[df8[,c(paste0('support_', cand[i]))] < 1, c(paste0('support_', cand[i]))] <- 
   1.05 + .01*(df8[df8[,c(paste0('support_', cand[i]))] < 1, c(paste0('support_', cand[i]))])
 #enforce global max of 24
df8[df8[,c(paste0('support_', cand[i]))] > 24, c(paste0('support_', cand[i]))] <- 
23.99 + .0001*(df8[df8[,c(paste0('support_', cand[i]))] >24, c(paste0('support_', cand[i]))]-24)
 
}

# Test
for (i in cand) {
  for (j in cand) {
    ct <- sum(df8$weight_postdebate[df8[, i]<df8[, j]], na.rm = T) -
      sum(df8$weight_postdebate[df8[, i]>df8[, j]], na.rm = T)
    print(paste(i, j, "won:", ifelse(ct == T, i, j),
          "margin:", round(ct/sum(df8$weight_postdebate, na.rm = T), 3)))
    
  }
}

for (i in cand) {
  for (j in cand) {
    ct <- sum(df8$weight_postdebate[df8[, paste0("support_", i)]<df8[, paste0("support_", j)]],
              na.rm = T) -
      sum(df8$weight_postdebate[df8[, paste0("support_", i)]>df8[, paste0("support_", j)]],
          na.rm = T)
    print(paste(i, j, "won:", ifelse(ct == T, i, j),
                "margin:", round(ct/sum(df8$weight_postdebate, na.rm = T), 4)))
    
  }
}


for (i in cand) {
  for (j in cand) {
    ct <- sum(df8$weight_postdebate[df8[, paste0("syn_", i)]<df8[, paste0("support_", j)]],
              na.rm = T) -
      sum(df8$weight_postdebate[df8[, paste0("syn_", i)]>df8[, paste0("support_", j)]],
          na.rm = T)
    print(paste(i, j, "won:", ifelse(ct == T, i, j),
                "margin:", round(ct/sum(df8$weight_postdebate, na.rm = T), 4)))
    
  }
}



# Winers
c("warren", "harris", "biden",  "sanders","buttigieg",  "booker", "castro","orourke",
  "klobuchar", "gillibrand", "gabbard","inslee", "yang", "deblasio", "moulton",
  "hickenlooper","bennet","bullock","ryan", "sestak","delaney", "messam", 
  "gravel", "williamson")

for (i in 1:24) {
    ct <- sum(df8$weight_postdebate[df8[, paste0("CONSIDERING20_", nums[i])]==1], na.rm = T)-sum(df8$weight_postdebate[df8[, paste0("NOTCONSIDERING20_", nums[i])]==1], na.rm = T)
    print(paste(cand[i], "cond:", round(ct/sum(df8$weight_postdebate, na.rm = T), 4)))

}
# Create Final Correlation Matrix
df_cor2 <- cov.wt(df8[!is.na(df8$weight_postdebate), 
                      c("gender", paste0('support_', cand2))],
                  wt = df8$weight_postdebate[!is.na(df8$weight_postdebate)], cor = T)$cor
df_cor2 <- cov.wt(df8[!is.na(df8$weight_postdebate), cand2],
                  wt = df8$weight_postdebate[!is.na(df8$weight_postdebate)], cor = T)$cor

for (i in 1:24){
  df8[, paste0('rank_', cand[i])] <-  wtd.rank(df8[, paste0('support_', cand[i])], df8$weight_fullsample)/nrow(df8)
}


df_cor8f <- df8 %>% filter(!is.na(df8$weight_fullsample)) %>% select(starts_with('support_')) %>%
  cov.wt(., wt = df8$weight_fullsample[!is.na(df8$weight_fullsample)], cor = T)
df_cor8f <- df_cor8f$cor


df_cor8fr <- df8 %>% filter(!is.na(df8$weight_fullsample)) %>% select(starts_with('rank_')) %>%
  cov.wt(., wt = df8$weight_fullsample[!is.na(df8$weight_fullsample)], cor = T)
df_cor8fr <- df_cor8fr$cor

df_cor8r <- df8 %>% filter(!is.na(df8$weight_postdebate)) %>% select(starts_with('rank_')) %>%
  cov.wt(., wt = df8$weight_postdebate[!is.na(df8$weight_postdebate)], cor = T)
df_cor8r <- df_cor8r$cor
library(ggplot2)
p <- ggplot(df8, aes(x = harris, y = support_harris, weight = weight_fullsample)) +
  geom_smooth(method="loess") + geom_point(alpha = .5) + theme_dfp() +
  xlab("Harris Temp Ranking") + ylab("Harris Final Ranking") + 
  scale_color_dfp(name="Type") + ggtitle_dfp("Rank Comparison") 
p <- gridExtra::grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                             
                             widths=c(4,1,2), heights=c(4,.25,.25))

p <- ggplot(df8, aes(x = support_harris, y = ..density.., weight = weight_fullsample)) + 
  geom_histogram(binwidth = 1) + #xlim(1, 25) +
  xlab("Ranked Support") + ylab("Percent") +
  scale_y_continuous(labels = scales::percent) +
    ggtitle_dfp("Histogram\n Harris Support") +
    theme_dfp() + scale_color_dfp(name="Type")
p <- gridExtra::grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                             
                             widths=c(4,1,2), heights=c(4,.25,.25))


img <- png::readPNG("~/Downloads/dfpStataRviz-master/dfp-line-logo-black.png")
require(gridExtra)
logo <- grid::rasterGrob(img, interpolate=TRUE)

#### Your code with p <- ggplot() as your plot

p <- gridExtra::grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  
                  widths=c(4,1,2), heights=c(4,.25,.25))

ggsave("~/Downloads/harris_rank.png", p, dpi=320, height = 6, width=6, units="in")




#corplot
df_cor8fr <- df_cor8fr[c(1:10,12:24), c(1:10,12:24)]
df_cor8r <- df_cor8r[c(1:10,12:24), c(1:10,12:24)]

df_cor8s <- df8 %>% filter(!is.na(df8$weight_postdebate)) %>% select(starts_with('support_')) %>%
  cov.wt(., wt = df8$weight_postdebate[!is.na(df8$weight_postdebate)], cor = T)
df_cor8s <- df_cor8s$cor
df_cor8s <- df_cor8s[c(1:11,13:24), c(1:11,13:24)]
quartz()
#colors = pal_dfp(6)
ggcorrplot(df_cor8s, hc.order = TRUE, method = "square", type = "lower", insig = "blank",
           colors = c("#E46726", "white", "#6D9EC1", "white")) + 
  ggtitle_dfp("Correlation Matrix:\n Post-Debate Rankings ") +
  xlab("") + ylab("") #+
  theme_dfp() + scale_color_dfp(name="Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 scale_x_discrete(labels = c("Castro", "Buttigieg", "Klobuchar", "ORourke", "Gillibrand", 
                             "Booker", "Harris", "Delaney", "Messam", "DeBlasio", 
                             "Williamson", "Gabbard", "Yang", "Bennett", "Hickenlooper",
                             "Inslee", "Bullock", "Sestak", "Moulton", "Ryan",
                              "Biden", "Sanders")) +
  scale_y_discrete(labels = c( "Warren", "Castro", "Buttigieg", "Klobuchar", "ORourke", "Gillibrand", 
                              "Booker", "Harris", "Delaney", "Messam", "DeBlasio", 
                              "Williamson", "Gabbard", "Yang", "Bennett", "Hickenlooper",
                              "Inslee", "Bullock", "Sestak", "Moulton", "Ryan",
                              "Biden"))
df_cor_small <- df_cor8r[c(2, 3, 5, 11, 14, 17, 19, 20, 22),
                         c(2, 3, 5, 11, 14, 17, 19, 20, 22)]
df_cor_small <- df_cor8s[c(3, 4, 6, 7, 12, 15, 18, 20, 21, 23),
                         c(3, 4, 6, 7, 12, 15, 18, 20, 21, 23)]
quartz()
p <- ggcorrplot(df_cor_small, hc.order = TRUE, method = "square", type = "lower", insig = "blank",
           colors = c("#E46726", "white", "#6D9EC1", "white")) + 
  ggtitle_dfp("Correlation Matrix:\n Post-Debate Rankings ") +
  xlab("") + ylab("") +
  theme_dfp() + scale_color_dfp(name="Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(labels = c("Yang", "Biden", "ORourke", "Harris", "Warren", 
                              "Booker", "Castro", "Buttigieg", "Klobuchar"))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_discrete(labels = c("Sanders", "Yang", "Biden", "ORourke", "Harris", "Warren", 
                              "Booker", "Castro", "Buttigieg"))
p <- gridExtra::grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                             
                             widths=c(4,1,2), heights=c(4,.25,.25))

#+   
  scale_x_discrete(labels = c("Gillibrand", "Warren", "Booker", "Harris", "ORourke",
                              "Castro", "Buttigieg", "Klobuchar", "DeBlasio", "Delaney",
                              "Messam", "Gabbard", "Williammson", "Moulton", "Yang",
                              "Ryan", "Inslee", "Bullock", "Sestak", "Bennet", "Hickenlooper",
                              "Biden")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_discrete(labels = c("Gillibrand", "Warren", "Booker", "Harris", "ORourke",
                              "Castro", "Buttigieg", "Klobuchar", "DeBlasio", "Delaney",
                              "Messam", "Gabbard", "Williammson", "Moulton", "Yang",
                              "Ryan", "Inslee", "Bullock", "Sestak", "Bennet", "Hickenlooper",
                              "Biden"))


# Validation checks
# Which Supporters are most easily predicted?
for(i in 1:24){
  wr <- wrmse(df8[, c(paste0('support_', cand[i]))], df8[, cand[i]], df8$weight_fullsample)
  print(cand[i])
  print(wr)    
}


# Four Way
sum(df8$weight_postdebate[df8$support_warren < df8$support_harris & 
                          df8$support_warren < df8$support_biden &
                          df8$support_warren < df8$support_sanders
                          ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)
sum(df8$weight_postdebate[df8$support_harris< df8$support_warren & 
                            df8$support_harris < df8$support_biden &
                            df8$support_harris < df8$support_sanders
                          ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)
sum(df8$weight_postdebate[df8$support_biden < df8$support_harris & 
                            df8$support_biden < df8$support_warren &
                            df8$support_biden < df8$support_sanders
                          ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)
sum(df8$weight_postdebate[df8$support_sanders < df8$support_harris & 
                            df8$support_sanders < df8$support_biden &
                            df8$support_sanders < df8$support_warren
                          ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)

# 3 way A
sum(df8$weight_postdebate[
                            df8$support_warren < df8$support_biden &
                            df8$support_warren < df8$support_sanders
                          ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)

sum(df8$weight_postdebate[
                            df8$support_biden < df8$support_warren &
                            df8$support_biden < df8$support_sanders
                          ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)
sum(df8$weight_postdebate[
                            df8$support_sanders < df8$support_biden &
                            df8$support_sanders < df8$support_warren
                          ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)

# 3 way A


sum(df8$weight_postdebate[
  df8$support_warren < df8$support_biden &
    df8$support_warren< df8$support_sanders
  ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)

sum(df8$weight_postdebate[
  df8$support_biden < df8$support_harris &
    df8$support_biden < df8$support_sanders
  ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)
sum(df8$weight_postdebate[
  df8$support_sanders < df8$support_biden &
    df8$support_sanders < df8$support_harris
  ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)

sum(df8$weight_postdebate[
  df8$support_harris < df8$support_biden &
    df8$support_harris< df8$support_sanders
  ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)

sum(df8$weight_postdebate[
  df8$support_biden < df8$support_harris &
    df8$support_biden < df8$support_sanders
  ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)
sum(df8$weight_postdebate[
  df8$support_sanders < df8$support_biden &
    df8$support_sanders < df8$support_harris
  ], na.rm = T)/sum(df8$weight_postdebate, na.rm = T)




# Black
df9 <- df8[df8$race3==2, ]
sum(df9$weight_postdebate[df9$support_warren < df9$support_harris & 
                            df9$support_warren < df9$support_biden &
                            df9$support_warren < df9$support_sanders
                          ], na.rm = T)/sum(df9$weight_postdebate, na.rm = T)
sum(df9$weight_postdebate[df9$support_harris< df9$support_warren & 
                            df9$support_harris < df9$support_biden &
                            df9$support_harris < df9$support_sanders
                          ], na.rm = T)/sum(df9$weight_postdebate, na.rm = T)
sum(df9$weight_postdebate[df9$support_biden < df9$support_harris & 
                            df9$support_biden < df9$support_warren &
                            df9$support_biden < df9$support_sanders
                          ], na.rm = T)/sum(df9$weight_postdebate, na.rm = T)
sum(df9$weight_postdebate[df9$support_sanders < df9$support_harris & 
                            df9$support_sanders < df9$support_biden &
                            df9$support_sanders < df9$support_warren
                          ], na.rm = T)/sum(df9$weight_postdebate, na.rm = T)

sum(df9$weight_postdebate[
  df9$support_biden < df9$support_warren], na.rm = T)/sum(df9$weight_postdebate, na.rm = T)

sum(df9$weight_postdebate[
  df9$support_harris >= df9$support_biden], na.rm = T)/sum(df9$weight_postdebate, na.rm = T)

sum(df9$weight_postdebate[
  df9$support_biden < df9$support_harris &
    df9$support_biden < df9$support_sanders
  ], na.rm = T)/sum(df9$weight_postdebate, na.rm = T)
sum(df9$weight_postdebate[
  df9$support_sanders < df9$support_biden &
    df9$support_sanders < df9$support_harris
  ], na.rm = T)/sum(df9$weight_postdebate, na.rm = T)

write.csv(df8, 'july_alldata.csv')

sum(df8$weight_postdebate[
  df8$support_harris < df8$support_biden &
    df8$race4 == 2
  ], na.rm = T)/sum(df8$weight_postdebate[df8$race4==2], na.rm = T)

sum(df8$weight_postdebate[
  df8$support_harris > df8$support_biden &
    df8$race4 == 2
  ], na.rm = T)/sum(df8$weight_postdebate[df8$race4==2], na.rm = T)


for (i in 1:25){
  print(cand1[i])
  print(sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                    df8[,paste0("CONSIDERING20_",i)] == 1],na.rm = T)
        -sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) & 
                                     df8[,paste0("NOTCONSIDERING20_",i)] == 1],
             na.rm = T))
  }


for (i in 1:24){
  print(paste(cand[i],
  round((weighted.median(df8[,paste0("support_",cand[i])], df8$weight_postdebate) - 
    weighted.median(df8[,paste0("support_",cand[i])], df8$weight_predebate)),2)
  ))
}

for (i in 1:24){
  print(paste(cand[i],
              round((weighted.median(df8[,paste0("support_",cand[i])], df8$weight_postdebate) - 
                       weighted.median(df8[,paste0("support_",cand[i])], df8$weight_predebate)),2)
  ))
}

for (i in 1:24){
  print(paste(cand[i],
              round((weighted.median(df8[,paste0("support_",cand[i])], df8$weight_postdebate)),2)
  ))
}


for (i in 1:25){
  print(paste(cand1[i],
  (sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                    df8[,paste0("CONSIDERING20_",i)] == 1],na.rm = T)
        -sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) & 
                                     df8[,paste0("NOTCONSIDERING20_",i)] == 1],
             na.rm = T))-
          (sum(df8$weight_predebate[!is.na(df8$weight_predebate) &
                                                      df8[,paste0("CONSIDERING20_",i)] == 1],na.rm = T)
                          -sum(df8$weight_predebate[!is.na(df8$weight_predebate) & 
                                                       df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                               na.rm = T))))
}

# net considering
for (i in 1:25){
  print(paste(cand1[i],
              (sum(df8$weight_fullsample[!is.na(df8$weight_fullsample) &
                                           df8[,paste0("CONSIDERING20_",i)] == 1],na.rm = T)
               -sum(df8$weight_fullsample[!is.na(df8$weight_fullsample) &
                                            df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                    na.rm = T))/sum(!is.na(df8$weight_fullsample))))
}


for (i in 1:25){
  print(paste(cand1[i],
              (sum(df8$weight_fullsample[!is.na(df8$weight_fullsample) &
                                           df8[,paste0("CONSIDERING20_",i)] == 1],na.rm = T)
               -sum(df8$weight_fullsample[!is.na(df8$weight_fullsample) &
                                            df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                    na.rm = T))/sum(!is.na(df8$weight_fullsample)),
              (sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                           df8[,paste0("CONSIDERING20_",i)] == 1],na.rm = T)
               -sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                            df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                    na.rm = T))/sum(!is.na(df8$weight_postdebate)),
              #pre
              (sum(df8$weight_predebate[!is.na(df8$weight_predebate) &
                                           df8[,paste0("CONSIDERING20_",i)] == 1],na.rm = T)
               -sum(df8$weight_predebate[!is.na(df8$weight_predebate) &
                                            df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                    na.rm = T))/sum(!is.na(df8$weight_predebate))
              ))
}

for (i in 1:25){
  print(paste(cand1[i],
              # Consiering All
              sum(df8$weight_fullsample[!is.na(df8$weight_fullsample) &
                                           df8[,paste0("CONSIDERING20_",i)] == 1],
                  na.rm = T)/sum(!is.na(df8$weight_fullsample)),
              
              # Not Considering All
               sum(df8$weight_fullsample[!is.na(df8$weight_fullsample) &
                                            df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                    na.rm = T)/sum(!is.na(df8$weight_fullsample)),
              #ConsideringPost
              sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                          df8[,paste0("CONSIDERING20_",i)] == 1],
                  na.rm = T)/sum(!is.na(df8$weight_postdebate)),
              # Not Considering Post
              sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                          df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                  na.rm = T)/sum(!is.na(df8$weight_postdebate)),
              # Considering Pre
              sum(df8$weight_predebate[!is.na(df8$weight_predebate) &
                                          df8[,paste0("CONSIDERING20_",i)] == 1],
                  na.rm = T)/sum(!is.na(df8$weight_predebate)),
              # Not Considering Pre
              sum(df8$weight_predebate[!is.na(df8$weight_predebate) &
                                          df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                  na.rm = T)/sum(!is.na(df8$weight_predebate))
  ))
}

df1 <- read.csv("~/Downloads/dfp_netconsider.csv")
df1$shift <- df1$post - df1$pre 
library(PropCIs)
diffscoreci(7, 21, 13, 17,
            conf.level=0.95)
ggplot(data=df1, aes(x=cand, y=shift)) +
#  geom_pointrange() + 
 # geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
 #xlab("Label") 
  theme_bw()  # use a white background
#0.02642229, 0.02366347,
df1 <- df1[!df1$cand %in% c("gillibrand", "gravel", "swalwell", "hickenlooper", "inslee"),]
df1$se <- c(0.02601463, 0.03189145, 0.02638918, 0.02748636, 0.02833783, 0.02849689,
            0.02897637, 0.03451388, 0.02544967, 0.02983778,
            0.02580791, 0.02304944, 0.02691504, 0.02658129, 0.03364202, 0.03065637, 0.0268581, 0.03115616, 0.0291451, 0.02850638)
quartz()
df1$cand <- stringr::str_to_title(df1$cand)
p <- ggplot(df1, aes(reorder(cand,shift), shift)) + #geom_bar(stat="identity", width=0.5) +
  geom_point() + 
  geom_errorbar(aes(ymin=shift-1.96*se, ymax=shift+1.96*se)) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip 
  coord_flip() +theme_dfp() + scale_color_dfp(name="Type") + xlab("Candidate") + 
  ylab("Change in Percent Considering - Not Considering") +
  ggtitle("Post-Debate Shift in Net Considering")
p <- gridExtra::grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                                  
                                  widths=c(4,1,2), heights=c(4,.25,.25))


df1$se2 <- c (0.01343772, 0.01732389, 0.01607577, 0.01535553, 0.01682327,
              0.01702681, 0.01566556, 0.02048729, 0.01582644, 
              0.01696097, 0.0160287, 0.01287201, 0.01249092, 0.01746054,
              0.02524215, 0.01651249, 0.01624026, 0.01562988, 0.0160112, 0.01926041)
p <- ggplot(df1, aes(reorder(cand,-post), post)) + geom_bar(stat="identity", width=0.5) +
  geom_errorbar(aes(ymin=post-1.96*se2, ymax=post+1.96*se2)) +
 theme_dfp() + scale_color_dfp(name="Type") + xlab("Candidate") + 
  #coord_flip() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept=0, lty=1) +
  ylab("Considering - Not Considering") + ggtitle("Net Considering: Post-Debate") 
p <- gridExtra::grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                             
                             widths=c(4,1,2), heights=c(4,.25,.25))

  
for (i in 1:25){
  print(paste(cand1[i],
  (sum(df8$weight_fullsample[!is.na(df8$weight_fullsample) & df8$dsa == 1 &
                                     df8[,paste0("CONSIDERING20_",i)] == 1],na.rm = T)
         -sum(df8$weight_fullsample[!is.na(df8$weight_fullsample) & df8$dsa == 1 &
                                      df8[,paste0("NOTCONSIDERING20_",i)] == 1],
              na.rm = T))))
}

for (i in 1:24){
  print(paste(cand[i],
              (wtd.mean(df8[,paste0("support_",cand[i])], df8$weight_postdebate, na.rm = T)- 
                 wtd.mean(df8[,paste0("support_",cand[i])], df8$weight_predebate, na.rm = T) )
              ))
}
for (i in 1:24){
  print(paste(cand[i],
              (wtd.var(df8[,paste0("cons_",cand[i])], df8$weight_postdebate, na.rm = T)/sum(!is.na(df8$weight_postdebate) - 
                 wtd.var(df8[,paste0("support_",cand[i])], df8$weight_predebate, na.rm = T) )
               )
              
  ))
}
# group se
for (i in 1:25){
  print(paste(cand1[i],
              sqrt(wtd.var(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                               df8[,paste0("CONSIDERING20_",i)] == 1],
                       na.rm = T)/sum(!is.na(df8$weight_postdebate)) +
                 wtd.var(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                                 df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                         na.rm = T)/sum(!is.na(df8$weight_postdebate)) +
                 wtd.var(df8$weight_predebate[!is.na(df8$weight_predebate) &
                                                 df8[,paste0("CONSIDERING20_",i)] == 1],
                         na.rm = T)/sum(!is.na(df8$weight_predebate)) +
               wtd.var(df8$weight_predebate[!is.na(df8$weight_predebate) &
                                              df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                       na.rm = T)/sum(!is.na(df8$weight_predebate))
               )
              )
        )

}

for (i in 1:25){
  print(paste(cand1[i],
              sqrt(wtd.var(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                                   df8[,paste0("CONSIDERING20_",i)] == 1],
                           na.rm = T)/sum(!is.na(df8$weight_postdebate)) +
                     wtd.var(df8$weight_postdebate[!is.na(df8$weight_postdebate) &
                                                     df8[,paste0("NOTCONSIDERING20_",i)] == 1],
                             na.rm = T)/sum(!is.na(df8$weight_postdebate))
              )
  )
  )
  
}

for (i in 1:25){
  print(cand1[i])
  print((sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) & df8$dsa == 1 &
                                     df8[,paste0("CONSIDERING20_",i)] == 1],na.rm = T)
         -sum(df8$weight_postdebate[!is.na(df8$weight_postdebate) & df8$dsa == 1 &
                                      df8[,paste0("NOTCONSIDERING20_",i)] == 1],
              na.rm = T)))
}


# PCA 
require(survey)
df2 <- df8[!is.na(df8$weight_postdebate), c('weight_postdebate', paste0('support_', cand))]
df2 <- df8[!is.na(df8$weight_postdebate), ]
d1 <- svydesign(id = ~1, weights = ~weight_postdebate, data = df2)
pc <- svyprcomp(~support_biden + support_sanders + support_harris + support_orourke +
                  support_booker + support_klobuchar + support_warren + support_hickenlooper+
                  support_gillibrand + support_delaney + support_castro + support_deblasio +
                  support_gabbard + support_buttigieg + support_inslee + support_ryan + support_moulton+
                  support_yang + support_williamson + support_gravel + support_bullock + support_bennet
                + support_messam + support_sestak, design=d1,scale=TRUE,scores=TRUE)
biplot(pc)
print(pc$rotation)

# Logit
library(survey)
df8$birthyr2 <- df8$birthyr^2
d2 <- svydesign(id = ~1, weights = ~weight_postdebate*harris_var, data = df8[!is.na(df8$weight_postdebate), ])
f1 <- -1*support_harris ~ factor(NAMEID_20_KH) + birthyr + birthyr2
f1 <- -1*support_sanders ~ factor(inputregstate) + factor(educ) + factor(race) +
  factor(gender) + factor(urbancity) + factor(faminc_new) + factor(INSTFAV_dsa) + cand_allocator_m4a_1 + factor(INSTITUTION) +
  union_combined_1 + union_combined_2 + union_combined_4 + union_combined_5 + union_combined_6 +
  cand_allocator_abt_1 + SOCIAL_DOMINANCE_INFERIOR + LABELS_1 +LABELS_2 + LABELS_3 + LABELS_4 + factor(SYSTEM) +factor(FEAR) +factor(EMPATHY)
#f1 <- -1*support_sanders ~ factor(INSTFAV_dsa) + cand_allocator_m4a_1 + factor(INSTITUTION) +cand_allocator_abt_1
reg  <- survey::svyglm(formula = f1, design = d2)

ggplot(df8, aes(x = 2018-birthyr, y = support_harris, weight = weight_postdebate*harris_var)) +
  geom_smooth(method = "loess") + xlim(20, 100)

  summary(reg)
  1-reg$deviance/reg$null.deviance
for (i in 1:24){
  print(paste(cand[i],
              round((weighted.median(df8[,paste0("support_",cand[i])], df8$weight_postdebate) - 
                       weighted.median(df8[,paste0("support_",cand[i])], df8$weight_predebate)),2)
  ))
}

df_sup <- df8 %>% select(., starts_with('support_')) 
df_sup$support_gravel <- NULL
col_sup <- colnames(df_sup)
df_sup$support_biden <- NULL
col_sup1 <- colnames(df_sup)

df8$top1 <- apply(df8[, colnames(df8) %in% col_sup], 1, FUN=min)
df8$top2 <- apply(df8[, colnames(df8) %in% col_sup1], 1, FUN=min)

cand2 <- c("bennet", "booker","bullock", "buttigieg","castro", 
          "deblasio", "delaney","gabbard", "gillibrand", 
          "harris", "hickenlooper","inslee",  "klobuchar","messam", "moulton",
          "orourke",  "ryan", "sanders", "warren","williamson", "yang", "sestak")
for (i in 1:22){
  tmp1 <- df8$top1
  tmp2 <- df8$top2
  print(paste(cand2[i],
                100*round(((df8 %>% filter(as.numeric(get(paste0("support_", cand2[i]))) == tmp2) %>%
                   select(weight_postdebate) %>% sum(., na.rm = T)) - (df8 %>% filter(as.numeric(get(paste0("support_", cand2[i]))) == tmp1) %>%
                select(weight_postdebate) %>% sum(., na.rm = T)))/sum(df8$weight_postdebate, na.rm = T)
              ,3)))
              
}
summary(df8$top1)
summary(df8$top2)

