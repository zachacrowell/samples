# Config
require(data.table)
require(dplyr)
require(tidyr)
require(Hmisc)
library(data.table)
library(dplyr)
library(caret)
library(pROC)
library(WeightedROC)
library(survey)
library(ggplot2)
library(RColorBrewer)
library(Hmisc)

# Read in Data
df <- data.table::fread("gunzip -c ~/Downloads/data/cces18_output.txt.gz", data.table = F)

# Average Z Scores for Policy Qs
 ### Prep: weighted z-score
wtd_zscore <- function(x, w){
  (x - Hmisc::wtd.mean(x, w, na.rm = T))/sqrt(Hmisc::wtd.var(x, w, na.rm = T))
}

df1 <- df[!is.na(df$commonpostweight), ]

## Mean Racial Resentment Score
df1$r1_z <- wtd_zscore(df1$CC18_422a, df1$commonpostweight)
df1$r1_z[is.na(df1$r1_z)] <- 0
df1$r2_z <- wtd_zscore(df1$CC18_422b, df1$commonpostweight)
df1$r2_z[is.na(df1$r2_z)] <- 0
df1$r3_z <- wtd_zscore(df1$CC18_422e, df1$commonpostweight)
df1$r3_z[is.na(df1$r3_z)] <- 0
df1$r4_z <- wtd_zscore(df1$CC18_422f, df1$commonpostweight)
df1$r4_z[is.na(df1$r4_z)] <- 0
df1$r5_z <- wtd_zscore(df1$CC18_422g, df1$commonpostweight)
df1$r5_z[is.na(df1$r5_z)] <- 0
df1$r6_z <- wtd_zscore(df1$CC18_422h, df1$commonpostweight)
df1$r6_z[is.na(df1$r6_z)] <- 0

df1$score_antiracism <- (-1*df1$r1_z + df1$r2_z + df1$r3_z - df1$r4_z -df1$r5_z +
                           df1$r6_z)/
(as.numeric(!is.na(df1$CC18_422a)) + as.numeric(!is.na(df1$CC18_422b)) +
   as.numeric(!is.na(df1$CC18_422e)) + as.numeric(!is.na(df1$CC18_422f)) +
   as.numeric(!is.na(df1$CC18_422g)) + as.numeric(!is.na(df1$CC18_422h))
 )

# Mean Gun Control Score
df1$g1_z <- wtd_zscore(df1$CC18_320a, df1$commonpostweight)
df1$g1_z[is.na(df1$g1_z)] <- 0
df1$g2_z <- wtd_zscore(df1$CC18_320c, df1$commonpostweight)
df1$g2_z[is.na(df1$g2_z)] <- 0
df1$g3_z <- wtd_zscore(df1$CC18_320d, df1$commonpostweight)
df1$score_guncontrol <- (-1*df1$g1_z - df1$g2_z + df1$g3_z)/
  (as.numeric(!is.na(df1$CC18_320a))  + as.numeric(!is.na(df1$CC18_320c)) + 
     as.numeric(!is.na(df1$CC18_320d)))


## Mean Abortion Score
df1$a1_z <- wtd_zscore(df1$CC18_321a, df1$commonpostweight)
df1$a1_z[is.na(df1$a1_z)] <- 0
df1$a2_z <- wtd_zscore(df1$CC18_321b, df1$commonpostweight)
df1$a2_z[is.na(df1$a2_z)] <- 0
df1$a3_z <- wtd_zscore(df1$CC18_321c, df1$commonpostweight)
df1$a3_z[is.na(df1$a3_z)] <- 0
df1$a4_z <- wtd_zscore(df1$CC18_321d, df1$commonpostweight)
df1$a4_z[is.na(df1$a4_z)] <- 0
df1$a5_z <- wtd_zscore(df1$CC18_321e, df1$commonpostweight)
df1$a5_z[is.na(df1$a5_z)] <- 0
df1$a6_z <- wtd_zscore(df1$CC18_321f, df1$commonpostweight)
df1$a6_z[is.na(df1$a6_z)] <- 0
df1$score_prochoice <- (-1*df1$a1_z + df1$a2_z + df1$a3_z + df1$a4_z + df1$a5_z +
                          df1$a6_z)/(as.numeric(!is.na(df1$CC18_321a)) +
                                      as.numeric(!is.na(df1$CC18_321b)) +
     as.numeric(!is.na(df1$CC18_321c)) + as.numeric(!is.na(df1$CC18_321d)) +
  as.numeric(!is.na(df1$CC18_321e)) + as.numeric(!is.na(df1$CC18_321f)))

## Mean Immigration Score
df1$i1_z <- wtd_zscore(df1$CC18_322a, df1$commonpostweight)
df1$i1_z[is.na(df1$i1_z)] <- 0
df1$i2_z <- wtd_zscore(df1$CC18_322b, df1$commonpostweight)
df1$i2_z[is.na(df1$i2_z)] <- 0
df1$i3_z <- wtd_zscore(df1$CC18_322c_new, df1$commonpostweight)
df1$i3_z[is.na(df1$i3_z)] <- 0
df1$i4_z <- wtd_zscore(df1$CC18_322d_new, df1$commonpostweight)
df1$i4_z[is.na(df1$i4_z)] <- 0
df1$i5_z <- wtd_zscore(df1$CC18_322c, df1$commonpostweight)
df1$i5_z[is.na(df1$i5_z)] <- 0
df1$i6_z <- wtd_zscore(df1$CC18_322f, df1$commonpostweight)
df1$i6_z[is.na(df1$i6_z)] <- 0
df1$i7_z <- wtd_zscore(df1$CC18_417_c, df1$commonpostweight)
df1$i7_z[is.na(df1$i7_z)] <- 0
df1$score_proimmigration <- (df1$i1_z - df1$i2_z + df1$i3_z +
                            df1$i4_z + df1$i5_z + df1$i6_z +  df1$i7_z)/
  (as.numeric(!is.na(df1$CC18_322a)) + as.numeric(!is.na(df1$CC18_322b)) +
     as.numeric(!is.na(df1$CC18_322c_new)) +
     as.numeric(!is.na(df1$CC18_322d_new)) +
     as.numeric(!is.na(df1$CC18_322c)) +
     as.numeric(!is.na(df1$CC18_322f)) +
     as.numeric(!is.na(df1$CC18_417_c))
   )

## Mean HealthCare Score
df1$h1_z <- wtd_zscore(df1$CC18_327a, df1$commonpostweight)
df1$h1_z[is.na(df1$h1_z)] <- 0
df1$h2_z <- wtd_zscore(df1$CC18_327c, df1$commonpostweight)
df1$h2_z[is.na(df1$h2_z)] <- 0
df1$h3_z <- wtd_zscore(df1$CC18_327d, df1$commonpostweight)
df1$h3_z[is.na(df1$h3_z)] <- 0
df1$h4_z <- wtd_zscore(df1$CC18_327e, df1$commonpostweight)
df1$h4_z[is.na(df1$h4_z)] <- 0


df1$score_prohealth <- (-df1$h1_z + df1$h2_z + df1$h3_z + df1$h4_z)/
  (as.numeric(!is.na(df1$CC18_327a)) + as.numeric(!is.na(df1$CC18_327c)) + 
     as.numeric(!is.na(df1$CC18_327d)) + as.numeric(!is.na(df1$CC18_327e))
  )

# Mean OPen Trade
df1$t1_z <- wtd_zscore(df1$CC18_331a, df1$commonpostweight)
df1$t1_z[is.na(df1$t1_z)] <- 0
df1$t2_z <- wtd_zscore(df1$CC18_331b, df1$commonpostweight)
df1$t2_z[is.na(df1$t2_z)] <- 0
df1$t3_z <- wtd_zscore(df1$CC18_331c, df1$commonpostweight)
df1$t3_z[is.na(df1$t3_z)] <- 0
df1$t4_z <- wtd_zscore(df1$CC18_332e, df1$commonpostweight)
df1$t4_z[is.na(df1$t4_z)] <- 0


df1$score_protrade <- (df1$t1_z + df1$t2_z + df1$t3_z)/
  (as.numeric(!is.na(df1$CC18_331a)) + as.numeric(!is.na(df1$CC18_331b)) + 
     as.numeric(!is.na(df1$CC18_331c)) + as.numeric(!is.na(df1$CC18_332e))
  )


## Mean Enviro Score
df1$e1_z <- wtd_zscore(df1$CC18_415a, df1$commonweight_vv)
df1$e1_z[is.na(df1$e1_z)] <- 0
df1$e2_z <- wtd_zscore(df1$CC18_415b, df1$commonweight_vv)
df1$e2_z[is.na(df1$e2_z)] <- 0
df1$e3_z <- wtd_zscore(df1$CC18_415c, df1$commonweight_vv)
df1$e3_z[is.na(df1$e3_z)] <- 0
df1$e4_z <- wtd_zscore(df1$CC18_415d, df1$commonweight_vv)
df1$e4_z[is.na(df1$e4_z)] <- 0
df1$e5_z <- wtd_zscore(df1$CC18_417_a, df1$commonweight_vv)
df1$e5_z[is.na(df1$e5_z)] <- 0
df1$e6_z <- wtd_zscore(df1$CC18_332b, df1$commonweight_vv)
df1$e6_z[is.na(df1$e6_z)] <- 0
df1$e7_z <- wtd_zscore(df1$CC18_332c, df1$commonweight_vv)
df1$e7_z[is.na(df1$e7_z)] <- 0


df1$score_enviro <- (-1*(df1$e1_z + df1$e2_z - df1$e3_z + df1$e4_z) + df1$e5_z + 
                       df1$e6_z + df1$e7_z)/
  (as.numeric(!is.na(df1$CC18_415a)) + as.numeric(!is.na(df1$CC18_415b)) +
     as.numeric(!is.na(df1$CC18_415c)) + as.numeric(!is.na(df1$CC18_415d)) + 
     as.numeric(!is.na(df1$CC18_417_a)) + as.numeric(!is.na(df1$CC18_332b)) + 
     as.numeric(!is.na(df1$CC18_332c)))

# Policy grid: CC18_325, CC18_327, CC18_331,
# CC18_332?, CC18_324, CC18_415, CC18_417?

# Policy CC18_326, CC18_335?, CC18_416





# Subset to self-id voted & has nonnull weight & voted trump/3rd party
df2 <- df1[replace_na(df1$CC18_401, 0)==5 & replace_na(df1$CC18_317,0) %in% c(1, 3), ]


# df1 <- df1[df1$HouseCand1Party == "Democratic" & df1$HouseCand2Party == "Republican",]

table(df1$CC18_409, df1$CC18_412)
table(df1$CC18_412, df1$CC18_317)
table(df1$CC18_410b, useNA = 'always')

# partyline = D, senate = D & competetitive, senate2 = D, hoouse = D & compeititve
df2$dem <- ifelse(replace_na(df2$CC18_409, 0) == 1  |
                  (replace_na(df2$CC18_410b, 0)==1 &
                    (df2$SenCand1Party %in% c("Democratic", "Republican")
                     & df2$SenCand2Party == "Republican"))|
                  replace_na(df2$CC18_410bx, 0)==1 |
                  (replace_na(df2$CC18_412, 0) == 1 &
                     (df2$HouseCand1Party == "Democratic" 
                      & df2$HouseCand2Party == "Republican")),
                  1, 0)


# Add Features
## Race
df2$race2[df2$race == 1 & (df2$hispanic == 2 | is.na(df2$hispanic))] <- "white"
df2$race2[df2$race == 2 & (df2$hispanic == 2 | is.na(df2$hispanic))] <- "black"
df2$race2[df2$race == 3 | df2$hispanic == 1] <- "latino"
df2$race2[df2$race == 4 & (df2$hispanic == 2 | is.na(df2$hispanic))] <- "asian"
df2$race2[df2$race == 5 & (df2$hispanic == 2 | is.na(df2$hispanic))] <- "native_american"
df2$race2[df2$race == 8 & (df2$hispanic == 2 | is.na(df2$hispanic))] <- "middle_eastern"
df2$race2[df2$race == 6 & (df2$hispanic == 2 | is.na(df2$hispanic))] <- "mixed"
df2$race2[df2$race == 7 & (df2$hispanic == 2 | is.na(df2$hispanic))] <- "other"
df2$race2 <- as.factor(df2$race2)

## Binned Age
df2$age <- (2016-as.numeric(df2$birthyr))
df2$age_binned <- NA
df2$age_binned[df2$age < 30] <- "18-29"
df2$age_binned[df2$age > 30 & df2$age < 40] <- "30-39"
df2$age_binned[df2$age > 40 & df2$age < 50] <- "40-49"
df2$age_binned[df2$age > 50 & df2$age < 65] <- "50-64"
df2$age_binned[df2$age > 65] <- "65"
df2$age_binned <- as.factor(df2$age_binned)

# Manual Feature Selection: Removing vars that have 
# too many nulls, redundant, nongeneralizable/national, or post-primary.
trmp <- c("CC18_302, CC18_308a", "app_dtrmp")
cols <- colnames(df2[, c(7, 8, 12, 473, 62:74, 76:87, 107:157, 173, 203,
                         206:209, 211, 212, 214, 215, 219, 221:235, 237:242,
                         284:286, 395:408, 447:454, 470, 541, 545, 552, 560,
                         565,570, 578:582)])
df3 <- df2[, colnames(df2) %in% cols]

df3$nacount_child18num <- ifelse(is.na(df3$numchildren), 1, 0)
df3$numchildren[is.na(df3$numchildren)] <- 0
# Remove fields with more than 1000 missing values in training set. 
nonvar <- df3[, sapply(df3, function(x) sum(is.na(x)) > 7000)] %>% colnames(.)
df3[, colnames(df3) %in% nonvar] <- NULL

# Check
#df3 %>% sapply(., function(x) sum(is.na(x)))

# Greate Factors
df3[, sapply(df3, is.integer)] <- as.factor(df3[, sapply(df3, is.integer)])

#-------

# Prep yvar, xvars, and weights for GBM
xvars <- df3[, colnames(df3) != 'dem']
yvar <- as.factor(df3$dem)
levels <- unique(yvar)
yvar <- factor(yvar, labels=make.names(levels))
# Defunct
# Combine into 1 df
df4 <- cbind(xvars, yvar, df2$commonpostweight)
df4$commonweight_vv <- df4$`df1$commonweight_vv`
df4$`df1$commonweight_vv` <- NULL

# Primarily using defulats, 1 cv (as it drops more marginal features),
# and commonweight_vv base on previous runs
set.seed(643)
gbm1 <- caret::train(xvars, yvar, distribution="bernoulli", method="gbm",
                     weights = df1$commonweight_vv,
                     trControl = trainControl(method = 'cv',  number = 1,
                                              returnResamp ='none',
                                              summaryFunction = twoClassSummary,
                                              classProbs = TRUE),
                     metric = 'ROC',
                     preProc = c("center", "scale")
)
# genearte summary stats
predx <- predict(object=gbm1, df3, type='raw', weight = df2$commonweight_vv)
#auc <- roc(ifelse(yvar=="X1",1,0), ifelse(predx=="X1",1,0))
wroc <- WeightedROC(ifelse(predx=="X1",1,0), ifelse(yvar=="X1",1,0), df1$commonweight_vv)
wauc <- WeightedAUC(wroc)
# Summarize
## Rel Influence
summary(gbm1)
## ACC
print(postResample(pred=predx, obs=yvar))
## AUC & WAUC
#print(auc$auc)
print(wauc)