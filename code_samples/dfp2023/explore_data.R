# Config

#Functions
source("~/github/samples/code_samples/dfp2023/helper_functions.R")



# Load Ddata
#df <- readRDS("~/Downloads/cumulative_2006-2022.rds")
library(data.table)
df <- fread("~/Downloads/dataverse_files/CCES22_Common_OUTPUT_vv_topost.csv")
df <- df[!is.na(df$vvweight_post), ]

# check NA
sapply(df, function(x) sum(is.na(x)))

# Check Sum Stats
sapply(df, summary)


# Check Freq of Most Common Class (i.e. how sparse is a var?)
sapply(df, function(x) sum(!duplicated(x)))
df_int <-df %>% select(where(is.integer) | where(is.factor))
sapply(df_int, function(x) sum(x==get_mode(x), na.rm = T)/nrow(df_int))

# Check Standardized Var
df_num <-df %>% select(where(is.numeric))
sapply(df_num, function(x) sd((x-min(x, na.rm = T))/
                                (max(x, na.rm = T)-min(x, na.rm = T)),
                              na.rm = T))
       
       
       # Get Numeric Vars
       df_num <- select_if(df, is.numeric)
       rmv <- c("train", "high_school")
       df_num <- df_num %>% select(-all_of(rmv))
       
       
       df_cor <- cor(df_num, method = "spearman")
       corrplot(df_cor)
       