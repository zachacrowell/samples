library(readstata13)
df <- read.dta13('~/Downloads/dataverse_files/CCES2018_OUTPUT.dta')

# Export
write.table(df, '~/Downloads/data/cces18_output.txt', sep = '\t',
            row.names = F, na = "")
system('gzip ~/Downloads/data/cces18_output.txt')

df1 <- df[df$voted==1, ]

table(df1$HouseCand1Party, df1$HouseCand2Party)
table(df1$HouseCand2Party)
df2 <- df1[df1$HouseCand1Party == "Democratic" & df1$HouseCand2Party == "Republican",]
df2 <- df1[!is.na(df1$commonpostweight), ]

table(df2$CC18_409, df2$CC18_412)
table(df2$CC18_412, df2$CC18_317)
table(df2$CC18_410b, useNA = 'always')
library(dplyr)
coalesce(a, b, c)
df2$dem <- ifelse(coalesce(df2$CC18_409, 0) == 1  |
                  (coalesce(df2$CC18_410b, 0)==1 &
                    (df2$SenCand1Party %in% c("Democratic", "Republican")
                     & df2$SenCand2Party == "Republican"))|
                  coalesce(df2$CC18_410bx, 0)==1 |
                  (coalesce(df2$CC18_412, 0) == 1 &
                     (df2$HouseCand1Party == "Democratic" 
                      & df2$HouseCand2Party == "Republican")),
                  1, 0
                    )
table(df2$dem)
df2$dem[df2$inputstate==23 & df2$CC18_410b == 3] <- 1
table(df2$dem)

df2$rep <- ifelse(coalesce(df2$CC18_409, 0) == 2  |
                    (coalesce(df2$CC18_410b, 0)==2 &
                       (df2$SenCand1Party %in% c("Democratic", "Republican")
                        & df2$SenCand2Party == "Republican"))|
                    coalesce(df2$CC18_410bx, 0)==2 |
                    (coalesce(df2$CC18_412, 0) == 2 &
                       (df2$HouseCand1Party == "Democratic" 
                        & df2$HouseCand2Party == "Republican")),
                  1, 0
)

dem <- sum(df2$commonpostweight[df2$dem==1])
rep <- sum(df2$commonpostweight[df2$rep==1])


sum(df2$commonpostweight[df2$CC18_317==1 & df2$dem==1], na.rm = T)/dem
sum(df2$commonpostweight[df2$CC18_317==2 & df2$dem==1], na.rm = T)/dem
sum(df2$commonpostweight[df2$CC18_317==3 & df2$dem==1], na.rm = T)/dem
sum(df2$commonpostweight[is.na(df2$CC18_317) & df2$dem==1], na.rm = T)/dem

sum(df2$commonpostweight[df2$CC18_317==1 & df2$rep==1], na.rm = T)/rep
sum(df2$commonpostweight[df2$CC18_317==2 & df2$rep==1], na.rm = T)/rep
sum(df2$commonpostweight[df2$CC18_317==3 & df2$rep==1], na.rm = T)/rep
sum(df2$commonpostweight[is.na(df2$CC18_317) & df2$rep==1], na.rm = T)/rep

CC18_410b
tapply(df2$commonpostweight[!is.na(df2$CC18_317) & df2$dem == 2],
       df2$CC18_332c[!is.na(df2$CC18_332c) & df2$dem == 2],  sum)

tapply(df2$commonpostweight[!is.na(df2$CC18_422c) & df2$inputstate == 38 & 
                              df2$CC18_410b == 1],
       df2$CC18_422c[!is.na(df2$CC18_422c) & df2$inputstate == 38 & 
                       df2$CC18_410b == 1],  sum)


