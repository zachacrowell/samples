CCES16: Data Cleaning and Standardization
================

``` r
# Create New Vars

#ObamatoNA
df$dropoff[df$CC16_326 == 1 & !df$CC16_410a %in% c(1,2)] <- 1
df$dropoff[df$CC16_326 == 1 & df$CC16_410a == 1] <- 0


# Two Way Voting
df$clinton[df$CC16_410a == 2] <- 1
df$clinton[df$CC16_410a == 1] <- 0

# Two Way 2012
df$twoway_2012[df$CC16_326==1] <- "Obama"
df$twoway_2012[df$CC16_326==2] <- "Romney"
df$twoway_2012 <- as.factor(df$twoway_2012)

## Race
df$race2[df$race == 1 & (df$hispanic == 2 | is.na(df$hispanic))] <- "white"
df$race2[df$race == 2 & (df$hispanic == 2 | is.na(df$hispanic))] <- "black"
df$race2[df$race == 3 | df$hispanic == 1] <- "latino"
df$race2[df$race == 4 & (df$hispanic == 2 | is.na(df$hispanic))] <- "asian"
df$race2[df$race == 5 & (df$hispanic == 2 | is.na(df$hispanic))] <- "native_american"
df$race2[df$race == 8 & (df$hispanic == 2 | is.na(df$hispanic))] <- "middle_eastern"
df$race2[df$race == 6 & (df$hispanic == 2 | is.na(df$hispanic))] <- "mixed"
df$race2[df$race == 7 & (df$hispanic == 2 | is.na(df$hispanic))] <- "other"
df$race2 <- as.factor(df$race2)

## Binned Age
df$age <- (2016-as.numeric(df$birthyr))

df$age_binned <- NA
df$age_binned[df$age < 30] <- "18-29"
df$age_binned[df$age > 30 & df$age < 40] <- "30-39"
df$age_binned[df$age > 40 & df$age < 50] <- "40-49"
df$age_binned[df$age > 50 & df$age < 65] <- "50-64"
df$age_binned[df$age > 65] <- "65"
df$age_binned <- as.factor(df$age_binned)

# Average Z Scores for Policy Qs
 ### Prep: weighted z-score
wtd_zscore <- function(x, w){
  (x - Hmisc::wtd.mean(x, w, na.rm = T))/sqrt(Hmisc::wtd.var(x, w, na.rm = T))
}

## Mean Racial Resentment Score
df$CC16_422c[df$CC16_422c == 6] <- NA
df$CC16_422e[df$CC16_422e == 6] <- NA
df$CC16_422f[df$CC16_422f == 6] <- NA
df$r1_z <- wtd_zscore(df$CC16_422c, df$commonweight_post)
df$r1_z[is.na(df$r1_z)] <- 0
df$r2_z <- wtd_zscore(df$CC16_422d, df$commonweight_post)
df$r2_z[is.na(df$r2_z)] <- 0
df$r3_z <- wtd_zscore(df$CC16_422e, df$commonweight_post)
df$r3_z[is.na(df$r3_z)] <- 0
df$r4_z <- wtd_zscore(df$CC16_422f, df$commonweight_post)
df$r4_z[is.na(df$r4_z)] <- 0

df$score_antiracism <- (-1*df$r1_z - df$r2_z + df$r3_z + df$r4_z)/
(as.numeric(!is.na(df$CC16_422c)) + as.numeric(!is.na(df$CC16_422d)) +
   as.numeric(!is.na(df$CC16_422e)) + as.numeric(!is.na(df$CC16_422f)))

# Mean Gun Control Score
df$CC16_330d[df$CC16_330d == 9] <- NA

df$g1_z <- wtd_zscore(df$CC16_330a, df$commonweight_post)
df$g1_z[is.na(df$g1_z)] <- 0
df$g2_z <- wtd_zscore(df$CC16_330b, df$commonweight_post)
df$g2_z[is.na(df$g2_z)] <- 0
df$g3_z <- wtd_zscore(df$CC16_330d, df$commonweight_post)
df$g3_z[is.na(df$g3_z)] <- 0
df$g4_z <- wtd_zscore(df$CC16_330e, df$commonweight_post)
df$g4_z[is.na(df$g4_z)] <- 0

df$score_guncontrol <- (-1*df$g1_z + df$g2_z - df$g3_z + df$g4_z)/
  (as.numeric(!is.na(df$CC16_330a)) + as.numeric(!is.na(df$CC16_330b)) +
     as.numeric(!is.na(df$CC16_330d)) + as.numeric(!is.na(df$CC16_330e)))

## Mean Abortion Score
df$a1_z <- wtd_zscore(df$CC16_332a, df$commonweight_post)
df$a1_z[is.na(df$a1_z)] <- 0
df$a2_z <- wtd_zscore(df$CC16_332b, df$commonweight_post)
df$a2_z[is.na(df$a2_z)] <- 0
df$a3_z <- wtd_zscore(df$CC16_332c, df$commonweight_post)
df$a3_z[is.na(df$a3_z)] <- 0
df$a4_z <- wtd_zscore(df$CC16_332d, df$commonweight_post)
df$a4_z[is.na(df$a4_z)] <- 0
df$a5_z <- wtd_zscore(df$CC16_332e, df$commonweight_post)
df$a5_z[is.na(df$a5_z)] <- 0
df$a6_z <- wtd_zscore(df$CC16_332f, df$commonweight_post)
df$a6_z[is.na(df$a6_z)] <- 0

df$score_prochoice <- (-1*df$a1_z + df$a2_z + df$a3_z + df$a4_z + df$a5_z +
                          df$a6_z)/(as.numeric(!is.na(df$CC16_332a)) +
                                      as.numeric(!is.na(df$CC16_332b)) +
     as.numeric(!is.na(df$CC16_332c)) + as.numeric(!is.na(df$CC16_332d)) +
  as.numeric(!is.na(df$CC16_332e)) + as.numeric(!is.na(df$CC16_332f)))

## Mean Immigration Score
df$i1_z <- wtd_zscore(df$CC16_331_1, df$commonweight_post)
df$i1_z[is.na(df$i1_z)] <- 0
df$i2_z <- wtd_zscore(df$CC16_331_2, df$commonweight_post)
df$i2_z[is.na(df$i2_z)] <- 0
df$i3_z <- wtd_zscore(df$CC16_331_3, df$commonweight_post)
df$i3_z[is.na(df$i3_z)] <- 0
df$i7_z <- wtd_zscore(df$CC16_331_7, df$commonweight_post)
df$i7_z[is.na(df$i7_z)] <- 0

df$score_proimmigration <- (-1*df$i1_z + df$i2_z - df$i3_z + df$i7_z)/
  (as.numeric(!is.na(df$CC16_331_1)) + as.numeric(!is.na(df$CC16_331_2)) +
     as.numeric(!is.na(df$CC16_331_3)) + as.numeric(!is.na(df$CC16_331_7)))

## Mean Enviro Score
df$CC16_333d[df$CC16_333d == 5] <- NA
df$e1_z <- wtd_zscore(df$CC16_333a, df$commonweight_post)
df$e1_z[is.na(df$e1_z)] <- 0
df$e2_z <- wtd_zscore(df$CC16_333b, df$commonweight_post)
df$e2_z[is.na(df$e2_z)] <- 0
df$e3_z <- wtd_zscore(df$CC16_333c, df$commonweight_post)
df$e3_z[is.na(df$e3_z)] <- 0
df$e4_z <- wtd_zscore(df$CC16_333d, df$commonweight_post)
df$e4_z[is.na(df$e4_z)] <- 0

df$score_enviro <- (-1*(df$e1_z + df$e2_z - df$e3_z + df$e4_z))/
  (as.numeric(!is.na(df$CC16_333a)) + as.numeric(!is.na(df$CC16_333b)) +
     as.numeric(!is.na(df$CC16_333c)) + as.numeric(!is.na(df$CC16_333d)))

## Mean Crime Reform Score
df$c1_z <- wtd_zscore(df$CC16_334a, df$commonweight_post)
df$c1_z[is.na(df$c1_z)] <- 0
df$c2_z <- wtd_zscore(df$CC16_334b, df$commonweight_post)
df$c2_z[is.na(df$c2_z)] <- 0
df$c3_z <- wtd_zscore(df$CC16_334c, df$commonweight_post)
df$c3_z[is.na(df$c3_z)] <- 0
df$c4_z <- wtd_zscore(df$CC16_334d, df$commonweight_post)
df$c4_z[is.na(df$c4_z)] <- 0

df$score_crimereform <- (-1*df$c1_z - df$c2_z + df$c3_z + df$c4_z)/
  (as.numeric(!is.na(df$CC16_334a)) + as.numeric(!is.na(df$CC16_334b)) +
     as.numeric(!is.na(df$CC16_334c)) + as.numeric(!is.na(df$CC16_334d)))
```

``` r
# Remove externous values from variables of potential interest.

df$immstat[df$immstat == "6" | df$immstat == "None"] <- NA
df$union[df$union > 3] <- NA
df$unionhh[df$unionhh > 3] <- NA
df$child18[df$child18 > 2] <- NA

df$milstat_1[!df$milstat_1 %in% c("1", "2")] <- NA
df$milstat_2[!df$milstat_2 %in% c("1", "2")] <- NA
df$milstat_3[!df$milstat_3 %in% c("1", "2")] <- NA
df$milstat_4[!df$milstat_4 %in% c("1", "2")] <- NA
df$milstat_5[!df$milstat_5 %in% c("1", "2")] <- NA

df$newsint[df$newsint == 6] <- NA

df$healthins_1[!df$healthins_1 %in% c("1", "2")] <- NA
df$healthins_2[!df$healthins_2 %in% c("1", "2")] <- NA
df$healthins_3[!df$healthins_3 %in% c("1", "2")] <- NA
df$healthins_4[!df$healthins_4 %in% c("1", "2")] <- NA
df$healthins_5[!df$healthins_5 %in% c("1", "2")] <- NA
df$healthins_6[!df$healthins_6 %in% c("1", "2")] <- NA

df$phone[df$phone > 4] <- NA
df$internethome[df$internethome > 3] <- NA
df$internetwork[df$internetwork > 3] <- NA
df$industryclass[df$industryclass == "parents"] <- NA
df$pew_prayer[df$pew_prayer == "non-religious/Don't care"] <- NA
df$pew_religimp[df$pew_religimp > 4] <- NA

df$edloan[df$edloan %in% c("2016-10-08 11:26:40.147", "2016-10-09 06:19:14.174",
                           "2016-10-09 10:31:18.158", "2016-10-11 10:25:17.258", 
                           "2016-10-11 20:39:35.428", "2016-10-20 09:44:05.241",
                           "2016-10-21 11:54:21.881", "016-11-11 16:05:28.467",
                           "2016-12-03 16:43:08.547", "2016-12-07 17:15:23.929",
                           "2016-10-05 13:59:24.127", "2016-10-09 21:11:26.382")] <- NA
```

``` r
# Remove unvalidated votes w/o stated Clinton Trump vote
df <- df[!is.na(df$clinton) & !is.na(df$CL_E2016GVM), ]
# Export Cleaned Data
write.table(df, "cces2016_cleaned.txt",
            sep = "\t", row.names = F)
system("gzip cces2016_cleaned.txt")
```
