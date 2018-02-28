library(tidyverse)

###Import CDC Data and Set Var Types
VS16MORT <- read_csv("VS16MORT.csv", col_types = cols(Age_Value = col_integer(),
                                                      Race = col_integer()))

# ##List format 2006-2016
# files <- c()
# for (i in 6:16) {
#   if (i <= 9) {
#     files <- append(files, paste("VS", 0, i, "MORT.csv", sep = ""))
#   } else {
#     files <- append(files, paste("VS", i, "MORT.csv", sep = ""))
#   }
# }
# rm(i)
# 
# data <- list()
# for (i in 1:length(files)) {
#   name <- substr(files[i], 1, 8)
#   data[[name]] <- read_csv(files[i], col_types = cols(Age_Value = col_integer(),
#                                                                 Race = col_integer()))
# }

#VS_MORT <- readRDS("VS_MORT.rds")

#grep("^K70", VS16MORT[,"ICD10"][[1]]) %>% length
#grep("^F10", VS16MORT[,"ICD10"][[1]]) %>% length

###Vector of ICD10 Alcohol Related Mortality Causes
ALC_ICD10 <- c("K70", "F10", "E244", "G312", "G621", "G721", "I426", "K292", "K852",
  "K860", "R780", "X45", "X65", "Y15")

##Index of Obs with Matching Alcohol Related Mortality Causes
ALC_index <- NULL
for (i in 1:length(ALC_ICD10)) {
  ALC_index <- c(ALC_index, grep(paste0("^", ALC_ICD10[i], sep = ""),
                                 VS16MORT[,"ICD10"][[1]]))
}
ALC_index <- sort(unique(ALC_index))
rm(i)

# indexALC <- function(x) {
#   result <- NULL
#   ALC_ICD10 <- c("K70", "F10", "E244", "G312", "G621", "G721", "I426", "K292", "K852",
#                  "K860", "R780", "X45", "X65", "Y15")
#   for (i in 1:length(ALC_ICD10)) {
#     result <- c(result, grep(paste0("^", ALC_ICD10[i], sep = ""),
#                              x[, "ICD10"][[1]]))
#   }
#   result <- sort(unique(result))
#   return(result)
# }
# indices <- lapply(VS_MORT, indexALC)


##Aggregate various Asian races into single group
for (i in 1:nrow(VS16MORT)) {
  if (VS16MORT$Race[i]>=4){
    VS16MORT$Race[i] <- 4
  }
}
rm(i)

VS16MORT$RACE <- factor(VS16MORT$Race,
                        labels = c("White", "Black", "Native American", "Asian"))


# agg_race <- function(x) {
#   for (i in 1:nrow(x)) {
#     if (x$Race[i]>=4){
#       x$Race[i] <- 4
#     }
#   }
#   return(x)
# }
# agg_race_test <- lapply(VS_MORT, agg_race)

# for (i in 1:length(VS_MORT)) {
#   for (j in 1:nrow(VS_MORT[i])) {
#     if (VS_MORT[i]$Race)
#   }
# }

###Subset of Mortalities Related to Alcohol
MORT_ALC <- VS16MORT %>%
  filter(1:nrow(VS16MORT) %in% ALC_index)



###Proportion of Total Mortalities Related to Alcohol, by Race
(MORT_ALC$Race %>% table / VS16MORT$Race %>% table) %>% 
  barplot(ylim=c(0,0.08), yaxs = "i", xaxs = "i", axes=F,
     main = "Proportion of Total Mortalities Related \nto Alcohol by Race, 2016",
     xlab = "Race",
     ylab = "Percentage",
     names.arg = c("", "", "", ""))
axis(2, at = seq(0, 0.08, 0.01), labels = paste(0:8, "%", sep = ""))
grid(nx = 0, ny = 16)
par(new=T)
(MORT_ALC$Race %>% table / VS16MORT$Race %>% table) %>% 
  barplot(ylim=c(0,0.08), yaxs = "i", xaxs = "i", axes=F,
          main = "Proportion of Total Mortalities Related \nto Alcohol by Race, 2016",
          xlab = "Race",
          ylab = "Percentage",
          col = "grey25",
          names.arg = c("White", "Black", "Native American", "Asian"))

###Age Distribution of Mortality, All Causes
VS16MORT$Age_Value[VS16MORT$Age_Value!=999] %>%
  as.numeric() %>%
  density() %>%
  plot(ylim=c(0, 0.04), col="Blue", 
       main = "Distribution of Mortality Age by Cause",
       xlab = "Age",
       ylab = "Proportion")

###Age Distribution of Mortality, Alcohol Related
MORT_ALC$Age_Value[MORT_ALC$Age_Value!=999] %>%
  as.numeric() %>%
  density() %>%
  lines(col="Red")

###Average Mortality Age, All Causes
VS16MORT$Age_Value[VS16MORT$Age_Value!=999] %>%
  as.numeric() %>%
  mean() %>%
  abline(v=., col="Blue", lty=2)

###Average Mortality Age, Alcohol Related
MORT_ALC$Age_Value[MORT_ALC$Age_Value!=999] %>%
  as.numeric() %>%
  mean() %>%
  abline(v=., col="Red", lty=2)

###Average Mortality Age, All Causes, by Race
VS16MORT[VS16MORT$Age_Value!=999,] %>%
  select(Age_Value, Race) %>%
  group_by(Race) %>%
  summarise(avg=mean(Age_Value))

###Average Mortality Age, Alcohol Related, by Race
MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
  select(Age_Value, Race) %>%
  group_by(Race) %>%
  summarise(avg=mean(Age_Value))

###White Subset of Mortality, All Causes
WHITE_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==1) %>%
  select(Age_Value)

###Black Subset of Mortality, All Causes
BLACK_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==2) %>%
  select(Age_Value)

###American Indian Subset of Mortality, All Causes
AI_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==3) %>%
  select(Age_Value)

###Asian Subset of Mortality, All Causes
ASIAN_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==4) %>%
  select(Age_Value)

###Kernel Density Plot, Age Distribution of Mortality, All Causes, by Race
density(AI_MORT_AGE[[1]]) %>%
  plot(ylim=c(0,0.03), col="Blue")
density(WHITE_MORT_AGE[[1]]) %>%
  lines(col="Red")
density(BLACK_MORT_AGE[[1]]) %>%
  lines(col="Green")
density(ASIAN_MORT_AGE[[1]]) %>%
  lines(col="Black")

###White Subset of Mortality, Alcohol Related
WHITE_MORT_ALC_AGE <- MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
  filter(Race==1) %>%
  select(Age_Value)

###Black Subset of Mortality, Alcohol Related
BLACK_MORT_ALC_AGE <- MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
  filter(Race==2) %>%
  select(Age_Value)

###American Indian Subset of Mortality, Alcohol Related
AI_MORT_ALC_AGE <- MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
  filter(Race==3) %>%
  select(Age_Value)

###Asian Subset of Mortality, Alcohol Related
ASIAN_MORT_ALC_AGE <- MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
  filter(Race==4) %>%
  select(Age_Value)

###Kernel Density Plot, Age Distribution of Alcohol Related Mortality, by Race
density(WHITE_MORT_ALC_AGE[[1]]) %>%
  plot(xlim=c(0, 110), ylim=c(0, 0.05), yaxs = "i", xaxs = "i", col="Blue", 
       lty=2, axes=F,
       main = "Age Distribution of Alcohol Related Mortality by Race, 2016",
       xlab = "Age",
       ylab = "Percentage")
density(BLACK_MORT_ALC_AGE[[1]]) %>%
  lines(col="Green", lty=2)
density(ASIAN_MORT_ALC_AGE[[1]]) %>%
  lines(col="Black", lty=2)
density(AI_MORT_ALC_AGE[[1]]) %>%
  lines(col="Red")
axis(2, at = seq(0, 0.05, 0.01), labels = paste(0:5, "%", sep = ""))
axis(1, at = seq(0, 120, 10))
grid(nx = 11, ny = 5)
legend("topright", legend = c("White", "Black", "Native American", "Asian"),
       col = c("Green", "Blue", "Red", "Black"), lty = c(2,2,1,2), bty = "o", xjust = 1,
       yjust = 1, text.width = 15)


AI_MALE_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==3) %>%
  filter(Sex=="M") %>%
  select(Age_Value)

AI_FEMALE_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==3) %>%
  filter(Sex=="F") %>%
  select(Age_Value)

density(AI_MALE_MORT_AGE[[1]]) %>%
  plot(xlim=c(0, 110), ylim=c(0, 0.03), yaxs = "i", xaxs = "i", col="Blue", 
       lty=2, axes=F,
       main = "Age Distribution of Total Mortality Among \n Native Americans by Sex, 2016",
       xlab = "Age",
       ylab = "Percentage")
density(AI_FEMALE_MORT_AGE[[1]]) %>%
  lines(col="Green", lty=2)
axis(2, at = seq(0, 0.05, 0.01), labels = paste(0:5, "%", sep = ""))
axis(1, at = seq(0, 120, 10))
grid(nx = 11, ny = 5)
legend("topright", legend = c("Female", "Male"),
       col = c("Green", "Blue"), lty = c(1,1), bty = "o", xjust = 1,
       yjust = 1, text.width = 15)



AI_MALE_MORT_ALC_AGE <- MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
  filter(Race==3) %>%
  filter(Sex=="M") %>%
  select(Age_Value)

AI_FEMALE_MORT_ALC_AGE <- MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
  filter(Race==3) %>%
  filter(Sex=="F") %>%
  select(Age_Value)

density(AI_MALE_MORT_ALC_AGE[[1]]) %>%
  plot(xlim=c(0, 110), ylim=c(0, 0.04), yaxs = "i", xaxs = "i", col="Blue", 
       lty=2, axes=F,
       main = "Age Distribution of Alcohol Related Mortality Among \n Native Americans by Sex, 2016",
       xlab = "Age",
       ylab = "Percentage")
density(AI_FEMALE_MORT_ALC_AGE[[1]]) %>%
  lines(col="Green", lty=2)
axis(2, at = seq(0, 0.05, 0.01), labels = paste(0:5, "%", sep = ""))
axis(1, at = seq(0, 120, 10))
grid(nx = 11, ny = 4)
legend("topright", legend = c("Female", "Male"),
       col = c("Green", "Blue"), lty = c(1,1), bty = "o", xjust = 1,
       yjust = 1, text.width = 15)




