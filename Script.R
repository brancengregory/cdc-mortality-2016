library(tidyverse)
library(RColorBrewer)

cols <- RColorBrewer::brewer.pal(4, "Set1")

###Import CDC Data and Set Var Types
VS16MORT <- read_csv("data/VS16MORT.csv", col_types = cols(Age_Value = col_integer(),
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

VS16MORT <- VS16MORT %>%
  filter(.$Age_Value <= 130)

MORT_ALC <- MORT_ALC %>%
  filter(.$Age_Value <= 130)

# ###Average Mortality Age, All Causes
# VS16MORT$Age_Value[VS16MORT$Age_Value!=999] %>%
#   as.numeric() %>%
#   mean() %>%
#   abline(v=., col="Blue", lty=2)
# 
# ###Average Mortality Age, Alcohol Related
# MORT_ALC$Age_Value[MORT_ALC$Age_Value!=999] %>%
#   as.numeric() %>%
#   mean() %>%
#   abline(v=., col="Red", lty=2)



# ###Average Mortality Age, All Causes, by Race
# VS16MORT[VS16MORT$Age_Value!=999,] %>%
#   select(Age_Value, Race) %>%
#   group_by(Race) %>%
#   summarise(avg=mean(Age_Value))
# 
# ###Average Mortality Age, Alcohol Related, by Race
# MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
#   select(Age_Value, Race) %>%
#   group_by(Race) %>%
#   summarise(avg=mean(Age_Value))

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


AI_MALE_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==3) %>%
  filter(Sex=="M") %>%
  select(Age_Value)

AI_FEMALE_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==3) %>%
  filter(Sex=="F") %>%
  select(Age_Value)


AI_MALE_MORT_ALC_AGE <- MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
  filter(Race==3) %>%
  filter(Sex=="M") %>%
  select(Age_Value)

AI_FEMALE_MORT_ALC_AGE <- MORT_ALC[MORT_ALC$Age_Value!=999,] %>%
  filter(Race==3) %>%
  filter(Sex=="F") %>%
  select(Age_Value)



#############################################################################
#################Graphs#############################
#####################################

###Age Distribution of Mortality, All Causes
VS16MORT$Age_Value[VS16MORT$Age_Value!=999] %>%
  as.numeric() %>%
  density() %>%
  plot(xlim=c(0, 120), ylim=c(0, 0.04), col="Blue", 
       main = "Distribution of Mortality Age by Cause, 2016",
       xlab = "Age",
       ylab = "Proportion",
       axes = F,
       yaxs = "i", xaxs = "i")
axis(2, at = seq(0, 0.04, 0.01), labels = paste(0:4, "%", sep = ""))
axis(1, at = seq(0, 120, 10))
grid(nx = 12, ny = 4)
###Age Distribution of Mortality, Alcohol Related
MORT_ALC$Age_Value[MORT_ALC$Age_Value!=999] %>%
  as.numeric() %>%
  density() %>%
  lines(col="Red")
x <- density(as.numeric(VS16MORT$Age_Value[VS16MORT$Age_Value!=999]))$x
y <- density(as.numeric(VS16MORT$Age_Value[VS16MORT$Age_Value!=999]))$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0,0,1,0.3), border = NA)
x <- density(as.numeric(MORT_ALC$Age_Value[MORT_ALC$Age_Value!=999]))$x
y <- density(as.numeric(MORT_ALC$Age_Value[MORT_ALC$Age_Value!=999]))$y
polygon(c(min(x),x),c(min(y),y), col=rgb(1,0,0,0.3), border = NA)
legend("topright", legend = c("All Causes", "Alcohol Related"),
       col = c("Blue", "Red"), lty = c(1,1), bty = "o", xjust = 1,
       yjust = 1, text.width = 15)

#############################################
###Kernel Density Plot, Age Distribution of Mortality, All Causes, by Race
density(WHITE_MORT_AGE[[1]]) %>%
  plot(xlim=c(0,110), ylim=c(0,0.03), col="Red",
       main = "Age Distribution of Mortality, All Causes, by Race, 2016",
       xlab = "Age",
       ylab = "Proportion",
       axes = F,
       yaxs = "i", xaxs = "i")
grid(nx = 11, ny = 3)
x <- density(WHITE_MORT_AGE[[1]])$x
y <- density(WHITE_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(1,0,0,0.2), border = NA)
density(BLACK_MORT_AGE[[1]]) %>%
  lines(col="Green")
x <- density(BLACK_MORT_AGE[[1]])$x
y <- density(BLACK_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0,1,0,0.2), border = NA)
density(ASIAN_MORT_AGE[[1]]) %>%
  lines(col="Purple")
x <- density(ASIAN_MORT_AGE[[1]])$x
y <- density(ASIAN_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0.6274510,0.1254902,0.9411765,0.2), border = NA)
density(AI_MORT_AGE[[1]]) %>%
  lines(col="Blue")
x <- density(AI_MORT_AGE[[1]])$x
y <- density(AI_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0,0,1,0.2), border = NA)
axis(2, at = seq(0, 0.03, 0.01), labels = paste(0:3, "%", sep = ""))
axis(1, at = seq(0, 110, 10))
legend("topleft", legend = c("White", "Black", "Asian", "Native American"),
       col = c("Red", "Green", "Purple", "Blue"), lty = 1, bty = "o", xjust = 1,
       yjust = 1, text.width = 25, ncol = 1)

###############################################
###Kernel Density Plot, Age Distribution of Alcohol Related Mortality, by Race
density(WHITE_MORT_ALC_AGE[[1]]) %>%
  plot(xlim=c(0, 110), ylim=c(0, 0.05), yaxs = "i", xaxs = "i", col="Red", 
       lty=1, axes=F,
       main = "Age Distribution of Alcohol Related Mortality by Race, 2016",
       xlab = "Age",
       ylab = "Percentage")
grid(nx = 11, ny = 5)
x <- density(WHITE_MORT_ALC_AGE[[1]])$x
y <- density(WHITE_MORT_ALC_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(1,0,0,0.2), border = NA)
density(BLACK_MORT_ALC_AGE[[1]]) %>%
  lines(col="Green", lty=1)
x <- density(BLACK_MORT_ALC_AGE[[1]])$x
y <- density(BLACK_MORT_ALC_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0,1,0,0.2), border = NA)
density(ASIAN_MORT_ALC_AGE[[1]]) %>%
  lines(col="Purple", lty=1)
x <- density(ASIAN_MORT_ALC_AGE[[1]])$x
y <- density(ASIAN_MORT_ALC_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0.6274510,0.1254902,0.9411765,0.2), border = NA)
density(AI_MORT_ALC_AGE[[1]]) %>%
  lines(col="Blue")
x <- density(AI_MORT_ALC_AGE[[1]])$x
y <- density(AI_MORT_ALC_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0,0,1,0.2), border = NA)
axis(2, at = seq(0, 0.05, 0.01), labels = paste(0:5, "%", sep = ""))
axis(1, at = seq(0, 120, 10))
legend("topright", legend = c("White", "Black", "Asian", "Native American"),
       col = c("Red", "Green", "Purple", "Blue"), lty = 1, bty = "o", xjust = 1,
       yjust = 1, text.width = 15)

######################################
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

############################################

density(AI_MALE_MORT_AGE[[1]]) %>%
  plot(xlim=c(0, 110), ylim=c(0, 0.03), yaxs = "i", xaxs = "i", col=sex_col[n], 
       lty=1, axes=F,
       main = "Age Distribution of Total Mortality Among \n Native Americans by Sex, 2016",
       xlab = "Age",
       ylab = "Percentage")
grid(nx = 11, ny = 3)
x <- density(AI_MALE_MORT_AGE[[1]])$x
y <- density(AI_MALE_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=col2rgb(sex_col[n])[[1]]/255, border = NA)
density(AI_FEMALE_MORT_AGE[[1]]) %>%
  lines(col="Red", lty=1)
x <- density(AI_FEMALE_MORT_AGE[[1]])$x
y <- density(AI_FEMALE_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(1,0,0,0.3), border = NA)
axis(2, at = seq(0, 0.05, 0.01), labels = paste(0:5, "%", sep = ""))
axis(1, at = seq(0, 120, 10))
legend("topright", legend = c("Female", "Male"),
       col = c(sex_col[1], sex_col[n]), lty = c(1,1), bty = "o", xjust = 1,
       yjust = 1, text.width = 15)

################################################

density(AI_MALE_MORT_ALC_AGE[[1]]) %>%
  plot(xlim=c(0, 110), ylim=c(0, 0.04), yaxs = "i", xaxs = "i", col="Blue", 
       lty=1, axes=F,
       main = "Age Distribution of Alcohol Related Mortality Among \n Native Americans by Sex, 2016",
       xlab = "Age",
       ylab = "Percentage")
grid(nx = 11, ny = 4)
x <- density(AI_MALE_MORT_ALC_AGE[[1]])$x
y <- density(AI_MALE_MORT_ALC_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0,0,1,0.3), border = NA)
density(AI_FEMALE_MORT_ALC_AGE[[1]]) %>%
  lines(col="Red", lty=1)
x <- density(AI_FEMALE_MORT_ALC_AGE[[1]])$x
y <- density(AI_FEMALE_MORT_ALC_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(1,0,0,0.3), border = NA)
axis(2, at = seq(0, 0.05, 0.01), labels = paste(0:5, "%", sep = ""))
axis(1, at = seq(0, 120, 10))
legend("topright", legend = c("Female", "Male"),
       col = c("Red", "Blue"), lty = c(1,1), bty = "o", xjust = 1,
       yjust = 1, text.width = 15)

#########################################

AGE <- NULL
for (i in 1:nrow(VS16MORT)) {
  if ( VS16MORT$Age_Value[i] < 5 ) {
    AGE[i] <- 1
  } else if ( VS16MORT$Age_Value[i] < 10 ) {
    AGE[i] <- 2
  } else if ( VS16MORT$Age_Value[i] < 15 ) {
    AGE[i] <- 3
  } else if ( VS16MORT$Age_Value[i] < 20 ) {
    AGE[i] <- 4
  } else if ( VS16MORT$Age_Value[i] < 25 ) {
    AGE[i] <- 5
  } else if ( VS16MORT$Age_Value[i] < 30 ) {
    AGE[i] <- 6
  } else if ( VS16MORT$Age_Value[i] < 35 ) {
    AGE[i] <- 7
  } else if ( VS16MORT$Age_Value[i] < 40 ) {
    AGE[i] <- 8
  } else if ( VS16MORT$Age_Value[i] < 45 ) {
    AGE[i] <- 9
  } else if ( VS16MORT$Age_Value[i] < 50 ) {
    AGE[i] <- 10
  } else if ( VS16MORT$Age_Value[i] < 55 ) {
    AGE[i] <- 11
  } else if ( VS16MORT$Age_Value[i] < 60 ) {
    AGE[i] <- 12
  } else if ( VS16MORT$Age_Value[i] < 65 ) {
    AGE[i] <- 13
  } else if ( VS16MORT$Age_Value[i] < 70 ) {
    AGE[i] <- 14
  } else if ( VS16MORT$Age_Value[i] < 75 ) {
    AGE[i] <- 15
  } else if ( VS16MORT$Age_Value[i] < 80 ) {
    AGE[i] <- 16
  } else if ( VS16MORT$Age_Value[i] < 85 ) {
    AGE[i] <- 17
  } else if ( VS16MORT$Age_Value[i] >= 85) {
    AGE[i] <- 18
  }
}

VS16MORT <- VS16MORT %>%
  add_column(AGE)

###White Subset of Mortality, All Causes
WHITE_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==1) %>%
  select(AGE)

###Black Subset of Mortality, All Causes
BLACK_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==2) %>%
  select(AGE)

###American Indian Subset of Mortality, All Causes
AI_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==3) %>%
  select(AGE)

###Asian Subset of Mortality, All Causes
ASIAN_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==4) %>%
  select(AGE)




density(WHITE_MORT_AGE[[1]]) %>%
  plot(xlim=c(0,110), ylim=c(0,0.03), col="Red",
       main = "Age Distribution of Mortality, All Causes, by Race, 2016",
       xlab = "Age",
       ylab = "Proportion",
       axes = F,
       yaxs = "i", xaxs = "i")
grid(nx = 11, ny = 3)
x <- density(WHITE_MORT_AGE[[1]])$x
y <- density(WHITE_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(1,0,0,0.2), border = NA)
density(BLACK_MORT_AGE[[1]]) %>%
  lines(col="Green")
x <- density(BLACK_MORT_AGE[[1]])$x
y <- density(BLACK_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0,1,0,0.2), border = NA)
density(ASIAN_MORT_AGE[[1]]) %>%
  lines(col="Purple")
x <- density(ASIAN_MORT_AGE[[1]])$x
y <- density(ASIAN_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0.6274510,0.1254902,0.9411765,0.2), border = NA)
density(AI_MORT_AGE[[1]]) %>%
  lines(col="Blue")
x <- density(AI_MORT_AGE[[1]])$x
y <- density(AI_MORT_AGE[[1]])$y
polygon(c(min(x),x),c(min(y),y), col=rgb(0,0,1,0.2), border = NA)
axis(2, at = seq(0, 0.03, 0.01), labels = paste(0:3, "%", sep = ""))
axis(1, at = seq(0, 110, 10))
legend("topleft", legend = c("White", "Black", "Asian", "Native American"),
       col = c("Red", "Green", "Purple", "Blue"), lty = 1, bty = "o", xjust = 1,
       yjust = 1, text.width = 25, ncol = 1)

test <- matrix(nrow = 18, ncol = 4)
for (i in 1:4) {
  for (j in 1:18) {
    test[j,i] <- TABLE[j,i]/age_by_race_total[[i]][j]
  }
}


#####################################
Bridged_Race_Population <- read_delim("~/Downloads/Bridged-Race Population Estimates 1990-2016 (1).txt",
                                      "\t", escape_double = FALSE, trim_ws = TRUE)


Bridged_Race_Population %>%
  filter(.$Notes=="Total") %>%
  filter(.$`Race Code`=="2054-5") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0, 0.03), type="l", col="magenta")
par(new=T)
Bridged_Race_Population %>%
  filter(.$Notes=="Total") %>%
  filter(.$`Race Code`=="2106-3") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0, 0.03), type="l", col="green")
par(new=T)
Bridged_Race_Population %>%
  filter(.$Notes=="Total") %>%
  filter(.$`Race Code`=="A-PI") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0, 0.03), type="l", col="black")
par(new=T)
Bridged_Race_Population %>%
  filter(.$Notes=="Total") %>%
  filter(.$`Race Code`=="1002-5") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0,0.03), type="l", col="cyan")

Bridged_Race_Population %>%
  filter(.$`Gender Code`=="F") %>%
  filter(.$`Race Code`=="2054-5") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0, 0.03), type="l", col="magenta")
par(new=T)
Bridged_Race_Population %>%
  filter(.$`Gender Code`=="F") %>%
  filter(.$`Race Code`=="2106-3") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0, 0.03), type="l", col="green")
par(new=T)
Bridged_Race_Population %>%
  filter(.$`Gender Code`=="F") %>%
  filter(.$`Race Code`=="A-PI") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0, 0.03), type="l", col="black")
par(new=T)
Bridged_Race_Population %>%
  filter(.$`Gender Code`=="F") %>%
  filter(.$`Race Code`=="1002-5") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0,0.03), type="l", col="cyan")

Bridged_Race_Population %>%
  filter(.$`Gender Code`=="M") %>%
  filter(.$`Race Code`=="2054-5") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0, 0.03), type="l", col=cols[1])
par(new=T)
Bridged_Race_Population %>%
  filter(.$`Gender Code`=="M") %>%
  filter(.$`Race Code`=="2106-3") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0, 0.03), type="l", col=cols[2])
par(new=T)
Bridged_Race_Population %>%
  filter(.$`Gender Code`=="M") %>%
  filter(.$`Race Code`=="A-PI") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0, 0.03), type="l", col=cols[3])
par(new=T)
Bridged_Race_Population %>%
  filter(.$`Gender Code`=="M") %>%
  filter(.$`Race Code`=="1002-5") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric() %>%
  (function(x){x/sum(x)}) %>%
  plot(ylim=c(0,0.03), type="l", col=cols[4])

############

test <- Bridged_Race_Population %>%
  filter(.$`Gender Code`=="F") %>%
  filter(.$`Race Code`=="1002-5") %>%
  filter(!.$`Age Code`=="0") %>%
  filter(!.$`Age Code`==">85") %>%
  select(Population) %>%
  .[[1]]

test2 <- Bridged_Race_Population %>%
  filter(.$`Gender Code`=="F") %>%
  filter(.$`Race Code`=="1002-5") %>%
  filter(!.$`Age Code`=="0") %>%
  filter(!.$`Age Code`==">85") %>%
  select(Population) %>%
  .[[1]] %>%
  (function(x){x/sum(x)})

AI_FEMALE_MORT_AGE %>%
  filter(.$Age_Value<85) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  (function(x){x/test}) %>%
  plot()

AI_FEMALE_MORT_AGE %>%
  filter(.$Age_Value<85) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  (function(x){x/test}) %>%
  (function(x){x*test2*1000}) %>%
  plot()

###############

test <- Bridged_Race_Population %>%
  filter(.$`Gender Code`=="F") %>%
  filter(.$`Race Code`=="2054-5") %>%
  filter(!.$`Age Code`=="0") %>%
  filter(!.$`Age Code`==">85") %>%
  select(Population) %>%
  .[[1]]

test2 <- Bridged_Race_Population %>%
  filter(.$`Gender Code`=="F") %>%
  filter(.$`Race Code`=="2054-5") %>%
  filter(!.$`Age Code`=="0") %>%
  filter(!.$`Age Code`==">85") %>%
  select(Population) %>%
  .[[1]] %>%
  (function(x){x/sum(x)})


BLACK_FEMALE_MORT_AGE <- VS16MORT[VS16MORT$Age_Value!=999,] %>%
  filter(Race==2) %>%
  filter(Sex=="F") %>%
  select(Age_Value)

BLACK_FEMALE_MORT_AGE %>%
  filter(.$Age_Value<85) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  (function(x){x/test}) %>%
  plot()

BLACK_FEMALE_MORT_AGE %>%
  filter(.$Age_Value<85) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  (function(x){x/test}) %>%
  (function(x){x*test2*1000}) %>%
  plot()

