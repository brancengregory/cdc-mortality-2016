####Import Necessary Libraries####
library(tidyverse)
library(RColorBrewer)

####Define Color Palettes for Race and Sex Groupings####
race_col <- brewer.pal(n = 4, name = "Set1")
sex_col <- c("Red", "Blue")

####Add Alpha Helper Function####
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

####Import Data####
MORT_16 <- read_csv("data/VS16MORT.csv", col_types = cols(Age_Value = col_integer(),
                                                          Age_Recode_52 = col_integer(),
                                                          Race = col_integer()))

Bridged_Race_Pop_16 <- read_delim("~/Downloads/Bridged-Race Population Estimates 1990-2016 (2).txt",
                                                             "\t", escape_double = FALSE, trim_ws = TRUE)

####Define Alcohol Related Death Codes (ICD-10)####
ALC_ICD10 <- c("K70", "F10", "E244", "G312", "G621", "G721", "I426", "K292", "K852",
               "K860", "R780", "X45", "X65", "Y15")

####Make Index of Obs with Matching Alcohol Related Mortality Causes, Remove Codes####
ALC_index <- NULL
for (i in 1:length(ALC_ICD10)) {
  ALC_index <- c(ALC_index, grep(paste0("^", ALC_ICD10[i], sep = ""),
                                 MORT_16[,"ICD10"][[1]]))
}
ALC_index <- sort(unique(ALC_index))
rm(i, ALC_ICD10)

####Add Alcohol Related Indicator Variable####
isALC <- rep(0, nrow(MORT_16))
for (i in 1:length(ALC_index)) {
  isALC[ALC_index[i]] <- 1
}
rm(i)

####Add indicator to data####
MORT_16 <- MORT_16 %>%
  add_column(isALC)

####Remove Indicator Object and Index####
rm(isALC, ALC_index)

####Make Age Group####
MORT_16 <- MORT_16 %>%
  mutate(AGE_GROUP = ifelse(Age_Recode_52 == 52, NA,
                     ifelse(Age_Recode_52 < 23, 1,
                     ifelse(Age_Recode_52 < 27, 2,
                     ifelse(Age_Recode_52 == 27, 3,
                     ifelse(Age_Recode_52 == 28, 4,
                     ifelse(Age_Recode_52 == 29, 5,
                     ifelse(Age_Recode_52 == 30, 6,
                     ifelse(Age_Recode_52 == 31, 7,
                     ifelse(Age_Recode_52 == 32, 8,
                     ifelse(Age_Recode_52 == 33, 9,
                     ifelse(Age_Recode_52 == 34, 10,
                     ifelse(Age_Recode_52 == 35, 11,
                     ifelse(Age_Recode_52 == 36, 12,
                     ifelse(Age_Recode_52 == 37, 13,
                     ifelse(Age_Recode_52 == 38, 14,
                     ifelse(Age_Recode_52 == 39, 15,
                     ifelse(Age_Recode_52 == 40, 16,
                     ifelse(Age_Recode_52 == 41, 17,
                     ifelse(Age_Recode_52 == 42, 18,
                     ifelse(Age_Recode_52 >= 43, 19, NA)))))))))))))))))))))


###############GRAPHS################

####Age Distribution of Mortality, All Causes####
MORT_16 %>%
  filter(.$Age_Value < 999) %>%
  select(Age_Value) %>%
  .[[1]] %>%
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
MORT_16 %>%
  filter(.$isALC == 1) %>%
  filter(.$Age_Value < 999) %>%
  select(Age_Value) %>%
  .[[1]] %>%
  as.numeric() %>%
  density() %>%
  lines(col="Red")
x <- MORT_16 %>%
  filter(.$Age_Value < 999) %>%
  select(Age_Value) %>%
  .[[1]] %>%
  as.numeric() %>%
  density() %>%
  (function(x){x$x})
y <- MORT_16 %>%
  filter(.$Age_Value < 999) %>%
  select(Age_Value) %>%
  .[[1]] %>%
  as.numeric() %>%
  density() %>%
  (function(x){x$y})
polygon(c(min(x),x),c(min(y),y), col=add.alpha(sex_col[2], 0.3), border = NA)
x <- MORT_16 %>%
  filter(isALC == 1) %>%
  filter(.$Age_Value < 999) %>%
  select(Age_Value) %>%
  .[[1]] %>%
  as.numeric() %>%
  density() %>%
  (function(x){x$x})
y <- MORT_16 %>%
  filter(isALC == 1) %>%
  filter(.$Age_Value < 999) %>%
  select(Age_Value) %>%
  .[[1]] %>%
  as.numeric() %>%
  density() %>%
  (function(x){x$y})
polygon(c(min(x),x),c(min(y),y), col=add.alpha(sex_col[1], 0.3), border = NA)
legend("topright", legend = c("All Causes", "Alcohol Related"),
       col = c("Blue", "Red"), lty = c(1,1), bty = "o", xjust = 1,
       yjust = 1, text.width = 15)

####Total Mortality Rate, All Races####
x1 <- MORT_16 %>%
  filter(is.na(.$AGE_GROUP)==F) %>%
  select(AGE_GROUP) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  as.numeric()

x2 <- Bridged_Race_Pop_16 %>%
  filter(.$Notes == "Total" & (is.na(Race) == T) & (is.na(`Age Group`)==F)) %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric()

y1 <- x1/x2
plot(y1, type = "l")

####Alcohol Related Mortality Rate####
x3 <- MORT_16 %>%
  filter(isALC == 1) %>%
  filter(is.na(.$AGE_GROUP)==F) %>%
  select(AGE_GROUP) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  as.numeric()
x3 <- c(x3[1:2], 0, 0, x3[3:length(x3)])

y2 <- x3/x2
plot(y2, type = "l")

####Alcohol Share of Total Mortality####
z1 <- y2/y1
plot(z1, type = "l")


####Kernel Density Plot, Age Distribution of Mortality, All Causes, by Race####
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


####Kernel Density Plot, Age Distribution of Alcohol Related Mortality, by Race####
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


####Mortatlity Rate, All Causes, by Age Group and Race####
x4 <- MORT_16 %>%
  filter(is.na(AGE_GROUP)==F) %>%
  filter(Race_Recode_5==1) %>%
  select(AGE_GROUP) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  as.numeric

x5 <- Bridged_Race_Pop_16 %>%
  filter(Race=="White") %>%
  filter(Notes=="Total") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric()

x6 <- MORT_16 %>%
  filter(is.na(AGE_GROUP)==F) %>%
  filter(Race_Recode_5==2) %>%
  select(AGE_GROUP) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  as.numeric

x7 <- Bridged_Race_Pop_16 %>%
  filter(`Race Code`=="2054-5") %>%
  filter(Notes=="Total") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric()

x8 <- MORT_16 %>%
  filter(is.na(AGE_GROUP)==F) %>%
  filter(Race_Recode_5==3) %>%
  select(AGE_GROUP) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  as.numeric

x9 <- Bridged_Race_Pop_16 %>%
  filter(`Race Code`=="1002-5") %>%
  filter(Notes=="Total") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric()

x10 <- MORT_16 %>%
  filter(is.na(AGE_GROUP)==F) %>%
  filter(Race_Recode_5==4) %>%
  select(AGE_GROUP) %>%
  .[[1]] %>%
  as.numeric() %>%
  table() %>%
  as.numeric

x11 <- Bridged_Race_Pop_16 %>%
  filter(`Race Code`=="A-PI") %>%
  filter(Notes=="Total") %>%
  select(Population) %>%
  .[[1]] %>%
  as.numeric()

y3 <- x4/x5
y4 <- x6/x7
y5 <- x8/x9
y6 <- x10/x11
plot(y3, type = "l")
lines(y4, col="blue")
lines(y5, col="red")
lines(y6, col="green")



