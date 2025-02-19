install.packages('pacman')
library('pacman')
p_load('MASS', 'dplyr', 'tidyverse', 'ggplot2', 'ggthemes', 'readxl', 'ggcorrplot', 'Hmisc', 'datawizard', "outliers", "pracma", "reshape2", "viridis", 'rcompanion')

df <- read.csv("..\\work\\Obesity_2024.csv", na.strings = c('',NA))


glimpse(df)

#####################################################
#First we'll need to deal with NA data
#####################################################

df = df[0:1000, ]

nrow(df) - nrow( df[apply(df, 1, function(x) any(is.na(x))), ])
#There are 970 rows with missing data.
#A lot of these rows only have 1 to 5 columns filled out of 47, the rest are NAs; 
#it is best to remove them.

nrow( df[apply(df, 1, function(x) any(is.na(x))), ])
#There's still 30 rows with NA values.
#We'll replace each NA with their respective column's median value

na_cols <- names(which(colSums(is.na(df)) > 0))

medians <- apply(df[apply(df, 1, function(x) !any(is.na(x))), na_cols], 2, median)
medians

i <- 1

for(value in medians) {
  
    df[na_cols[i]][is.na(df[na_cols[i]])] <- value
  
  i <- i + 1
}

#All NA values have been successfully removed.
nrow(df[apply(df, 1, function(x) any(is.na(x))), na_cols])


remove(na_cols)
remove(medians)
remove(i)
remove(value)

colnames(df) <- c("Gender", "Age", "Family_History_Overweight", "Food_Between_Meals", "Smoke", "Transportaion",
                  "City", "Physical_Activity_Time", "Breakfast_In_Week", "Grains_In_Week", "Dairy_In_Week",
                  "Caffeine_In_Week", "Seafood_In_Week", "BRS_Q1", "BRS_Q2", "BRS_Q3", "BRS_Q4", "BRS_Q5", "BRS_Q6",
                  "K10_Q1", "K10_Q2", "K10_Q3", "K10_Q4", "K10_Q5", "K10_Q6", "K10_Q7", "K10_Q8", "K10_Q9", "K10_Q10",
                  "PSQI_Q1", "PSQI_Q2", "PSQI_Q3", "PSQI_Q4", "PSQI_Q5A", "PSQI_Q5B", "PSQI_Q5C", "PSQI_Q5D", "PSQI_Q5E",
                  "PSQI_Q5F", "PSQI_Q5G", "PSQI_Q5H", "PSQI_Q5I", "PSQI_Q6", "PSQI_Q7", "PSQI_Q8", "PSQI_Q9", "Obesity_Indicator")

fixing_vector <- function(vect) {
  vect <- tolower(vect)
  vect <- trimws(vect)
  
  return(vect)
}

i <- 1

for(col in colnames(df)) {
  
  df[, col] <- fixing_vector(df[, col])
  
  i <- i + 1
}

remove(i, col)

#Check for duplicated rows

sum(duplicated(df)) #None

#####################################################
#Now, we can calculate important metrics.
#####################################################


#First we'll calculate the PSQI

########
#Component 1
########



unique(df$PSQI_Q9)
df$PSQI_Q9 <- factor(df$PSQI_Q9,
                levels = c("very good", "fairly good", "fairly bad", "very bad"),
                ordered = T)

comp1 <- df$PSQI_Q9

########
#Component 2
########

PSQI_ranks = c("not during past month", "less than once a week", "once or twice a week", "three or more times a week")

df$PSQI_Q5A <- factor(df$PSQI_Q5A,
                 levels = PSQI_ranks,
                 ordered = T)

df$PSQI_Q2 <- as.integer(df$PSQI_Q2)

comp2_b <- cut(df$PSQI_Q2,
               breaks = c(-Inf, 15, 30, 60, Inf),
               ordered = T)

#Subtract 1 from each vector because changing type ups all values by 1 for some mysterious reason
comp2 <- (as.integer(df$PSQI_Q5A) - 1) + (as.integer(comp2_b) - 1)

comp2[comp2 == 1 | comp2 == 2] = 1
comp2[comp2 == 3 | comp2 == 4] = 2
comp2[comp2 == 5 | comp2 == 6] = 3

comp2 <- factor(comp2, ordered = T)

########
#Component 3
########

df$PSQI_Q4 <- as.double(df$PSQI_Q4)

comp3 <- df$PSQI_Q4
comp3 <- cut(comp3, breaks = c(-Inf, 5, 6, 7, Inf), ordered = T, right = T)

########
#Component 4
########

p_load(lubridate)

calculate_sleep_duration <- function(sleep_time, wake_time) {
  #Parse the times and convert to minutes since midnight
  sleep <- hour(parse_time(sleep_time)) * 60 + minute(parse_time(sleep_time))
  wake <- hour(parse_time(wake_time)) * 60 + minute(parse_time(wake_time))
  
  duration <- if_else(wake > sleep,
                      (wake - sleep) / 60,
                      (wake + 1440 - sleep) / 60)  #1440 minutes in a day
  
  return(duration)
}

sleep_time <- as.vector(mapply(calculate_sleep_duration, df$PSQI_Q1, df$PSQI_Q3))

df$PSQI_Q1 <- hour(parse_time(df$PSQI_Q1)) * 60 + minute(parse_time(df$PSQI_Q1))
df$PSQI_Q3 <- hour(parse_time(df$PSQI_Q3)) * 60 + minute(parse_time(df$PSQI_Q3))



sleep_eff <- (df$PSQI_Q4 / sleep_time) * 100

comp4 <- cut(sleep_eff, breaks = c(Inf, 85, 75, 65, -Inf), ordered = T)

########
#Component 5
########

df$PSQI_Q5B <- factor(df$PSQI_Q5B, 
                      levels = PSQI_ranks,
                      ordered = T)

df$PSQI_Q5C <- factor(df$PSQI_Q5C, 
                      levels = PSQI_ranks,
                      ordered = T)

df$PSQI_Q5D <- factor(df$PSQI_Q5D, 
                      levels = PSQI_ranks,
                      ordered = T)

df$PSQI_Q5E <- factor(df$PSQI_Q5E, 
                      levels = PSQI_ranks,
                      ordered = T)

df$PSQI_Q5F <- factor(df$PSQI_Q5F, 
                      levels = PSQI_ranks,
                      ordered = T)

df$PSQI_Q5G <- factor(df$PSQI_Q5G, 
                      levels = PSQI_ranks,
                      ordered = T)

df$PSQI_Q5H <- factor(df$PSQI_Q5H, 
                      levels = PSQI_ranks,
                      ordered = T)

df$PSQI_Q5I <- factor(df$PSQI_Q5I, 
                      levels = PSQI_ranks,
                      ordered = T)

comp5 <- ( (as.integer(df$PSQI_Q5A) - 1) + 
             (as.integer(df$PSQI_Q5B) - 1) +
             (as.integer(df$PSQI_Q5C) - 1) + 
             (as.integer(df$PSQI_Q5D) - 1) + 
             (as.integer(df$PSQI_Q5E) - 1) + 
             (as.integer(df$PSQI_Q5F) - 1) + 
             (as.integer(df$PSQI_Q5G) - 1) + 
             (as.integer(df$PSQI_Q5H) - 1) +
             (as.integer(df$PSQI_Q5I) - 1) )

comp5 <- cut(comp5, breaks = c(1, 9, 18, 27), labels = c(1, 2, 3), right = T)
comp5 <- factor(replace_na(as.integer(comp5), 0), ordered = T)

########
#Component 6
########

df$PSQI_Q6 <- factor(df$PSQI_Q6,
                levels = PSQI_ranks,
                ordered = T)

comp6 = df$PSQI_Q6

########
#Component 7
########
df$PSQI_Q7 <- factor(df$PSQI_Q7,
                levels = PSQI_ranks,
                ordered = T)

df$PSQI_Q8 <- factor(df$PSQI_Q8,
                  levels = c("no problem at all", "only a very slight problem", "somewhat of a problem", "a very big problem"),
                  ordered = T)


comp7 <- (as.integer(df$PSQI_Q7) - 1) + (as.integer(df$PSQI_Q8) - 1)

comp7 <- cut(comp7, breaks = c(1,2,4,6), labels = c(1,2,3), ordered = T, right = T)
comp7 <- factor(replace_na(as.integer(comp7), 0), ordered = T)

########
#PSQI
########

PSQI <- (as.integer(comp1) - 1) + (as.integer(comp2) - 1) + (as.integer(comp3) - 1) + (as.integer(comp4) - 1) + (as.integer(comp5) - 1) + (as.integer(comp6) - 1) + (as.integer(comp7) - 1)

remove(comp1, comp2, comp3, comp4, comp5, comp6, comp7, calculate_sleep_duration, PSQI_ranks, comp2_b)

       
######################################################################################

#Second we'll calculate the BRS

df$BRS_Q1 <- as.integer(df$BRS_Q1)
df$BRS_Q2 <- 6 - as.integer(df$BRS_Q2)
df$BRS_Q3 <- as.integer(df$BRS_Q3)
df$BRS_Q4 <- 6 - as.integer(df$BRS_Q4)
df$BRS_Q5 <- as.integer(df$BRS_Q5)
df$BRS_Q6 <- 6 - as.integer(df$BRS_Q6)

BRS <- rowSums(cbind(df$BRS_Q1, df$BRS_Q2, df$BRS_Q3, df$BRS_Q4, df$BRS_Q5, df$BRS_Q6))

######################################################################################

#Third we'll calculate the K10


K10_ranks = c("never", "almost never", "sometimes", "fairly often", "very often")

df$K10_Q1 <- factor(df$K10_Q1,
                levels = K10_ranks,
                ordered = T)

df$K10_Q2 <- factor(df$K10_Q2,
                levels = K10_ranks,
                ordered = T)

df$K10_Q3 <- factor(df$K10_Q3,
                levels = K10_ranks,
                ordered = T)

df$K10_Q4 <- factor(df$K10_Q4,
                levels = K10_ranks,
                ordered = T)

df$K10_Q5 <- factor(df$K10_Q5,
                levels = K10_ranks,
                ordered = T)

df$K10_Q6 <- factor(df$K10_Q6,
                levels = K10_ranks,
                ordered = T)

df$K10_Q7 <- factor(df$K10_Q7,
                levels = K10_ranks,
                ordered = T)

df$K10_Q8 <- factor(df$K10_Q8,
                levels = K10_ranks,
                ordered = T)

df$K10_Q9[df$K10_Q9 == 'alfairly often never'] <- 'almost never'
                                                                                                                                                                                                                            
df$K10_Q9 <- factor(df$K10_Q9,
                levels = K10_ranks,
                ordered = T)

df$K10_Q10[df$K10_Q10 == 'alfairly often never'] <- 'almost never'

df$K10_Q10 <- factor(df$K10_Q10,
                levels = K10_ranks,
                ordered = T)

K10 <- rowSums(cbind(df$K10_Q1, df$K10_Q2, df$K10_Q3, df$K10_Q4, df$K10_Q5, df$K10_Q6, df$K10_Q7, df$K10_Q8, df$K10_Q9, df$K10_Q10))

######################################################################################

#Finally, we categorize the scores
PSQI_cat <- cut(PSQI, breaks = c(0, 4, 21), labels = c('good', 'poor'), right = T)
BRS_cat <- cut(BRS, breaks = c(5, 13, 21, 30), labels = c('low resilience', 'normal resilience', 'high resilience'), right = T)
K10_cat <- cut(K10, breaks = c(9, 19, 29, 50), labels = c('likely to be well', 'likely to have a moderate disorder', 'likely to have a severe disorder'), right = T)

ranks_1 = c("never", "1 time", "2 times", "3 times", "4 times","5 times", "6 or 7 times")
ranks_2 = c("never", "1 time", "2 times", "3 times", "4 times","5 times or more")

df$Gender <- as.factor(df$Gender)
df$Age <- as.integer(df$Age)
df$Family_History_Overweight <- as.factor(df$Family_History_Overweight)
df$Food_Between_Meals <- factor(df$Food_Between_Meals)
df$Smoke <- as.factor(df$Smoke)
df$Transportaion <- as.factor(df$Transportaion)
df$City <- as.factor(df$City)
df$Physical_Activity_Time <- as.double(df$Physical_Activity_Time)
df$Breakfast_In_Week <- factor(df$Breakfast_In_Week, levels = ranks_1, ordered = T)
df$Grains_In_Week <- factor(df$Grains_In_Week, levels = ranks_2, ordered = T)
df$Dairy_In_Week <- factor(df$Dairy_In_Week, levels = ranks_2, ordered = T)
df$Caffeine_In_Week <- factor(df$Caffeine_In_Week, levels = ranks_2, ordered = T)
df$Seafood_In_Week <- factor(df$Seafood_In_Week, levels = ranks_2, ordered = T)
df$Obesity_Indicator <- factor(df$Obesity_Indicator, levels = c("insufficient_weight", "normal_weight",
                                                                "overweight_level_i", "overweight_level_ii",
                                                                "obesity_type_i", "obesity_type_ii", "obesity_type_iii"), ordered = T)


df_clean <- df

install.packages('writexl')

write_csv(df_clean, "cleaned_dataset.csv")

df <- cbind(df, PSQI, BRS, K10, PSQI_cat, BRS_cat, K10_cat, sleep_time, sleep_eff)

write_csv(df, "cleaned_dataset_extra_columns.csv")

#Generate summary statistics for all fields

describe(df)

remove(comp2_b, comp4_b, comp4_b1, comp4_b2, comp5_f, ranks_1, ranks_2)


#Surprisingly, no NA!
sum(apply(df, 1, function(x) is.na(x)))

#########################Normalization & Outlier detection####################################

#Check all correlation between numeric fields


corr <- cor(sapply(df, as.numeric), method = 'spearman')

cor_df <- round(corr, 2) 

melted_cor <- melt(cor_df)

melted_cor %>% ggplot(aes(x = melted_cor[,1], y =melted_cor[,2], fill = melted_cor[,3])) +
  geom_tile() +
  geom_text(aes(melted_cor[,2], melted_cor[,1], label = melted_cor[,3]), size = 2) +
  theme(text = element_text(size = 7.5),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_gradient2(low = "red", high = "blue",
                       limit = c(-1,1), name="Correlation")

cor.test(df$sleep_time, df$sleep_eff, method = "spearman")

#Outliers

grubbs.test(df$PSQI, type = 10) #There are outliers
df$PSQI[df$PSQI == 17] <- median(df$PSQI)
grubbs.test(df$BRS, type = 10) #No Outliers
grubbs.test(df$K10, type = 10) #No Outliers

grubbs.test(df$sleep_eff, type = 10) #There are outliers
df$sleep_eff[df$sleep_eff >= 130 | df$sleep_eff <= 55] <- median(df$sleep_eff)
grubbs.test(df$sleep_eff, type = 10) #There are no more outliers

grubbs.test(df$sleep_time, type = 10) #There are outliers
df$sleep_time[df$sleep_time >= 12 | df$sleep_time <= 5.25] <- median(df$sleep_time)
grubbs.test(df$sleep_time, type = 10) #There are no more outliers

grubbs.test(df$Age, type = 10) #There are outliers
df$Age[df$Age >= 42] <- median(df$Age)
grubbs.test(df$Age, type = 10) #There are no more outliers

grubbs.test(df$Physical_Activity_Time, type = 10) #No outliers 

#BRS Q1 through Q6 naturally do not have any outliers.

#Correlation between PSQI & BRS

#Since normality is implausible in the context of discrete values, be it numeric or categorical, then we'll be strictly using either spearman or kendall methods.

numeric_vectors <- names(df)[sapply(df, is.numeric)]

cor_df_numeric <- round(cor(df[, numeric_vectors]),2)

melted_cor_numeric <- melt(cor_df_numeric)

melted_cor_numeric %>% ggplot(aes(x = melted_cor_numeric[,1], y =melted_cor_numeric[,2], fill = melted_cor_numeric[,3])) +
  geom_tile() +
  ylab('') +
  xlab('') +
  geom_text(aes(melted_cor_numeric[,2], melted_cor_numeric[,1], label = melted_cor_numeric[,3]), size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_gradient2(low = "red", high = "blue",
                       limit = c(-1,1), name="Correlation")

rcorr(matrix(c(df$PSQI, df$BRS, df$K10, df$sleep_eff, df$sleep_time), ncol=5, nrow = 1000, dimnames = list(c(1:1000), c("PQSI", "BRS", "K10", "sleep_eff", "sleep_time"))), type='spearman')

#Only significant & relevant relations: 
# Between K10 & BRS, which is -49%
# Between PSQI & Sleep Efficiency, which is 19%
# Between PSQI & Sleep Time, which is 21%
                
df %>% ggplot(aes(x = sleep_eff, y = PSQI, color=PSQI_cat)) +
  geom_point(size = 2) +
  xlab("Sleep Time") +
  ylab("Sleep Efficiency") +
  theme_bw() +
  geom_smooth(color = "blue", method = 'lm') +
  scale_color_colorblind() +
  facet_grid(~PSQI_cat, scales = 'free')

df %>%
  group_by(PSQI_cat) %>%
  select(PSQI, sleep_eff) %>%
  summarise(correlation = cor(PSQI, sleep_eff, method='spearman'))

df %>% ggplot(aes(x = BRS, y = K10, colour = BRS_cat)) +
  geom_point(size = 2) +
  geom_smooth() +
  xlab("Brief Resilience Scale") +
  ylab("Kessler Scale") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme_bw() +
  scale_color_excel_new() +
  facet_grid(~BRS_cat, scales = "free")

df %>%
  group_by(BRS_cat) %>%
  select(BRS, K10) %>%
  summarise(correlation = cor(BRS, K10, method='spearman'))

#Heat map for important variables

important_vectors <- numeric_vectors

cor.test(as.numeric(df$Obesity_Indicator), as.numeric(df$Family_History_Overweight), method = 'spearman')
cor.test(as.numeric(df$Obesity_Indicator), df$Age, method = 'spearman')
cor.test(as.numeric(df$Obesity_Indicator), as.numeric(df$PSQI_Q9), method = 'spearman')
cor.test(as.numeric(df$Obesity_Indicator), as.numeric(df$K10), method = 'spearman')
cor.test(as.numeric(df$Obesity_Indicator), as.numeric(df$BRS), method = 'spearman')
cor.test(as.numeric(df$Obesity_Indicator), as.numeric(df$PSQI), method = 'spearman')
cor.test(as.numeric(df$Obesity_Indicator), as.numeric(df$Physical_Activity_Time), method = 'spearman')
cor.test(as.numeric(df$Obesity_Indicator), as.numeric(df$Smoke), method='spearman')
cor.test(as.numeric(df$Obesity_Indicator), as.numeric(df$Gender), method='spearman')

Age_Groups_Count <- df %>% group_by(Obesity_Indicator, Age) %>%
  summarise(t = n())

Age_Groups_Count$Age <- cut(Age_Groups_Count$Age, breaks = c(13, 23, 32, 41), labels = c("14 - 23", "24 - 32", "33 - 41"))

par(mfrow=c(3,2))

p1 <- Age_Groups_Count %>% ggplot(aes(x = Age, y = t/1000, fill = Obesity_Indicator)) +
  xlab("Age Groups") +
  ylab("Percentage") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0.01, 0.1, 0.01)) +
  geom_bar(stat = 'identity', position = 'dodge')

Physical_Groups_Count <- df %>% group_by(Obesity_Indicator, Physical_Activity_Time) %>%
  summarise(t = n())
  
Physical_Groups_Count$Physical_Activity_Time <- cut(Physical_Groups_Count$Physical_Activity_Time, breaks = c(-Inf, 1, 2, Inf), labels = c('Low', 'Medium', 'High'))

p2 <- Physical_Groups_Count %>% ggplot(aes(x = Physical_Activity_Time, y = t/1000, fill = Obesity_Indicator)) +
  xlab("Physical Activity") +
  ylab("Percentage") +
  theme_bw() +
  geom_bar(stat = 'identity', position = 'dodge') 

df %>% ggplot(aes(y= as.numeric(df$Obesity_Indicator), x =df$Age)) +
  geom_point()

Gender_Count <- df %>% group_by(Obesity_Indicator, Gender) %>%
  summarise(t = n())

p3 <- Gender_Count %>% ggplot(aes(x = Gender, y = t, fill = Obesity_Indicator)) +
  geom_bar(stat = 'identity', position = 'dodge')

Smoke_Count <- df %>% group_by(Obesity_Indicator, Smoke) %>%
  summarise(t = n())

p4 <- Smoke_Count %>% ggplot(aes(x = Smoke, y = t/1000, fill = Obesity_Indicator)) +
  ylab('Percentage') +
  geom_bar(stat = 'identity', position = 'dodge')

Family_Count <- df %>% group_by(Obesity_Indicator, Family_History_Overweight) %>%
  summarise(t = n())

p5 <- Family_Count %>% ggplot(aes(x = Family_History_Overweight, y = t/1000, fill = Obesity_Indicator)) +
  xlab("Family History") +
  ylab("Percentage") +
  geom_bar(stat = 'identity', position = 'dodge')

p6 <- df %>% ggplot(aes(x = Transportaion, fill = Obesity_Indicator)) +
  geom_histogram(stat = 'count', position = 'dodge') +
  facet_grid(~Transportaion, scales = 'free')


important_vectors <- append(important_vectors, c("Obesity_Indicator", "Family_History_Overweight",
                            "Seafood_In_Week", "Caffeine_In_Week", "Dairy_In_Week",
                            "Grains_In_Week", "Breakfast_In_Week", "Smoke", "Food_Between_Meals",
                            "Gender"))

important_vectors <- important_vectors[!important_vectors %in% c("BRS_Q1", "BRS_Q2", "BRS_Q3", "BRS_Q4", "BRS_Q5", "BRS_Q6",
                             "PSQI_Q1", "PSQI_Q2", "PSQI_Q3", "PSQI_Q4")]


chisq.test(df$Transportaion, df$Obesity_Indicator)
chisq.test(df$City, df$Obesity_Indicator)

cramerV(df$Transportaion, df$Obesity_Indicator)
cramerV(df$City, df$Obesity_Indicator)

cor_df_numeric <- round(cor(sapply(df[, important_vectors], as.numeric)),2)

melted_cor_numeric <- melt(cor_df_numeric)

melted_cor_numeric %>% ggplot(aes(x = melted_cor_numeric[,1], y =melted_cor_numeric[,2], fill = melted_cor_numeric[,3])) +
  geom_tile() +
  ylab('') +
  xlab('') +
  geom_text(aes(melted_cor_numeric[,2], melted_cor_numeric[,1], label = melted_cor_numeric[,3]), size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_gradient2(low = "red", high = "blue",
                       limit = c(-1,1), name="Correlation")


