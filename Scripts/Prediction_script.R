# Predicting heart disease 

library(tidyverse)
library(tree)
library(stringi)
library(rmarkdown)
library(knitr)

path <- "C:/Users/ckeeter/Documents/Kaggle/Heart disease/heart.csv"

df <- read.csv(path)    # Name file

# Data ~EXPLORATION~ 

head(df)                # Lettuce see what we're working with 

names(df) <- c("age",   # Renaming some columns 
               "male", 
               "chest_pain",
               "BP", 
               "chol", 
               "fbs", 
               "rest_ecg", 
               "max_HR", 
               "exang", 
               "st_seg", 
               "st_slope", 
               "num_vessel",
               "defect", 
               "disease"
               )
table(df$age)          # Still seeing what's up 
hist(df$age)

hist_age <- ggplot(
  data = df,
  aes(
    x = age
    )
) + 
  geom_histogram(
    binwidth = 5,
    fill = "blue",
    color = "black"
  ) + 
  labs(
    title = "Distribution of Age",
    x = "Age (years)",
    y =  NULL
  ) + 
  theme_classic(
    
  )

hist_age

table(df$male)

df <- df %>% 
  mutate(sex = ifelse(
    male == "1", 'male', 'female'
   )
  )

table(df$sex)

bar_sex <- ggplot(
  data = df,
  aes(
    x = sex
  )
) + 
  geom_histogram(
    stat = "count",
    fill = "blue",
    color = "black"
  ) + 
  labs( 
    title = "Distribution of Males and Females",
    x = "Sex",
    y = NULL
    ) + 
  theme_classic(
    
  )

bar_sex

table(df$chest_pain)   # This should be recoded to categorical predictors for decision tree 

df <- df %>%           # So I did it   
  mutate(chest_pain_class = 
           ifelse(chest_pain == "0", 'asymptomatic', ifelse(
             chest_pain == "1", 'angina', ifelse(
               chest_pain == "2", 'atypical_angina', ifelse(
                 chest_pain == "3", 'nonanginal pain', NA 
                 )
               )
             )
           )
         )

table(df$chest_pain_class)

chest_pain_hist <- ggplot(
  data= df,
  aes(
    x = chest_pain_class
  )
) + 
  geom_histogram(
    stat = "count",
    color = "black",
    fill = "blue"
  ) + 
  labs(
    title = "Distribution of Chest Pain",
    x = "Type of Chest Pain"
  ) + 
  scale_x_discrete(
    labels = c("Agina", 
                "Atypical Agina", 
                "Nonanginal Pain", 
                "Asymptomatic"
      )
    ) + 
  theme_classic(
    
  )
  
chest_pain_hist

hist(df$BP)

bp_histo <- ggplot(
  data = df,
  aes(
    x = BP
  )
) + 
  geom_histogram(
    binwidth = 10,
    fill = "blue",
    color = "black"
  ) + 
  scale_x_continuous(
    breaks = pretty(df$BP, 
                    n = 10)
  ) + 
  labs(
    title = "Distribution of Resting Systolic Blood Pressure",
    x = "Blood Pressure (mmHg)",
    y = NULL
  ) + 
  theme_classic(
    
  )

bp_histo

hist(df$chol)

chol_histo <- ggplot(
  data = df,
  aes(
    x = chol
  )
) + 
  geom_histogram(
    binwidth = 30,
    fill = "blue",
    color = "black"
  ) + 
  scale_x_continuous(
    breaks = pretty(df$chol,
                    n = 15)
  ) + 
  labs(
    title = "Distribution of Total Cholesterol (mg/dL)",
    x = "Total Cholesterol",
    y = NULL
  ) + 
  theme_classic(
    
  )

chol_histo

table(df$fbs)    # Coded as 1,0 

df <- df %>%     # Coded so a physiologist can understand (lol)
  mutate(fbs_cat = 
           ifelse(fbs == "1", 'overv120', 'under_120')
         )

table(df$fbs_cat)

fbs_histo <- ggplot(
  data = df,
  aes(
   x = fbs_cat
  )
) + 
  geom_histogram(
    stat = "count",
    color = "black",
    fill = "blue"
  ) + 
  scale_x_discrete(
    labels = c("Over 120 mmol/L",
               "Under 120 mmol/L")
  ) + 
  labs(
    title = "Distribution of Fasting Blood Glucose Levels",
    x = "Fasting Blood Glucose",
    y = NULL
  ) + 
  theme_classic(
    
  )

fbs_histo

table(df$rest_ecg)

df <- df %>% 
  mutate(rest_ecg_cat = 
           ifelse(rest_ecg == "0", 'normal', ifelse(
             rest_ecg == "1", 'abnormal_st', ifelse(
               rest_ecg == "2", 'LVH', NA 
               )
             )
           )
         )

table(df$rest_ecg_cat)

ecg_cat_hist <- ggplot(
  data = df, 
  aes(
    x = rest_ecg_cat 
  )
) + 
  geom_histogram(
    stat = "count",
    color = "black",
    fill = "blue"
  ) + 
  scale_x_discrete(
    labels = c("Abnormal St", "LVH", "Normal ST")
  ) + 
  labs(
    title = "Distribution of Abnormal St Segment Interpretations",
    y = NULL,
    x = "Resting ST Segment Interpretation"
  ) + 
  theme_classic(
    
  )

ecg_cat_hist

table(df$rest_ecg_cat)

hist(df$max_HR)

max_HR_hist <- ggplot(
  data =df, 
  aes(
    x = max_HR
  )
) + 
  geom_histogram(
    binwidth = 10,
    fill = "blue",
    color = "black"
  ) + 
  scale_x_continuous(
    breaks = pretty(df$max_HR, 
                    n = 10)
  ) + 
  labs(
    title = "Distribution of Max Heart Rate",
    x = "Heart Rate (bpm)",
    y = NULL
  ) + 
  theme_classic(
    
  )

max_HR_hist

table(df$exang)

df <- df %>% 
  mutate(ex_ang =
           ifelse(exang == "0", 'no', 'yes')
         )

table(df$ex_ang)

ex_ang_hist <- ggplot(
  data = df,
  aes(
    x = ex_ang 
  )
) + 
  geom_histogram(
    stat = "count",
    fill = "blue",
    color = "black"
  ) + 
  scale_x_discrete(
    labels = c("No", "Yes")
  ) + 
  labs(
    title = "Individuals who Experienced Angina during Exercise",
    x = "Experienced Angina?",
    y = NULL
  ) + 
  theme_classic(
    
  )

ex_ang_hist

hist(df$st_seg)

st_dep <- ggplot(
  data = df,
  aes(
    x = st_seg
  )
) + 
  geom_histogram(
    binwidth = .3,
    color = "black",
    fill = "blue"
  ) + 
  scale_x_continuous(
    breaks = pretty(df$st_seg,
                    n = 15)
  ) + 
  labs(
    title = "Distribution of ST Segment Depression Relative to Rest",
    x = "ST Segment Depression Duration (1mm)",
    y = NULL
  ) + 
  theme_classic(
    
  )

st_dep

table(df$st_slope)

df <- df %>% 
  mutate(slope_cat = 
           ifelse(st_slope == "0", 'no_slope', ifelse(
             st_slope == "1", 'flat', ifelse(
               st_slope == "2", 'downslope', NA
               )
             )
           )
  )
 
slope_hist <- ggplot(
  data = df,
  aes(
    x = slope_cat
  )
) + 
  geom_histogram(
    stat = "count",
    fill = "blue",
    color = "black"
  ) + 
  scale_x_discrete(
    labels = c("Downsloping", "Flat", "Absent Slope")
  ) + 
  labs(
    title = "Characterizing ST Slope",
    x = "Slope Characteristic",
    y = NULL
  ) + 
  theme_classic(
    
  )

slope_hist

table(df$num_vessel) 

num_ves_histo <- ggplot(
  data = df,
  aes(
    x = num_vessel
  )
) + 
  geom_histogram(
    fill = "blue",
    color = "black",
    stat = "count"
  ) + 
  labs(
    title = "Number of Blood Vessels Seen (Flouroscopy)",
    x = "Number of Vessels",
    y = NULL
  ) + 
  theme_classic(
    
  )

num_ves_histo

table(df$defect)  

table(df$disease)

df <- df %>% 
  mutate(disease = 
           ifelse(disease == "0", 'HD', 'No_HD')
         )

table(df$disease)

disease_histo <- ggplot(
  data = df,
  aes(
    x = disease 
  )
) + 
  geom_histogram(
    stat = "count",
    color = "black",
    fill = "blue"
  ) + 
  scale_x_discrete(
    labels = c("Heart Disease",
               "No Heart Disease")
  ) +
  labs(
    title = "Distribution of Heart Disease",
    x = "Presence of Heart Disease",
    y = NULL
  )
  
  
disease_histo

# Let's do some more cleaning 

tree_df <- df %>% 
  select("age",
         "sex",
         "BP",
         "chol",
         "max_HR",
         "st_seg",
         "chest_pain_class",
         "fbs_cat",
         "rest_ecg_cat",
         "ex_ang",
         "slope_cat",
         "disease"
         )

# Stats 

names(df)

df <- df %>% 
  mutate(disease_code = 
           ifelse(disease == "HD", 1,0))

disease_fit <- aov(disease_code ~ age + male + chest_pain + BP + chol + fbs + rest_ecg + max_HR + exang + st_seg + st_slope + num_vessel,
    df)

disease_lm <- lm(disease_code ~ age + male + chest_pain + BP + chol + fbs + rest_ecg + max_HR + exang + st_seg + st_slope + num_vessel,
                  df)

summary(disease_fit)

summary(disease_lm)

summary.lm(disease_fit)

# Time to garden 

train = sample(1:nrow(tree_df), nrow(tree_df)/2)

test = -train 

training_data <- tree_df[train,] 

training_data$sex <- as.factor(training_data$sex)
training_data$chest_pain_class <- as.factor(training_data$chest_pain_class)
training_data$fbs_cat <- as.factor(training_data$fbs_cat)
training_data$rest_ecg_cat <- as.factor(training_data$rest_ecg_cat)
training_data$ex_ang <- as.factor(training_data$ex_ang)

testing_data <- tree_df[test, ]

testing_data$sex <- as.factor(testing_data$sex)
testing_data$chest_pain_class <- as.factor(testing_data$chest_pain_class)
testing_data$fbs_cat <- as.factor(testing_data$fbs_cat)
testing_data$rest_ecg_cat <- as.factor(testing_data$rest_ecg_cat)
testing_data$ex_ang <- as.factor(testing_data$ex_ang)

tree_df$disease <- as.factor(tree_df$disease)

fifty_fifty_model <- tree(as.factor(disease) ~ . , training_data)  # We're seeing if we can predict st segment slope 

plot(fifty_fifty_model)
text(fifty_fifty_model, pretty = 0)


# Time to prune 

predicted_test = predict(fifty_fifty_model, testing_data, type = "class")

mean(predicted_test != testing_data$disease)   # Misclassifies ~35-40% of the time 

# Cross validation 
cv_tree <- cv.tree(fifty_fifty_model, FUN = prune.misclass)

plot(cv_tree$size,      
     cv_tree$dev,
     type = "b"
     )

# Time to keep pruning 

pruned_model <- prune.misclass(fifty_fifty_model,
                               best = 6)

plot(pruned_model)
text(pruned_model, 
     pretty = 0)

tree_predict <- predict(pruned_model, 
                        testing_data, 
                        type = "class"
                        )
cv_pruned_tree <- cv.tree(pruned_model, FUN = prune.misclass)

mean(tree_predict != testing_data$disease)  # Misclassifies ~28% of the time

# Time for the 80/20 model 

train2 = sample(1:nrow(tree_df), nrow(tree_df)*.8)

test2 = -train 

training_data2 <- tree_df[train2,]  

training_data2$sex <- as.factor(training_data2$sex)
training_data2$chest_pain_class <- as.factor(training_data2$chest_pain_class)
training_data2$fbs_cat <- as.factor(training_data2$fbs_cat)
training_data2$rest_ecg_cat <- as.factor(training_data2$rest_ecg_cat)
training_data2$ex_ang <- as.factor(training_data2$ex_ang)

testing_data2 <- tree_df[test2, ]

testing_data2$sex <- as.factor(testing_data2$sex)
testing_data2$chest_pain_class <- as.factor(testing_data2$chest_pain_class)
testing_data2$fbs_cat <- as.factor(testing_data2$fbs_cat)
testing_data2$rest_ecg_cat <- as.factor(testing_data2$rest_ecg_cat)
testing_data2$ex_ang <- as.factor(testing_data2$ex_ang)

eighty_twenty_model <- tree(as.factor(disease) ~ . , training_data2) 

plot(eighty_twenty_model)
text(eighty_twenty_model, pretty = 0)


# Time to prune 

predicted_test2 = predict(eighty_twenty_model, testing_data2, type = "class")

mean(predicted_test2 != testing_data2$disease)  

# Cross validation 
cv_tree2 <- cv.tree(eighty_twenty_model, FUN = prune.misclass)

plot(cv_tree2$size,      
     cv_tree2$dev,
     type = "b"
)

# Time to keep pruning 

pruned_model2 <- prune.misclass(eighty_twenty_model,
                               best = 8)

plot(pruned_model2)
text(pruned_model2, 
     pretty = 0)

tree_predict2 <- predict(pruned_model2, 
                        testing_data2, 
                        type = "class"
)

cv_pruned_tree2 <- cv.tree(pruned_model2, FUN = prune.misclass)

mean(tree_predict2 != testing_data2$disease)


