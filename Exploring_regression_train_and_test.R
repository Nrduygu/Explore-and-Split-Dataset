

library(tidyverse)
data = read.csv("numeric_data_TR.csv")
data %>% 
  count(var_x1, sort = TRUE)  # birinci deðiþkenin count'ýný alýr
data %>% 
  count(var_x2, sort = TRUE)  # ikinci deðiþkenin count'ýný alýr

# grafik oluþturalým

ggplot(data, aes(var_x1, var_x2)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Coding Exercises") 

# basit bir lojistik regresyon modeli oluþturalým

model_glm <- data() %>%
  select(-var_x3) %>%
  glm(var_x1 ~ .,
      family = "binomial",
      data = .)
summary(model_glm)

# veriyi train ve test olarak ikiye ayýralým

library(caret)
data_select_one <- data %>%
  select(-var_x3)

set.seed(1234)
in_train <- createDataPartition(data$var_x1, p = 0.8, list = FALSE)  
training <- data_select_one[in_train,]
testing <- data_select_one[-in_train,]


data_select_glm <- train(var_x1 ~ ., method='glm', family='binomial',
                   data = training,
                   trControl = trainControl(method = "boot",
                                            sampling='up'))
# print model
data_select_glm

