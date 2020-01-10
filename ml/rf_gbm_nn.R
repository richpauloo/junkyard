library(dplyr)
library(randomForest)
library(gbm)
library(keras)
library(ggplot2)

# only need to do this once. takes a while
#install_keras()

# view the 3 classes
table(iris$Species)

# versicolor and virginica are less separable
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
  geom_point()

# train/test split. holdout 25% of data for testing
set.seed(1)
i <- sample(1:nrow(iris), 0.75 * nrow(iris), replace = FALSE)
train <- iris[i,  ]
test  <- iris[-i, ]

# ------------------------------------------------------------------------
# randomforest 
# ------------------------------------------------------------------------
m1  <- randomForest(Species ~ Sepal.Length + Sepal.Width, 
                    data = train)
m1p <- predict(m1, test)

# confusion matrix
cm1 <- table(test$Species, m1p)
cm1

# overall error rate: sum of off diagonals divided by total
em1 <- (sum(cm1) - sum(diag(cm1))) / sum(cm1)
em1

# ------------------------------------------------------------------------
# boosted tree
# ------------------------------------------------------------------------
m2  <- gbm(Species ~ Sepal.Length + Sepal.Width, 
           data = train, 
           distribution = "multinomial",
           n.trees = 500)
m2p <- predict(m2, test, n.trees = 500, type = "response")
m2p <- colnames(m2p)[ apply(m2p, 1, which.max) ]

# confusion matrix
cm2 <- table(test$Species, m2p)
cm2

# overall error rate: sum of off diagonals divided by total
em2 <- (sum(cm2) - sum(diag(cm2))) / sum(cm2)
em2


# ------------------------------------------------------------------------
# neural network, modified from:
# https://github.com/leonjessen/keras_tensorflow_on_iris
# ------------------------------------------------------------------------

# scale and rename data
nn_dat = iris %>% 
  as_tibble() %>%
  mutate(
    class_num   = as.numeric(Species) - 1, # factor, so = 0, 1, 2
    class_label = Species
  ) %>%
  select(contains("Sepal"), class_num, class_label)

head(nn_dat, 3)

# train/test split
x_train = nn_dat[i,  ] %>% select(contains("Sepal")) %>% as.matrix()
y_train = nn_dat[i,  ] %>% pull(class_num) %>% to_categorical(3)

x_test  = nn_dat[-i, ] %>% select(contains("Sepal")) %>% as.matrix()
y_test  = nn_dat[-i, ] %>% pull(class_num) %>% to_categorical(3)

# build and compile 
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 2, activation = 'relu', input_shape = 2) %>% 
  layer_dense(units = 3, activation = 'softmax') %>% 
  compile(
    loss      = "categorical_crossentropy",
    optimizer = 'rmsprop',
    #optimizer = optimizer_sgd(lr = 0.01, decay = 1e-6, momentum = 0.9, nesterov = TRUE),
    metrics   = c("accuracy")
  )

# train the network
history = model %>% 
  fit(
    x = x_train, y = y_train,
    epochs           = 200,
    batch_size       = 4,
    validation_split = 0
  )
plot(history)

# model predictions on test data
m3p <- predict_classes(model, x_test)

# confusion matrix
cm3 <- table(test$Species, m3p)
cm3

# overall error rate: sum of off diagonals divided by total
em3 <- (sum(cm3) - sum(diag(cm3))) / sum(cm3)
em3


# ------------------------------------------------------------------------
# compare error rates
# ------------------------------------------------------------------------
data.frame(model = c("Random Forest","Boosted Tree","Neural Network"),
           error = c(em1, em2, em3)) %>% 
  ggplot(aes(forcats::fct_reorder(model, error), error)) + 
  geom_col() +
  coord_flip() +
  labs(x = "", y = "Error")

