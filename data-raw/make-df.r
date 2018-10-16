dir.create("../data")

lm_patho <- read.csv("df.csv")
ridge_train <- read.csv("ridge_train.csv")
ridge_test <- read.csv("ridge_test.csv")


save(lm_patho, file = "../data/lm_patho.rda")
save(ridge_train, file = "../data/ridge_train.rda")
save(ridge_test, file = "../data/ridge_test.rda")
