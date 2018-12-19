dir.create("../data")

lm_patho <- read.csv("df.csv")
ridge_train <- read.csv("ridge_train.csv")
ridge_test <- read.csv("ridge_test.csv")
emnist_train <- read.csv("emnist-letters-train-sample.csv")
emnist_test <- read.csv("emnist-letters-test-sample.csv")

save(lm_patho, file = "../data/lm_patho.rda")
save(ridge_train, file = "../data/ridge_train.rda")
save(ridge_test, file = "../data/ridge_test.rda")
save(emnist_train, file = "../data/emnist_train.rda")
save(emnist_test, file = "../data/emnist_test.rda")
