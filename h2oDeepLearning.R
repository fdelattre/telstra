# # The following two commands remove any previously installed H2O packages for R.
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# # Next, we download packages that H2O depends on.
# pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# # Now we download, install and initialize the H2O package for R.
# install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tibshirani/8/R")))

library(h2o)
localH2O = h2o.init(nthreads = 6)

# source('~/datascience/challenges/telstra/base.R')
# source('~/datascience/challenges/telstra/utils.R')

train_file <- "train.csv"
test_file <- "test.csv"

train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)

y <- "fault_severity"
x <- setdiff(names(train), y)
train[,y] <- as.factor(train[, y])

cv.deepl <- h2o.deeplearning(
  x,
  y,
  distribution = "multinomial",
  training_frame = train,
  activation = "RectifierWithDropout",
  input_dropout_ratio = 0.3,
  nfolds = 5,
  stopping_metric = "logloss",
  hidden = c(300, 300, 300),
  epochs = 10,
  l1 = 1e-5
)

h2o.logloss(cv.deepl, xval = TRUE)

h2o.shutdown(prompt = F)
