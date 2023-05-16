rm(list = ls())
farm <- read.csv('framingham.csv', header = T)
farm <- farm[!(farm$chol0 == -9 | farm$chol2 == -9 | farm$chol4 == -9 | farm$chol6 == -9 | farm$chol8 == -9 | farm$chol10 == -9),]
set.seed(1)
x <- model.matrix(death~., farm)[,-1]
y <- farm$death

# a
lasso <- function(pred, resp, covars){
  cv <- cv.glmnet(pred, resp, alpha = 1, family = "binomial")
  fit <- glmnet(pred, resp, alpha = 1, family = "binomial", lambda = cv$lambda.min)
  probabilities <- predict(fit, newx = pred)
  preds <- ifelse(probabilities > 0.5, 1, 0)
  print(mean(preds == y))
  return(coef(fit))
}

lasso(x, y, names(x))

# b
ridge <- function(pred, resp, covars){
  cv <- cv.glmnet(pred, resp, alpha = 0, family = "binomial")
  fit <- glmnet(pred, resp, alpha = 0, family = "binomial", lambda = cv$lambda.min)
  probabilities <- predict(fit, newx = pred)
  preds <- ifelse(probabilities > 0.5, 1, 0)
  print(mean(preds == y))
  return(coef(fit))
}

ridge(x, y, names(x))

# c
glm_fit <- glm(death ~ ., data = farm, family = binomial)
glm_probs <- predict(glm_fit, farm, type = "response")
preds <- ifelse(glm_probs > 0.5, 1, 0)
print(mean(preds == y))
print(summary(glm_fit))
