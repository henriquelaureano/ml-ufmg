# ==================================================================== #
path <- "C:/Users/henri/Dropbox/Scripts/aprendizado de maquina/poster/"

da <- read.csv(paste0(path, "data.csv"))

da <- da[ , -c(1, 33)]

table(da$diagnosis)

set.seed(22)

ls <- c(
  sample(size = 285
         , x = as.numeric(row.names(subset(da, diagnosis == "B"))))
  , sample(size = 169
           , x = as.numeric(row.names(subset(da, diagnosis == "M"))))
)

da.tr <- da[ls, ] ; da.te <- da[-ls, ]

dam.tr <- da.tr[ , 1:11] ; dam.te <- da.te[ , 1:11]

dase.tr <- da.tr[ , c(1, 12:21)] ; dase.te <- da.te[ , c(1, 12:21)]

daw.tr <- da.tr[ , c(1, 22:31)] ; daw.te <- da.te[ , c(1, 22:31)]
# ==================================================================== #
# logistic regression ==================================================
# ==================================================================== #
rl.dam <- glm(diagnosis ~ ., dam.tr, family = binomial)

drop1(rl.dam, test = "Chisq")

rl.dam <-
  update(rl.dam, . ~ . - perimeter_mean) ; drop1(rl.dam, test = "Chisq")
rl.dam <-
  update(
    rl.dam, . ~ . - compactness_mean) ; drop1(rl.dam, test = "Chisq")
rl.dam <-
  update(rl.dam, . ~ . - concavity_mean) ; drop1(rl.dam, test = "Chisq")
rl.dam <-
  update(rl.dam, . ~ . - fractal_dimension_mean) ; drop1(
    rl.dam, test = "Chisq")
rl.dam <-
  update(rl.dam, . ~ . - symmetry_mean) ; drop1(rl.dam, test = "Chisq")

library(pROC)

plot.roc(
  roc(dam.te$diagnosis, predict(rl.dam, dam.te, type = "response"))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(
  dam.te$diagnosis != ifelse(
    predict(rl.dam, dam.te, type = "response") < .301, "B", "M"))

rl.dase <- glm(diagnosis ~ ., dase.tr, family = binomial)

drop1(rl.dase, test = "Chisq")

rl.dase <-
  update(rl.dase, . ~ . - concavity_se) ; drop1(rl.dase, test = "Chisq")
rl.dase <-
  update(rl.dase, . ~ . - symmetry_se) ; drop1(rl.dase, test = "Chisq")
rl.dase <-
  update(rl.dase, . ~ . - perimeter_se) ; drop1(rl.dase, test = "Chisq")
rl.dase <-
  update(
    rl.dase, . ~ . - smoothness_se) ; drop1(rl.dase, test = "Chisq")
rl.dase <-
  update(
    rl.dase, . ~ . - concave.points_se) ; drop1(rl.dase, test = "Chisq")
rl.dase <-
  update(rl.dase, . ~ . - texture_se) ; drop1(rl.dase, test = "Chisq")

plot.roc(
  roc(
    dase.te$diagnosis, predict(rl.dase, dase.te, type = "response"))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(
  dase.te$diagnosis != ifelse(
    predict(rl.dase, dase.te, type = "response") < .359, "B", "M"))

table(
  dase.te$diagnosis, ifelse(
    predict(rl.dase, dase.te, type = "response") < .359, "B", "M")
  , dnn = list("obs", "pred"))

rl.daw <- glm(diagnosis ~ ., daw.tr, family = binomial)

drop1(rl.daw, test = "Chisq")

rl.daw <-
  update(rl.daw, . ~ . - radius_worst) ; drop1(rl.daw, test = "Chisq")
rl.daw <-
  update(
    rl.daw, . ~ . - fractal_dimension_worst) ; drop1(
      rl.daw, test = "Chisq")
rl.daw <-
  update(
    rl.daw, . ~ . - perimeter_worst) ; drop1(rl.daw, test = "Chisq")
rl.daw <-
  update(
    rl.daw, . ~ . - concavity_worst) ; drop1(rl.daw, test = "Chisq")
rl.daw <-
  update(
    rl.daw, . ~ . - compactness_worst) ; drop1(rl.daw, test = "Chisq")
rl.daw <-
  update(
    rl.daw, . ~ . - symmetry_worst) ; drop1(rl.daw, test = "Chisq")

plot.roc(
  roc(
    daw.te$diagnosis, predict(rl.daw, daw.te, type = "response"))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(
  daw.te$diagnosis != ifelse(
    predict(rl.daw, daw.te, type = "response") < .48, "B", "M"))

table(
  daw.te$diagnosis, ifelse(
    predict(rl.daw, daw.te, type = "response") < .359, "B", "M")
  , dnn = list("obs", "pred"))

legenda <- list(
  space = "top"
  , text = list(c("Benign", "Malignant"))
  , points = list(col = trellis.par.get("superpose.symbol")$col[1:2]
                  , pch = 16, cex = .8)
  , columns = 2)

library(latticeExtra)

print(xyplot(texture_worst ~ area_worst, daw.te
             , pch = 16, groups = diagnosis
             , xlab = "Área", ylab = "Textura", key = legenda)
      , position = c(0, 0, .5, 1), more = TRUE)

print(xyplot(texture_worst ~ area_worst, daw.te
             , pch = 16
             , groups = ifelse(
               predict(rl.daw, daw.te, type = "response") < .48
               , "B", "M")
             , xlab = "Área", ylab = "Textura", key = legenda)
      , position = c(.5, 0, 1, 1))

print(xyplot(texture_worst ~ smoothness_worst, daw.te
             , pch = 16, groups = diagnosis
             , xlab = "Suavidade", ylab = "Textura", key = legenda)
      , position = c(0, 0, .5, 1), more = TRUE)

print(xyplot(texture_worst ~ smoothness_worst, daw.te
             , pch = 16
             , groups = ifelse(
               predict(rl.daw, daw.te, type = "response") < .48
               , "B", "M")
             , xlab = "Suavidade", ylab = "Textura", key = legenda)
      , position = c(.5, 0, 1, 1))

print(xyplot(texture_worst ~ concave.points_worst, daw.te
             , pch = 16, groups = diagnosis
             , xlab = "Pontos côncavos", ylab = "Textura"
             , key = legenda)
      , position = c(0, 0, .5, 1), more = TRUE)

print(xyplot(texture_worst ~ concave.points_worst, daw.te
             , pch = 16
             , groups = ifelse(
               predict(rl.daw, daw.te, type = "response") < .48
               , "B", "M")
             , xlab = "Pontos côncavos", ylab = "Textura"
             , key = legenda)
      , position = c(.5, 0, 1, 1))

print(xyplot(area_worst ~ smoothness_worst, daw.te
             , pch = 16, groups = diagnosis
             , xlab = "Suavidade", ylab = "Área", key = legenda)
      , position = c(0, 0, .5, 1), more = TRUE)

print(xyplot(area_worst ~ smoothness_worst, daw.te
             , pch = 16
             , groups = ifelse(
               predict(rl.daw, daw.te, type = "response") < .48
               , "B", "M")
             , xlab = "Suavidade", ylab = "Área", key = legenda)
      , position = c(.5, 0, 1, 1))

print(xyplot(area_worst ~ concave.points_worst, daw.te
             , pch = 16, groups = diagnosis
             , xlab = "Pontos côncavos", ylab = "Área", key = legenda)
      , position = c(0, 0, .5, 1), more = TRUE)

print(xyplot(area_worst ~ concave.points_worst, daw.te
             , pch = 16
             , groups = ifelse(
               predict(rl.daw, daw.te, type = "response") < .48
               , "B", "M")
             , xlab = "Pontos Côncavos", ylab = "Área", key = legenda)
      , position = c(.5, 0, 1, 1))

print(xyplot(smoothness_worst ~ concave.points_worst, daw.te
             , pch = 16, groups = diagnosis
             , xlab = "Pontos côncavos", ylab = "Suavidade"
             , key = legenda)
      , position = c(0, 0, .5, 1), more = TRUE)

print(xyplot(smoothness_worst ~ concave.points_worst, daw.te
             , pch = 16
             , groups = ifelse(
               predict(rl.daw, daw.te, type = "response") < .48
               , "B", "M")
             , xlab = "Pontos Côncavos", ylab = "Suavidade"
             , key = legenda)
      , position = c(.5, 0, 1, 1))
# ==================================================================== #
# naive bayes ==========================================================
# ==================================================================== #
library(e1071)

nb.dam <- naiveBayes(diagnosis ~ ., dam.tr)

plot.roc(roc(dam.te$diagnosis, as.numeric(predict(nb.dam, dam.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(nb.dam, dam.te))

nb.dase <- naiveBayes(diagnosis ~ ., dase.tr)

plot.roc(roc(dase.te$diagnosis, as.numeric(predict(nb.dase, dase.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(nb.dase, dase.te))

nb.daw <- naiveBayes(diagnosis ~ ., daw.tr)

plot.roc(roc(daw.te$diagnosis, as.numeric(predict(nb.daw, daw.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(nb.daw, daw.te))
# ==================================================================== #
# linear discrimant analysis ===========================================
# ==================================================================== #
library(MASS)

dl.dam <- lda(diagnosis ~ ., dam.tr)

plot.roc(
  roc(dam.te$diagnosis, as.numeric(predict(dl.dam, dam.te)$class))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(dl.dam, dam.te)$class)

dl.dase <- lda(diagnosis ~ ., dase.tr)

plot.roc(
  roc(dase.te$diagnosis, as.numeric(predict(dl.dase, dase.te)$class))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(dl.dase, dase.te)$class)

dl.daw <- lda(diagnosis ~ ., daw.tr)

plot.roc(
  roc(daw.te$diagnosis, as.numeric(predict(dl.daw, daw.te)$class))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(dl.daw, daw.te)$class)
# ==================================================================== #
# quadratic discrimant analysis ========================================
# ==================================================================== #
dq.dam <- qda(diagnosis ~ ., dam.tr)

plot.roc(
  roc(dam.te$diagnosis, as.numeric(predict(dq.dam, dam.te)$class))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(dq.dam, dam.te)$class)

dq.dase <- qda(diagnosis ~ ., dase.tr)

plot.roc(
  roc(dase.te$diagnosis, as.numeric(predict(dq.dase, dase.te)$class))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(dq.dase, dase.te)$class)

dq.daw <- qda(diagnosis ~ ., daw.tr)

plot.roc(
  roc(daw.te$diagnosis, as.numeric(predict(dq.daw, daw.te)$class))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(dq.daw, daw.te)$class)
# ==================================================================== #
# regularized discrimant analysis ======================================
# ==================================================================== #
library(klaR)

dr.dam <- rda(diagnosis ~ ., dam.tr)

plot.roc(
  roc(dam.te$diagnosis, as.numeric(predict(dr.dam, dam.te)$class))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(dr.dam, dam.te)$class)

dr.dase <- rda(diagnosis ~ ., dase.tr)

plot.roc(
  roc(dase.te$diagnosis, as.numeric(predict(dr.dase, dase.te)$class))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(dr.dase, dase.te)$class)

dr.daw <- rda(diagnosis ~ ., daw.tr)

plot.roc(
  roc(daw.te$diagnosis, as.numeric(predict(dr.daw, daw.te)$class))
  , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(dr.daw, daw.te)$class)
# ==================================================================== #
# linear model =========================================================
# ==================================================================== #
lm.dam <- lm(as.numeric(diagnosis) ~ ., dam.tr)

drop1(lm.dam, test = "Chisq")

lm.dam <-
  update(
    lm.dam, . ~ . - compactness_mean) ; drop1(lm.dam, test = "Chisq")
lm.dam <-
  update(lm.dam, . ~ . - fractal_dimension_mean) ; drop1(
    lm.dam, test = "Chisq")
lm.dam <-
  update(lm.dam, . ~ . - concavity_mean) ; drop1(lm.dam, test = "Chisq")
lm.dam <-
  update(
    lm.dam, . ~ . - smoothness_mean) ; drop1(lm.dam, test = "Chisq")

plot.roc(roc(dam.te$diagnosis, predict(lm.dam, dam.te))
         , print.auc = TRUE, print.thres = TRUE, las = 1)
mean(
  dam.te$diagnosis != ifelse(predict(lm.dam, dam.te) < 1.371, "B", "M"))

lm.dase <- lm(as.numeric(diagnosis) ~ ., dase.tr)

drop1(lm.dase, test = "Chisq")

lm.dase <-
  update(lm.dase, . ~ . - area_se) ; drop1(lm.dase, test = "Chisq")
lm.dase <-
  update(lm.dase, . ~ . - texture_se) ; drop1(lm.dase, test = "Chisq")

plot.roc(roc(dase.te$diagnosis, predict(lm.dase, dase.te))
         , print.auc = TRUE, print.thres = TRUE, las = 1)
mean(
  dase.te$diagnosis != ifelse(
    predict(lm.dase, dase.te) < 1.386, "B", "M"))

lm.daw <- lm(as.numeric(diagnosis) ~ ., daw.tr)

drop1(lm.daw, test = "Chisq")

lm.daw <-
  update(
    lm.daw, . ~ . - perimeter_worst) ; drop1(lm.daw, test = "Chisq")
lm.daw <- update(
  lm.daw, . ~ . - fractal_dimension_worst) ; drop1(
    lm.daw, test = "Chisq")

plot.roc(roc(daw.te$diagnosis, predict(lm.daw, daw.te))
         , print.auc = TRUE, print.thres = TRUE, las = 1)
mean(
  daw.te$diagnosis != ifelse(predict(lm.daw, daw.te) < 1.444, "B", "M"))
# ==================================================================== #
# k nearest neighbor regression ========================================
# ==================================================================== #
library(FNN)

risco <- 0
for (i in 1:nrow(dam.te)){
  knn.dam <-
    knn.reg(
      dam.tr[ , -1], dam.te[ , -1], as.numeric(dam.tr[ , 1]), k = i)
  risco[i] <-
    mean(dam.te$diagnosis != ifelse(knn.dam$pred < 1.5, "B", "M"))
}

xyplot(risco ~ 1:nrow(dam.te), type = "l"
       , panel = function(...){
         panel.xyplot(...)
         panel.abline(v = which.min(risco), col = 2)
         panel.text(
           10, .13, labels = paste("k =", which.min(risco)), col = 2)
       })

knn.dam <-
  knn.reg(
    dam.tr[ , -1], dam.te[ , -1], as.numeric(dam.tr[ , 1])
    , k = which.min(risco))

plot.roc(roc(dam.te$diagnosis, knn.dam$pred)
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != ifelse(knn.dam$pred < 1.6, "B", "M"))

risco <- 0
for (i in 1:nrow(dase.te)){
  knn.dase <-
    knn.reg(
      dase.tr[ , -1], dase.te[ , -1], as.numeric(dase.tr[ , 1]), k = i)
  risco[i] <-
    mean(dase.te$diagnosis != ifelse(knn.dase$pred < 1.5, "B", "M"))
}

xyplot(risco ~ 1:nrow(dase.te), type = "l"
       , panel = function(...){
         panel.xyplot(...)
         panel.abline(v = which.min(risco), col = 2)
         panel.text(
           22, .18, labels = paste("k =", which.min(risco)), col = 2)
       })

knn.dase <-
  knn.reg(
    dase.tr[ , -1], dase.te[ , -1], as.numeric(dase.tr[ , 1])
    , k = which.min(risco))

plot.roc(roc(dase.te$diagnosis, knn.dase$pred)
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != ifelse(knn.dase$pred < 1.472, "B", "M"))

risco <- 0
for (i in 1:nrow(daw.te)){
  knn.daw <-
    knn.reg(
      daw.tr[ , -1], daw.te[ , -1], as.numeric(daw.tr[ , 1]), k = i)
  risco[i] <-
    mean(daw.te$diagnosis != ifelse(knn.daw$pred < 1.5, "B", "M"))
}

xyplot(risco ~ 1:nrow(daw.te), type = "l"
       , panel = function(...){
         panel.xyplot(...)
         panel.abline(v = which.min(risco), col = 2)
         panel.text(
           32, .08, labels = paste("k =", which.min(risco)), col = 2)
       })

knn.daw <-
  knn.reg(
    daw.tr[ , -1], daw.te[ , -1], as.numeric(daw.tr[ , 1])
    , k = which.min(risco))

plot.roc(roc(daw.te$diagnosis, knn.daw$pred)
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != ifelse(knn.daw$pred < 1.352, "B", "M"))
# ==================================================================== #
# linear support vector machine ========================================
# ==================================================================== #
svml.dam <- svm(diagnosis ~ ., dam.tr, kernel = "linear")

(svml.dam <- tune(svm, diagnosis ~ ., data = dam.tr, kernel = "linear"
                  , ranges = list(
                    cost = c(
                      .0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                    , gamma = c(.01, .1, .25, .5)
                  )))
svml.dam <-
  svm(diagnosis ~ ., dam.tr, kernel = "linear", cost = .1, gamma = .01)

plot.roc(roc(dam.te$diagnosis, as.numeric(predict(svml.dam, dam.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(svml.dam, dam.te))

svml.dase <- svm(diagnosis ~ ., dase.tr, kernel = "linear")

(svml.dase <- tune(svm, diagnosis ~ ., data = dase.tr, kernel = "linear"
                   , ranges = list(
                     cost = c(
                       .0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                     , gamma = c(.01, .1, .25, .5)
                   )))
svml.dase <-svm(
  diagnosis ~ ., dase.tr, kernel = "linear", cost = 7.5, gamma = .01)

plot.roc(roc(dase.te$diagnosis, as.numeric(predict(svml.dase, dase.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(svml.dase, dase.te))

svml.daw <- svm(diagnosis ~ ., daw.tr, kernel = "linear")

(svml.daw <- tune(svm, diagnosis ~ ., data = daw.tr, kernel = "linear"
                  , ranges = list(
                    cost = c(
                      .0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                    , gamma = c(.01, .1, .25, .5)
                  )))

svml.daw <-
  svm(diagnosis ~ ., daw.tr, kernel = "linear", cost = 1, gamma = .01)

plot.roc(roc(daw.te$diagnosis, as.numeric(predict(svml.daw, daw.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(svml.daw, daw.te))
# ==================================================================== #
# polynomial support vector machine ====================================
# ==================================================================== #
svmp.dam <- svm(diagnosis ~ ., dam.tr, kernel = "polynomial")

(svmp.dam <- tune(
  svm, diagnosis ~ ., data = dam.tr, kernel = "polynomial"
  , ranges = list(cost = c(.0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                  , gamma = c(.01, .1, .25, .5))
))

svmp.dam <- svm(
  diagnosis ~ ., dam.tr, kernel = "polynomial", cost = 5, gamma = .5)

plot.roc(roc(dam.te$diagnosis, as.numeric(predict(svmp.dam, dam.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(svmp.dam, dam.te))

svmp.dase <- svm(diagnosis ~ ., dase.tr, kernel = "polynomial")

(svmp.dase <- tune(
  svm, diagnosis ~ ., data = dase.tr, kernel = "polynomial"
  , ranges = list(cost = c(.0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                  , gamma = c(.01, .1, .25, .5))
))

svmp.dase <- svm(
  diagnosis ~ ., dase.tr, kernel = "polynomial", cost = .5, gamma = .5)

plot.roc(roc(dase.te$diagnosis, as.numeric(predict(svmp.dase, dase.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(svmp.dase, dase.te))

svmp.daw <- svm(diagnosis ~ ., daw.tr, kernel = "polynomial")

(svmp.daw <- tune(
  svm, diagnosis ~ ., data = daw.tr, kernel = "polynomial"
  , ranges = list(cost = c(.0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                  , gamma = c(.01, .1, .25, .5))
))

svmp.daw <- svm(
  diagnosis ~ ., daw.tr, kernel = "polynomial", cost = 2.5, gamma = .5)

plot.roc(roc(daw.te$diagnosis, as.numeric(predict(svmp.daw, daw.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(svmp.daw, daw.te))
# ==================================================================== #
# radial support vector machine ========================================
# ==================================================================== #
svmr.dam <- svm(diagnosis ~ ., dam.tr, kernel = "radial")

(svmr.dam <- tune(
  svm, diagnosis ~ ., data = dam.tr, kernel = "radial"
  , ranges = list(cost = c(.0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                  , gamma = c(.01, .1, .25, .5))
))

svmr.dam <-
  svm(diagnosis ~ ., dam.tr, kernel = "radial", cost = 10, gamma = .01)

plot.roc(roc(dam.te$diagnosis, as.numeric(predict(svmr.dam, dam.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(svmr.dam, dam.te))

svmr.dase <- svm(diagnosis ~ ., dase.tr, kernel = "radial")

(svmr.dase <- tune(
  svm, diagnosis ~ ., data = dase.tr, kernel = "radial"
  , ranges = list(cost = c(.0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                  , gamma = c(.01, .1, .25, .5))
))

svmr.dase <-
  svm(diagnosis ~ ., dase.tr, kernel = "radial", cost = 10, gamma = .01)

plot.roc(roc(dase.te$diagnosis, as.numeric(predict(svmr.dase, dase.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(svmr.dase, dase.te))

svmr.daw <- svm(diagnosis ~ ., daw.tr, kernel = "radial")

(svmr.daw <- tune(
  svm, diagnosis ~ ., data = daw.tr, kernel = "radial"
  , ranges = list(cost = c(.0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                  , gamma = c(.01, .1, .25, .5))
))

svmr.daw <-
  svm(diagnosis ~ ., daw.tr, kernel = "radial", cost = 5, gamma = .01)

plot.roc(roc(daw.te$diagnosis, as.numeric(predict(svmr.daw, daw.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(svmr.daw, daw.te))
# ==================================================================== #
# sigmoid support vector machine =======================================
# ==================================================================== #
svms.dam <- svm(diagnosis ~ ., dam.tr, kernel = "sigmoid")

(svms.dam <- tune(
  svm, diagnosis ~ ., data = dam.tr, kernel = "sigmoid"
  , ranges = list(cost = c(.0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                  , gamma = c(.01, .1, .25, .5))
))

svms.dam <- svm(
  diagnosis ~ ., dam.tr, kernel = "sigmoid", cost = 7.5, gamma = .01)

plot.roc(roc(dam.te$diagnosis, as.numeric(predict(svms.dam, dam.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(svms.dam, dam.te))

svms.dase <- svm(diagnosis ~ ., dase.tr, kernel = "sigmoid")

(svms.dase <- tune(
  svm, diagnosis ~ ., data = dase.tr, kernel = "sigmoid"
  , ranges = list(cost = c(.0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                  , gamma = c(.01, .1, .25, .5))
))

svms.dase <- svm(
  diagnosis ~ ., dase.tr, kernel = "sigmoid", cost = 10, gamma = .01)

plot.roc(roc(dase.te$diagnosis, as.numeric(predict(svms.dase, dase.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(svms.dase, dase.te))

svms.daw <- svm(diagnosis ~ ., daw.tr, kernel = "sigmoid")

(svms.daw <- tune(
  svm, diagnosis ~ ., data = daw.tr, kernel = "sigmoid"
  , ranges = list(cost = c(.0001, .001, .01, .1, .5, 1, 2.5, 5, 7.5, 10)
                  , gamma = c(.01, .1, .25, .5))
))

svms.daw <- svm(
  diagnosis ~ ., daw.tr, kernel = "sigmoid", cost = 5, gamma = .01)

plot.roc(roc(daw.te$diagnosis, as.numeric(predict(svms.daw, daw.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(svms.daw, daw.te))
# ==================================================================== #
# bagging ==============================================================
# ==================================================================== #
library(randomForest)

b.dam <-
  randomForest(x = dam.tr[ , -1], y = dam.tr[ , 1], importance = FALSE)

plot.roc(roc(dam.te$diagnosis, as.numeric(predict(b.dam, dam.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(b.dam, dam.te))

b.dase <- randomForest(
  x = dase.tr[ , -1], y = dase.tr[ , 1], importance = FALSE)

plot.roc(roc(dase.te$diagnosis, as.numeric(predict(b.dase, dase.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(b.dase, dase.te))

b.daw <-
  randomForest(x = daw.tr[ , -1], y = daw.tr[ , 1], importance = FALSE)

plot.roc(roc(daw.te$diagnosis, as.numeric(predict(b.daw, daw.te)))
         , print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(b.daw, daw.te))
# ==================================================================== #
# classification tree ==================================================
# ==================================================================== #
library(tree)

t.dam <- tree(diagnosis ~ ., dam.tr)

t.dam <-
  prune.tree(t.dam, best = cv.tree(t.dam, FUN = prune.tree)$
               size[which.min(cv.tree(t.dam, FUN = prune.tree)$dev)])
plot.roc(
  roc(
    dam.te$diagnosis, as.numeric(
      predict(t.dam, dam.te, type = "class")
    )), print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict(t.dam, dam.te, type = "class"))

t.dase <- tree(diagnosis ~ ., dase.tr)

t.dase <-
  prune.tree(t.dase, best = cv.tree(t.dase, FUN = prune.tree)$
               size[which.min(cv.tree(t.dase, FUN = prune.tree)$dev)])
plot.roc(
  roc(
    dase.te$diagnosis, as.numeric(
      predict(t.dase, dase.te, type = "class")
    )), print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict(t.dase, dase.te, type = "class"))

t.daw <- tree(diagnosis ~ ., daw.tr)

t.daw <-
  prune.tree(t.daw, best = cv.tree(t.daw, FUN = prune.tree)$
               size[which.min(cv.tree(t.daw, FUN = prune.tree)$dev)])
plot.roc(
  roc(
    daw.te$diagnosis, as.numeric(
      predict(t.daw, daw.te, type = "class")
    )), print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict(t.daw, daw.te, type = "class"))
# ==================================================================== #
# boosting =============================================================
# ==================================================================== #
library(bst)
library(adabag)

bo.dam <- boosting(diagnosis ~ ., data = dam.tr)

plot.roc(
  roc(
    dam.te$diagnosis, as.numeric(factor(
      predict.boosting(bo.dam, dam.te)$class
    ))), print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dam.te$diagnosis != predict.boosting(bo.dam, dam.te)$class)

table(dam.te$diagnosis, predict.boosting(bo.dam, dam.te)$class
      , dnn = list("obs", "pred"))

bo.dase <- boosting(diagnosis ~ ., data = dase.tr)

plot.roc(
  roc(
    dase.te$diagnosis, as.numeric(factor(
      predict.boosting(bo.dase, dase.te)$class
    ))), print.auc = TRUE, print.thres = TRUE, las = 1)

mean(dase.te$diagnosis != predict.boosting(bo.dase, dase.te)$class)

bo.daw <- boosting(diagnosis ~ ., data = daw.tr)

plot.roc(
  roc(
    daw.te$diagnosis, as.numeric(factor(
      predict.boosting(bo.daw, daw.te)$class
    ))), print.auc = TRUE, print.thres = TRUE, las = 1)

mean(daw.te$diagnosis != predict.boosting(bo.daw, daw.te)$class)
# ==================================================================== #