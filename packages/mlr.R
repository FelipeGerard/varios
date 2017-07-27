
library(tidyverse)
library(stringr)
library(mlr)

dat <- iris %>%
  set_names(names(iris) %>% tolower() %>% str_replace_all('\\.', '_'))

lrn <- makeLearner(cl = 'classif.rpart',
                   predict.type = 'prob')
# %>% 
#   makeMulticlassWrapper('onevsrest')


tsk <- makeClassifTask('iris',
                       data = dat,
                       target = 'species')

tune_cv <- makeResampleDesc('CV', iters = 5L)

tune_grid <- makeParamSet(
  makeIntegerParam('minsplit', lower = 1, upper = tsk$task.desc$size),
  makeIntegerParam('maxdepth', lower = 1, upper = 10),
  makeNumericParam('cp', lower = -10, upper = 0, trafo = exp)
)
tune_ctrl <- makeTuneControlRandom(maxit = 50)

tune_res <- tuneParams(learner = lrn,
                       task = tsk,
                       resampling = tune_cv,
                       par.set = tune_grid,
                       control = tune_ctrl)
tune_res
tune_res$x


lrn_opt <- setHyperPars(lrn, par.vals = tune_res$x)


cv <- crossval(learner = lrn_opt,
               task = tsk,
               iters = 5,
               show.info = T)

mod_best <- train(lrn_opt, tsk)

pred <- predict(mod_best, tsk)
calculateConfusionMatrix(pred)

generateHyperParsEffectData(tune_res, trafo = T, partial.dep = T)$data %>% 
  arrange(mmce.test.mean)




