getwd()
proj=read.csv("C:/Statistics/Final Dataset/kick.csv",header=TRUE)
smp_size <- floor(0.70 * nrow(proj))
set.seed(123)
train_ind <- sample(seq_len(nrow(proj)), size = smp_size)

train <- proj[train_ind, ]
test <- proj[-train_ind, ]
dim(train)
dim(test)
fix(train)
summary(proj)
pairs(proj)
contrasts(proj$state)
set.seed(21)
glm.fits <- glm(state~main_category+goal+backers+Season,data=proj,family=binomial,control=list(maxit=100),subset=unlist(train))
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
glm.probs <-predict(glm.fits,test,type='response')
glm.pred <- glm.probs
glm.pred[glm.probs>.5] <- "successful"
glm.pred[glm.probs<=.5]<- "failed"
table(glm.pred,test$state)

