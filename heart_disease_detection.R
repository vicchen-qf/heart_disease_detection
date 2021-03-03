## Data Preperation
### Import Data
data_ <- read.csv('./heart.csv')
colnames(data_) <- c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 
                     'thalach', 'exang', 'oldpeak', 'slope', 'ca', 'thal', 'target')

### Data Cleasning
data <- data_ %>%
    filter(
        thal != 0 & ca != 4
    ) %>% 
    mutate(
        sex = case_when(
            sex == 0 ~ '-female',
            sex == 1 ~ '-male'
        ),
        cp = case_when(
            cp == 0 ~ '-asymptomatic',
            cp == 1 ~ '-atypical angina', 
            cp == 2 ~ '-non-anginal pain',
            cp == 3 ~ '-typical angina'
        ),
        fbs = case_when(
            fbs == 0 ~ ' < 120mg/ml',
            fbs == 1 ~ ' > 120mg/ml'
        ),
        restecg = case_when(
            restecg == 0 ~ '-leftventricular hypertrophy', 
            restecg == 1 ~ '-normal',
            restecg == 2 ~ '-ST wave abnormality'
        ),
        exang = case_when(
            exang == 0 ~ '-no',
            exang == 1 ~ '-yes'
        ), 
        slope = case_when(
            slope == 0 ~ '-downsloping',
            slope == 1 ~ '-flat',
            slope == 2 ~ '-upsloping'
        ),
        thal = case_when(
            thal == 1 ~ '-fixed defect', 
            thal == 2 ~ '-normal',
            thal == 3 ~ '-reversable defect'
        ),
        target = case_when(
            target == 0 ~ '> 50% diameter narrowing',
            target == 1 ~ '< 50% diameter narrowing'
        ),
        ca = as.factor(ca)
    ) %>%
    mutate_if(is.character, as.factor) %>% 
    dplyr::select(everything())

colnames(data) <- c('Age', 'Sex', 'ChestPainType',
                    'RestingBloodPressure', 'Cholesterol', 
                    'FastingBloodSugar', 'RestEcg',
                    'MaxHeartRateAchieved', 'ExerciseInducedAngina',
                    'STDepression', 'PeakExerciseST',
                    'NumMajorVessels', 'ThalliumDefect',
                    'target')

data_xgb <- data_ %>%
    filter(
        thal != 0 & ca != 4
    ) %>% 
    mutate(
        
        restecg = case_when(
            restecg == 0 ~ 1, #'-leftventricular hypertrophy', 
            restecg == 1 ~ 0, #'-normal',
            restecg == 2 ~ 2  #'-ST wave abnormality'
        ),
        
        thal = case_when(
            thal == 1 ~ 2, #'-fixed defect', 
            thal == 2 ~ 0, #'-normal',
            thal == 3 ~ 1  #'-reversable defect'
        ),
        target = case_when(
            target == 0 ~ '> 50% diameter narrowing',
            target == 1 ~ '< 50% diameter narrowing'
        )
    ) %>%
    mutate_if(is.character, as.factor) %>% 
    dplyr::select(everything())

colnames(data_xgb) <- c('Age', 'Sex', 'ChestPainType',
                        'RestingBloodPressure', 'Cholesterol', 
                        'FastingBloodSugar', 'RestEcg',
                        'MaxHeartRateAchieved', 'ExerciseInducedAngina',
                        'STDepression', 'PeakExerciseST',
                        'NumMajorVessels', 'ThalliumDefect',
                        'target')

### Train Test Split
set.seed(100)

train <- createDataPartition(data$target, p=.75, list=FALSE)
train_data <- data[train, ]
test_data <- data[-train, ]
train_xgb <- data_xgb[train, ]
test_xgb <- data_xgb[-train, ]

tr_target_ <- train_data$target
te_target_ <- test_data$target

tr_target <- as.numeric(tr_target_) - 1
te_target <- as.numeric(te_target_) - 1

tr_data <- sparse.model.matrix(target ~ .-1, data=train_xgb)
te_data <- sparse.model.matrix(target ~ .-1, data=test_xgb)

dtrain <- xgb.DMatrix(data=tr_data, label=tr_target)
dtest <- xgb.DMatrix(data=te_data, label=te_target)

### Performance Function
performance <- function(pred.prob, pred.class, method, test, positive){
    con <- confusionMatrix(pred.class,test,positive=positive)
    Sensitivity <- con$byClass[1]
    Specificity <- con$byClass[2]
    ROCit_obj <- rocit(score=pred.prob,class=test)
    AUC <- ROCit_obj$AUC
    ACC <- sum(pred.class==test)/length(test)
    
    plot(ROCit_obj);title(method)
    text(0.7,0.4,paste("AUC = ",round(AUC,3),"\n","ACC = ",round(ACC,3)),cex = 1.5)
    return(c(Sensitivity,Specificity,AUC = AUC,ACC=ACC))
}

## EDA
### Numerical
corr <- cor(data[sapply(data, is.numeric)],method = 'pearson')
corr

pairs(data[sapply(data, is.numeric)], lower.panel=NULL)
library(corrplot)
corrplot(corr, method = "ellipse", type="upper")

par(mfrow=c(1,5))
boxplot(formula = Age ~ target, 
        data = data,       
        xlab = "Disease or not",          
        ylab = "Age") 
boxplot(formula = Cholesterol ~ target, 
        data = data,       
        xlab = "Disease or not",          
        ylab = "Cholesterol") 
boxplot(formula = RestingBloodPressure ~ target, 
        data = data,       
        xlab = "Disease or not",          
        ylab = "Resting Blood Pressure") 
boxplot(formula = MaxHeartRateAchieved ~ target, 
        data = data,       
        xlab = "Disease or not",          
        ylab = "Max Heart-Rate Achieved") 
boxplot(formula = STDepression ~ target, 
        data = data,       
        xlab = "Disease or not",          
        ylab = "ST-Depression") 

ggplot(data, aes(x=Age, y=Cholesterol, color=target, shape=target)) + 
    geom_point() +
    geom_smooth(se = FALSE)
ggplot(data, aes(x=Age, y=STDepression, color=target, shape=target)) +
    geom_point() +
    geom_smooth(se = FALSE)

ggplot(data, aes(x=Age, y=MaxHeartRateAchieved, color=target, shape=target)) + 
    geom_point() +
    geom_smooth(se = FALSE)
ggplot(data, aes(x=Age, y=RestingBloodPressure, color=target, shape=target)) + 
    geom_point() +
    geom_smooth(se = FALSE)

ggplot(data, aes(x=Cholesterol, y=STDepression, color=target, shape=target)) + 
    geom_point() +
    geom_smooth(se = FALSE)
ggplot(data, aes(x=Cholesterol, y=MaxHeartRateAchieved, color=target, shape=target)) + 
    geom_point() +
    geom_smooth(se = FALSE)

ggplot(data, aes(x=MaxHeartRateAchieved, y=STDepression, color=target, shape=target)) + 
    geom_point() +
    geom_smooth(se = FALSE)
ggplot(data, aes(x=RestingBloodPressure, y=Cholesterol, color=target, shape=target)) + 
    geom_point() +
    geom_smooth(se = FALSE)

data %>%
    ggplot(aes(x=Age,fill=target))+
    geom_histogram()+
    xlab("Age") + 
    ylab("Number")+
    guides(fill = guide_legend(title = "target"))

data %>%
    ggplot(aes(x=Cholesterol,fill=target))+
    geom_histogram()+
    xlab("Cholesterol") + 
    ylab("Number")+
    guides(fill = guide_legend(title = "target"))

data %>%
    ggplot(aes(x=RestingBloodPressure,fill=target))+
    geom_histogram()+
    xlab("RestingBloodPressure") + 
    ylab("Number")+
    guides(fill = guide_legend(title = "target"))

data %>%
    ggplot(aes(x=MaxHeartRateAchieved,fill=target))+
    geom_histogram()+
    xlab("MaxHeartRateAchieved") + 
    ylab("Number")+
    guides(fill = guide_legend(title = "target"))

data %>%
    ggplot(aes(x=STDepression,fill=target))+
    geom_histogram()+
    xlab("STDepression") + 
    ylab("Number")+
    guides(fill = guide_legend(title = "target"))

data_disease <- data[data$target=='> 50% diameter narrowing',]
data_disease %>%
    ggplot(aes(x=Age,fill=Sex))+
    geom_histogram()+
    xlab("Age") + 
    ylab("numbers of people with disease")+
    guides(fill = guide_legend(title = "Gender"))

data_disease %>%
    ggplot(aes(x=Cholesterol,fill=Sex))+
    geom_histogram()+
    xlab("Cholesterol") + 
    ylab("numbers of people with disease")+
    guides(fill = guide_legend(title = "Gender"))

data_disease %>%
    ggplot(aes(x=RestingBloodPressure,fill=Sex))+
    geom_histogram()+
    xlab("RestingBloodPressure") + 
    ylab("numbers of people with disease")+
    guides(fill = guide_legend(title = "Gender"))

data_disease %>%
    ggplot(aes(x=STDepression,fill=Sex))+
    geom_histogram()+
    xlab("STDepression") + 
    ylab("numbers of people with disease")+
    guides(fill = guide_legend(title = "Gender"))

### PCA
pca <- prcomp(data[sapply(data, is.numeric)], scale = TRUE)
pca$rotation

plot(pca, type = "line", main = "Scree Plot")
abline(h = 1, col = "red")

vars <- (pca$sdev)^2
props <- vars / sum(vars)
cumulative_props <- cumsum(props)
cumulative_props
fviz_screeplot(pca)

col <- c('#99CC00', '#CCFF00', '#99FF00', '#66FF00', '#33FF00')
x <- c('PC1', 'PC2', 'PC3', 'PC4', 'PC5')
bar <- barplot(cumulative_props, names.arg = x, xlab = 'PC', 
               y_lab = 'variance explained', col = col, ylim = c(0, 1))
text(x = bar, y = round(cumulative_props, digit = 4), 
     label = round(cumulative_props, digit = 4), pos = 3, cex = 1.3, col = "red")

top2_pca.data <- pca$x[, 1:2]
head(top2_pca.data)
cat('this data set dim:', dim(top2_pca.data))

pc1 <- pca$rotation[, 1]   
pc2 <- pca$rotation[, 2] 
par(mfrow=c(1,2))
pc1[order(pc1, decreasing = FALSE)]
dotchart(pc1[order(pc1, decreasing=FALSE)] ,   
         main="Loading Plot for PC1",                      
         xlab="Variable Loadings",                         
         col="red")

pc2[order(pc2, decreasing = FALSE)]
dotchart(pc2[order(pc2, decreasing=FALSE)] ,   
         main="Loading Plot for PC2",                      
         xlab="Variable Loadings",                         
         col="red")

fviz_pca(pca, habillage = data$target, addEllipses=TRUE, ellipse.level=0.95) + 
    labs(title = "PCA", x = "PC1", y = "PC2")

### Categorical
bar_plot <- function(y, x){
    g1 <- ggplot(data, 
                 aes(x=factor(eval(parse(text=x))),
                     fill=factor(eval(parse(text=y))))) +
        geom_bar(stat='count') +
        scale_fill_brewer(palette="YlGnBu") +
        theme_hc() +
        labs(y=y,
             fill=y,
             x=x)
    
    g2 <- ggplot(data, 
                 aes(x=factor(eval(parse(text=x))),
                     fill=factor(eval(parse(text=y))))) +
        geom_bar(stat='count', position='fill') +
        scale_y_continuous(breaks=seq(0, 1, .2),
                           label=percent) +
        scale_fill_brewer(palette="YlGnBu") +
        theme_hc() +
        labs(y=y,
             fill=y,
             x=x)
    
    figure <- ggarrange(g1, g2, ncol=2, 
                        common.legend = TRUE, legend = "bottom")
    figure
}

bar_plot('target', 'Sex')
bar_plot('target', 'ChestPainType')
bar_plot('target', 'FastingBloodSugar')
bar_plot('target', 'RestEcg')
bar_plot('target', 'ExerciseInducedAngina')
bar_plot('target', 'PeakExerciseST')
bar_plot('target', 'NumMajorVessels')
bar_plot('target', 'ThalliumDefect')

log.odds <- function(covariate, response){
    tab <- matrix(xtabs(~ covariate + response), ncol=2)
    odr <- summary(oddsratio(tab, log=TRUE))
    est <- odr[, 1]
    sde <- odr[, 2]
    dci <- est - 1.96 * sde
    uci <- est + 1.96 * sde
    
    return(list(tab=tab, est=est, dci=dci, uci=uci))
}

od_sex <- log.odds(data$Sex, data$target)
od_cp <- log.odds(data$ChestPainType, data$target) #3
od_fbs <- log.odds(data$FastingBloodSugar, data$target)
od_rect <- log.odds(data$RestEcg, data$target) # 2
od_exang <- log.odds(data$ExerciseInducedAngina, data$target)
od_slope <- log.odds(data$PeakExerciseST, data$target)# 2
od_ca <- log.odds(data$NumMajorVessels, data$target) # 3
od_thal <- log.odds(data$ThalliumDefect, data$target) # 2

sex_df <- data.frame(covariate=c('sex\nfemale\nv.s.\nmale'),
                     est=c(od_sex$est), dci=c(od_sex$dci), uci=c(od_sex$uci))
cp_df <- data.frame(covariate=c('cp\nasymptomatic\nv.s.\natypical\nangina',
                                'cp\natypical\nangina\nv.s.\nnon-anginal\npain',
                                'cp\nnon-anginal\npain\nv.s.\ntypical\nangina'),
                    est=c(od_cp$est), dci=c(od_cp$dci), uci=c(od_cp$uci))
fbs_df <- data.frame(covariate=c('fbs\n< 120mg/ml \nv.s.\n > 120mg/ml'),
                     est=c(od_fbs$est), dci=c(od_fbs$dci), uci=c(od_fbs$uci))
rect_df <- data.frame(covariate=c('restecg\nleft\nventricular\nhypertrophy\nv.s.\nnormal',
                                  'restecg\nnormal\nv.s.\nST-T wave\nabnormality'),
                      est=c(od_rect$est), dci=c(od_rect$dci), uci=c(od_rect$uci))
exang_df <- data.frame(covariate=c('exang\nno v.s. yes'),
                       est=c(od_exang$est), dci=c(od_exang$dci), uci=c(od_exang$uci))
slope_df <- data.frame(covariate=c('slope\ndownsloping\nv.s.\nflat',
                                   'slope\nflat\nv.s.\nupsloping'),
                       est=c(od_slope$est), dci=c(od_slope$dci), uci=c(od_slope$uci))
ca_df <- data.frame(covariate=c('ca\n0 v.s. 1', 'ca\n1 v.s. 2', 'ca\n2 v.s. 3'),
                    est=c(od_ca$est), dci=c(od_ca$dci), uci=c(od_ca$uci))
thal_df <- data.frame(covariate=c('thal\nfixed defect\nv.s.\nnormal',
                                  'thal\nnormal\nv.s.\nreversable\ndefect'),
                      est=c(od_thal$est), dci=c(od_thal$dci), uci=c(od_thal$uci))

od <- rbind(sex_df, cp_df, fbs_df, rect_df, exang_df, slope_df, ca_df, thal_df)

od_plot <- function(df){
    ggplot(df, aes(x=covariate, y=est, group=1))+
        geom_hline(yintercept=0, color='red')+
        geom_errorbar(width=0.1, aes(ymin=dci, ymax=uci), lwd=1)+
        geom_point(shape=21, size=2, fill='white')+
        #ylim(c(-2.5,6.5))+
        #labs(title ="Confidence Interval for log odds ratio",y = "95% Confidence Interval") +
        theme(plot.title=element_text(hjust=0.5)) +
        geom_text(aes(x=covariate,y=uci, label=round(uci, 3)), nudge_y=0.3, cex=4) +
        geom_text(aes(x=covariate,y=dci, label=round(dci, 3)), nudge_y=-0.3, cex=4) +
        geom_label_repel(aes(x=covariate, y=est, label=round(est, 3)), nudge_x=0.3, cex=3.5)
}
od_plot(od)

## Naive Bayes
### Model Training
fit_nb <- naiveBayes(target~., data = train_data)
pred_nb <- predict(fit_nb, newdata = test_data, type = 'class')
cat('accuracy:', mean(pred_nb == test_data$target))

nb_model <- naiveBayes(target~.-FastingBloodSugar -RestingBloodPressure,
                       data=train_data,prob=TRUE)
nb_prob <- predict(nb_model, newdata=test_data, type='raw')[,2]
nb_pred <- ifelse(nb_prob > 0.5, '> 50% diameter narrowing',
                  '< 50% diameter narrowing')
cat('accuracy:', mean(nb_pred == test_data$target))

### Performance Summary
confusionMatrix(factor(nb_pred), factor(te_target_))
nb_perform <- performance(nb_prob, as.factor(nb_pred),
                          'Naive Bayes', as.factor(te_target_),
                          '> 50% diameter narrowing')

## KNN
### Optimal K
train_X <- train_data[, -14]
test_X <- test_data[, -14]
k_value <- data.frame(k=seq(1, 20), 
                      ACC=0)
for(i in 1:20){
    knn_pred <- knn(cl=tr_target, train=tr_data, test=te_data,
                    k=i, prob=TRUE)
    k_value$ACC[i] <- mean(knn_pred==te_target)
}

k_best <- k_value$k[which.max(k_value$ACC)]

knn_pred <- knn(cl=tr_target, train=tr_data, test=te_data,
                k=k_best, prob=TRUE)
knn_prob <- attr(knn_pred, 'prob')
knn_pred <- ifelse(knn_pred == 1, '> 50% diameter narrowing',
                   '< 50% diameter narrowing')

### Performance Summary
confusionMatrix(factor(knn_pred), factor(te_target_))
performance(knn_prob, as.factor(knn_pred),
            paste('KNN k=', k_best), as.factor(te_target_), 
            '> 50% diameter narrowing')

## Logistic
### Model Training
log_model <- glm(target ~., data=train_data, family=binomial)
log_prob <- predict(log_model, test_data, type="response")
log_pred <- ifelse(log_prob > 0.5, '> 50% diameter narrowing',
                   '< 50% diameter narrowing')
mean(log_pred == test_data$target)

### Cutpoint
cutpoints <- data.frame(cut=seq(0.1, 0.9, by = 0.01),
                        ACC=0)
for(i in 1:nrow(cutpoints)){
    pred_log <- ifelse(log_prob > cutpoints$cut[i], '> 50% diameter narrowing',
                       '< 50% diameter narrowing')
    cutpoints$ACC[i] <- mean(pred_log==test_data$target)
}

cut_best <- cutpoints$cut[which.max(cutpoints$ACC)]
log_pred <- ifelse(log_prob > cut_best, '> 50% diameter narrowing',
                   '< 50% diameter narrowing')

### Performance Summary
coef <- coef(log_model)
CI <- data.frame(confint(log_model))%>% 
    mutate(name=rownames(.),beta_hat=coef)
colnames(CI) <- c("L","U","name","beta_hat")

ggplot(CI[-1,], aes(x = name, y = beta_hat, group = 1)) +
    geom_hline(yintercept = 0, col='red') + 
    geom_errorbar(width = 0.1, aes(ymin = L, ymax = U), lwd = 1) +
    geom_point(shape=21, size=3, fill="white")+
    theme(plot.title = element_text(hjust=0.5,size=15),
          axis.title = element_text(size=13),
          axis.text = element_text(size=13))+
    labs(title = TeX("Confidence Interval for $\\beta $"),y = "95% Confidence Interval") +
    coord_flip()

confusionMatrix(factor(log_pred), factor(te_target_))
performance(log_prob, as.factor(log_pred),
            'Logistic', as.factor(te_target_), 
            '> 50% diameter narrowing')

## Group Lasso
### Data Preparation
train_X <- model.matrix(~., train_data[, -14])[, -1]
train_X_std <- apply(train_X, 2, function(x){(x-min(x))/(range(x)[2]-range(x)[1])})
train_Y <- ifelse(tr_target==0,-1,1)
test_X <- model.matrix(~., test_data[, -14])[, -1]
test_X_std <- NULL
test_Y <- te_target_

for (i in 1:dim(test_X)[2]){
    test_X_std <- cbind(test_X_std, (test_X[, i] - min(train_X[, i]))/(range(train_X[ ,i])[2]- range(train_X[ ,i])[1]))
}

### Grouping & Training
group <- c(1:2, rep(3, 3), 4, 5, 6, rep(7, 2), 8, 9, 10, rep(11,2), rep(12, 3), rep(13, 2))
path <- gglasso(x=train_X_std, y=train_Y,
                group=group,
                lambda=exp(seq(-12, 0, by=0.2)),
                loss='logit')
apply(path$beta, 2, function(x){rownames(path$beta)[which(x == 0)]})
path.d <- data.frame(lambda=path$lambda,t(path$beta))

### CV
glcv <- cv.gglasso(x = train_X_std, y = train_Y, group = group,
                   lambda = exp(seq(-12, 0, by = 0.2)),
                   lambda.factor = 0.5,
                   loss = "logit", 
                   pred.loss = "misclass")
plot(glcv)

### Best Lambda
beta <- coef(glcv, s = "lambda.min")
beta.name <- rownames(beta)[-1][order(abs(beta[-1]),decreasing = T)]
est <- data.frame(name = rownames(beta)[-1],beta = abs(beta[-1]))

### Prediction
gl_prob <- predict(glcv, newx=test_X_std, s=glcv$lambda.min, type = "link")
gl_prob <- 1/(1 + exp(-gl_prob))
gl_pred <- ifelse(gl_prob > 0.5, '> 50% diameter narrowing',
                  '< 50% diameter narrowing')

### Performance Summary
ggplot(data=est)+
    geom_bar(aes(reorder(name, beta), beta), stat = "identity", position = "identity") +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = "Group Lasso",y = "beta",x = "variable")
performance(c(gl_prob), as.factor(gl_pred),
            'XGBoost', as.factor(te_target_), 
            '> 50% diameter narrowing')

## Random Forest
### Model Training
rf <- randomForest(target ~., data=train_data)
rf_pred <- predict(rf, newdata=test_data, type = 'class')
cat('accuracy:', mean(rf_pred == test_data$target))

### Grid Search CV
ntree <- which.min(rf$err.rate[, 1])
cat('best tree size:', ntree)

hyper_grid <- expand.grid(mtry = seq(2, 12, by = 2),
                          node_size = seq(3, 9, by = 2),
                          sample_size = c(0.55, 0.632, 0.7, 0.8),
                          OOB_error = 0)
for (i in 1:nrow(hyper_grid)) {
    # train model
    model <- ranger(formula = target ~ ., data = train_data, 
                    num.trees = ntree, 
                    mtry = hyper_grid$mtry[i],
                    min.node.size = hyper_grid$node_size[i], 
                    sample.fraction = hyper_grid$sample_size[i])
    hyper_grid$OOB_error[i] <- model$prediction.error
}
min_OOB_error <- hyper_grid %>% 
    dplyr::arrange(OOB_error) %>% 
    head(10)

ACC_rf <- data.frame(mtry=rep(0, 10),
                     node_size=rep(0, 10),
                     sample_size=rep(0, 10),
                     OOB_error=rep(0, 10),
                     ACC=rep(0, 10))
for (i in 1:10){
    rf_param <- min_OOB_error[i,]
    
    rf_ <- randomForest(formula=target ~., data=train_data,
                        ntree=ntree, 
                        mtry=rf_param$mtry,
                        nodesize=rf_param$node_size,
                        sampsize=ceiling(rf_param$sample_size * nrow(train_data)))
    
    rf_pred <- predict(rf_, newdata=test_data, type='class')
    acc <- mean(rf_pred==test_data$target)
    ACC_rf[i, ] <- cbind(min_OOB_error[i,], ACC=acc)
}

best_rf_param <- ACC_rf %>%
    dplyr::arrange(desc(ACC)) %>%
    head(1)

### Model After Tuning
rf_best <- randomForest(formula=target ~., data=train_data,
                        ntree=ntree, 
                        mtry=best_rf_param$mtry,
                        nodesize=best_rf_param$node_size,
                        sampsize=ceiling(best_rf_param$sample_size * nrow(train_data)))

rf_prob <- predict(rf_best, test_data, type='prob')[,2]
rf_pred <- ifelse(rf_prob > 0.5, '> 50% diameter narrowing',
                  '< 50% diameter narrowing')

### Performance Summary
confusionMatrix(factor(rf_pred), factor(te_target_))
performance(rf_prob, as.factor(rf_pred),
            'Random Forest', as.factor(te_target_),
            '> 50% diameter narrowing')
imp <- randomForest::importance(rf_best) 
randomForest::varImpPlot(rf_best,main="variable importance plot") 

## XGBOOST
### Parameters (After Grid Search)
params <- list(booster='gbtree',
               objective='binary:logistic',
               eval_metric='auc', # aucpr, ndcg, cox-nloglik
               eta=0.1,           #learning rate
               max_depth=12,       #tree depth
               min_child_weight=1.,
               #max_delta_step=0,
               subsample=1,       #percentage data use in every iteration
               colsample_bytree=0.6,  #percentage covariate use in every iteration
               lambda=0.7, 
               alpha=1
)

### Model Training
xgb_model <- xgb.train(params=params,
                       data=dtrain,
                       nrounds=200,
                       print_every_n=10,
                       watchlist=list(val=dtest, train=dtrain),
                       early_stopping_rounds=10,
                       maximize=T)

xgb_prob <- predict(xgb_model, dtest)
xgb_pred <- ifelse(xgb_prob > 0.5, '> 50% diameter narrowing',
                   '< 50% diameter narrowing')

### Performance Summary
confusionMatrix(factor(xgb_pred), factor(te_target_))
imp <- xgb.importance(feature_names=colnames(tr_data),
                      model=xgb_model)
xgb.ggplot.importance(importance_matrix=imp,
                      measure='Gain')
performance(xgb_prob, as.factor(xgb_pred),
            'XGBoost', as.factor(te_target_), 
            '> 50% diameter narrowing')

xgb.plot.tree(feature_names=colnames(tr_data),
              model=xgb_model,
              trees=110) # best iteration

### Grid Search
searchGridSubCol <- expand.grid(eta=seq(0.02, 0.1, by=0.02),
                                max_depth=seq(10, 15, by=1),
                                subsample=seq(0.6, 1.0, by=0.1),
                                colsample_bytree=seq(0.6, 1.0, by=0.1),
                                lambda=seq(0.6, 1.0, by=0.1),
                                alpha=seq(0.6, 1.0, by=0.1))
ntrees <- 200

system.time(AUCHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    #Extract Parameters to test
    currentEta <- parameterList[["eta"]]
    currentDepth <- parameterList[["max_depth"]]
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentLambda <- parameterList[["lambda"]]
    currentAlpha <- parameterList[["alpha"]]
    
    xgboostModelCV <- xgb.cv(data=dtrain, nrounds=ntrees, nfold=5, showsd=TRUE, 
                             metrics = "auc", booster = "gbtree",
                             "eval_metric"="auc", "objective"="binary:logistic",
                             "eta"=currentEta, "max_depth"=currentDepth,                                
                             "subsample"=currentSubsampleRate, "colsample_bytree"=currentColsampleRate,
                             "lambda"=currentLambda, "alpha"=currentAlpha,
                             print_every_n=10, 
                             early_stopping_rounds=10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    #auc <- tail(xvalidationScores$test_auc_mean, 2)
    auc <- xvalidationScores$test_auc_mean[xgboostModelCV$best_ntreelimit]
    #xgbcv$evaluation_log$test_auc_mean[xgbcv$best_ntreelimit]
    output <- return(c(auc, currentEta, currentDepth, currentSubsampleRate, 
                       currentColsampleRate, currentLambda, currentAlpha
    ))}))

output <- as.data.frame(t(AUCHyperparameters))
varnames <- c('TestAUC', 'eta', 'Depth', 'SubSampleRate', 
              'ColSampRate', 'Lambda', 'Alpha')
names(output) <- varnames
output[which.max(output$TestAUC), ][, -1]

### SHAP Value Plot
shap.score <- function(model, tr_dmat, shap_approx=FALSE){
    
    shap_contrib <- predict(model, tr_dmat,
                            predcontrib=TRUE, 
                            approxcontrib=shap_approx)
    shap_contrib <- as.data.table(shap_contrib)
    shap_contrib[, BIAS:=NULL]
    mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing=T)]
    
    return(list(shap_score=shap_contrib,
                mean_shap_score=mean_shap_score))
}
std.val <- function(x){
    return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
}
shap.long <- function(shap, tr_mat, top_n){
    if (missing(top_n)) top_n <- dim(tr_mat)[2]
    
    shap_score_sub <- as.data.table(shap$shap_score)
    shap_score_sub <- shap_score_sub[, names(shap$mean_shap_score)[1:top_n], with = F]
    shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
    
    fv_sub <- as.data.table(as.matrix(tr_mat))[, names(shap$mean_shap_score)[1:top_n], with = F]
    fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
    fv_sub_long[, stdfvalue := std.val(value), by = "variable"]
    
    names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue" )
    shap_long <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
    shap_long[, mean_value := mean(abs(value)), by = variable]
    setkey(shap_long, variable)
    return(shap_long) 
}

shap_values <- shap.score(model=xgb_model,
                          tr_dmat=tr_data)
shap_long <- shap.long(shap=shap_values,
                       tr_mat=as.matrix(tr_data),
                       top_n=13)
shap.plot.summary(shap_long, x_bound  = 1.7)

g1 <- shap.plot.dependence(data_long = shap_long,
                           x = 'ChestPainType', y = 'ChestPainType',
                           color_feature = 'ExerciseInducedAngina') +
    ggtitle("SHAP values of Chest Pain Type vs. Exercise-Induced Angina")

g1





