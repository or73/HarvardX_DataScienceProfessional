# -----------------------------------------------------------------------
# ---------------------------------------------- CLEAN WORKSPACE
# -----------------------------------------------------------------------
rm(list = ls())
gc()

# -----------------------------------------------------------------------
# ---------------------------------------------- SETTING WORK DIRECTORY
# -----------------------------------------------------------------------
setwd('~/capstoneProject2')

# -----------------------------------------------------------------------
# ---------------------------------------------- LOAD FILES
# -----------------------------------------------------------------------
source('./functions.R')

# -----------------------------------------------------------------------
# ---------------------------------------------- LOAD PACKAGES/LIBRARIES
# -----------------------------------------------------------------------
load.packages.list()

# -----------------------------------------------------------------------
# ---------------------------------------------- LOAD DATASETS
# -----------------------------------------------------------------------
# Validate if files have been loaded previously
cat('Loading  Data Set...\n')
# Loading Dataset
zip.fileName <- 'heart-disease-uci-data.zip'
cat('Validating if file exists...\n')
if (!file.exists('data', 'heart.csv') & file.exists(zip.fileName)) {
    cat('unzipping file...\n')
    unzip(zip.fileName,
          list = TRUE)
} else {cat('File already exist...\n')}


hd.set <- read.csv(file.path('data', 'heart.csv'),
                   header = TRUE)
hd.set.numeric <- read.csv(file.path('data', 'heart.csv'),
                           header = TRUE)
names(hd.set)

# -----------------------------------------------------------------------
# ---------------------------------------------- DATA ANALYSIS
# -----------------------------------------------------------------------
cat('Validating if any value is empty...\n')
# Check if there are any blank values in the dataset
sapply(hd.set,
       function(x) sum(is.na(x))) # No NA values
cat('Validating if any value is NA, NULL, NaN...\n')
all(is.empty(hd.set))

# Summary data
summary(hd.set)

# Data Set structure
str(hd.set)

rm(new.pkg, packages.list, zip.fileName)

# ------------------------------------ COL NAMES
# Numerical Variables
    # Discrete
    # Continuos
        # age     -> age in years
        # testbps -> trestbpsresting blood pressure (in mm Hg on admission to the hospital)
        # chol    -> cholserum cholestoral in mg/dl
        # thalach -> thalachmaximum heart rate achieved
        # oldpeak -> oldpeakST depression induced by exercise relative to rest
# Categorical Variables
    # Nominal
        # sex     -> sex(1 = male; 0 = female)
        # fbs     -> fbs(fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
        # exang   -> exangexercise induced angina (1 = yes; 0 = no)
        # target  -> target1 or 0
    # Ordinal
        # ca      -> canumber of major vessels (0-3) colored by flourosopy
        # cp      -> cpchest pain type (0 - 3)
        # restecg -> restecgresting electrocardiographic results
        # slope   -> slopethe slope of the peak exercise ST segment
        # thal    -> thal3 = normal; 6 = fixed defect; 7 = reversable defect

# -----------------------------------------------------------------------
# ---------------------------------------------- DATA ANALYSIS
# -----------------------------------------------------------------------

hd.set <- data.frame(lapply(hd.set, function(x) factor(x)))

# Types of Attributes - All types are integer, except 'oldpeak'
sapply(hd.set, class)

# Dimensions of Dataset - Amount of instances (271) and attributes (14)
dim(hd.set)

# Peek at the Data - First 10 data values
head(hd.set, 10)

# Statistical Summary - Summary of each attribute
summary(hd.set)

# ----------------------------------- TARGET VARIABLE PLOT
target.distribution <- as.data.frame(round(prop.table(table(hd.set$target)),
                                           digits = 2))
target.distribution.freq <- lapply(target.distribution$Freq, function(x) paste(as.character(x*100), '%', sep = ''))
target.distribution$Freq[1] = target.distribution.freq[[1]]
target.distribution$Freq[2] = target.distribution.freq[[2]]
target.distribution

# Table wit Target Variable Distribution
table.basic(target.distribution, 'Target Variable Distribution', 'small')

# Graph of Target Variable Distribution
graph.geom.bar(hd.set, 'Distribution of target Variable', 'target', 'count', target.distribution)

# Exploring the variable's correlation
hd.set.correlation <- cor(hd.set.numeric %>% select(-target))

corrplot::corrplot(hd.set.correlation,
                   order = 'hclust',
                   tl.cex = 0.8,
                   addrect = 8)
# Data Transformation
hd.set2 <- hd.set %>% select(-findCorrelation(hd.set.correlation,
                                              cutoff = 0.9))
ncol(hd.set2)

# Data Pre-Processing
# Principle Component Analysis (PCA)
hd.set.pre.processing <- prcomp(hd.set.numeric %>% select(-target),
                                scale = TRUE,
                                center = TRUE)
summary(hd.set.pre.processing)

# Compute the proportion of variance explained
hd.set.variance <- hd.set.pre.processing$sdev^2
hd.set.proportion.variance.explained <- hd.set.variance / sum(hd.set.variance)
hd.set.cummulative <- cumsum(hd.set.proportion.variance.explained) # Cummulative percent explained
table.proportion.variance.explained <- tibble(comp = seq(1:ncol(hd.set.numeric %>% select(-target))),
                                              hd.set.proportion.variance.explained,
                                              hd.set.cummulative)
ggplot(table.proportion.variance.explained,
       aes(x = comp, y = hd.set.cummulative)) +
    geom_point() +
    geom_abline(intercept = 0.95,
                color = 'red',
                slope = 0)


# PCA Applied to the Transformed Dataset
hd.set.pre.processing2 <- prcomp(hd.set2,
                                 scale = TRUE,
                                 center = TRUE)
summary(hd.set.pre.processing2)

# Compute the proportion of variance explained
hd.set.variance2 <- hd.set.pre.processing2$sdev^2
hd.set.proportion.variance.explained2 <- hd.set.variance2 / sum(hd.set.variance2)
hd.set.cummulative2 <- cumsum(hd.set.proportion.variance.explained2) # Cummulative percent explained
table.proportion.variance.explained2 <- tibble(comp = seq(1:ncol(hd.set2)),
                                               hd.set.proportion.variance.explained2,
                                               hd.set.cummulative2)
ggplot(table.proportion.variance.explained2,
       aes(x = comp, y = hd.set.cummulative2)) +
    geom_point() +
    geom_abline(intercept = 0.95,
                color = 'red',
                slope = 0)

# Linear Discriminant Analysis (LDA)
hd.set.pre.processing.lda <- MASS::lda(target ~ .,
                                       data = hd.set.numeric,
                                       center = TRUE,
                                       scale = TRUE)
hd.set.pre.processing.lda

# Dataframe of the LDA for visualization
hd.set.predict.lda <- predict(hd.set.pre.processing.lda,
                              hd.set.numeric)$x %>%
    as_tibble() %>%
    cbind(target = hd.set.numeric$target)


# Data Analysis - Between independent variables and 'target'
# ------------------------------------- Target group
group.target <- hd.set %>% group_by(target)

# Target Vs Sex
target.sex <- group.target %>% dplyr::count(sex)
graph.target.geom.bar(target.sex, 'sex', 'Target Vs Sex', 'Split Variables with Target', 'Sex', 'Count', 'categorical', NULL, NULL)

# ------------------------------------- Target Vs fbs
target.fbs <- group.target %>% dplyr::count(fbs)
graph.target.geom.bar(target.fbs, 'fbs', 'Target Vs fbs', 'Split Variables with Target', 'fbs', 'Count', 'categorical', NULL, NULL)

# ------------------------------------- Target Vs exang
target.exang <- group.target %>% dplyr::count(exang)
graph.target.geom.bar(target.exang, 'exang', 'Target Vs exang', 'Split Variables with Target', 'exang', 'Count', 'categorical', NULL, NULL)

# ------------------------------------- Target Vs slope
target.slope <- group.target %>% dplyr::count(slope)
graph.target.geom.bar(target.slope, 'slope', 'Target Vs slope', 'Split Variables with Target', 'slope', 'Count', 'categorical', NULL, NULL)

# ------------------------------------- Target Vs ca
target.ca <- group.target %>% dplyr::count(ca)
graph.target.geom.bar(target.ca, 'ca', 'Target Vs ca', 'Split Variables with Target', 'ca', 'Count', 'categorical', NULL, NULL)

# ------------------------------------- Target Vs cp
target.cp <- group.target %>% dplyr::count(cp)
graph.target.geom.bar(target.cp, 'cp', 'Target Vs cp', 'Split Variables with Target', 'cp', 'Count', 'categorical', NULL, NULL)

# ------------------------------------- Target Vs restecg
target.restecg <- group.target %>% dplyr::count(restecg)
graph.target.geom.bar(target.restecg, 'restecg', 'Target Vs restecg', 'Split Variables with Target', 'restecg', 'Count', 'categorical', NULL, NULL)

# ------------------------------------- Target Vs thal
target.thal <- group.target %>% dplyr::count(thal)
graph.target.geom.bar(target.thal, 'thal', 'Target Vs thal', 'Split Variables with Target', 'thal', 'Count', 'categorical', NULL, NULL)

# ------------------------------------- Target Vs age
target.age <- group.target %>% dplyr::count(age)
target.age$numeric <- as.numeric(as.character(target.age$age))
graph.target.geom.bar(target.age, 'age', 'Target Vs age', 'Split Variables with Target', 'age', 'Count', 'continuous', c(35, 80), c(0, 20))

# ------------------------------------- Target Vs chol
target.chol <- group.target %>% dplyr::count(chol)
target.chol$numeric <- as.numeric(as.character(target.chol$chol))
graph.target.geom.bar(target.chol, 'chol', 'Target Vs chol', 'Split Variables with Target', 'chol', 'Count', 'continuous', c(125, 410), c(0, 6))

# ------------------------------------- Target Vs oldpeak
target.oldpeak <- group.target %>% dplyr::count(oldpeak)
target.oldpeak$numeric <- as.numeric(as.character(target.oldpeak$oldpeak))
graph.target.geom.bar(target.oldpeak, 'oldpeak', 'Target Vs oldpeak', 'Split Variables with Target', 'oldpeak', 'Count', 'continuous', c(0, 6), c(0, 17))

# ------------------------------------- Target Vs trestbps
target.trestbps <- group.target %>% dplyr::count(trestbps)
target.trestbps$numeric <- as.numeric(as.character(target.trestbps$trestbps))
graph.target.geom.bar(target.trestbps, 'trestbps', 'Target Vs trestbps', 'Split Variables with Target', 'trestbps', 'Count', 'continuous', c(94, 194), c(0, 35))

# ------------------------------------- Target Vs thalach
target.thalach <- group.target %>% dplyr::count(thalach)
target.thalach$numeric <- as.numeric(as.character(target.thalach$thalach))
graph.target.geom.bar(target.thalach, 'thalach', 'Target Vs thalach', 'Split Variables with Target', 'thalach', 'Count', 'continuous', c(85, 195), c(0, 10))

# -----------------------------------------------------------------------
# ---------------------------------------------- CREATNG DATA PARTITIONS
# -----------------------------------------------------------------------
cat('Creating training & test sets...\n')
set.seed(1)
hd.set3 <- cbind(target = hd.set$target, hd.set2)
test.index <- caret::createDataPartition(hd.set3$target,
                                         times = 1,
                                         p = 0.2,
                                         list = FALSE)
train.set <- hd.set3[-test.index,]
test.set <- hd.set3[test.index,]

train.set.numeric <- hd.set.numeric[-test.index,]
test.set.numeric <- hd.set.numeric[test.index,]

cat('Training & Test sets are ready...\n')
cat('Training & Test data sets are loaded')

# ------------------------------------- Target group
group.target <- hd.set %>% group_by(target)

# ------------------------------------- trainControl
# used to control the computational nuances of the train function
control <- trainControl(method = 'cv', # the resampling method k-fold cross validation
                        number = 15,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)
process <- c('center', 'scale') # to normalize the data

rm(test.index, target.age, target.ca, target.chol, target.cp, target.exang, target.fbs, target.oldpeak, target.restecg, target.sex, target.slope, target.thal, target.thalach, target.trestbps)


# -----------------------------------------------------------------------
# ---------------------------------------------- LOGISTIC REGRESSION MODEL
# -----------------------------------------------------------------------

model.regression <- glm(target ~ .,
                        data = train.set,
                        family = binomial(link = 'logit'),
                        maxit = 100)

# --------------------------------------- Using Stepwise Backward Elimination
# --------------------------------------- to select variables and improve model
model.regression.vars <- stepAIC(model.regression,
                                 direction = 'both',
                                 trace = FALSE)
summary(model.regression.vars)
confint(model.regression.vars)

# --------------------------------------- Predictions on test.set &
# ---------------------------------------   evaluating model performance
predict <- predict(model.regression.vars,
                   test.set,
                   type = 'response')

ROC.prediction <- ROCR::prediction(predict,
                                   test.set$target)
ROC.performance <- ROCR::performance(ROC.prediction,
                                     'tpr',
                                     'fpr')

plot(ROC.performance,
     main = 'ROC Curve',
     type = 'l',
     col = 'blue')

auc <- attributes(ROCR::performance(ROC.prediction, 'auc'))$y.values[[1]]

# Confusion Matrix
predClass <- as.factor(ifelse(predict >= 0.5, 1, 0))
confusionMatrix(as.factor(test.set$target), predClass)

# ///////// *************** DOCUMENT ENDS *************** /////////
