# Machine Learning Basics

# Basics of Evaluating Machine Learning Algorithms

# Caret Package, Training and Test Sets, and Overal Accuracy
#  In this course, we will be using the caret package, which has several useful
#     functions for building and assessing machine learning methods.
# You can load it like this.
library(boot) # bootstrap and CV
library(broom)
library(car)
library(caret)
library(class) # K-Nearest Neighbors package
library(dplyr)
library(dslabs)
library(generics)
library(GGally)
library(ggplot2)
library(glmnet) # Generalized Linear Models - LASSO and Ridge Regression
library(gridExtra)
library(ISLR)
library(knitr)
library(leaps) # best subset, FSS and BSS regression
library(lubridate)
library(magrittr)
library(MASS) library(MASS) # LDA package
library(purrr)
install.packages('radr', dependencies = TRUE)
library(radr)
library(rvest)
library(stringr)
library(cellranger)
library(DBI)
library(tidyr)
library(tidyverse)


# Later, we show how these functions are useful.  In this video, we focus on
#    describing ways in which machine learning algorithms are evaluated.
# So let's get started.
# For our first introduction to machine learning concepts, we'll start with a
#    boring and simple example-- predict sex-- female or male-- using height.

# We explain machine learning step by step and this example will let us sit down
#    the first building block.  Soon enough, we'll be attacking more interesting
#    challenges.  For this first example, we will use the heights data set in
#    the ds labs package, which you can load like this.

library( dslabs )
data( heights )

# We start by defining the outcome and predictors.  In this example, we have
#    only one predictor.  So y is sex and x is height.

y <- heights$sex
x <- heights$height

# This is clearly a categorical outcome since y can be male or female, and we
#    only have one predictor, height.  We know that we will not be able to
#    predict y very accurately based on x because male and female heights are
#    not that different relative to within group variability, but can we do
#    better than guessing?
#    The answer to this question, we need to quantify the definition of better.
#    Ultimately, a machine learning algorithm is evaluated on how it performs in
#    the real world with others running the code.
# However, when developing an algorithm, we usually have a data set for which we
#    know the outcomes as we do with the heights.  We know the sex of every
#    student.  Therefore, to mimic the ultimate evaluation process, we typically
#    split the data into two and act as if we don't know the outcome for one of
#    these two sets.  We stop pretending we don't know the outcome to evaluate
#    the algorithm, but only after we're done constructing it.
# We refer to the groups of which we know the outcome and use to develop the
#    algorithm as the training set, and the group for which we pretend we don't
#    know the outcome as the test set.  A standard way of generating the
#    training and test sets is by randomly splitting the data.
# The caret package includes the function createDataPartition that helps us
#    generate indexes for randomly splitting the data into training and test
#    sets.
# The argument times in functions is used to define how many random samples of
#    indexes to return.  The argument p is used to define what proportion of
#    the index represented and the argument list is used to decide you want
#    indexes to be returned as a list or not. Here we're going to use it like
#    this.

set.seed( 2 )
test_index <- createDataPartition( y, times = 1, p = 0.5, list = FALSE )

# We can use this index to define the training set and test set like this.

train_set <- heights[ -test_index, ]
test_set <- heights[ test_index, ]

# We will now develop an algorithm using only the training set.  Once we're
#    done developing the algorithm, we will freeze it, and evaluate it using
#    the tests.  The simplest way to evaluate the algorithm when the outcomes
#    are categorical is simply by reporting the proportion of cases that were
#    correctly predicted in the test studies.
# This metric is usually referred to as overall accuracy.  To demonstrate the
#    use of overall accuracy, we will build two competing algorithms and
#    compare them.
# Let's start by developing the simplest possible machine learning algorithm--
#    guessing the outcome.  We can do that using the sample function like this.

y_hat <- sample( c( 'Male', 'Female' ),
                 length( test_index ), replace = TRUE )

# Note that we're completely ignoring the predictor and simply guessing the sex
#    OK, let's move on. In machine learning applications, it is useful to use
#    factors to represent the categorical outcomes.  Our functions developed
#    for machine learning,  such as those in the caret package, require or
#    recommend that categorical outcomes be coded as factors.
# So we can do that like this.

y_hat %>%
    factor( levels = levels( test_set$sex ) )

# The overall accuracy is simply defined as the overall proportion that is
#    predicted correctly. We can compute that using this simple line of code.

mean( y_hat == test_set$sex )

# Not surprisingly, our accuracy is about 50%-- we're guessing.  Now, can we do
#    better?  Exploratory data as it suggests we can because on average, males
#    are slightly taller than females. You can see it by typing this code.

heights %>%
    group_by( sex ) %>%
    summarize( mean( height ), sd( height ) )

# But how do we make use of this insight?
# Let's try a simple approach.  Predict male if height is within two standard
#    deviations from the average male.  We can do that using this very simple code.

y_hat <- ifelse( x > 62, 'Male', 'Female' ) %>%
    factor( levels = levels( test_set$sex ) )

# The accuracy goes way up from 50% to 80%, which we can see by typing this
#    line, but can we do better?

mean( y == y_hat )

# The accuracy goes way up from 50% to 80%, which we can see by typing this
#    line, but can we do better?
# In the example above, we use the cutoff of 62 inches, but we can examine the
#    accuracy obtained for other cutoffs and then take the value that provides
#    the best result.  But remember it is important that we pick the best
#    value on the training set.  The test set is only for evaluation.
#    Although for this simplistic example, it is not much of a problem.  Later,
#    we will learn that evaluating an algorithm on the training set can lead to
#    overfitting, which often results in dangerously over optimistic assessments.
# OK, so let's choose a different cutoff. We examined the accuracy we obtain with
#    10 different cutoffs and pick the one yielding the best result.  We can do
#    that with this simple piece of code.

cutoff <- seq( 61, 70 )
accuracy <- map_dbl( cutoff, function( x ) {
    y_hat <- ifelse( train_set$height > x, 'Male', 'Female' ) %>%
        factor( levels = levels( test_set$sex ) )
    mean( y_hat == train_set$sex )
} )
class( cutoff )
class(accuracy )
cutoff
accuracy


# We can make a plot showing the accuracy on the training set for males and
#    females.  Here it is.

qplot( cutoff, accuracy ) +
    geom_line( )

# We see that the maximum value is a 83.6%.

max( accuracy )

# Much higher than 50%, and it is maximized with the cutoff of 64 inches.

best_cutoff <- cutoff[ which.max( accuracy ) ]
best_cutoff

# Now, we can test this cut off on our test set to make sure accuracy is not
#    overly optimistic.  Now, we get an accuracy of 81.7%.

y_hat <- ifelse( test_set$height > best_cutoff, 'Male', 'Female' ) %>%
    factor( levels = levels( test_set$sex ) )
y_hat <- factor( y_hat )
mean( y_hat == test_set$sex )

# We see that is a bit lower than the accuracy observed on the training set,
#    but it's still better than guessing.  And by testing on a data that we
#    did not train on, we know it is not due to overfitting.


# Q1
# For each of the following, indicate whether the outcome is continuous or
#    categorical.

# Digit reader   <- categorical
# Movie recommendations   <- continuous
# Spam filter    <- categorical
# Hospitalizations   <- continuous
# Siri    <- categorical


# Q2
# How many features are available to us for prediction in the mnist digits
#    dataset?
#    You can download the mnist dataset using the read_mnist() function from
#       the dslabs package.

library( dslabs )

mnist <- read_mnist()
names(mnist)
dim(mnist$train$images)

# Answer:   784


# Q3
# In the digit reader example, the outcomes are stored here: y <- mnist$train$labels.

y <- mnist$train$labels
y
class( y )

#
# Do the following operations have a practical meaning?
#
y[5] + y[6]
y[5] > y[6]

# Yes, because 9 + 2 = 11 and 9 > 2.   <-*
# No, because y is not a numeric vector.
# No, because 11 is not one digit, it is two digits.
# No, because these are labels representing a category, not a number. A 9 represents a type of digit, not the number 9.

# -------       -------       -------       -------       -------       -------

# Confusion Matrix
# We previously developed a decision rule that predicts male if the student
#    is taller than 64 inches.  Now, given that the average female is about 65
#    inches, this prediction rule seems wrong.
# What happened?
#    If a student is the height of the average female, shouldn't we predict
#    female?
# Generally speaking, overall accuracy can be a deceptive measure.
# To see this, we'll start by constructing what is referred to as the confusion
#    matrix, which basically tabulates each combination of prediction and actual
#    value.  We can do this in R using the function table, like this.

table( predicted = y_hat, actual = test_set$sex )

# If we study this table closely, it reveals a problem.
# If we compute the accuracy separately for each sex, we get the following.

test_set %>%
    mutate( y_hat = y_hat ) %>%
    group_by( sex ) %>%
    summarize( accuracy = mean( y_hat == sex ) )

# We get that we get a very high accuracy for males, 93%, but a very low accuracy
#    for females, 42%.  There's an imbalance in the accuracy for males and females.
#    Too many females are predicted to be male.  In fact, we're calling close to
#    half females males.
# How can our overall accuracy we so high, then?
#     This is because of the prevalence.
# There are more males in the data sets than females.  These heights were collected
#    from three data science courses, two of which had more males enrolled.  At
#    the end, we got that 77% of the students were male.

prev <- mean( y == 'Male' )
prev

# So when computing overall accuracy, the high percentage of mistakes made for
#    females is outweighted by the gains in correct calls for men.  This can
#    actually be a big problem in machine learning.
# If your training data is biased in some way, you are likely to develop an
#    algorithm that are biased as well.  The fact that we evaluated on a test set
#    does not matter, because that test set was also derived from the original
#    biased data set.  This is one of the reasons we look at metrics other than
#    overall accuracy when evaluating a machine learning algorithm.
# There are several metrics that we can use to evaluate an algorithm in a way
#    that prevalence does not cloud our assessments.  And these can all be derived
#    from what is called the confusion matrix.  A general improvement to using
#    over accuracy is to study sensitivity and specificity separately.
# To define sensitivity and specificity, we need a binary outcome.  When the
#    outcomes are categorical, we can define these terms for a specific category.
#    In the digits example, we can ask for the specificity in the case of correctly
#    predicting 2 as opposed to some other digit.
# Once we specify a category of interest then we can talk about positive outcomes,
#    when y is 1, and negative outcomes, when y is zero.
# In general, sensitivity is defined as the ability of an algorithm to predict a
#    positive outcome when the actual outcome is positive.  So we're going to call
#    y hat equals 1 whenever y equals 1.  Because an algorithm that calls everything
#    positive, so it says y hat equals 1 no matter what, has perfect sensitivity,
#    this metric on its own is not enough to judge an algorithm.
# For this reason, we also examine specificity, which is generally defined as the
#    ability of an algorithm to not predict the positive, so y hat equals 0, when
#    the actual outcome is not a positive, y equals zero.
# We can summarize in the following way.

# High sensitivity means y equals 1 implies y hat equals 1.  Y = 1   =>   Y_hat = 1
# High specificity means y equals 0 implies y hat equals 0.  Y = 0   =>   Y_hat = 0

# Now there's another way to define specificity, and it's by the proportion of
#    positive calls that are actually positive.  So in this case, high specificity
#    is defined as y hat equals 1 implies y equals 1.  Y_hat = 1   =>   Y = 1

# To provide a precise definition, we name the four entries of the confusion matrix.

#                      | Actually Positive    | Actually Negative    |
#                      |---                   |---                   |
# | Predicted Positive | True Positives (TP)  | False Positives (FP) |
# |---                 |---                   |---                   |
# | Predicted Negative | False Negatives (FN) | True Negatives (TN)  |

# So when an outcome that is actually positive is predicted as positive, we call
#    this a true positive, TP for short.  When an actually negative result is
#    called positive, it's predictive positive, then we call it a false positive,
#    or FP.  When an actually positive result is predicted negative, we call it
#    a false negative, or FN.
# And when it actually negative results get predicted as a negative, we call it
#    a true negative, or TN.  Now we can provide more specific definitions.
# Sensitivity is typically quantified by true positives divided by the sum of
#    true positives plus false negatives, or the proportion of actual positives,
#    the first column of the confusion matrix that are called positives.  This
#    quantity is referred to as the true positive rate or recall.

# Sensitivity = TP / ( TP + FN )    <= True Positive Rate (TPR) or Recall

# Specificity is typically quantified as the true negatives divided by the sum
#    of the two negatives plus the false positives, or the proportions of negatives,
#    the second column of our confusion matrix that are called negatives.
#    This quantity is also called the true negative rate.

# Specificity = TN / ( TN + FP )    <= True Negative Rate (TNR)

# Now there's another way of quantifying specificity, which is the true positives
#    divided by the sum of the true positives plus false positives, or the proportion
#    of outcomes called positives, the first row of our confusion matrix, that
#    are actually positives.  This quantity is referred to as precision, and also
#    as the positive predictive value, PPV.

# Specificity = TP / ( TP + FP )     <= Precision or Positive Predictive Value (PPV)

# Note that unlike the true positive rate and the true negative rate, precision
#    depends on the prevalence, since higher prevalence implies you can get higher
#    precision, even when guessing.
# The multiple names can be confusing, so we include a table to help us remember
#    the terms.
# The table includes a column that shows the definition if we think of the proportions
#    as probabilities.  And here is that table.

#   | A measure of | Name1              | Name2     | Definition       | Probability Representation |
#   | ---          |---                 |---        |---               |---                         |
#   |              |                    |           |                  |                            |
#   |Sensitivity   | True Positive Rate | Recall    | TP / ( TP + FN ) | Pr( Y_hat = 1 | Y = 1 )    |
#   |              |      (TPR)         |           |                  |                            |
#   | ---          |---                 |---        |---               |---                         |
#   |              |                    |           |                  |                            |
#   | Specificity  | True Negative Rate | 1 - FPR   | TN / ( TN + FP)  | Pr( Y_hat = 0 | Y = 0 )    |
#   |              |      (TNR)         |           |                  |                            |
#   | ---          |---                 |---        |---               |---                         |
#   |              |                    |           |                  |                            |
#   | Sepcificity  | Positive Predicted | Precision | TN / ( TN + FP ) | Pr( Y = 1 | Y_hat = 1 )    |
#   |              | Value (PPV)        |           |                  |                            |

# The confusion matrix function in the caret package computes all these metrics
#    for us once we define what a positive is.
# The function expects factors as inputs, and the first level is considered the
#    positive outcome, or y equals 1.  In our example, female is the first level
#    because it comes before male alphabetically.  So if we type this line of code
#    for our predictions, we get the confusion matrix information all given to us
#    in one shot.  We can see that the high overall accuracy is possible despite
#    relatively low sensitivity.

library( caret )
install.packages('e1071', dependencies = TRUE)
confusionMatrix( data = y_hat, reference = test_set$sex )

# As we hinted at previously, the reason this happens is the low prevalence, 23%.
#    The proportion of females is low. Because prevalence is low, failing to call
#    actual females females, low sensitivity, does not lower the accuracy as much
#    as it would have increased if incorrectly called males females.
# This is an example of why it is important to examine sensitivity and specificity,
#    and not just accuracy.  Before applying this algorithm to general data sets,
#    we need to ask ourselves if prevalence will be the same in the real world.

# -------       -------       -------       -------       -------       -------

# Balanced Accuracy and F1 Score
# OK.  Let's look at another metric.  Although in general we recommend studying
#    both specificity and sensitivity, very often it is useful to have a one
#    number summary, for example, for optimization purposes.  One metric that is
#    preferred over overall accuracy is the average of specificity and sensitivity,
#    referred to as the balanced accuracy.  Because specificity and sensitivity
#    are raised, it is more appropriate to compute the harmonic average of
#    specificity and sensitivity like this.
# In fact, the F1 score, a widely used one number summary, is the harmonic average
#    of precision and recall.  Because it is easy to write, you often see this
#    harmonic average written like this. All right.

# F1-score = 2 / ( ( 1 / recall ) + ( 1 / precision ) )
#          = 2 ( precision * recall ) / ( precision + recall )

# Let's discuss some other considerations.  Note that depending on the context,
#    some types of errors are more costly than others.  For example, in the case
#    of plane safety, it is much more important to maximize sensitivity over
#    specificity.  Failing to predict a plane will malfunction before it crashes
#    is a much more costly error than grounding a plane when in fact the plane is
#    in perfect condition.  In a capital murder criminal case, the opposite is
#    true, since a false positive can lead to killing an innocent person.
# The F1 score can be adopted to weigh specificity and sensitivity differently.
# To do this, we define beta to represent how much more important sensitivity is
#    compared to specificity, and consider a weighted harmonic average using this
#    formula.

# 1 / ( (Beta^2 / recall ( 1 + Beta^2 ) ) + 1 / precision ( 1 + Beta^2 ) )

# The F_meas function in the caret package computes the summary with beta
#    defaulting to one.  So let's rebuild our prediction algorithm, but this time
#    maximizing the F score instead of overall accuracy.  We can do that by just
#    editing the code and using this instead.

cutoff <- seq( 61, 70 )

F_1 <- map_dbl( cutoff, function( x ) {
    y_hat <- ifelse( train_set$height > x, 'Male', 'Female' ) %>%
        factor( levels = levels( test_set$sex ) )
    F_meas( data = y_hat, reference = factor( train_set$sex ) )
} )
F_1
cutoff
# As before, we can plot the F1 measure versus the different cutoffs.

plot( cutoff, F_1 )
lines( cutoff, F_1 )

# And we see that it is maximized at 61% when we use a cutoff of 66 inches.

max( F_1 )
best_cutoff <- cutoff[ which.max( F_1 ) ]
best_cutoff

# A cutoff of 66 inches makes much more sense than 64.  Furthermore, it balances
#    the specificity and sensitivity of our confusion matrix as seen here.

y_hat <- ifelse( test_set$height > best_cutoff, 'Male', 'Female' ) %>%
    factor( levels = levels( test_set$sex ) )
confusionMatrix( data = y_hat, reference = test_set$sex )

# We now see that we do much better than guessing, and that both sensitivity and
#    specificity are relatively high.  We have built our first machine learning
#    algorithm.  It takes height as a predictor, and predicts female if you are
#    66 inches or shorter.

# -------       -------       -------       -------       -------       -------

# Prevalence Matters in Practice
# A machine learning algorithm with very high sensitivity and specificity may not
#    be useful in practice when prevalence is close to either 0 or 1.  To see this,
#    consider the case of a doctor that specializes in a rare disease and is
#    interested in developing an algorithm to predict who has the disease.
# The doctor shares data with you, and you develop an algorithm with very high
#    sensitivity.  You explain that this means that if a patient has a disease,
#    the algorithm is very likely to predict correctly.  You also tell the doctor
#    that you are also concerned because based on the data set you analyzed,
#    about half the patients have the disease, the probability of Y hat equals
#    was 1/2.

# Pr( Y_hat = 1 ) = 1/2

# The doctor is neither concerned nor impressed and explains that what is important
#    is the precision of the test, the probability of y equals 1 given that Y hat
#    equals 1.

# Pr( Y = 1 | Y_hat = 1 )

# Using Bayes' theorem, we can connect the two measures.

# Pr( Y | Y_hat = 1 ) = Pr( Y_hat = 1 | Y = 1 ) Pr( Y = 1 ) / Pr( Y_hat = 1 )

# The probability of Y equals given Y hat equals 1 equals the probably of Y hat
#    equals 1 given Y equals 1 times the probability of Y equals 1 divided by the
#    probability of Y hat being equal to 1.
# The doctor knows that the prevalence of the disease is 5 in 1,000.  The prevalence
#    of the disease in your data set was 50%.  This implies that that ratio, the
#    probability of Y equals is 1 divided by the proportion of Y hat equals 1 is
#    about 1 in 100.  And therefore, the position of your algorithm is less than
#    0.01.
# The doctor does not have much use for your algorithm.

# -------       -------       -------       -------       -------       -------

# ROC and Precision-Recall Curves
# When comparing two or more methods--for example, guessing versus using a height
#    cutoff in our predict sex with height example-- we looked at accuracy and F1.
# The second method, the one that used height, clearly outperformed.  However,
#    while for the second method we consider several cutoffs, for the first one
#    we only considered one approach, guessing with equal probability.
# Note that guessing male with higher probability would give us higher accuracy
#    due to the bias in the sample.  You can see this by writing this code, which
#    predicts male.  By guessing 90% of the time, we make our accuracy go up to
#    0.72.

p <- 0.9
y_hat <- sample( c( 'Male', 'Female' ),
                 length( test_index ),
                 replace = TRUE,
                 prob = c( p, 1 - p ) ) %>%
    factor( levels = levels( test_set$sex ) )
mean( y_hat == test_set$sex )

# But as previously described, this would come at a cost of lower sensitivity.
#    The curves we describe in this video will help us see this.
# Note that for each of these parameters, we can get a different sensitivity
#    and specificity.  For this reason, a very common approach to evaluating
#    methods is to compare them graphically by plotting both.  A widely used
#    plot that does this is the receiver operating characteristic or ROC curve.

# sensitivity * TPR Vs 1 - specificity or false positive rate (FPR)

# The ROC curve plus sensitivity, the true positive rate, versus one minus
#    specificity, or the false positive rate.  Here is an ROC curve for guessing
#    sex, but using different probabilities of guessing male.
# The ROC curve for guessing always looks like this, like the identity line.
# Note that a perfect algorithm would shoot straight to one and stay up there,
#    perfect sensitivity for all values of specificity.  So how does our second
#    approach compare?
#    We can construct an ROC curve for the height-based approach using this code.

cutoffs <- c( 50, seq( 60, 75 ), 80 )

height_cutoff <- map_df( cutoffs, function( x ) {
    y_hat <- ifelse( test_set$height > x,
                     'Male',
                     'Female' ) %>%
        factor( levels = c( 'Female', 'Male' ) )
    list( method = 'Height cutoff',
          FPR = 1 - specificity( y_hat, test_set$sex ),
          TPR = sensitivity( y_hat, test_set$sex ) )
} )
height_cutoff

ggplot( data = height_cutoff, aes( FPR, TPR ) ) +
    geom_line( aes( FPR, color = 'red' ) ) +
    geom_line( aes( TPR , color = 'cyan' ) ) +
    geom_point() +
    labs( x = '1-Specificity', y = 'Sensitivity' ) +
    scale_color_discrete( name = 'Method', labels = c( 'Guessing', 'Height cutoff' ) )

# By plotting both curves together, we are able to compare sensitivity for different
#    values of specificity.  We can see that we obtain higher sensitivity with
#    the height-based approach for all values of specificity, which imply it is,
#    in fact, a better method.
# Note that when making ROC curves, it is often nice to add the cutoff used to
#    the points.  It would look like this.  ROC curves are quite useful for comparing
#    methods.  However, they have one weakness, and it is that neither of the
#    measures plotted depend on prevalence.
# In cases in which prevalence matters, we may instead make a precision recall plot.

guessing <- map_df( probs,
                    function( p ) {
                        y_hat <- sample( c( 'Male', 'Female' ),
                                         length( test_index ),
                                         replace = TRUE,
                                         prob = c( p, 1 - p ) ) %>%
                            factor( levels = c( 'Female', 'Male' ) )
                        list( method = 'Ghess',
                              recall = sensitivity( y_hat, test_set$sex ),
                              precision = precision( y_hat, test_set$sex ) )
                    } )

height_cutoff <- map_df( cutoffs,
                         function( x ) {
                             y_hat <- ifelse( test_set$height > x,
                                              'Male',
                                              'Female' ) %>%
                                 factor( levels = c( 'Female', 'Male' ) )
                             list( method = 'Height Cutoff',
                                   recall = sensitivity( y_hat, test_set$sex ),
                                   precision = precision( y_hat, test_set$sex ) )
                         } )

# The idea is similar, but we instead plot precision against recall.  Here's what
#    the plot looks like comparing our two methods.  From this plot, we immediately
#    see that the precision of guessing is not high.  This is because the prevalence
#    is low.
# If we change positives to mean male instead of females, the ROC curve remains
#    the same, but the precision recall plot changes.  And it looks like this.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Confusion Matrix
# The following questions all ask you to work with the dataset described below.
#
# The reported_heights and heights datasets were collected from three classes taught in the Departments of Computer Science and Biostatistics, as well as remotely through the Extension School. The Biostatistics class was taught in 2016 along with an online version offered by the Extension School. On 2016-01-25 at 8:15 AM, during one of the lectures, the instructors asked student to fill in the sex and height questionnaire that populated the reported_height dataset. The online students filled out the survey during the next few days, after the lecture was posted online. We can use this insight to define a variable which we will call type, to denote the type of student, inclass or online.
#
# The code below sets up the dataset for you to analyze in the following exercises:
#
library(dslabs)
library(dplyr)
library(lubridate)

data('reported_heights')

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
    filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
    mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), 'inclass','online')) %>%
    select(sex, type)
head( dat )
y <- factor(dat$sex, c('Female', 'Male'))
x <- dat$type


# Q1
# What is the proportion of females in class and online? (That is, calculate the proportion of the in class students who are female and the proportion of the online students who are female.)

dat %>%
    group_by( type ) %>%
    summarize( prop_female = mean( sex == 'Female' ) )

# In class
# 0.66
# Online
# 0.37

# Q2
# If you used the type variable to predict sex, what would the prediction accuracy be?
y <- factor(dat$sex, c('Female', 'Male'))
x <- dat$type

y_hat <- ifelse( dat$type == 'inclass', 'Female', 'Male' ) %>%
    factor( levels = levels( y ) )
mean( y_hat == y )
# 0.63

# Q3
# Write a line of code using the table function to show the confusion matrix, assuming the prediction is y_hat and the truth is y.
table( predicted = y_hat, actual = y )

# Q4
# What is the sensitivity of this prediction?
sensitivity( y_hat, y )   # 0.38

# Q5
# What is the specificity of this prediction?
specificity( y_hat, y )   # 0.84

# Q6
# What is the prevalence (% of females) in the dat dataset defined above?
confusionMatrix( data = y_hat, reference = y )   # 0.45
mean( y == 'Female' )   # 0.45

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Practice with Machine Learning
# We will practice building a machine learning algorithm using a new dataset, iris, that provides multiple predictors for us to use to train. To start, we will remove the setosa species and we will focus on the versicolor and virginica iris species using the following code:

library(caret)
data(iris)
iris <- iris[-which(iris$Species == 'setosa'),]
y <- iris$Species

#
# The following questions all involve work with this dataset.
#
# Q1
# First let us create an even split of the data into train and test partitions using createDataPartition. The code with a missing line is given below:
#
set.seed(2)
test_index <- createDataPartition( y, times = 1, p = 0.5, list = FALSE) # Create alist of 50% of the rows in the original dataset we can use for training
test <- iris[test_index,]
train <- iris[-test_index,]
# Which code should be used in place of # line of code above?
# test_index <- createDataPartition(y,times=1,p=0.5)
# test_index <- sample(2,length(y),replace=FALSE)
# test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)   <-*
# test_index <- rep(1,length(y))

# Q2
# Next we will figure out the singular feature in the dataset that yields the greatest overall accuracy. You can use the code from the introduction and from Q1 to start your analysis.
#
# Using only the train iris data set, which of the following is the singular feature for which a smart cutoff (simple search) yields the greatest overall accuracy?

foo <- function( x ) {
    rangedValues <- seq( range( x )[ 1 ],
                         range( x )[ 2 ],
                         by = 0.1 )
    sapply( rangedValues,
            function( i ) {
                y_hat <- ifelse( x > i,
                                 'virginica',
                                 'versicolor' )
                mean( y_hat == train$Species )
            } )
}
predictions <- apply( train[ , -5 ], 2, foo )
sapply( predictions, max )

# Sepal.Length
# Sepal.Width
# Petal.Length   <-*
# Petal.Width

# Q3
# Using the smart cutoff value calculated on the training data, what is the overall accuracy in the test data?
#

predictions <- foo( train[ , 3 ] )

rangedValues <- seq( range( train[ , 3 ] )[ 1 ],
                     range( train[ , 3 ] )[ 2 ],
                     by = 0.1 )

cutoffs <- rangedValues[ which( predictions == max( predictions ) ) ]
cutoffs
y_hat <- ifelse( test[ , 3 ] > cutoffs[ 1 ],
                 'virginica',
                 'versicolor' )
y_hat
mean( y_hat == test$Species )
# 0.90


# Q4
# Notice that we had an overall accuracy greater than 96% in the training data, but the overall accuracy was lower in the test data. This can happen often if we overtrain. In fact, it could be the case that a single feature is not the best choice. For example, a combination of features might be optimal. Using a single feature and optimizing the cutoff as we did on our training data can lead to overfitting.
#
# Given that we know the test data, we can treat it like we did our training data to see if the same feature with a different cutoff will optimize our predictions.
#
# Which feature best optimizes our overall accuracy?
# Sepal.Length
# Sepal.Width
# Petal.Length
# Petal.Width    <-*

# Q5
# Now we will perform some exploratory data analysis on the data.
#
# Notice that Petal.Length and Petal.Width in combination could potentially be more information than either feature alone.
#
# Optimize the combination of the cutoffs for Petal.Length and Petal.Width in the train data and report the overall accuracy when applied to the test dataset. For simplicity, create a rule that if either the length OR the width is greater than the length cutoff or the width cutoff then virginica or versicolor is called. (Note, the F1 will be similarly high in this example.)
#
# What is the overall accuracy for the test data now?

cutoff_L <- seq( 4, 7, by = 0.1 )
cutoff_W <- seq( 0, 3, by = 0.1 )

accuracy <- map_dbl( cutoff_L,
                     function( x ) {
                         y_hat <- ifelse( train$Petal.Length > x,
                                          'virginica',
                                          'versicolor' ) %>%
                             factor( levels = levels( train$Species ) )
                         mean( y_hat == train$Species )
                     } )
accuracy

plot( cutoff_L, accuracy )

max( accuracy )
best_cutoff_L <- cutoff_L[ which.max( accuracy ) ]
best_cutoff_L

accuracy <- map_dbl( cutoff_W,
                     function( x ) {
                         y_hat <- ifelse( train$Petal.Width > x,
                                          'virginica',
                                          'versicolor' ) %>%
                             factor( levels = levels( train$Species ) )
                         mean( y_hat == train$Species )
                     } )
accuracy

plot( cutoff_W, accuracy )

max( accuracy )

best_cutoff_W <- cutoff_W[ which.max( accuracy ) ]
best_cutoff_W

# Try on test
y_hat <- ifelse( test$Petal.Length > best_cutoff_L & test$Petal.Width > best_cutoff_W,
                 'virginica',
                 'versicolor')
y_hat

mean( y_hat == test$Species )
# 0.92

# A different option
library(caret)
data(iris)
iris <- iris[ -which( iris$Species == 'setosa' ), ]
y <- iris$Species

plot( iris, pch = 21, bg = iris$Species)

set.seed( 2 )
test_index <- createDataPartition( y, times = 1, p = 0.5, list = FALSE )
test <- iris[ test_index, ]
train <- iris[ -test_index, ]

petalLengthRange <- seq( range( train[ , 3 ] )[ 1 ],
                         range( train[ , 3 ])[ 2 ],
                         by = 0.1 )
petalWidthRange <- seq( range( train[ , 4 ] )[ 1 ],
                        range( train[ , 4 ] )[ 2 ],
                        by = 0.1)
cutoffs <- expand.grid( petalLengthRange, petalWidthRange )
cutoffs

id <- sapply( seq( nrow( cutoffs ) ),
              function( i ) {
                  y_hat <- ifelse( train[ , 3 ] > cutoffs[ i, 1 ] | train[ , 4 ] > cutoffs[ i, 2 ],
                                   'virginica',
                                   'versicolor')
                  mean( y_hat == train$Species )
                  }) %>%
    which.max
id

optimalCutoff <- cutoffs[ id, ] %>% as.numeric
optimalCutoff

y_hat <- ifelse( test[ ,3 ] > optimalCutoff[ 1 ] & test[ , 4 ] > optimalCutoff[ 2 ],
                 'virginica',
                 'versicolor')
y_hat
mean( y_hat == test$Species )
# 0.92


# -------       -------       -------       -------       -------       -------

# Conditional Probabilities
# In machine learning applications, we rarely can predict outcomes perfectly.
# Note, that spam detectors often miss emails that are clearly spam, Siri often
#    misunderstands the words we're saying, and your bank often thinks your card
#    was stolen when it was not.  Instead, you are on vacation.
# The most common reason for not being able to build perfect algorithms is that
#    it is impossible.  To see this, note that most data sets will include groups
#    of observations with the same exact observed values for all predictors, and
#    thus, resulting in the same prediction.
# But they have different outcomes, making it impossible to make the predictions
#    right for all these observations.  We saw a simple example of this in the
#    previous videos for any given height, x, you will have both males and females
#    that are x inches tall.  So you can't predict them all right.
# However, none of this means that we can't build useful algorithms that are much
#    better than guessing, and in some cases, better than expert opinion.
# To achieve this in an optimal way, we make use of probabilistic representations
#    of the problem.  Observations with the same observed values for the predictors
#    may not all be the same, but we can assume that they all have the same
#    probability of this class or that class.
# We will write this idea out mathematically for the case of categorical data.
# We use the notation X1 = little x1, all the way to Xp = little xp, to represent
#    the fact that we have observed values, little x1 up to little xp.
# For covariates, X1 through Xp.
# This does not imply that the outcome, y, will take a specific value, but rather,
#    that it implies a specific probability.  Specifically, we denote the
#    conditional probabilities of each class, k, using this notation.
# To avoid writing out all the predictors, we'll use the following notation.
# We will use bold letters to represent all the predictors like this.
# We will also use the following notation for the conditional probability of being
#    in class k.  We can write it like this.
# Before we continue, a word of caution.  We'll be using the notation p of x to
#    represent conditional probabilities as functions.
# Do not confuse this with the p that we use to represent the number of predictors.
# Now, let's continue.  Knowing these probabilities can guide the construction of
#    an algorithm that makes the best prediction.
# For any given set of predictors, x, we'll predict the class, k, with the largest
#    probability among p1x, p2x, all the way up to p capital Kx.
# In mathematical notation, we can write it like this.  But it's not this simple,
#    because we don't know the pk of xs.  In fact, estimating these conditional
#    probabilities can be thought of as the main challenge of machine learning.
# The better our algorithm estimates p hat of kx, the better our predictor will be.
# So, how good will our prediction be will depend on two things-- how close the
#    maximum probability is to 1, and how close our estimate of the probabilities
#    are to the actual probabilities.  We can't do anything about the first
#    restriction, as it is determined by the nature of the problem.
# So our energy goes into finding ways to best estimate condition of probabilities.
# The first restriction does imply that we have limits as to how well even the best
#    possible algorithm can perform. You should get used to the idea that while in
#    some challenges we will be able to achieve almost perfect accuracy-- digit
#    readers, for example-- and others, our success is restricted by the randomness
#    of the process-- movie recommendations, for example.
# And before we continue, we note that defining our prediction by maximizing the
#    probability is not always optimal in practice and depends on the context.
# As previously discussed, sensitivity and specificity may differ in importance in
#    different contexts.  But even in these cases, having a good estimate of the
#    conditional probabilities will suffice for us to build an optimal prediction
#    model, since we can control specificity and sensitivity however we wish.
# For example, we can simply change the cutoff used to predict one class versus
#    another.  In the plane example we gave previously, we may ground the plane
#    anytime the probability of my function is higher than 1 in 1,000, as opposed
#    to the default 1/2 used when error types are equally undesired.

# -------       -------       -------       -------       -------       -------

# Conditional Expectations and Loss Function
#  In this video, we make a connection between conditional probabilities and
#    conditional expectations. For binary data, you can think of the conditional
#    probably of y equals 1 when x equals x as a proportion of 1s in the stratum
#    of the population for which x equals x.
# Many of the algorithms we will learn can be applied to both categorical and
#    continuous data due to the connection between conditional probabilities
#    and conditional expectations.
# Because the expectation is the average of values, y1 through yn in the population,
#    in the case in which y's are 0s or 1s, the expectation is equivalent to the
#    probability of randomly picking a 1 since the average is simply the proportion
#    of 1s.
# Therefore, the conditional expectation is equal to the conditional probability.
# We, therefore, often only use the expectation to know both the conditional
#    probability and the conditional expectation.  So why do we even care about
#    the conditional expectation?
#    Just like with categorical outcomes, in most applications, the same observed
#    predictors does not guarantee the same continuous outcome.
# Instead, we assume that the outcome follows the same conditional distribution,
#    and we will now explain why we look and use the conditional expectations
#    to define our predictors.  Before we start describing approaches to optimizing
#    the way we build algorithm for continuous outcomes, we first need to define
#    what we mean when we say one approach is better than the other.
# With binary outcomes, we have already described how sensitivity, specificity,
#    accuracy, and F1 can be used as quantifications.  However, these metrics are
#    not useful for continuous outcomes.
# The general approach of defining best in machine learning is to define a loss
#    function.  The most common used one is a squared loss function.  If y hat
#    is our predictor and y is our actual outcome, the squared loss function is
#    simply the difference squared.
# Because we often have a test set with many observations, say n observations,
#    we use the mean squared error given by this formula.  Note that if the
#    outcomes are binary, the mean squared error is equivalent to accuracy since
#    y hat minus y squared is one of the prediction was correct and 0 otherwise.
# And the average is just taking the proportion of correct predictions.
# In general, our goal is to build an algorithm that minimizes the loss so it
#    is as close to 0 as possible.  Because our data is usually a random sample,
#    the mean squared error is a random variable.  So it is possible that an
#    algorithm minimize mean squared error on a specific data to look, but that
#    in general, another algorithm will do better.
# We, therefore, try to find algorithms that minimize the mean squared error
#    on average.  That is, we want the algorithm that minimizes the average of
#    the squared loss across many, many random samples.  The mathematical equation
#    for this is this here.
# The expectation of the squared error.  Note that this is a theoretical concept
#    because in principle, we only have one data set to work with.
# However, we will later learn of techniques that permit us to estimate this
#    quantity.  Before we continue, note that there are other loss functions other
#    than squared loss.
# For example, we can use absolute value instead of squaring the errors.
# But in this course, we focus on minimizing squared loss since it is the most widely
#    used.  So why do we care about the conditional expectation in machine learning?
#    This is because the expected value has an attractive mathematical property.
#    It minimizes the expected squared loss.  Specifically, of all possible y hats,
#    the conditional expectation of y given x minimizes the expected loss given x.
# Due to this property, a succinct description of the main task of machine learning
#    is that we use data to estimate the conditional probability for any set of
#    features x1 through xp.
# This, of course, is easier said than done since this function can take any shape
#    and p, the number of covariance, can be very large.  OK, so consider a case
#    in which we only have one predictor-- x.
# The expectation of y given x can be any function of x-- a line, a parabola, a
#    sine wave, a step function, anything.  It gets even more complicated when we
#    consider cases with larger number of covariance in which case, f of x is a
#    function of a multi-dimensional vector x.  For example, in our digit reader example, the number of covariance was 784.
# The main way in which computing, machine learning algorithms differ is in the
#    approach to estimating this conditional expectation, and we are going to learn
#    a few of those approaches.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Conditional Probabilities Review
# Q1
# In a previous module, we covered Bayes' theorem and the Bayesian paradigm. Conditional probabilities are a fundamental part of this previous covered rule.

# P( A|B ) = P( B|A ) P( A ) / P( B )

# We first review a simple example to go over conditional probabilities.
#
# Assume a patient comes into the doctor’s office to test whether they have a particular disease.
#
# The test is positive 85% of the time when tested on a patient with the disease (high sensitivity): P( test+ | disease ) = 0.85
# The test is negative 90% of the time when tested on a healthy patient (high specificity): P( test- | healthy ) = 0.90
# The disease is prevalent in about 2% of the community: P( disease ) = 0.02
# Using Bayes' theorem, calculate the probability that you have the disease if the test is positive.

# P( disease | test+ ) = P( test+ | disease ) P( disease ) / P( healthy )
# P( test+ | disease ) = 0.85
# P( disease ) = 0.02
# P( healthy ) = ?


#                       P( test+ | disease ) = 0.85
#                      /
#  P( disease ) = 0.02
#/                     \
#                       P( test- | disease ) = 0.15
#\                      P( test+ | healthy ) = 0.10
#  P( healthy ) = 0.98 /
#   \                  \
#                       P( test- | healthy ) = 0.90

# P( healthy ) = [ P( test+ | disease) P( disease ) ] + [ P( test+ | healthy ) P( healthy ) ]
#              = [ 0.85 * 0.02 ] + [ 0.1 * 0.98 ] = 0.017 * 0.098 = 0.115

# P( disease | test+ ) = 0.85 * ( 0.02 / 0.115 ) = 0.85 * 0.1739 = 0.14
# 0.14


# The following 4 questions (Q2-Q5) all relate to implementing this calculation using R.
#
# We have a hypothetical population of 1 million individuals with the following conditional probabilities as described below:
#
#     The test is positive 85% of the time when tested on a patient with the disease (high sensitivity): P( test+ | disease ) = 0.85
#     The test is negative 90% of the time when tested on a healthy patient (high specificity): P( test- | healthy ) = 0.90
#     The disease is prevalent in about 2% of the community: P( disease ) = 0.02
#     Here is some sample code to get you started:
#
set.seed( 1 )
disease <- sample( c( 0, 1 ),
                   size = 1e6,
                   replace = TRUE,
                   prob = c( 0.98, 0.02 ) )
test <- rep( NA, 1e6 )
test[ disease == 0 ] <- sample( c( 0, 1 ),
                                size = sum( disease == 0 ),
                                replace = TRUE,
                                prob = c( 0.90, 0.10 ) )
test[ disease == 1 ] <- sample( c( 0, 1 ),
                                size = sum( disease == 1 ),
                                replace = TRUE,
                                prob = c( 0.15, 0.85 ) )
test

# Q2
# What is the probability that a test is positive?
mean( test )   # 0.115

# Q3
# What is the probability that an individual has the disease if the test is negative?
mean( disease[ test == 0 ] )  # 0.003

# Q4
# What is the probability that you have the disease if the test is positive?
# Remember: calculate the conditional probability the disease is positive assuming a positive test.
mean( disease[ test == 1 ] == 1 )  # 0.14

# Q5
# If the test is positive, what is the relative risk of having the disease?
#     First calculate the probability of having the disease given a positive test, then normalize it against the disease prevalence.
mean( disease[ test == 1 ] == 1 ) / mean( disease == 1 )   # 7.35

# -------       -------       -------       -------       -------       -------

# Comprehension Check> Conditional Probabilities Practice
# Q1
# We are now going to write code to compute conditional probabilities for being male in the heights dataset. Round the heights to the closest inch. Plot the estimated conditional probability  for each .
#
# Part of the code is provided here:
#
library(dslabs)
data('heights')
head( heights, 1 )
# MISSING CODE
heights %>%
    mutate(height = round(height)) %>%
    group_by(height) %>%
    summarize(p = mean(sex == 'Male')) %>%
qplot( height,
       p,
       data = . )

# Which of the following blocks of code can be used to replace MISSING CODE to make the correct plot?
#
# heights %>%
#     group_by(height) %>%
#     summarize(p = mean(sex == 'Male')) %>%
#
# heights %>%
#     mutate(height = round(height)) %>%
#     group_by(height) %>%
#     summarize(p = mean(sex == 'Female')) %>%
#
# heights %>%
#     mutate(height = round(height)) %>%
#     summarize(p = mean(sex == 'Male')) %>%
#
# heights %>%    <-*
#     mutate(height = round(height)) %>%
#     group_by(height) %>%
#     summarize(p = mean(sex == 'Male')) %>%


# Q2
# In the plot we just made in Q1 we see high variability for low values of height. This is because we have few data points. This time use the quantile (\ 0.1,0.2,\dots,0.9 \)and the cut function to assure each group has the same number of points. Note that for any numeric vector x, you can create groups based on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).
#
# Part of the code is provided here:
#
ps <- seq(0, 1, 0.1)
head( heights )
# MISSING CODE
heights %>%
    mutate( g = cut( height, quantile( height, ps ), include.lowest = TRUE ) ) %>%
    group_by( g ) %>%
    summarize( p = mean( sex == 'Male' ), height = mean( height ) ) %>%
    qplot( height, p, data = . )

# Which of the following lines of code can be used to replace MISSING CODE to make the correct plot?
#
# mutate(g = cut(male, quantile(height, ps), include.lowest = TRUE)) %>%
#
# mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%   <-*
#
# mutate(g = cut(female, quantile(height, ps), include.lowest = TRUE)) %>%
#
# mutate(g = cut(height, quantile(height, ps))) %>%

# Q3
# You can generate data from a bivariate normal distrubution using the MASS package using the following code.

Sigma <- 9 * matrix( c( 1, 0.5, 0.5, 1 ), 2, 2 )
dat <- MASS::mvrnorm( n = 10000, c( 69, 69 ), Sigma ) %>%
    data.frame() %>% setNames( c( 'x', 'y' ) )
Sigma
dat

# And make a quick plot using plot(dat).
#
# Using an approach similar to that used in the previous exercise, let's estimate the conditional expectations and make a plot. Part of the code has been provided for you:

ps <- seq( 0, 1, 0.1 )
dat %>%
    #MISSING CODE
    mutate( g = cut( x, quantile( x, ps ), include.lowest = TRUE ) ) %>%
    group_by( g ) %>%
    summarize( y = mean( y ), x = mean( x ) ) %>%
    qplot( x, y, data = . )

# Which of the following blocks of code can be used to replace MISSING CODE to make the correct plot?
#
# mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%   <-*
# group_by(g) %>%
# summarize(y = mean(y), x = mean(x)) %>%
#
# mutate(g = cut(x, quantile(x, ps))) %>%
# group_by(g) %>%
# summarize(y = mean(y), x = mean(x)) %>%
#
# mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
# summarize(y = mean(y), x = mean(x)) %>%
#
# mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
# group_by(g) %>%
# summarize(y =(y), x =(x)) %>%

# -------       -------       -------       -------       -------       -------

# Linear Regression for Prediction
# Linear regression can be considered a machine learning algorithm.
# As you will see, it is too rigid to be useful in general, but for some
#    challenges it works rather well. It also serves as a baseline approach.
#    If you can't beat it with a more complex approach, you probably want to
#    stick to linear regression.  To quickly make the connection between
#    regression and machine learning, we will reformulate Galten study with
#    heights a continuous outcome. We're going to load it up using this code.

library( HistData )
galton_heights <- GaltonFamilies %>%
    filter( childNum == 1 & gender == 'male' ) %>%
    select( father, childHeight ) %>%
    rename( son = childHeight )
head( galton_heights )

# Suppose you're tasked with building a machine learning algorithm that predicts
#    the son's height y using the father's height x.  Let's start by generating
#    some testing and train sets using this code.

library( caret )
y <- galton_heights$son
test_index <- createDataPartition( y,
                                   times = 1,
                                   p = 0.5,
                                   list = FALSE )

train_set <- galton_heights %>%
    slice( -test_index )

test_set <- galton_heights %>%
    slice( test_index )

train_set
test_set

# In this case, if we're ignoring the father's height and guessing the son's
#    height, we would guess the average height of son's.  So our prediction would
#    be the average, which we can get this way.

avg <- mean( train_set$son )
avg

# R squared loss is about 6.60 which you can see by typing this code.

mean( ( avg - test_set$son )^2 )

#    Now can we do better?
#    In the regression course, we learned that if a pair xy follows a bivariate
#    normal distribution, as the son and father's heights do, the conditional
#    expectation which is what we want to estimate in machine learning is
#    equivalent to the regression line.

# f(x) = E( Y | X = x ) = Beta0 + Beta1x

# We also introduced least squares as a method for estimating the slope and
#    intercept.  We can write this code to quickly get that fitted model.

fit <- lm( son ~ father,
           data = train_set )
fit

# This gives us an estimate of the conditional expectation, which is a simple
#    formula.  It's 38, plus 0.47x.

# f( x ) = 38 + 0.47x

# We can see that this does indeed provide an improvement over our guessing
#    approach which gave us a loss of 6.6. Now we get a loss of 4.78, a little
#    bit lower.

y_hat <- fit$coef[ 1 ] + fit$coef[ 2 ] * test_set$father
mean(( y_hat - test_set$son ) ^2 )

# -------       -------       -------       -------       -------       -------

# Predict Function
# Before we continue with the concepts that connect linear regression to machine
#    learning, let's describe the function predict.
# The predict function is very useful for machine learning applications.  This
#    function takes a fitted object from functions such a lm or glm and a data
#    frame with the new predictors for which you want to predict and returns a
#    prediction.  So in our current example, instead of writing out the formula
#    for the regression line, we can use the function predict like this.

y_hat <- predict( fit, test_set )
y_hat

# We can see that we get the same result for the loss by computing it just like
#    we did before.  And we see that we once again get 4.78.

y_hat <- predict( fit, test_set )
mean(( y_hat - test_set$son )^2 )

# It's because using predict is equivalent to using the regression line.  Note
#    that predict does not always return objects of the same type.  To learn
#    about the specifics, you need to look at the help file for the type of
#    fitted object that is being used.
# So if we use lm, we might get a different object type than if we use glm.
# To learn about the specifics, you can look at the help file for predict.lm and
#    predict.glm.
# There'll be other examples like this, where the function is called, say, knn.
# Then we have to look at the help file for predict.knn.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Linear Regression

# Q1
# Create a data set using the following code:

set.seed( 1 )
n <- 100
Sigma <- 9 * matrix( c( 1.0, 0.5, 0.5, 1.0 ), 2, 2 )
dat <- MASS::mvrnorm( n = 100, c( 69, 69 ), Sigma ) %>%
    data.frame() %>% setNames(c('x', 'y'))

library( caret )
set.seed(1)
results <- function(){
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x , data = train_set)
    y_hat <- predict(fit, test_set)
    rmse <- sqrt(mean((y_hat - test_set$y)^2))
}

RMSE <- replicate( n, results() )
RMSE
mean( RMSE )   # 2.4886
sd( RMSE )     # 0.1229


# Use the caret package to partition the dataset into test and training sets of equal size. Train a linear model and calculate the RMSE. Repeat this exercise 100 times and report the mean and standard deviation of the RMSEs. (Hint: You can use the code shown in a previous course inside a call to replicate using a seed of 1.
#
# Mean: 2.4886
# SD: 0.1229

# Q2
# Now we will repeat the above but using larger datasets. Repeat the previous exercise but for datasets with n <- c(100, 500, 1000, 5000, 10000). Save the average and standard deviation of RMSE from the 100 repetitions using a seed of 1. Hint: use the sapply or map functions.

library( purrr )
set.seed( 1 )
myRMSE <- function( size ) {
    Sigma <- 9 * matrix( c( 1.0, 0.5, 0.5, 1.0 ), 2, 2 )
    dat <- MASS::mvrnorm( n = size, c( 69, 69 ), Sigma) %>%
        data.frame() %>%
        setNames( c( 'x', 'y' ) )
    RMSEs <- replicate( size, {
        y <- dat$y
        test_index <- createDataPartition( y, times = 1, p = 0.5, list = FALSE )
        train_set <- dat %>% slice( -test_index )
        test_set <- dat %>% slice( test_index )
        fit <- lm( y ~ x , data = train_set )
        y_hat <- predict( fit, test_set )
        rmse <- sqrt( mean( ( y_hat - test_set$y )^2 ) )
    } )
    list( RMSEs, mean( RMSEs ), sd( RMSEs ) )
}

n <- c( 100, 500, 1000, 5000, 10000 )

#set.seed( 1 )
RES <- map( n, myRMSE )
RES
myRMSE

# function     100    500      1000     5000     10000
# mean       2.4886   2.7209   2.5555   2.6184   2.6184
# sd         0.1180   0.0800   0.0456   0.0230   0.0168

# Mean, 100:     2.4886
# SD, 100:
# Mean, 500:     2.7209
# SD, 500:       0.0800
# Mean, 1000:    2.5555
# SD, 1000:      0.0456
# Mean, 5000:    2.6248
# SD, 5000:      0.0230
# Mean, 10000:   2.6184
# SD, 10000:     0.0168

# Q3
# What happens to the RMSE as the size of the dataset becomes larger?
# On average, the RMSE does not change much as n gets larger, but the variability of the RMSE decreases.   <-*
# Because of the law of large numbers the RMSE decreases; more data means more precise estimates.
# n = 10000 is not sufficiently large. To see a decrease in the RMSE we would need to make it larger.
# The RMSE is not a random variable.

# Q4
# Now repeat the exercise from Q1, this time making the correlation between x and y larger, as in the following code:

set.seed( 1 )
n <- 100
Sigma <- 9 * matrix( c( 1.0, 0.95, 0.95, 1.0 ), 2, 2 )
dat <- MASS::mvrnorm( n = 100, c( 69, 69 ), Sigma ) %>%
    data.frame() %>%
    setNames( c( 'x', 'y' ) )

set.seed( 1 )
results <- function() {
    y <- dat$y
    test_index <- createDataPartition( y, times = 1, p = 0.5, list = FALSE )
    train_set <- dat %>% slice( -test_index )
    test_set <- dat %>% slice( test_index )
    fit <- lm( y ~ x , data = train_set )
    y_hat <- predict( fit, test_set )
    rmse <- sqrt( mean( ( y_hat - test_set$y )^2 ) )
}


set.seed( 1 )
R_MSE <- replicate( n, results() )
mean( R_MSE )
sd( R_MSE )

# Note what happens to RMSE - set the seed to 1 as before.
# Mean:  0.9167
# SD:    0.062

# Q5
# Which of the following best explains why the RMSE in question 4 is so much lower than the RMSE in question 1?

#         Q1         Q4
# mean    2.48     0.9167
# sd      0.1229   0.062

# It is just luck. If we do it again, it will be larger.
# The central limit theorem tells us that the RMSE is normal.
# When we increase the correlation between x and y, x has more predictive power and thus provides a better estimate of y.   <-*
# These are both examples of regression so the RMSE has to be the same.

# Q6
# Create a data set using the following code.

set.seed( 1 )
n <- 1000
Sigma <- matrix( c( 1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0 ), 3, 3 )
dat <- MASS::mvrnorm( n = 100, c( 0, 0, 0 ), Sigma ) %>%
    data.frame() %>%
    setNames( c( 'y', 'x_1', 'x_2' ) )
cor( dat )

# Note that y is correlated with both x_1 and x_2 but the two predictors are independent of each other, as seen by cor(dat).
# Use the caret package to partition into a test and training set of equal size. Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a linear model for each.

set.seed(1)
results_x_1 <- function(){
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x_1 , data = train_set)
    y_hat <- predict(fit, test_set)
    rmse <- sqrt(mean((y_hat - test_set$y)^2))
}


set.seed(1)
R_MSE_x_1 <- replicate(n, results_x_1())
mean(R_MSE_x_1)
sd(R_MSE_x_1)   # mean: 0.6326    sd: 0.0405



set.seed(1)
results_x_2 <- function(){
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x_2 , data = train_set)
    y_hat <- predict(fit, test_set)
    rmse <- sqrt(mean((y_hat - test_set$y)^2))
}

set.seed(1)
R_MSE_x_2 <- replicate(n, results_x_2())
mean( R_MSE_x_2 )
sd( R_MSE )   # mean: 0.6504   sd: 0.0411



set.seed(1)
results_x_1_x_2 <- function(){
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x_1 + x_2 , data = train_set)
    y_hat <- predict(fit, test_set)
    rmse <- sqrt(mean((y_hat - test_set$y)^2))
}

set.seed(1)
R_MSE_x_1_x_2 <- replicate( n, results_x_1_x_2() )
mean( R_MSE_x_1_x_2 )
sd( R_MSE_x_1_x_2 )   # mean: 0.3397    sd: 0.0253
#                 mean     sd
# y ~ x_1         0.6326   0.0405
# y ~ x_2         0.6504   0.0411
# y ~ x_1 + X_2   0.3397   0.0253

# Which of the three models performs the best (has the lowest RMSE)?
# x_1
# x_2
# x_1 and x_2    <-*

# Q7
# Report the lowest RMSE of the three models tested in Q6.
# 0.307


# Q8
# Repeat the exercise from q6 but now create an example in which x_1 and x_2 are highly correlated.

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
data.frame() %>% setNames(c('y', 'x_1', 'x_2'))

# Use the caret package to partition into a test and training set of equal size. Compare the RMSE when using just x_1, just x_2, and both x_1 and x_2.
# Compare the results from q6 and q8. What can you conclude?
# Unless we include all predictors we have no predictive power.
# Adding extra predictors improves RMSE regardless of whether the added predictors are correlated with other predictors or not.
# Adding extra predictors results in over fitting.
# Adding extra predictors can improve RMSE substantially, but not when the added predictors are highly correlated with other predictors.   <-*


set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

# -------       -------       -------       -------       -------       -------

# Regression for a Categorical Outcome
# All right let's continue with regression.  The regression approach can also be
#    applied to categorical data.  To illustrate this, we will apply it to our
#    previous example of predicting sex, male or female, using heights.

library( dslabs )
data( 'heights' )

y <- heights$height
set.seed( 2 )
test_index <- createDataPartition( y,
                                   times = 1,
                                   p = 0.5,
                                   list = FALSE )
train_set <- heights %>% slice( -test_index )
test_set <- heights %>% slice( test_index )

# If we define the outcome Y as 1 for females or 0 for males and X as the height,
#    in this case, we are interested in the conditional probability of being female
#    given the height.

# Y = 1   -> Females
# Y = 0   -> Males
# Pr( Y = 1 | X = x )   -> Probability of being female given the height

# As an example, let's provide a prediction for a student that is 66 inches tall.
# What is the condition of probably of being female if you're 66 inches tall?
#     In our dataset, we can estimate this by rounding to the nearest inch and
#     computing the average for these values.  You can do that using this code,
#     and you find that the conditional probability is 24%.

train_set %>%
    filter( round( height ) == 66 ) %>%
    summarize( mean( sex == 'Female' ) )

# Let's see what it looks like for several values of X.  We'll repeat the same
#    exercise but for 60 inches, 61 inches, et cetera. Note that we're removing
#    X values for which we have very few data points.  And if we do this, we get
#    the following results.
# Since the results from this plot look close to linear, and it is the only
#    approach we currently know, we will try regression.  We assume that the
#    conditional probability of Y being equals 1 given X is a line-- intercept
#    plus slope times height.  If we convert the factors to 0s and 1s, we can
#    estimate beta 0 and beta 1 with least squares using this piece of code.

lm_fit <- mutate( train_set,
                  y = as.numeric( sex == 'Female' ) ) %>%
    lm( y ~ height, data = . )
lm_fit


# Once we have the estimates, we can obtain an actual prediction.  Our estimate
#    of the conditional probability is going to be beta 0 hat plus beta 1 hat
#    times x.
# To form a prediction, we define a decision rule.  We predict female if the
#    conditional probability is bigger than 50%.  Now we can use the confusion
#    matrix function to see how we did. We see that we got an accuracy of 78.5%.

p_hat <- predict( lm_fit, test_set )
y_hat <- ifelse( p_hat > 0.5, 'Female', 'Male' ) %>% factor()
confusionMatrix( y_hat, test_set$sex )

# -------       -------       -------       -------       -------       -------

# Logistic Regression
# Note that the function beta 0 plus beta 1x can take any value, including negatives
#    and values larger than 1.  In fact, the estimate that we obtained for our
#    conditional probability using linear regression goes from negative 0.4 to
#    1.12.  But we're estimating a probability that's between 0 and 1.
# So can we avoid this?
#     Logistic regression is an extension of linear regression that assures us
#     the estimate of the conditional probability is, in fact, between 0 and 1.
# This approach makes use of the logistic transformation introduced in the data
#    visualization course, which you can see here.

#  g( p ) = log ( p / ( 1 - p ) )

# The logistic transformation converts probabilities to log odds.
# As discussed in the data visualization course, the odds tells us how much more
#    likely something will happen compared to not happen.
# So if p is equal to 0.5, this means that the odds are 1 to 1.  Thus, the odds
#    are 1.  If p is 0.75, the odds are 3 to 1.
# A nice characteristic of this transformation is that it transforms probabilities
#    to be symmetric around 0.  Here's a plot of the logistic transformation versus
#    the probability.
# Now, how do we fit this model?
# We can no longer use least squares.
# Instead, we compute something called the maximum likelihood estimate.  You can
#    learn more about this concept in a statistical theory textbook.  In R, we
#    can fit the logistic regression model with the function GLM, which stands
#    for Generalized Linear Models.  This function is more general than logistic
#    regression, so we need to specify the model we want.  We do this through the
#    family parameter.  Here's the code that fits a logistic regression model to
#    our data.

glm_fit <- train_set %>%
    mutate( y = as.numeric( sex == 'Female' ) ) %>%
    glm( y ~ height,
         data = .,
         family = 'binomial' )
glm_fit

# Just like with linear regression, we can obtain predictions using the predict
#    function.  However, once we read the help file predict.glm, we realize
#    that when using predict with a GLM object, we have to specify that we want
#    type equals response if we want the conditional probabilities.

p_hat_logit <- predict( glm_fit,
                        newdata =  test_set,
                        type = 'response' )
p_hat_logit

# This is because the default is to return the logistic transform values.  Now
#    that we've done it, we can see how well our model fit.  Note that this model
#    fits the data slightly better than the line.  Because we have an estimate of
#    the conditional probability, we can obtain predictions using code like this.

y_hat_logit <- ifelse( p_hat_logit > 0.5,
                       'Female',
                       'Male' ) %>%
    factor()
confusionMatrix( y_hat_logit,
                 test_set$sex )

# And once we look at the confusion matrix, we see that our accuracy has increased
#    slightly to about 80%. Note that the resulting predictions are similar.
# This is because the two estimates of our conditional probability are larger than
#    a half in roughly the same regions.  You can see that in this plot.
# Both linear and logistic regression provide an estimate for the conditional
#    expectation, which, in the case of binary data, is equivalent to a conditional
#    probability.  So we can use it in machine learning applications.  However,
#    once we move on to more complex examples, we will see that linear regression
#    and logistic regression are limited and not flexible enough to be useful.
# The techniques we will learn are essentially approaches to estimating conditional
#    probabilities or conditional expectations in ways that are more flexible.

# -------       -------       -------       -------       -------       -------

# Case Study> 2 or 7
#  In the simple examples we've examined up to now, we only had one predictor.
# We actually do not consider these machine learning challenges, which are
#    characterized by having many predictors.  So let's go back to the digit
#    example, in which we had 784 predictors.
# However, for elicitive purpose, we would look at a subset of this data set,
#    where we only have two predictors and two categories.
# We want to build an algorithm that can determine if a digit is a two or a seven
#    from the two predictors.  We're not quite ready to build an algorithm with
#    784 predictors.  So we will extract two simple predictors from the 784.
# These will be the proportion of dark pixels that are in the upper left quadrant
#    and the proportion of pixels that are black in the lower right quadrant.
# To have a more manageable data set, we will select a random sample of 1,000
#    digits from the training set that has 60,000 digits.  500 will be in the
#    training set, and 500 will be in the test set.  We actually include these
#    examples in the DS Lab package.  And you can load it using this line of code.

library( dslabs )
data( 'mnist_27' )
head( mnist_27, 2 )
str( mnist_27 )

# We can explore this data by plotting the two predictors and use colors to denote
#    the labels.  You can see them here.

mnist_27$train %>%
    mutate( y = factor( y ) ) %>%
    ggplot( aes( x_1,
                 x_2,
                 fill = y,
                 color = y ) ) +
    geom_point()


mnist_27$train %>%
    mutate( y = factor( y ) ) %>%
    ggplot( aes( x_1,
                 x_2,
                 fill = y,
                 color = y ) ) +
    geom_point() +
    stat_ellipse( type = 'norm',
                  lwd = 1.5 )


# We can immediately see some patterns.  For example, if x1, the first predictor,
#    which represents the upper left panel, is large, then the digit is probably
#    a seven.  Also, for smaller values of the second predictor, the lower right
#    panel, the twos appear to be in the mid-range values.  To connect these
#    insights to the original data, let's look at the images of the digits with
#    the largest and smallest values of x1.  Here are the images.
# This makes a lot of sense. The image on the left, which is a seven, has a lot
#    of dark in the upper left quadrant.  So x1 is big.  The digit on the right,
#    which is a two, has no black on the upper left quadrant.
# So x1 is small. OK.  Now let's look at the original images corresponding to the
#    largest and smallest values of the second predictor, x2, which represents
#    the lower right quadrant.
# Here we see that they're both sevens.  The seven on the left has a lot of black
#    on the lower right quadrant.  The seven on the right has very little black
#    on the lower right quadrant.  So we can start getting a sense for why these
#    predictors are informative, but also why the problem will be somewhat
#    challenging.  So let's try building a machine learning algorithm with what
#    we have.  We haven't really learned any algorithm yet.  So let's start with
#    logistic regression.  The model will be simply like this.

# p( x1, x2 ) = Pr( Y = 1 | X1 = x1, X2 = x2 )
#             = g^-1( Beta0 + Beta1 x1 + B2 x2 )
# with g^-1 the inverse of the lofistic function:
#    g^-1( x ) = exp( x ) / [ 1 + exp(x ) ]

fit <- glm( y ~ x_1 + x_2,
            data = mnist_27$train,
            family = 'binomial' )
fit

# The conditional probability of being a seven given the two predictors x1 and x2
#    will be a linear function of x1 and x2 after the logistic transformation.
# We can fit it using the glm function like this.  And now we can build a decision
#    rule based on the estimate of the conditional probability.
# Whenever it is bigger than 0.5, we predict a seven.  Whenever it's not, we predict
#    a two.  So we write this code.

p_hat <- predict( fit, newdata = mnist_27$test )
y_hat <- factor( ifelse( p_hat > 0.5, 7, 2 ) )
confusionMatrix( data = y_hat,
                 reference = mnist_27$test$y )

# Then we compute the confusion matrix, and we see that we achieve an accuracy of
#    79%. Not bad for our first try.
# But can we do better?
# Now before we continue, I want to point out that, for this particular data set,
# I know the true conditional probability.  This is because I constructed this
#    example using the entire set of 60,000 digits.  I use this to build the true
#    conditional probability p of x1, x2.  Now note that this is something we don't
#    have access to in practice, but included here in this example because it lets
#    us compare estimates to our true conditional probabilities.
# And this teaches us the limitations of the different algorithms.  So let's do
#    that here.  We can access and plot the true conditional probability.  We can
#    use this code.

mnist_27$true_p %>%
    ggplot( aes( x_1, x_2, fill = p ) ) +
    geom_raster()

# And it looks like this.  We will improve this plot by choosing better colors.
# And we'll also draw a curve that separates the pairs, x1, x2, for which the
#    conditional probably is bigger than 0.5 and lower than 0.5.  We use this code.

mnist_27$true_p %>%
    ggplot( aes( x_1, x_2, z =p, fill = p ) ) +
    geom_raster() +
    scale_fill_gradientn( colors = c( '#f8766d', 'white', '#00bfc4') ) +
    stat_contour( breaks = c( 0.5 ), color = 'black' )

# And now the plot looks like this. So we can see the true conditional probability.
#    So to start understanding the limitations of logistic regression, we can
#    compare the true conditional probability to the estimated conditional
#    probability.  Let's compute the boundary that divides the values of x1 and x2
#    that make the estimated conditional probably lower than 0.5 and larger than 0.5.# So at this boundary, the conditional probability is going to be equal to 0.5.
# Now we can do a little bit of math, shown here.  And if we do this, we will see
#    that the boundary can't be anything other than a straight line, which implies
#    that our logistic regression approach has no chance of capturing the non-linear
#    nature of our true conditional probability.
# You can see that the boundary of the true conditional probability is a curve.
# Now to see where the mistakes were made, we can again plot the test data with
#    x1 and x2 plotted against each other and color used to show the label.
# If we do this, we can see where the mistakes are made.  Because logistic
#    regression divides the sevens and the twos with a line, we will miss several
#    points that can't be captured by this shape.  So we need something more
#    flexible.  Logistic regression forces our estimates to be a plane and our
#    boundary to be a line.
# We need a method that permits other shapes.  We will start by describing the
#    nearest neighbor algorithm and some kernel approaches.  To introduce the
#    concepts behind these approaches, we will again start with a simple one
#    -dimensional example and describe the concept of smoothing.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Logistic Regression
# Q1
# Define a dataset using the following code:
#
set.seed(2)
make_data <- function(n = 1000, p = 0.5,
                      mu_0 = 0, mu_1 = 2,
                      sigma_0 = 1,  sigma_1 = 1){

    y <- rbinom(n, 1, p)
    f_0 <- rnorm(n, mu_0, sigma_0)
    f_1 <- rnorm(n, mu_1, sigma_1)
    x <- ifelse(y == 1, f_1, f_0)

    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

    list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
         test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
str( dat )

# Note that we have defined a variable x that is predictive of a binary outcome y:

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# Generate 25 different datasets changing the difference between the two classes using

delta <- seq(0, 3, len=25)
delta

# and plot accuracy vs mu_1.

set.seed(1)
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
    dat <- make_data(mu_1 = d)
    fit_glm <- dat$train %>% glm(y ~ x, family = 'binomial', data = .)
    y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
    mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)


# Which is the correct plot?
# Res (y-axis) vs delta (x-axis). Roughly linear increase in delta as res increases.   <-*
# Res (y-axis) vs delta (x-axis). Roughly linear decrease in delta as res increases.
# Res (y-axis) vs delta (x-axis). No correlation between res and delta.
# Res (y-axis) vs delta (x-axis). Res is 0.5 across range of delta.
# unanswered

# -------       -------       -------       -------       -------       -------

# Smoothing
# Before continuing with machine learning algorithms, we introduce the important
#    concept of smoothing.  Smoothing is a very powerful technique used all across
#    data analysis. Other names given to this technique are curve fitting and low
#    band pass filtering.  It's designed to detect trends in the presence of noisy
#    data in cases in which the shape of the trend is unknown.
# The smoothing name comes from the fact that to accomplish this feat we assume
#    that the trend is smooth, as in a smooth surface, and the noise is unpredictably
#    wobbly.  Something like this.
# Part of what we explain here are the assumptions that permit us to extract a
#    trend from the noise.  To understand why we cover this topic, note that the
#    concepts behind smoothing techniques are extremely useful in machine learning
#    because conditional expectations and probabilities can be thought of as trends
#    of unknown shapes that we need to estimate in the presence of uncertainty.
# To explain these concepts, we will focus first on a problem with just one
#    predictor.  Specifically we try to estimate the time trend in the popular vote
#    from the 2008 election, the difference between Obama and McCain.  You can
#    load the data like this and we can see a plot here.

data( 'polls_2008' )
qplot( day, margin, data = polls_2008 )

# For the purposes of this example, do not think of it as a forecasting problem.
# We're simply interested in learning the shape of the trend after collecting all
#    the data. We assume that for any given day x, there's a true preference among
#    the electorate, f of x, but due to the uncertainty introduced by polling,
#    each data point comes with an error, epsilon.  A mathematical model for the
#    observed poll margin, y, is y equals f of x plus epsilon.

# Yi = f( xi ) + Epsiloni

# To think of this as a machine learning problem, consider that we want to predict
#    y given the day x.  And that if we knew it, we would use the conditional
#    expectation, f of x equals expectation of y given x.

# f( x ) = E( Y | X = x )

# But we don't know it, so we have to estimate it.  We're going to start by using
#    regression, the only method we know, to see how it does.

resid <- ifelse( lm( margin ~ day, data = polls_2008 )$resid > 0,
                 '+',
                 '-')
polls_2008 %>%
    mutate( resid = resid ) %>%
    ggplot( aes( day, margin ) ) +
    geom_smooth( method = 'lm',
                 se = FALSE,
                 color = 'black' ) +
    geom_point( aes( color = resid ), size = 3 )

# The line we see does not appear to describe the trend very well.
# Note, for example, that on September 4, this is day negative 62, 62 days until
#    Election Day, the Republican Convention was held.  This appeared to give
#    McCain a boost in the polls, which can be clearly seen in the data.
# The regression line does not capture this. To further see the lack of fit, we
#    note that points above the fitted line, blue, and those below, red, are not
#    evenly distributed. We therefore need an alternative, a more flexible approach.

# -------       -------       -------       -------       -------       -------

# Bin Smoothing and Kernels
# The general idea of bin smoothing is to group data points into strata in which
#    the value of f of x can be assumed to be constant.  We can make this assumption
#    because we think f of x changes slowly.  And as a result, f of x is almost
#    constant in small windows of time.  An example of this idea is to assume for
#    the poll data that public opinion remain approximately the same within a
#    week's time.  With this assumption in place, we have several data points
#    with the same expected value.  So if we fix a day to be in the center of our
#    week-- call it x0--then for any day x, such that the absolute value of x
#    minus x0 is less than 3.5, we assume that f of x is a constant.  Let's call
#    it mu.

# for any other day x, that | x- x0 | <= 3.5, we assume f(x) is a constant
# f( x ) =  mu

# This assumption implies that the expected value of y given x is
#    approximately mu when the distance between x i and x0 is less than 3.5.

# E[ Yi | Xi = xi ] = mu,   if | xi - x0 | <= 3.5

# In smoothing, we call the size of the interval satisfying the condition the
#    distance between x i and x0 is less than 3.5 the window size, the bandwidth,
#    or the span.  These are all synonyms.
# Now, this assumption implies that a good estimate for f of x is the average of
#    the y values in the window.  If we define A0 to be the set of indexes i such
#    that x i minus x0 is less than 3.5 in absolute value and N0 as the number of
#    indexes in A0, then our estimate is given by this formula.  It's just simply
#    the average in the window.
# The idea behind bin smoothing is to make this calculation for each value of x.
# So we make each value of x the center and recompute that average.  So in the
#    poll example, for each day, we would compute the average for the values within
#    a week of the day that we're considering.  Let's look at two examples.
# Let's set the center at negative 125 and then also set it at negative 55.
# Here's what the data looks like.  The black points are the points that are used
#    to compute the average at those two points.  The blue line represents the
#    average that was computed.  By computing this average for every point, we
#    form an estimate of the underlying curve f of x.  In this animation, we see
#    the procedure happening, starting at negative 155 all the way up to election
#    day-- day 0.  At each value of x0, we keep the estimate f hat of x0 and move
#    on to the next point.
# The final result, which you can compute using this code, looks like this.

span <- 7
fit <- with( polls_2008,
             ksmooth( day,
                      margin,
                      x.points = day,
                      kernel = 'box',
                      bandwidth = span ) )
polls_2008 %>%
    mutate( smooth = fit$y ) %>%
    ggplot( aes( day, margin ) ) +
    geom_point( size = 3,
                alpha = .5,
                color = 'grey' ) +
    geom_line( aes( day, smooth ), color = 'red' )

# Note that the final result for the bin smoother is quite wiggly.  One reason
#    for this is that each time the window moves, two points change.  So if we
#    start with seven points and change two, that's a substantial percentage of
#    points that are changing.
# We can attenuate this somewhat by taking weighted averages that give the center
#    of a point more weight than those that are far away from the center, with
#    the two points at the edges receiving very little weight.
# We call the functions from which we compute these weights the kernel.
# Note that you can think of the bin smoother as an approach that uses a kernel.
# The formula looks like this. Each point receives a weight, in the case of bin
#    smoothers, between 0 for points that are outside the window and 1 divided by
#    N0 for points inside the window, with N0 the number of points in that week.
# In the code we showed, we told the function k-smooth to use the kernel 'box.'
# That is because the kernel that gives us bin smoother using this formulation
# looks like a box. Here's a picture.
# Now, the k-smooth function provides a way to obtain a smoother estimate.  This
#    is by using the normal or Gaussian density to assign weights.  So the kernel
#    will be the normal density, which we can see here.  In this animation, the
#    size of the points represent the weights they get in the weighted average.
# You can see that the points near the edges receive little weight.  The final
#    result, which you can get using this code, looks like this.

span <- 7
fit <- with( polls_2008,
             ksmooth( day,
                      margin,
                      x.points = day,day,
                      kernel = 'normal',
                      bandwidth = span ) )
polls_2008 %>%
    mutate( smooth = fit$y ) %>%
    ggplot( aes( day, margin ) ) +
    geom_point( size = 3, alpha = .5, color = 'grey' ) +
    geom_line( aes( day, smooth ), color = 'red' )

# Note that the final estimate looks smoother now.  Now, there are several functions
#    in R that implement bin smoothers or kernel approaches.
# One example, the one we showed, is k-smooth.  However, in practice, we typically
#    prefer methods that use slightly more complex models than fitting a constant.
# The final result that we saw for the smooth bin smoother is still somewhat wiggly.
# You can see in some parts-- for example, from negative 125 to negative 75,
#    we see that the function is more wiggly than we really expect.  We're now
#    going to learn about approaches that improve on this.

# -------       -------       -------       -------       -------       -------

# Local Weighted Regression (loess)
# A limitation of the bin smoother approach we just described is that we need small
#    windows for the approximately constant assumption to hold.  As a result, we
#    end up with a small number of data points to average.  And as a result of
#    this, we obtain imprecise estimates of our trend.  Here, we describe how
#    local weighted regression or loess permits us to consider larger windows.
# To do this, we will use a mathematical result referred to as Taylor's theorem,
#    which tells us that if you look close enough at any smooth function f, it
#    looks like a line.
# To see why this makes sense, consider the curved edges gardeners make.  They
#    make these using spades which are straight lines so they can generate curves
#    that are locally straight lines.  So instead of assuming the function is
#    approximately constant in a window, we assume the function is locally linear.
# With the linear assumption, we can consider larger window sizes than with a
#    constant.  So instead of the one-week window, we will instead consider a
#    larger window in which the trend is approximately linear.  We start with a
#    three-week window and later consider enabling other options.
# So the model for points that are in a three-week window looks like this.

# E[ Yi | Xi = xi ] = Beta0 + Beta1 ( xi  -x0 ),  if | xi - x0 | <= 10.5

# We assume that Y given X in that window is a line.  Now, for every point x0
#    loess defines a window and then fits a line within that window.  So here's
#    an example of those fits for x0 equals negative 125 and x0 equals negative
#    55.  The fitted values at x0 become our estimate of the trend.  In this
#    animation, we demonstrate the idea.  The final result is a smoother fit than
#    the bin smoother since we used larger sample sizes to estimate our local
#    parameters.  You can get the final estimate using this code, and it looks
#    like this.

total_days <- diff( range( polls_2008$day ) )
span <- 21 / total_days

fit <- loess( margin ~ day,
              degree = 1,
              span = span,
              data = polls_2008 )

polls_2008 %>%
    mutate( smooth = fit$fitted ) %>%
    ggplot( aes( day, margin ) ) +
    geom_point( size = 3, alpha = 0.5, color = 'grey' ) +
    geom_line( aes( day, smooth ), color = 'red' )

# Now, note that different spans give us different estimates.  We can see how
#    different window sizes lead to different estimates with this animation.
# Here are the final estimates.  We can see that with 0.1 the line is quite wiggly.
# With 0.15, it's slightly less.  Now, with 0.25, we get a rather smooth estimate.
# And with 0.66, it almost looks like a straight line.  There are three other
#    differences between loess and the typical bin smoother which we describe here.
#    The first is that rather than keeping the bin size the same, loess keeps the
#    number of points used in the local fit the same.  This number is controlled
#    via the span argument which expects a proportion.  So for example, if N is
#    a number of data points, and the span is 0.5, then for any given X, loess
#    will use 0.5 times N closest points to X for the fit.
# Another difference is that when fitting a line locally, loess uses a weighted
#    approach.  Basically, instead of least squares, we minimize a weighted version.
#    So we would minimize this equation.  However, instead of the Gaussian kernel,
#    loess uses a function called the Tukey tri-weight which you can see here.
# And to define weights, we use this formula.  The kernel for the tri-weight looks
#    like this.  The third difference is that loess has the option of fitting the
#    local model robustly.  An iterative algorithm is implemented in which, after
#    fitting a model in one iteration, outliers are detected and down-weighted
#    for the next iteration.
# To use this option, use the argument family equals symmetric.  One more important
#    point about loess.  Taylor's theorem also tells us that if you look at a
#    function close enough, it looks like a parabola and that you don't have to
#    look as close as you do for the linear approximation.
# This means we can make our windows even larger and fit parabolas instead of
#    lines, so the local model would look like this.  This is actually the default
#    procedure for the function loess.  You may have noticed that when we show
#    the code for loess, we set a parameter degree equals 1.
# This tells loess to fit polynomials of degree 1, a fancy name for lines.  If you
#    read the help page for loess, you'll see that the argument degree defaults
#    to 2.  So by default, loess fits parabolas not lines.  Here is a comparison
#    of fitting lines, the red dashed, and fitting parabolas, the orange solid.

total_days <- diff( range( polls_2008$day ) )
span <- 28 / total_days

fit_1 <- loess( margin ~ day,
                degree =  1,
                span = span,
                data = polls_2008 )
fit_2 <- loess( margin ~ day,
                span = span,
                data = polls_2008 )

polls_2008 %>%
    mutate( smooth_1 = fit_1$fitted,
            smooth_2 = fit_2$fitted ) %>%
    ggplot( aes( day, margin ) ) +
    geom_point( size = 3, alpha = 0.5, color = 'grey' ) +
    geom_line( aes( day, smooth_1 ), color = 'red', lty = 2 ) +
    geom_line( aes( day, smooth_2 ), color = 'orange', lty = 1 )

# Notice that degree equals 2 gives us a more wiggly result.  I personally prefer
#    degree equals 1 as it is less prone to this kind of noise.  Now, one final
#    note. This relates to ggplot. Note that ggplot uses loess and the geom smooth
#    function.  So if you type this, you get your points and fitted loess line.

polls_2008 %>%
    ggplot( aes( day, margin ) ) +
    geom_point() +
    geom_smooth()

# But be careful with the default table, as they are rarely optimal.  However,
#    you can change these quite easily as is demonstrated in this code, and now
#    we get a better fit.

polls_2008 %>%
    ggplot( aes( day, margin ) ) +
    geom_point() +
    geom_smooth( color = 'red',
                 span = 0.15,
                 metho.args = list( degree = 1 ) )


# -------       -------       -------       -------       -------       -------

# Comprehension Check: Smoothing
# Q1
# In the Wrangling course of this series, PH125.6x, we used the following code to obtain mortality counts for Puerto Rico for 2015-2018:

library( tidyverse )
library( purrr )
library( pdftools )
library( dslabs )


fn <- system.file( 'extdata',
                   'RD-Mortality-Report_2015-18-180531.pdf',
                   package = 'dslabs' )
fn <- pdf_text( './RD-Mortality-Report_2015-18-180531.pdf' )
fn
dat <- map_df( str_split( pdf_text( fn ),
                          '\n'),
               function( s ) {
                   s <- str_trim( s )
                   header_index <- str_which( s, '2015' )[ 1 ]
                   tmp <- str_split( s[ header_index ],
                                     '\\s+',
                                     simplify = TRUE )
                   month <- tmp[ 1 ]
                   header <- tmp[ -1 ]
                   tail_index  <- str_which( s, 'Total' )
                   n <- str_count( s, '\\d+' )
                   out <- c( 1:header_index,
                             which( n == 1 ),
                             which( n >= 28),
                             tail_index:length( s ) )
                   s[ -out ] %>%
                       str_remove_all( '[^\\d\\s]' ) %>%
                       str_trim() %>%
                       str_split_fixed( '\\s+', n = 6 ) %>%
                       .[ ,1:5 ] %>%
                       as_data_frame() %>%
                       setNames( c( 'day', header ) ) %>%
                       mutate( month = month,
                               day = as.numeric( day ) ) %>%
                       gather( year,
                               deaths,
                               -c( day, month ) ) %>%
                       mutate( deaths = as.numeric( deaths ) )
                   } ) %>%
    mutate( month = recode( month,
                            'JAN' = 1,
                            'FEB' = 2,
                            'MAR' = 3,
                            'APR' = 4,
                            'MAY' = 5,
                            'JUN' = 6,
                            'JUL' = 7,
                            'AGO' = 8,
                            'SEP' = 9,
                            'OCT' = 10,
                            'NOV' = 11,
                            'DEC' = 12 ) ) %>%
    mutate( date = make_date( year, month, day ) ) %>%
    filter( date <= '2018-05-01' )
dat

# *---------*
library(tidyverse)
library(stringr)
library(lubridate)
library(ggforce)
library(pdftools)

# note this is heavily borrowed from the original paper Kishore et al. 2018. Mortality in Puerto Rico after Hurricane Maria.  N Engl J Med 2018; 379:162-170 DOI: 10.1056/NEJMsa1803972 https://www.nejm.org/doi/full/10.1056/NEJMsa1803972

filename <- 'https://github.com/c2-d2/pr_mort_official/raw/master/data/RD-Mortality-Report_2015-18-180531.pdf'
txt <- pdf_text(filename)
dat <- lapply(seq_along(txt), function(i){
    s <- str_replace_all(txt[i], '\\*.*', '') %>%
        str_replace_all('Fuente: Registro Demográfico - División de Calidad y Estadísticas Vitales', '') %>%
        str_replace_all('Y(201\\d)\\*?', '\\1') %>%
        str_replace('SEP', '9') %>%
        str_replace('OCT', '10') %>%
        str_replace('NOV', '11') %>%
        str_replace('DEC', '12') %>%
        str_replace('JAN', '1') %>%
        str_replace('FEB', '2') %>%
        str_replace('MAR', '3') %>%
        str_replace('APR', '4') %>%
        str_replace('MAY', '5') %>%
        str_replace('JUN', '6') %>%
        str_replace('JUL', '7') %>%
        str_replace('AGO', '8') %>%
        str_replace('Total', '@')

    tmp <- str_split(s, '\n') %>% .[[1]] %>% str_trim %>% str_split_fixed('\\s+', 50) %>% .[,1:5] %>% as_tibble()
    colnames(tmp) <- tmp[2,]
    tmp <- tmp[-(1:2),]
    j <- which(tmp[,1]=='@')
    if(colnames(tmp)[1]=='2') { ## deal with february 29
        k <- which(tmp==29)
        the_number <- unlist(tmp[k,-1])
        the_number <- the_number[the_number!='']
        tmp[k, colnames(tmp)!='2016' & colnames(tmp)!='2'] <- 0
        tmp[k, '2016'] <- the_number
    }
    tmp <- tmp %>% slice(1:(j-1)) %>% mutate_all(funs(as.numeric)) %>%
        filter(!is.na(`2015`) & !is.na(`2016`) & !is.na(`2017`)  & !is.na(`2017`))
    tmp <- mutate(tmp, month = as.numeric(names(tmp)[1]))
    names(tmp)[1] <- 'day'
    tmp <- tmp[,c(6,1,2,3,4,5)]
    ones <- which(tmp$day==1) ##1 2 3 4 appears due to graph... let's take it out
    if(length(ones)>1) tmp <- tmp[-ones[-1],]
    if(any(diff(tmp$day)!=1)) stop(i) ## check days are ordered
    ##check if negative. this means a black was left and the diff between 2016 and 0 was used!
    tmp[tmp<0] <- NA
    gather(tmp, year, deaths, 3:6, convert = TRUE)
})
official <- do.call(rbind, dat) %>%
    arrange(year, month, day) %>%
    filter(!(month==2 & day==29 & year != 2016))
## add date
official <-official %>% mutate(date = ymd(paste(year, month, day,'-')))


official <- official %>%
    mutate(ymd = ymd(paste0('2000-',str_sub(as.character(official$date),-5)))) %>%
    mutate(deaths = ifelse(deaths != 0, deaths,NA))


official %>%
    filter(year < 2018) %>%
    ggplot(aes(ymd, deaths, color = factor(year)) )+
    geom_point(alpha = 0.3) +
    geom_smooth(method='loess', span = .1, formula = y ~ x) +
    geom_vline(xintercept = make_date(2017,09,20), lty=2, col='grey') +
    scale_x_date(date_labels = '%b', date_breaks = '1 months')  +
    theme_light() +
    annotate(geom = 'text', x = ymd('2000-09-16'), y = 100, label = 'deaths from maria')
# *---------*


# Use the loess function to obtain a smooth estimate of the expected number of deaths as a function of date. Plot this resulting smooth function. Make the span about two months long.

span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
    ggplot() +
    geom_point(aes(date, deaths)) +
    geom_line(aes(date, smooth), lwd = 2, col = 2)




span <- 60 / as.numeric(diff(range(official$date)))
fit <- official %>%
    mutate(x = as.numeric(date)) %>%
    loess(deaths ~ x, data = ., span = span, degree = 1)
official %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
    ggplot() +
    geom_point(aes(date, deaths)) +
    geom_line(aes(date, smooth), lwd = 2, col = 2)


# Which of the following plots is correct?
# Deaths (y-axis) vs date (x-axis). Dots in black with peaks at the beginning of each year. Red line with trend showing modest bumps corresponding to the peaks.   <-*
# Deaths (y-axis) vs date (x-axis). Dots in black with peaks at the beginning of each year. Red line with trend showing sharp, jagged spikes corresponding to the peaks.
# Deaths (y-axis) vs date (x-axis). Dots in black with peaks at the beginning of each year. Red line with trend showing modest linear increase with no relationship to the peaks.
# Deaths (y-axis) vs date (x-axis). Dots in black with peaks at the beginning of each year. Red line with trend showing very small bumps corresponding to the peaks.

# Q2
# Work with the same data as in Q1 to plot smooth estimates against day of the year, all on the same plot, but with different colors for each year.
#
# Which code produces the desired plot?

official %>%
    mutate(smooth = predict(fit),
           day = yday(date),
           year = as.character(year(date))) %>%
    ggplot(aes(day, smooth, col = year)) +
    geom_line(lwd = 2)

official %>%
    mutate(smooth = predict(fit, as.numeric(date)), day = mday(date), year = as.character(year(date))) %>%
    ggplot(aes(day, smooth, col = year)) +
    geom_line(lwd = 2)

official %>%
    mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
    ggplot(aes(day, smooth)) +
    geom_line(lwd = 2)

official %>%
    mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
    ggplot(aes(day, smooth, col = year)) +
    geom_line(lwd = 2)

#
# dat %>%
#     mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
#     ggplot(aes(day, smooth, col = year)) +
#     geom_line(lwd = 2)
#
# dat %>%
#     mutate(smooth = predict(fit, as.numeric(date)), day = mday(date), year = as.character(year(date))) %>%
#     ggplot(aes(day, smooth, col = year)) +
#     geom_line(lwd = 2)
#
# dat %>%
#     mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
#     ggplot(aes(day, smooth)) +
#     geom_line(lwd = 2)
#
# dat %>%
#     mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
#     ggplot(aes(day, smooth, col = year)) +
#     geom_line(lwd = 2)    <-*

# Q3
# Suppose we want to predict 2s and 7s in the mnist_27 dataset with just the second covariate. Can we do this? On first inspection it appears the data does not have much predictive power.
#
# In fact, if we fit a regular logistic regression the coefficient for x_2 is not significant!
#
# This can be seen using this code:

library( broom )
mnist_27$train %>%
    glm( y ~ x_2,
         family = 'binomial',
         data = .) %>%
    tidy()

# Plotting a scatterplot here is not useful since y is binary:

qplot( x_2,
       y,
       data = mnist_27$train )

# Fit a loess line to the data above and plot the results. What do you observe?
mnist_27$train %>%
    mutate( y = ifelse( y == '7', 1, 0 ) ) %>%
    ggplot( aes( x_2, y ) ) +
    geom_smooth(method = 'loess' )

# There is no predictive power and the conditional probability is linear.
# There is no predictive power and the conditional probability is non-linear.   <-*
# There is predictive power and the conditional probability is linear.
# There is predictive power and the conditional probability is non-linear.

# -------       -------       -------       -------       -------       -------

# Working with Matrices
# In machine learning, situations in which all predictors are numeric, or can
#    be converted to numeric in a meaningful way, are common.  The digits data
#    set is an example.  Every pixel records a number between 0 and 255.  We can
#    actually load the 60,000 digits using this code.

mnist <- read_mnist()
head( mnist )

# In these cases, it is often convenient to save the predictors in a matrix and
#    the outcomes in a vector rather than using a data frame.  In fact, we can
#    see that the data set that we just downloaded does this.  You can see that
#    the training data image is a matrix by typing this code.

class( mnist$train$images )

# This matrix represents 60,000 digits.  It's a pretty big matrix.  So for the
#    example, in this video, we'll take a more manageable subset.  We will take
#    the first 1,000 predictors and the first 1,000 labels, which we can do using
#    this code.

x <- mnist$train$images[ 1:1000, ]
y <- mnist$train$labels[ 1:1000 ]

# In machine learning, the main reason for using matrices is that certain
#    mathematical operations needed to develop efficient code can be performed
#    using techniques from a branch of mathematics called linear algebra.
#    In fact, linear algebra and matrix notation are key elements of the language
#    used in academic papers describing machine learning techniques.
# We will not cover linear algebra in detail here, but we'll demonstrate how to
#    use matrices in R, so that you can apply the linear algebra techniques already
#    implemented in R Base and other packages.
# To motivate the use of matrices, we will pose five challenges.
# First, we're going to study the distribution of the total pixel darkness and
#    how it varies by digits.
# Second, we're going to study the variation of each pixel and remove predictors,
#    columns, associated with pixels that don't change much and thus can't provide
#    much information for classification.
# Third, we're going to zero out low values that are likely smudges.
#    First, we're going to look at the distribution of all pixel values, use this
#    to pick a cutoff to define unwritten space, then make anything below that
#    cutoff a zero.
# Fourth, we're going to binarize the data.  We're going to first look at the
#    distribution of all pixel values, use this to pick a cutoff, and distinguish
#    between writing and no writing. Then convert all entries into either zero
#    or one.
# Then finally, we're going to scale each of the predictors in each entry to have
#    the same average and standard deviation.  To complete these, we'll have to
#    perform mathematical operations involving several variables.
# The tidal inverse is not developed to perform this type of mathematical operation.
# For this task, it is convenient to use matrices. Before we attack the challenges,
#    we will introduce matrix notation and basic R code to define and operate on
#    matrices.

# -------       -------       -------       -------       -------       -------

# Matrix Notation
#  In matrix algebra we have three main types of objects, scalars, vectors, and
#     matrices.  A scalar is just one number.  For example, a equals one, a is a
#     scalar.  To denote scalars in matrix notation, we usually use a lowercase
#     letter and we don't bold it.  Vectors are like the numeric vectors we define
#     in r.  They include several scalar entries.  For example, the column containing #     the first pixel is a vector.  It has length 1000.  Here is the code that
#     shows it.

length( x[ , 1 ] )

#  In matrix algebra we use the following notation to define vectors, like this.
# Similarly, we can use math notation to represent different features mathematically
#    by adding an index.  So here's x1, the first feature and x2, the second feature.
#    Both are vectors.  If we're writing out a column such as x1, in a sentence
#    we often use the notation x1 through xn and then we have the transpose symbol
#    t.  This transpose operation converts columns into rows and rows into columns.
# A matrix can be defined as a series of vectors of the same size joined together,
#    each forming a column.  So in our code, we can write it like this.

x_1 <- 1:5
x_2 <- 6:10
cbind( x_1, x_2 )

# Mathematically, we represent them with bold uppercase letters like this.  The
#    dimension of a matrix is often an important characteristic needed to assure
#    certain operations can be performed.  The dimension is a two number summary
#    defined as the number of rows and the number of columns.
# In r we can extract the dimensions of the matrix with the function dim like this.

dim( x )

# Note that vectors can be thought of as n by 1 matrices. However, in r, a vector
#    does not have dimensions.  You can see it by typing this.

dim( x_1 )

# However, we can explicitly convert a vector into a matrix using the as.matrix
#    function.  So if we do that, then we see that indeed this is a matrix that
#    is 5 by 1.  We can use this notation to denote an arbitrary number of predictors
#    with the following n by p matrix.

dim( as.matrix( x_1 ) )

# For example, if we have 784 columns we could do this.  P is 784, here's the
#    arbitrary matrix representing our data.  We store this into x.  So when you do
#    dim x, you can see it's 1000 by 784.

# -------       -------       -------       -------       -------       -------

# Converting a Vector to a Matrix
# We will learn several useful operations related to matrix algebra.
# We'll use some of the motivating examples we described in an earlier video to
#    demonstrate this.  It is often useful to convert a vector to a matrix.
#    For example, because the variables are pixels on a grid, we can convert the
#    rows of pixel intensities into a matrix representing this grid.  We can
#    convert a vector into a matrix with the matrix function and specifying the
#    number of rows and columns the resulting matrix should have.  The matrix is
#    filled by column.  The first column is filled first, and the second is filled
#    second, and so on.  So here's an example to illustrate what we mean.

my_vector <- 1:15
mat <- matrix( my_vector, 5, 3 )
mat

# If we define a vector, that's the numbers 1 through 15.  And then we use the
#    matrix function on this factor, and say it has five rows and three columns,
#    we end up with the following matrix.  We can fill in by row instead of by
#    column by using the byrow argument.  So, for example, to transpose the matrix
#    we just showed, we would use the matrix function like this.
# Now we have three rows, five columns, and we fill it in by row. Here's the code.

mat_t <- matrix( my_vector, 3, 5, byrow = TRUE )
mat_t

# This is essentially transposing the matrix.  In R, we can use the function t to
#    directly transpose a matrix.  Now notice that these two are the same.

identical( t( mat ), mat_t )

# An important warning.  The matrix function in R recycles values in the vector
#    without warnings.  If the product of columns and rows does not match the
#    length of the vector, this happens.

matrix( my_vector, 5, 5 )

# So look at what happens when I try to turn my vector, which has 15 entries,
#    into a 5 by 5 matrix.
# So how can we use this in practice?
#     Let's look at an example.
# To put the pixel intensities of, say, the third entry, which we know is a digit
#    that represents a 4, into a grid, we can use this.

grid <- matrix( x[ 3, ], 28, 28 )
grid

# To confirm that, in fact, we have done this correctly, we can use a function
#    image, which shows an image of the third argument.  Here's how we use it.

image( 1:28, 1:28, grid )

#    We can see that this looks like an upside down 4.  Now it looks upside down
#    because the top of this image, pixel one, is shown at the bottom.  This is
#    how R plots images.  So it's flipped.  If we want to flip it back, we can
#    use this code.

image( 1:28, 1:28, grid[ , 28:1 ] )

# And now we get an image that looks like a 4.

# -------       -------       -------       -------       -------       -------

# Row and Column Summaries and Apply
# OK.  So now let's start to attack the challenges that we posed earlier.  For
#    the first one which related to the total pixel darkness, we want to sum the
#    values of each row and then visualize how these values vary by digit.  The
#    function rowSums takes a matrix as input and computes the desired values.
# It takes the sum of each row.  So this little simple code does that very quickly.

sums <- rowSums( x )
sums

# We can also compute the averages with the function rowMeans like this.

avg <- rowMeans( x )
avg

# Once we have this, we can simply generate a box plot to see how the average
#    pixel intensity changes from digit to digit.  Here it is.
# From this plot, we see that, not surprisingly, ones use less ink than other
#    digits.  Note that we can also compute the column sums and averages using
#    the functions colSums and colMeans respectively.  The package matrixStats
#    adds functions that perform operations on each row or column very efficiently,
#    including the functions rowSds and colSds.
# Note that the functions just describe are performing an operation similar to
#    two functions that we've already learned, sapply and the per function map.
# They apply the same function to a part of our object.  In this case, either
#    each row or each column.  The apply function lets you apply any function,
#    not just sum or mean, to a matrix.
# The first argument of the apply function is the matrix.  The second is the
#    dimension that you want to apply the function to, one for rows, two for
#    columns.  And the third argument is the function.  So for example, rowMeans
#    can be written like this.

avgs <- apply( x, 1, mean )
avgs

# But note that just like sapply and map, we can perform any function.  So if we
#    wanted the standard deviation for each column, we could write this.

sds <- apply( x, 2, sd )
sds

# Now what you pay for in this flexibility is that these are not as fast as the
#    dedicated functions such as rowMeans, colMeans, etcetera.

# -------       -------       -------       -------       -------       -------

# Filtering Columns Based on Summaries
# All right.  Now let's turn to our second challenge.  Let's study the variation
#    of each pixel and remove columns associated with pixels that don't change
#    much, thus not informing the classification.  Although a simplistic approach,
#    we will quantify the variation of each pixel with its standard deviation
#    across all entries.  Since each column represents a pixel, we use the colSds
#    function from the matrix stats package like this.

library( matrixStats )
sds <- colSds( x )
sds

# A quick look at the distribution of these values shows that some pixels have very
#    low entry to entry variability.  This makes sense, since we don't write in
#    some parts of the box.  Here is the variance plotted by location.  We see
#    that there is little variation in the corners.  This makes sense.  We'd write
#    the digits in the center.  So we could remove features that have no variation
#    since these can't help us predict much.
# In the R basics course, we describe the operations used to extract columns.
# Here's an example showing the 351st and 352nd columns and the rows.

x[ , c( 351, 352 ) ]

# Here are the second and third rows.  We can also use logical indices to determine
#    which columns or rows to keep.  So if we wanted to remove uninformative
#    predictors from our Matrix, we could write this one line of code, like this.

new_x <- x[ , colSds( x ) > 60 ]
new_x
dim( new_x )

# Only the columns for which the standard deviation is above 60 are kept.  Here
#    we add an important warning related to subsetting matrices.  If you select
#    one column or one row, the result is no longer a matrix, but a vector.
# Here's an example.

class( x[ , 1 ] )
dim( x[ 1, ] )

# This could be a problem if you're assuming that operations on matrices will
#    result in matrices.  However, we can preserve the matrix class by using the
#    argument drop, like this.

class( x[ , 1, drop = FALSE ] )
dim( x[ , 1, drop = FALSE ] )

# -------       -------       -------       -------       -------       -------

# Indexing with Matrices and Binarizing the Data
# For our next challenge, we want to be able to look at a histogram of all our
#    pixels.  We already saw how we can turn vectors into matrices, but we can
#    also undo this and turn matrices into vectors.  Here's how it works.
# It's the function as vector.  Here's an example.

mat <- matrix( 1:15, 5, 3 )
mat

as.vector( mat )

# So to see a histogram of all our predictors, we can simply type this code.

qplot( as.vector( x ),
       bins = 30,
       color = I(' black' ) )

# When we look at this plot we see a clear dichotomy which is explained as parts
#    with ink and parts without ink. If we think that values below say, 25, are
#    smudges, we can quickly make them zero using this very simple code.

new_x <- x
new_x[ new_x < 50 ] <- 0
new_x

# To see what this does, let's look at a smaller matrix at a smaller example.
# Type this code and notice what happens.

mat <- matrix( 1:15, 5, 3 )
mat
mat[ mat < 3 ] <- 0
mat

# It changes all the values that are less than three to zero.  We can also use
#    more complicated logical operations with matrices like this.

mat <- matrix( 1:15, 5, 3 )
mat
mat[ mat > 6 & mat < 12 ] <- 0
mat

# Here's an example where we zero out all the values that are between 6 and 12.
# Now for the next challenge, we want a binarize the data.  The histogram we just
#    saw suggests that this data is mostly binary pixels, are either ink or no ink.
# Using what we've learned, we can binarize the data using just matrix operations.

bin_x <- x
bin_x[ bin_x < 255 / 2 ] <- 0
bin_x[ bin_x > 255 / 2 ] <- 1
bin_x

# For example, using this code we turn all the values below 255 divided by 2 to
#    0 and above it to 1.  But we can also convert it to a matrix using logicals
#    and coerce it into numbers like this.

bin_X <- ( x > 255 / 2 ) * 1
bin_X

# Here's an example showing that by converting things into 0 and 1 we don't lose
#    that much information.  The figure on the left includes all the pixel values.
# The picture on the right is binarized.  You can see it's three.

# -------       -------       -------       -------       -------       -------

# Vectorization for Matrices and Matrix Algebra Operations
# For our final challenge in which we're standardizing the rows or the columns,
#    we're going to use vectorization.  In R, we subtract a vector from a matrix,
#    the first element of each vector is subtracted from the first row of the
#    matrix.
# The second element from the vector is subtracted from the second row of the
#    matrix and so on.  So using mathematical notation, we would write it like
#    this.
# This is what R does when you subtract a vector from a matrix.  The same holds
#    true for other arithmetic operations.  This implies that we can scale each
#    row of a matrix using this simple code.

( x - rowMeans( x ) ) / rowSds( x )

# Now, if you want to scale each column, be careful because it does not work for
#    columns.  For columns, we would have to transpose a matrix.  So we would have
#    to do it like this.

t( t( x ) - colMeans( x ) )

# We transpose the matrix, subtract the columns, and then transpose it back.
# For this task, we can also use a function call sweep, which works in a similar
#    way to apply.  It takes each entry of a vector and subtracts it from the
#    corresponding row or column.  So for example, in this code, we take each
#    column.

X_mean_0 <- sweep( x, 2, colMeans( x ) )
X_mean_0

# There's a two there.  That tells you it's a column.  And it subtracts the column
#    mean from each column and returns the new matrix.  Now, the function sweep
#    actually has an other argument that lets you define the arithmetic
#    operation.  By default, it's subtraction.  But we can change it.
# So to divide by the standard deviation, we can do the following.

x_mean_0 <- sweep( x, 2, colMeans( x ) )
x_standardized <- sweep( x_mean_0, 2, colSds( x ), FUN = '/' )
x_standardized

# So we have seen powerful ways in which we can use matrix algebra in R to perform
#    certain tasks.
# Finally, although we do not cover matrix algebra operations such as matrix
#    multiplication, we share here the relevant commands for those that know the
#    mathematics and want to learn the code.
# Matrix multiplication is done with the following operation--percent star percent.
#    So the cross-product, for example, can be written like this.

t( x ) %*% x

# We can compute the cross-product directly with the function with that name
#    cross product x gives us as the cross-product.

crossprod( x )

# To compute the inverse of a function, we use solve.  Here it is applied to the
#    cross-product.

solve( crossprod( x ) )

# Finally, the qr decomposition is readily available by using the qr function like
#    this.

qr( x )

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Working with Matrices
# Q1
# Which line of code correctly creates a 100 by 10 matrix of randomly generated normal numbers and assigns it to x?
# x <- matrix(rnorm(1000), 100, 100)
# x <- matrix(rnorm(100*10), 100, 10)   <-*
# x <- matrix(rnorm(100*10), 10, 10)
# x <- matrix(rnorm(100*10), 10, 100)

# Q2
# Write the line of code that would give you the specified information about the matrix x that you generated in q1. Do not include any spaces in your line of code.

x <- matrix(rnorm(100*10), 100, 10)
x

# Dimension of x.
dim( x )   # 100 10
# Number of rows of x.
nrow( x )   # 100
# Number of columns of x.
ncol( x )   # 10

# Q3
# Which of the following lines of code would add the scalar 1 to row 1, the scalar 2 to row 2, and so on, for the matrix x?
# Select ALL that apply.

# x <- x + seq(nrow(x))   <-*
# x <- 1:nrow(x)
# x <- sweep(x, 2, 1:nrow(x),'+')
# x <- sweep(x, 1, 1:nrow(x),'+')   <-*


# Q4
# Which of the following lines of code would add the scalar 1 to column 1, the scalar 2 to column 2, and so on, for the matrix x?
#     Select ALL that apply.

head( x )

# x1 <- 1:ncol(x)
# x2 <- 1:col(x)
# x <- sweep(x, 2, 1:ncol(x), FUN = '+')   <-*
# x <- -x

# Q5
# Which code correctly computes the average of each row of x?
# mean(x)
# rowMedians(x)
# sapply(x,mean)
# rowSums(x)
# rowMeans(x)   <-*

# Which code correctly computes the average of each column of x?
# mean(x)
# sapply(x,mean)
# colMeans(x)   <-*
# colMedians(x)
# colSums(x)

# Q6
# For each digit in the mnist training data, compute the proportion of pixels that are in the grey area, defined as values between 50 and 205. (To visualize this, you can make a boxplot by digit class.)
#
# What proportion of pixels are in the grey area overall, defined as values between 50 and 205?


mnist <- read_mnist()
y <- rowMeans( mnist$train$images > 50 & mnist$train$images < 205 )
length( y )
mean( y )
qplot( as.factor( mnist$train$labels ),
       y,
       geom = 'boxplot' )

 # 0.061837

# -------       -------       -------       -------       -------       -------

# Distance
# The concept of distance is quite intuitive. For example, when we cluster animals
#    into subgroups, reptiles, amphibians, mammals, we're implicitly defining a
#    distance that permits us to say what animals are close to each other.
# Many machine learning techniques rely on being able to define distance between
#    observations using features or predictors.  As a review, let's define the
#    distance between two points, A and B, on the Cartesian plane, like these two.
# The Euclidean distance between AB is simply given by this formula.  Note that
#    this definition applies to the case of one dimension.  In which case, the
#    distance between two numbers is simply the absolute value of their difference.
# So if our two one dimensional numbers are A and B, the distance is simply this,
#    which turns into the absolute value.  In an earlier video, we introduced a
#    training data set with feature matrix measurements of 784 features.
# For illustrative purposes, we look at random samples of 2s and 7s. We can
#    generate this data set using this piece of code.

set.seed( 0 )
if( !exists( 'mnist' ) ) mnist <- read_mnist()
ind <- which( mnist$train$labels %in% c( 2, 7 ) ) %>% sample( 500 )
x <- mnist$train$images[ ind, ]   # Predictors
y <- mnist$train$labels[ ind ]    # Labels
x
y

# The predictors are on x and the labels are on y.  For the purposes of, for
#    example, smoothing, we're interested in describing distances between
#    observations.  In this case, digits.  Later, for the purposes of selecting
#    features, we might also be interested in finding pixels that behave similar
#    across samples.
# Now, to define distance, we need to know what points are, since mathematical
#    distance is computed between two points.  With high dimensional data, points
#    are no longer on the Cartesian plane.  Instead, points are higher dimensional.
#    We can no longer visualize them and need to think abstractly. For example,
#    in our digits example, a predictor, xi, is defined as a point in 784
#    dimensional space.  We can write it out like this.  Once we define points
#    this way, the Euclidean distance is defined very similar as it was for the
#    two dimensional case.
# For instance, the distance between observations 1 and 2 is given by this formula.
# Note that this is a non-negative number, just as it is for the two dimensions.
# So any two observations, there's a distance and it's just one number.  OK.
# Now let's look at an example.  Let's look at the first three observations.
# Let's look at their labels.

y[ 1:3 ]

# This is a 7, a 7, and a 2.  The vector of predictors for each of these
#    observations are going to be saved in these three objects.

x_1 <- x[ 1, ]
x_2 <- x[ 2, ]
x_3 <- x[ 3, ]

# Now let's look at the distances.  And remember, the first two numbers are a 7
#    and the third one is a 2.  We expect the distances between the same number,
#    like this, to be smaller than between different numbers.  And that's what
#    happens.  We can see it here.

sqrt( sum( ( x_1 - x_2 )^2 ) )
sqrt( sum( ( x_2 - x_3 )^2 ) )

# As expected, the 7s are closer to each other.  Now, if you know matrix algebra,
#    note that a faster way to compute this is using the cross-product.  So we
#    can actually type this.

sqrt( crossprod( x_1 - x_2 ) )
sqrt( crossprod( x_1 - x_3 ) )
sqrt( crossprod( x_2 - x_3 ) )

# We can also compute all the distances between all the observations at once
#    relatively quickly using the function, dist.
# If you feed it a matrix, the dist function computes the distance between each
#    row and produces an object of class dist.  Here's the code demonstrating this.

d <- dist( x )
d
class( d )

# Now there are several machine learning-related functions in R that take objects
#    of class dist as input.  But to access the entries using row and column
#    indices, we need to coerce this object into a matrix.  We can do this like
#    this.  If we look at the first three entries, we can see that the distances
#    that we calculated match what the function, dist, calculates.

as.matrix( d )[ 1:3, 1:3 ]

# We can also quickly see an image of these distances using the image function.

image( as.matrix( d ) )

# So we type this. We see a visual representation of the distance between every
#    pair of observations.  If we order that distances by labels, we can see that,
#    in general, the 2s are closer to each other and the 7s are closer to each
#    other.  We can achieve this using this code.

image( as.matrix( d )[ order( y ), order( y ) ] )

# Those red squares demonstrate that digits that are the same are closer to each
#    other. But another thing that comes out of this plot is that there appears
#    to be more uniformity in how the 7s are drawn since they appear to be closer.
# It's more red up there. Now, we can also compute distance between predictors.
# If N is the number of observations, the distance between two predictors, say
#    the first and the second, can be computed like this.  To compute the distance
#    between all pairs of the 784 predictors, we can transpose the matrix first
#    and then use the dist function.  We can write this code.

d <- dist( t( x ) )
dim( as.matrix( d ) )

# Note that the dimension of the resulting distance matrix is a 784 by 784 matrix.
# An interesting thing to note here is that, if we pick a predictor, a pixel,
#    we can see which pixels are close, meaning that they are either ink together
#    or they don't have ink together.  So as an example, let's just look at the
#    492nd pixel and let's look at the distances between each pixel and the 492nd
#    pixel.  Here is what it looks like.
# We can see the spatial pattern.  Not surprisingly, pixels that are physically
#    close on the image are actually also close mathematically.  So in summary,
#    the concept of distance is important in machine learning.  And we will see
#    this as we learn more about specific algorithms.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Distance
# Q1
# Load the following dataset:

library(dslabs)
data('tissue_gene_expression')

# This dataset includes a matrix x:

dim(tissue_gene_expression$x)

# This matrix has the gene expression levels of 500 genes from 189 biological samples representing seven different tissues. The tissue type is stored in y:

table(tissue_gene_expression$y)

# Which of the following lines of code computes the Euclidean distance between each observation and stores it in the object d?
# d <- dist(tissue_gene_expression$x, distance='maximum')
# d <- dist(tissue_gene_expression)
# d <- dist(tissue_gene_expression$x)   <-*
# d <- cor(tissue_gene_expression$x)

# Q2
# Compare the distances between observations 1 and 2 (both cerebellum), observations 39 and 40 (both colon), and observations 73 and 74 (both endometrium).

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

# Distance-wise, are samples from tissues of the same type closer to each other?
# No, the samples from the same tissue type are not necessarily closer.
# The two colon samples are closest to each other, but the samples from the other two tissues are not.
# The two cerebellum samples are closest to each other, but the samples from the other two tissues are not.
# Yes, the samples from the same tissue type are closest to each other.   <-*

# Q3
# Make a plot of all the distances using the image function to see if the pattern you observed in Q2 is general.
#
# Which code would correctly make the desired plot?
# image(d)
# image(as.matrix(d))   <- *
# d
# image()

# -------       -------       -------       -------       -------       -------

# Knn
# In this video, we will learn our first machine learning algorithm, the k-nearest
#    neighbors algorithm.  To demonstrate it, we're going to use the digits data
#    with two predictors that we created in a previous video.  K-nearest neighbors
#    is related to smoothing.  To see this, we will think about the conditional
#    probably, the probably of being a seven, y equals 1, given the two predictors.

# p( x1, x2 ) = Pr( Y = 1 | X1 = x1, X2 = x2 )

# This is because the zeros and ones we observe are noisy because some of the
#    regions of the conditional probability are not close to zero or one, which
#    means that you can go either way sometimes.
# So we have to estimate the conditional probability. How do we do this?
# We're going to try smoothing.  K-nearest neighbors is similar to bin smoothing.
# But it is easier to adapt to multiple dimensions. We first defined the distance
#    between observations based on the features.  Basically, for any point for
#    which you want to estimate the conditional probability, we look at the
#    k-nearest points and then take an average of these points.
# We refer to the set of points used to compute the average as a neighborhood.
# Due to the connection we described earlier between conditional expectations and
#    conditional probabilities, this gives us the estimated conditional probability,
#    just like bin smoothers gave us an estimated trend.  We can control,
#    flexibility of our estimate through k.  Larger Ks result in smaller estimates,
#    while smaller Ks result in more flexible and more wiggly estimates.
# So let's implement k-nearest neighbors.  We're going to compare it to logistic
#    regression, which will be the standard we need to beat.  We can write this
#    code to compute the glm predictions.

library( dimRed )
library( caret )
fit_glm <- glm( y ~ x_1 + x_2,
                data = mnist_27$train,
                family = 'binomial' )
p_hat_logistic <- predict( fit_glm,
                           mnist_27$test )
y_hat_logistic <- factor( ifelse( p_hat_logistic > 0.5, 7 , 2 ) )
confusionMatrix( data = y_hat_logistic,
                 reference = mnist_27$test$y )$overall[ 1 ]

# And notice that we have an accuracy of 0.76.  Now let's compare this to knn.
#    We will use the function knn3 which comes with the caret package.  If we
#    look at the help file of this package, we see that we can call it in one of
#    two ways.  In the first, we specify a formula and a data frame.  The data
#    frame contains all the data to be used.  The formula has the form outcome
#    tilde predictor 1 plus predictor 1 plus predictor 3 and so on.
# So in this m where we only have two predictors, we would type y--those are the
#    outcomes-- tilde x1 plus x2.  But if we're going to use all the predictors,
#    we can use a shortcut, and it's the dot.
# We would type y tilde dot.  And that says use all the predictors.  So the call
#    to knn3 looks simply like this.

knn_fit <- knn3( y ~ .,
                 data = mnist_27$train )
knn_fit

# The second way to call this function is that the first argument being the matrix
#    predictors and the second, a vector of outcomes.  So the code would look like
#    this instead.

x <- as.matrix( mnist_27$train[ , 2:3 ] )
y <- mnist_27$train$y
knn_fit <- knn3( x, y )
knn_fit

# We would define our matrix with the predictors.  Then when we would define a
#    vector with the outcomes.  And then we would call it simply like this.
# The reason we have two ways of doing this is because the formula is a quicker,
#    simpler way to write it when we're in a hurry.  But once we face large data
#    sets, we will want to use the matrix approach, the second approach.
# All right, now, for this function, we also need to pick a parameter, the number
#    of neighbors to include.  Let's start with the default, which is k equals 5.
#    We can write it explicitly like this.

knn_fit <- knn3( y ~ .,
                 data = mnist_27$train,
                 k = 5 )
knn_fit

# Because this data set is balanced-- there's many twos as there are sevens--and
#    we care just as much about sensitivity as we do about specificity-- both
#    mistakes are equally bad-- we will use accuracy to quantify performance.
# The predict function for this knn function produces either a probability for
#    each class, or it could actually produce the outcome that maximizes the
#    probability, the outcome with the highest probability.
# So we're going to use the code predict, the fitted object, the new data that
#    we're predicting for.  That's the test data set.  And then we're going to
#    type, type equals class.  This will give us the actual outcomes that are
#    predicted.

y_hat_knn <- predict( knn_fit,
                      mnist_27$test,
                      type = 'class' )
y_hat_knn

# So once we have this, we can compute our accuracy using the confusion matrix
#    formula like this.

confusionMatrix( data = y_hat_knn,
                 reference = mnist_27$test$y )$overall[ 'Accuracy' ]

# And we see that we already have an improvement over the logistic regression.
# Our accuracy is 0.815.

# -------       -------       -------       -------       -------       -------

# Overtraining and Oversmoothing
# Now to see why we improved over logistic regression in this case that we only
#    have two predictors, we can actually make some visualizations.  Here is the
#    true conditional probability on the left.  And on the right, you can see the
#    estimate that we obtained with knn with five neighbors.  So you see that the
#    estimate has the essence of the shape of the true conditional probability.

data( 'mnist_27' )
head( mnist, 1 )
str( mnist_27 )

mnist_27$test %>%
    ggplot( aes( x_1,
                 x_2,
                 color = y ) ) +
    geom_point()

library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family='binomial')
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

knn_fit <- knn3(y ~ ., data = mnist_27$train)

x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = 'class')
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall['Accuracy']

# Therefore we do better than logistic regression.  However, we can probably do
#    better.  Because if you look closely, this estimate, we see some isles of
#    blue in the red areas.  Intuitively this does not make much sense.  Why are
#    they on their own like that? This is due to what we call overtraining.
# To understand what overtraining is, notice that we have higher accuracy when we
#    predict on a training set than we compare on a test set.  We can do it using
#    this code.

y_hat_knn <- predict(knn_fit, mnist_27$train, type = 'class')
confusionMatrix(data = y_hat_knn,
                reference = mnist_27$train$y)$overall['Accuracy']
#> Accuracy
#>    0.882

y_hat_knn <- predict(knn_fit, mnist_27$test, type = 'class')
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall['Accuracy']
#> Accuracy
#>    0.815

# You can see that the accuracy computed on the training side is quite higher.
# It's 0.882 compared to what we get on the test set, which is only 0.815.  This
#    is because we overtrained.  Overtraining is at its worst when we set k equals
#    to 1.  With k equals to 1, the estimate for each point in the training set
#    is obtained with just the y corresponding to that point because you are your
#    closest neighbor.  So in this case, we obtain practically perfect accuracy
#    in the training set because each point is used to predict itself.  Perfect
#    accuracy will occur when we have unique predictors, which we almost do have
#    here.  So you can see that when we use a k equals to 1, our accuracy on a
#    training set is 0.995, five almost perfect accuracy.

knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = 'class')
confusionMatrix(data=y_hat_knn_1,
                reference=mnist_27$train$y)$overall['Accuracy']
#> Accuracy
#>    0.995

# However, when we check on the test set, the accuracy is actually worse than with
#    logistic regression.  It's only 0.735.  We can see that using this code.

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = 'class')
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall['Accuracy']
#> Accuracy
#>    0.735

# To see the over-fitting in a figure, we can plot the data and then use contours
#    to show what divides the twos from the sevens.  And this is what you get when
#    you use k equals 1.  Notice all the little islands that in the training set
#    fit the data perfectly.  You'll have this little red point on its own, and
#    a little island will be formed around it so that you get the perfect prediction.
# But once you look at the test set, that point is gone.  There's no red there
#    anymore.  Now there's perhaps a blue, and you make a mistake.  The estimated
#    conditional probability followed the training data too closely.  Although
#    it's not as bad, we see this overtraining with k equals 5, or the default.
#    So we should consider a larger k.  Let's try an example. Let's try a much
#    larger example.  Let's try 401.

knn_fit_401 <- knn3( y ~ .,
                     data = mnist_27$train,
                     k = 401 )
y_hat_knn_401 <- predict( knn_fit_401,
                          mnist_27$test,
                          type = 'class' )
confusionMatrix( data = y_hat_knn_401,
                 reference = mnist_27$test$y )$overall[ 'Accuracy' ]

# We can fit the model just by simply changing the k to 401 like this.  We see
#    that the accuracy on a test set is only 0.79, not very good, a similar
#    accuracy to logistic regression.  In fact, the estimates actually look quite
#    similar.  On the left is logistic regression.  On the right is k-nearest
#    neighbors with k equals 401.  The size of k is so large that it does not
#    permit enough flexibility.  We're almost including half the data to compute
#    each single estimated conditional probability.  We call this oversmoothing.
#    So how do we pick k?
#    Five seems to be too small.  401 seems to be too big.  Something in the
#    middle might be better.  So what we can do is we can repeat what we just
#    did for different values of k So we can try all the odd numbers between 3
#    and 251.

ks <- seq( 3, 251, 2 )

# And we'll do this using the map df function to repeat what we just did for each
#    k.  For comparative purposes, we will compute the accuracy by using both
#    training set--that's incorrect.
# We shouldn't do that, but just for comparison we're going to do it--and the test
#    set, which is the correct way to do it.  The code looks simply like this.
# Once we run that code, we can now plot the accuracy against the value of k, and
#    that looks like this.

library(purrr)
accuracy <- map_df(ks, function(k){
    fit <- knn3(y ~ ., data = mnist_27$train, k = k)

    y_hat <- predict(fit, mnist_27$train, type = 'class')
    cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
    train_error <- cm_train$overall['Accuracy']

    y_hat <- predict(fit, mnist_27$test, type = 'class')
    cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
    test_error <- cm_test$overall['Accuracy']

    list(train = train_error, test = test_error)
})

# First, note that the accuracy versus k plot is quite jagged. We don't not expect
#    this because small changes in k should not affect the algorithm's performance
#    too much.  The jaggedness is explained by the fact that the accuracy is
#    computed on this sample and therefore is a random variable.  This demonstrates
#    why we prefer to minimize the expectation loss, rather than the loss we observe
#    with one dataset.  We will soon learn a better way of estimating this expected
#    loss.  Now, despite the noise present in the plot, we still see a general
#    pattern.  Low values of k give low test set accuracy but high train set
#    accuracy, which is evidence of overtraining.  Large values of k result in
#    low accuracy, which is evidence of oversmoothing.  The maximum is achieved
#    somewhere between 25 and 41.  And the maximum accuracy is 0.85, substantially
#    higher than logistic regression.  In fact, the resulting estimate with k equals
#    to 41 looks quite similar to the true conditional probability, as we see in
#    this plot.  Now, is an accuracy of 0.85 what we should expect if we apply this
#    algorithm in the real world?  The answer is actually no because we broke a
#    golden rule of machine learning.  We selected the k using the test set.
# So how do we select the k?  In the next videos, we introduce the important concept
#    of cross-validation, which provides a way to estimate the expected loss for
#    a  given method using only the training set.

# -------       -------       -------       -------       -------       -------

# Comprehendion Check> Nearest Neighbors
# Q1
# Previously, we used logistic regression to predict sex based on height. Now we are going to use knn to do the same. Use the code described in these videos to select the F_1 measure and plot it against k. Compare to the F_1 of about 0.6 we obtained with regression. Set the seed to 1.

set.seed(1)
data( 'heights' )
library(caret)
ks <- seq( 1, 101, 3 )
F_1 <- sapply( ks, function( k ) {
    test_index <- createDataPartition( heights$sex,
                                       times = 1,
                                       p = 0.5,
                                       list = FALSE )
    test_set <- heights[ test_index, ]
    train_set <- heights[ -test_index, ]
    fit <- knn3( sex ~ height,
                 data = train_set,
                 k = k )
    y_hat <- predict( fit,
                      test_set,
                      type = 'class' ) %>%
        factor( levels = levels( train_set$sex ) )
    F_meas( data = y_hat,
            reference = test_set$sex )
} )
plot(ks, F_1)
max(F_1)

# What is the max value of F_1?
max( F_1 )    # 0.6297872

# At what value of k does the max occur?
ks[ which.max( F_1 ) ]    # 34 or 38



# Q2
# Next we will use the same gene expression example used in the Comprehension Check: Distance exercises. You can load it like this:
#
library(dslabs)
data( 'tissue_gene_expression' )

ks <- seq(1, 11, 2)

F_1 <- sapply(ks, function(k){

    set.seed(1)

    #create training index (rather than test index) as suggested by comments
    train_index <- createDataPartition(tissue_gene_expression$y, p = 0.5, list = F)

    # split original data set into x and y
    x <- tissue_gene_expression$x
    y <- tissue_gene_expression$y

    # split x into train and test sets
    train_set_x = x[train_index,]
    test_set_x = x[-train_index,]

    # split y into train and test sets
    train_set_y = y[train_index]
    test_set_y = y[-train_index]

    # merge x and y train sets as a list (as per original data set)
    train_set = list('x' = train_set_x, 'y' = train_set_y)

    # merge x and y test sets
    test_set = list('x' = test_set_x, 'y' = test_set_y)

    fit <- knn3(y ~ ., data = (as.data.frame(train_set)), k = k)

    y_hat <- predict(fit, (as.data.frame(test_set)), type = 'class') %>% factor(levels = levels(y))

    confusionMatrix(data=y_hat, reference=test_set_y)$overall['Accuracy']

})

plot(ks, F_1)
max(F_1)
F_1
ks


# Split the data into training and test sets, and report the accuracy you obtain. Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1.
#
# k=1   0.978
# k=3   0.967
# k=5   0.989
# k=7   0.967
# k=9   0.956
# k=11  0.956

# -------       -------       -------       -------       -------       -------

# k-fold Cross Validation
# In previous videos, we've described how the goal of machine learning is often
#    to find an algorithm that produces predictors, y hat for an outcome y, that
#    minimizes mean squared error.  When all we have to our disposal is one data
#    set, we can estimate the mean squared error with the observed mean squared
#    error like this.  These two quantities are often referred to as a true error
#    and the apparent error respectively.  There are two important characteristics
#    of the apparent error we should always keep in mind.
# First, it is a random variable since our data is random.  For example, the data
#    set we have may be a random sample from a larger population.  So an algorithm
#    having lower apparent error than another may be due to luck.
# Second, if we train an algorithm on the same data set that we used to compute
#    the apparent error, we might be overtraining.
# In general when we do this, the apparent error will be an underestimate of the
#    true error.  We saw an extreme example of this with the k-nearest neighbors
#    when we said k equals to 1.  Cross-validation is a technique that permits
#    us to alleviate both these problems.  There are several approaches.
# I will go over some of them here.  To understand cross-validation, it helps to
#    think of the true error, a theoretical quantity, as the average of many, many
#    apparent errors obtained by applying the algorithm to, let's call it, B, new
#    random samples of the data, none of them used to train the algorithm.
# When we think this way, we can think of the true error as the average of the
#    apparent errors obtained in each of the random samples.  The formula would
#    be like this.  Here B a large number that can be thought of as practically
#    infinite.  Now, this is a theoretical quantity because we only get to see
#    one set of outcomes.  We don't get to see them over and over again.
# The idea of cross-validation is to imitate this theoretical setup as best we
#    can with the data that we have.  To do this, we have to generate a series
#    of different random samples.  There are several approaches to doing this.
# But the general idea for all of them is to randomly generate smaller data sets
#    that are not used for training and instead are used to estimate the true error.
# The first one we describe, and the one we focus on in this video, is k-fold
#    cross-validation.  Let's describe it.  Remember, that generally speaking, a
#    machine learning challenge starts with a data set.
# And we need to build an algorithm using this data set that will eventually be
#    used in a completely independent data set.  So here we have the data set we
#    have in blue and the independent data set that we'll never see in yellow.
# So we don't get to see the yellow, so all we see is the blue.  So as we have
#    already described, to imitate the situation, we carve out a piece of our data
#    set and pretend it is an independent data set.  We divide the data set into
#    training set, blue, and a test set, red.  We'll train our algorithm exclusively
#    on the training set and use the test set only for evaluation purposes.
# We usually try to select a small piece of the data set so we have as much data
#    as possible to train.  However, we also want a test set to be large so that
#    we can obtain stable estimates of the loss.  Typical choices to use for the
#    size of the test that are 10% to 20% of the original data set.
# Let's reiterate that it is indispensable that we do not use the test set at all
#    when training our algorithm, not for filtering out rows, not for selecting
#    features, nothing.  Now, this presents a new problem.  Because for most
#    machine learning algorithms, we need to select parameters, for example, the
#    number of neighbors k in the k-nearest neighbors algorithm.  Here we'll refer
#    to the set of parameters as lambda.  So we need to optimize the algorithm
#    parameters lambda without using our test set.  And we know that if we optimize
#    and evaluated on the same data set, we will overtrain.  So here is where we
#    use cross-validation.  This is where cross-pollination is most useful.
# So let's describe k-fold cross-validation.  For each set of algorithm parameters
#    being considered, we want to estimate the MSE.  And then we will choose the
#    parameters with the smallest MSE.  Cross-validation will provide this estimate.
# First, it is important that before we start the cross-validation procedure we
#    fix all the algorithm parameters.  We're computing the MSE for a given
#    parameter.  So as we describe later, we will trained the algorithm on a set
#    of training sets.  The parameter lambda will be the same across all these
#    training sets.  We'll use the notation y hat i parentheses lambda to denote
#    the prediction obtained when we use a parameter lambda for observation i.
# So if we're going to imitate the definition of the expected loss, we could write
#    it like this.  It is the average over B samples that we've taken of the MSE
#    that we obtain on the data separated out as a test set.  For this formula,
#    we want to consider data sets that can be thought of as independent random
#    samples.  And you'll want to do this several times.  With k-fold
#    cross-validation, we do it k times.  In the cartoons we're showing, we use
#    as an example k equals 5.  We will eventually end up with k samples.
# But let's start by describing how to construct the first one.  We simply pick
#    N divided K rounded to the nearest integer.  Let's call that M. So we have
#    M observations that we pick at random and think of these as a random sample.
# We could denote them using this equation.  And here b equals 1.  It's the first,
#    what we call, fold.  So here's what it looks like graphically.  We have our
#    training set.  We separate out the test set.  And then we take our training
#    set, and we take a small sample of it, which we're going to call the validation
#    set, the first one.  And that is where we're going to test.  Now we can fit
#    the model in the training set, with the validation set separated out, and
#    compute the apparent error on the independent set like this.
# Note that this is just one sample and will therefore return a noisy estimate
#    of the true error.  This is why we take k sample not just one.  So graphically
#    it would look like this.  In k-fold cross-validation, we randomly split the
#    observations into k non-overlapping sets.  So now we repeat this calculation
#    for each of these sets, b going from 1 all the way up to k.  So we obtain k
#    estimates of the MSE.  In our final estimate, we compute the average like
#    this.  And this gives us an estimate of our loss.  A final step would be to
#    select the lambda, the parameters, that minimize the MSE.
# So this is how we use cross-validation to optimize parameters.  However, now we
#    have to take into account the fact that the optimization occurred on the
#    training data.  So we need to compute an estimate of our final algorithm
#    based on data that was not used to optimize this choice.  And this is why we
#    separated out the test set.  That is where we'll compute our final estimate
#    of the MSE.  So note that we can do cross-validation again.  Note that this
#    is not for optimization purpose.  This is simply to know what the MSE of our
#    final algorithm is.  So doing this would give us a better estimate.
# However, note that to do this, we have to go through the entire optimization
#    process k times.  You will soon learn that performing machine learning tasks
#    can take time because we're performing many complex computations.  And
#    therefore we're always looking for ways to reduce this.  So for the final
#    evaluation, we often just use one test set.  We use cross-validation to
#    optimize our algorithm.  But once we've optimized it, we're done, and we
#    want to have an idea of what our MSE is, we just use this one last test set.
#    Once we're satisfied with this model, and we want to make it available to
#    others, we could refit the model on the entire data set, but without changing
#    the parameters. Now, how do we pick the cross-validation k?
#    We used five in these examples.
#    We could use other numbers.  Large values of k are preferable because the
#    training data better imitate the original data.  However, larger values of
#    k will have much lower computation time.  For example, hundredfold
#    cross-validation will be 10 times slower than tenfold cross-validation.
# For this reason, the choices of k equals to 5 and 10 are quite popular.  Now,
#    one way we can improve the variance of our final estimate is to take more
#    samples.
# To do this, we would no longer require that training set be partitioned into
#    non-overlapping sets.  Instead we would just pick k sets of some size at
#    random.  One popular version of this technique, at each fold, picks observations
#    at random with replacement, which means that the same observation can appear
#    twice.  This approach has some advantages not discussed here is generally
#    referred to as the bootstrap approach.  In fact, this is the default approach
#    in the caret package.  In another video, we'll describe the concept of the bootstrap.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Cross-validation
# Q1
# Generate a set of random predictors and outcomes using the following code:
#
library( caret )
set.seed( 1996 )
n <- 1000
p <- 10000
x <- matrix( rnorm( n*p ), n, p )
colnames( x ) <- paste( 'x', 1:ncol( x ), sep = '_' )
y <- rbinom( n, 1, 0.5 ) %>% factor()

x_subset <- x[ ,sample( p, 100 ) ]

str( x )
str( y )
str( x_subset )

# Because x and y are completely independent, you should not be able to predict y using x with accuracy greater than 0.5. Confirm this by running cross-validation using logistic regression to fit the model. Because we have so many predictors, we selected a random sample x_subset. Use the subset when training the model.
#
# Which code correctly performs this cross-validation?
#
# fit <- train( x_subset, y )
# fit$results

fit <- train( x_subset, y, method = 'glm' )   # <-*
fit$results

# fit <- train( y, x_subset, method = 'glm' )
# fit$results

# fit <- test(x_subset, y, method = 'glm')
# fit$results

# Q2
# Now, instead of using a random selection of predictors, we are going to search for those that are most predictive of the outcome. We can do this by comparing the values for the  group to those in the  group, for each predictor, using a t-test. You can do perform this step like this:

# install.packages( 'devtools' )
#
source( 'https://bioconductor.org/biocLite.R' )
biocLite( 'BiocUpgrade' )
library( devtools )
BiocManager::install()
BiocInstaller::biocLite(c('genefilter'))
#
devtools::install_bioc( 'genefilter' )
biocLite( 'genefilter' )
library( genefilter )
#
tt <- colttests( x, y )

# Which of the following lines of code correctly creates a vector of the p-values called pvals?
# pvals <- tt$dm
# pvals <- tt$statistic
# pvals <- tt
pvals <- tt$p.value    # <-*

# Q3
# Create an index ind with the column numbers of the predictors that were 'statistically significantly' associated with y. Use a p-value cutoff of 0.01 to define 'statistically significantly.'
#
# How many predictors survive this cutoff?

ind <- which( pvals <= 0.01 )
length( ind )    # 108

# Q4
# Now re-run the cross-validation after redefinining x_subset to be the subset of x defined by the columns showing 'statistically significant' association with y.
#
# What is the accuracy now?

x_subset_sig <- x[ ,ind ]
x_subset_sig

fit_sig <- train( x_subset_sig, y, method = 'glm' )
fit_sig     # 0.76

# Q5
# Re-run the cross-validation again, but this time using kNN. Try out the following grid k = seq(101, 301, 25) of tuning parameters. Make a plot of the resulting accuracies.
#
# Which code is correct?

fit <- train(x_subset, y, method = 'knn', tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)    # <-*

# fit <- train(x_subset, y, method = 'knn')
# ggplot(fit)

# fit <- train(x_subset, y, method = 'knn', tuneGrid = data.frame(k = seq(103, 301, 25)))
# ggplot(fit)

# fit <- train(x_subset, y, method = 'knn', tuneGrid = data.frame(k = seq(101, 301, 5)))
# ggplot(fit)

# Q6
# In the previous exercises, we see that despite the fact that x and y are completely independent, we were able to predict y with accuracy higher than 70%. We must be doing something wrong then.
#
# What is it?
# The function train estimates accuracy on the same data it uses to train the algorithm.
# We are overfitting the model by including 100 predictors.
# We used the entire dataset to select the columns used in the model.    <-*
# The high accuracy is just due to random variability.

# Q7
# Use the train function to predict tissue from gene expression in the tissue_gene_expression dataset. Use kNN.
#
# What value of k works best?

library(dslabs)
data( 'tissue_gene_expression' )
tissue_gene_expression
fit <- with( tissue_gene_expression,
             train( x,
                    y,
                    method = 'knn',
                    tuneGrid = data.frame( k = seq(1, 7, 2) ) ) )
ggplot( fit )
fit$results

# -------       -------       -------       -------       -------       -------

# Bootstrap
#  In this video we describe the bootstrap.  We're going to use a very simple
#     example to do it.  Suppose the income distribution of a population is as
#     follows.  The population median is, in this case, about 45,000.  Suppose we
#     don't have access to the entire population, but want to estimate the median,
#     let's call it M. We take a sample of 250 and estimate the population median,
#     M, with the sample medium big M, like this.

hist( log10( income ) )
set.seed( 1 )
N <- 250
X <- sample( income, N )
M <- median( X )
M # 42978

# Now, can we construct a confidence interval?
# What's the distribution of the sample median?
#    From an Monte Carlo simulation, we see that the distribution of the sample
#    median is approximately normal with the following expected value and standard
#    errors. You can see it here.

B <- 10^5
Ms <- replicate( B,
                 {
                     X <- sample( income, N )
                     M <- median( X )
                 } )


# The problem here is that, as we have described before, in practice, we do not
#    have access to the distribution.  In the past, we've used the central limit
#    theorem, but the central limit theorem we studied applies to averages and
#    here we're interested in the median.  The bootstrap permits us to approximate
#    a Monte Carlo simulation without access to the entire distribution.  The
#    general idea is relatively simple.  We act as if the sample is the entire
#    population and sample with replacement data sets of the same size.
# Then we compute the summary statistic, in this case, the median, on what is
#    called the bootstrap sample.  There is theory telling us that the distribution
#    of the statistic obtained with bootstrap samples approximate the distribution
#    of our actual statistic.  This is how we construct bootstrap samples in an
#    approximate distribution.  This simple code.

B <- 10^5
M_stars <- replicate( B,
                      {
                          X_star <- sample( X, N, replace = TRUE )
                          M_star <- median( X_star )
                      } )
M_stars

# Now we can check how close it is to the actual distribution.  We can see it's
#    relatively close.  We see it's not perfect, but it provides a decent
#    approximation.  In particular, look at the quantities we need to form a 95%
#    confidence interval.

quantile( Ms, c( 0.05, 0.95 ) )
quantile( M_stars, c( 0.05, 0.95 ) )

# They are quite close.  This is much better than what we get if we mindlessly
#    use the central limit theorem, which would give us this confidence interval,
#    which is entirely wrong.

median( X ) + 1.96 * sd( X ) / sqrt( N ) * c( -1, 1 )

# If we know the distribution is normal, we can use a bootstrap to estimate the
#    mean, the standard error, and then form a confidence interval that way.

mean( Ms ) + 1.96 * sd( Ms ) * c( -1, 1 )
mean( M_stars ) + 1.96 * sd( M_stars ) * c( -1, 1 )

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Bootstrap
# Q1
# The createResample function can be used to create bootstrap samples. For example, we can create 10 bootstrap samples for the mnist_27 dataset like this:

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
sum( indexes$Resample01 == 3 )
sum( indexes$Resample01 == 4 )
sum( indexes$Resample01 == 7 )

# How many times do 3, 4, and 7 appear in the first resampled index?
#
# Enter the number of times 3 appears:   1
# Enter the number of times 4 appears:   4
# Enter the number of times 7 appears:   0

# Q2
# We see that some numbers appear more than once and others appear no times. This has to be this way for each dataset to be independent. Repeat the exercise for all the resampled indexes.

x <- sapply( indexes,
             function( ind ) {
                 sum( ind == 3 )
             } )
x
sum( x )

# What is the total number of times that 3 appears in all of the resampled indexes?
# 11

# Q3
# Generate a random dataset using the following code:
#

set.seed(1)
y <- rnorm(100, 0, 1)

# Estimate the 75th quantile, which we know is qnorm(0.75), with the sample quantile: quantile(y, 0.75).
#
# Run a Monte Carlo simulation with 10,000 repetitions to learn the expected value and standard error of this random variable. Set the seed to 1.

set.seed( 1 )
B <- 10000
q_75 <- replicate( B,
                   {
                       y <- rnorm( 100, 0, 1 )
                       quantile( y, 0.75 )
                   }
                 )
q_75
mean( q_75 )
sd( q_75 )

# Expected value    0.66
# Standard error    0.13

# Q4
# In practice, we can't run a Monte Carlo simulation. Use 10 bootstrap samples to estimate the standard error using just the initial sample y. Set the seed to 1.

set.seed( 1 )
y <- rnorm( 100, 0, 1 )
set.seed(1)
indexes <- createResample( y, 10 )
q_75_star <- sapply( indexes,
                     function( ind ) {
                         y_star <- y[ ind ]
                         quantile( y_star, 0.75 )
                     }
                    )
mean( q_75_star )
sd( q_75_star )

# Expected value   0.7312
# Standard error   0.0742

# Q5
# Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10. Set the seed to 1.

set.seed( 1 )
y <- rnorm( 100, 0, 1 )
set.seed(1)
indexes <- createResample( y, 10000 )
q_75_star <- sapply( indexes,
                     function( ind ) {
                         y_star <- y[ ind ]
                         quantile( y_star, 0.75 )
                     }
)
mean( q_75_star )
sd( q_75_star )

# Expected value   0.6737
# Standard error   0.0930

# Q6
# Compare the SD values obtained using 10 vs 10,000 bootstrap samples.
#
# What do you observe?
# The SD is substantially lower with 10,000 bootstrap samples than with 10.
# The SD is roughly the same in both cases.  <-*
# The SD is substantially higher with 10,000 bootstrap samples than with 10.

# -------       -------       -------       -------       -------       -------

# Generative Models
# We have described how when using square loss the conditional expectation or
#    probabilities provide the best approach to developing a decision rule.
# In a binary case, the best we can do is called Bayes' rule which is a decision
#    rule based on the true conditional probability, probably y equals one given
#    the predictors x.  We have described several approaches to estimating this
#    conditional probability.
# Note that in all these approaches, we estimate the conditional probability
#    directly and do not consider the distribution of the predictors.  In machine
#    learning, these are referred to as discriminative approaches.
# However, Bayes' theorem tells us that knowing the distribution of the predictors
#    x may be useful.  Methods that model the joint distribution of y and the
#    predictors x are referred to as generative models.
# We start by describing the most general generative model naive Bayes and then
#    proceed to describe some more specific cases, quadratic discriminant analysis
#    QDA and linear discriminant analysis LDA. Recall that Bayes' theorem tells
#    us that we can rewrite the conditional probability like this with the f's
#    representing the distribution functions of the predictors x for the two
#    classes y equals 1 and when y equals 0.
# The formula implies that if we can estimate these conditional distributions,
#    the predictors, we can develop a powerful decision realm.  However, this is
#    a big if.  As we go froward, we will encounter examples in which the predictors
#    x have many dimensions and we do not have much information about their
#    distribution.  So it will be very hard to estimate those conditional
#    distributions.  In these cases, naive Bayes would be practically impossible
#    to implement.
# However, there are instances in which we have a small number of predictors, not
#    much more than two, and many categories in which generated models can be
#    quite powerful.
# We describe two specific examples and use our previously described case study
#    to illustrate them.

# -------       -------       -------       -------       -------       -------

# Naive Bayes
# Let's start with a very simple and uninteresting, yet illustrative, example.
# Predict sex from the height example. We can get the data and generate training
#    and test set using this code.

library( caret )
data( 'heights' )
y <- heights$height
set.seed( 2 )
test_index <- createDataPartition( y,
                                   times = 1,
                                   p = 0.5,
                                   list = FALSE )
train_set <- heights %>% slice( -test_index )
test_set <- heights %>% slice( test_index )

# In this example, the naive Bayes approach is particularly appropriate.  Because
#    we know that the normal distribution is a very good approximation of the
#    conditional distributions of height given sex for both classes, females and
#    males.
# This implies that we can approximate the conditional distributions by simply
#    estimating averages and standard deviations from the data with this very
#    simple piece of code, like this.

params <- train_set %>%
    group_by( sex ) %>%
    summarize( avg = mean( height ),
               sd = sd( height ) )
params

# The prevalence, which we will denote with pi, which is equal to the probability
#    of y equals 1, can be estimated from the data as well like this.

pi <- train_set %>%
    summarize( pi = mean( sex == 'Female' ) ) %>%
    .$pi
pi

# We basically compute the proportion of females. Now we can use our estimates
#    of average and standard deviations to get the actual rule.
# We get the conditional distributions, f0 and f1, and then we use Bayes theorem
#    to compute the naive Bayes estimate of the conditional probability.

x <- test_set$height

f0 <- dnorm( x,
             params$avg[ 2 ],
             params$sd[ 2 ] )
f1 <- dnorm( x,
             params$avg[ 1 ],
             params$sd[ 1 ] )

p_hat_bayes <- f1 * pi / ( f1 * pi + f0 * ( 1 - pi ) )
p_hat_bayes

# This estimate of the conditional probably looks a lot like a logistic regression
#    estimate, as we can see in this graph.  In fact, we can show mathematically
#    that the naive Bayes approach is similar to the logistic regression approach
#    in this particular case. But we're not going to show that derivation here.

# -------       -------       -------       -------       -------       -------

# Controlling Prevalence
# One nice feature of the Naive Bayes approach is that it includes a parameter to
#    account for differences in prevalence.  Using our sample, we estimated the
#    conditional probabilities and the prevalence pi.  If we use hats to denote
#    the estimates, we can rewrite the estimate of the conditional probability
#    with this formula.
# As we discussed, our sample has much lower prevalence than the general population.
# We only have 23% women. So if we use our rule that the conditional probability
#    has to be bigger than 0.5 to predict females, our accuracy will be affected
#    due to the low sensitivity, which we can see by typing this code.

y_hat_bayes <- ifelse( p_hat_bayes > 0.5,
                       'Female',
                       'Male' )
sensitivity( data = factor( y_hat_bayes ),
             reference = factor( test_set$sex ) )

# Again, this is because the algorithm gives more weight to specificity to account
#    for the low prevalence.  You can see that we have very high specificity by
#    typing this code.

specificity( data = factor( y_hat_bayes),
             reference = factor( test_set$sex ) )

# This is due mainly to the fact that pi hat is substantially less than 0.5, so
#    we tend to predict male more often than female.  It makes sense for a machine
#    learning algorithm to do this in our sample because we do have a higher
#    percentage of males.  But if we were to extrapolate this to the general
#    population, our overall accuracy would be affected by the low sensitivity.
# The Naive Bayes approach gives us a direct way to correct this, since we can
#    simply force our estimate of pi to be different. So to balance specificity
#    and sensitivity, instead of changing the cutoff in the decision rule, we
#    could simply change pi hat.  Here in this code, we changed it to 0.5.

p_hat_bayes_unbiased <- f1 * 0.5 / ( f1 * 0.5 + f0 * ( 1 - 0.5 ) )
y_hat_bayes_unbiased <- ifelse( p_hat_bayes_unbiased > 0.5,
                                'Female',
                                'Male' )
p_hat_bayes_unbiased
y_hat_bayes_unbiased

# Now note the difference in sensitivity and the better balance. We can see it
#    using this code.

sensitivity( data = factor( y_hat_bayes_unbiased ),
            reference = factor( test_set$sex ) )

# This plot shows us that the new rule also gives us a very intuitive cutoff
#    between 66 and 67, which is about the middle of the female and male average
#    heights.

qplot( x,
       p_hat_bayes_unbiased,
       geom = 'line' ) +
    geom_hline( yintercept = 0.5,
                lty = 2 ) +
    geom_vline( xintercept = 67,
                lty = 2 )

# -------       -------       -------       -------       -------       -------

# qda and lda
# Quadratic discriminant analysis, or QDA, is a version of Naive Bayes in which
#    we assume that the conditional probabilities for the predictors are
#    multivariate normal.
# So the simple example we described in our Naive Bayes video was actually QDA.
# In this video, we're going to look at a slightly more complicated example where
#    we have two predictors.  It's the 2 or 7 example that we've previously seen.
#    We can load it with this code.

data( 'mnist_27' )

# In this case, we have two predictors. So we assume that their conditional
#    distribution is bivariate normal. This implies that we need to estimate two
#    averages, two standard deviations, and a correlation for each case, the 7s
#    and the 2s.  Once we have these, we can approximate the conditional
#    distributions.  We can easily estimate these parameters from the data using
#    this simple code.

params <- mnist_27$train %>%
    group_by( y ) %>%
    summarize( avg_1 = mean( x_1 ),
               avg_2 = mean( x_2 ),
               sd1 = sd( x_1 ),
               sd2 = sd( x_2 ),
               r = cor( x_1, x_2 ) )
params

# We can also visually demonstrate the approach. We plot the data and use contour plots
#    to give an idea of what the two estimated normal densities look like.

mnist_27$train %>%
    mutate( y = factor( y ) ) %>%
    ggplot( aes( x_1,
                 x_2,
                 fill = y,
                 color = y ) ) +
    geom_point( show.legend = FALSE ) +
    stat_ellipse( type = 'norm',
                  lwd = 1.5 )

# We show a curve representing a region that includes 95% of the points.  Once
#    you've estimated these two distributions, this defines an estimate for the
#    conditional probability of y equals 1 given x1 and x2.  We can the caret
#    package to fit the model and obtain predictors.  The code is quite simple
#    and it looks like this.

install.packages( 'caret' )
install.packages('e1071')
library( caret )
library(e1071)
data( 'mnist_27' )
train_qda <- train( y ~ ., method = 'qda', data = mnist_27$train )
train_qda

# We see that we obtain a relatively good accuracy of 0.82.  The estimated
#    conditional probability looks relatively similar to the true distribution,
#    as we can see here.

y_hat <- predict( train_qda,
                  mnist_27$test )
confusionMatrix( data = y_hat,
                 reference = mnist_27$test$y )$overall[ 'Accuracy' ]

# Although the fit is not as good as the one we obtain with kernel smoothers,
#    which we saw in a previous video.  And there's a reason for this.  The reason
#    is that we can show mathematically that the boundary must be a quadratic
#    function of the form x2 equals ax1 squared plus bx plus c.
# One reason QDA does not work as well as the kernel method is perhaps because
#    the assumption of normality do not quite hold.  Although for the 2s, the
#    bivariate normal approximation seems reasonable, for the 7 it does seem to
#    be off.  Notice the slight curvature.  Although QDA work well here, it becomes
#    harder to use as a number of predictors increases.  Here we have two predictors
#    and have to compute four means, four standard deviations, and two correlations.
# How many parameters would we have to estimate if instead of two predictors
#    we had 10?  The main problem comes from estimating correlations for 10
#    predictors.  With 10, we have 45 correlations for each class.  In general,
#    this formula tells us how many parameters we have to estimate, and it gets
#    big pretty fast.  Once the number of parameters approaches the size of our
#    data, the method becomes unpractical due to overfitting.

# K * ( 2p + p * ( p - 1 ) / 2 )

params <- mnist_27$train %>%
    group_by( y ) %>%
    summarize( avg_1 = mean( x_1 ),
               avg_2 = mean( x_2 ),
               sd_1 = sd( x_1 ),
               sd_2 = sd( x_2 ),
               r = cor( x_1, x_2 ) )
params <- params %>%
    mutate( sd_1 = mean( sd_1 ),
            sd_2 = mean( sd_2 ),
            r = mean( r ) )
params

train_lda <- train( y ~ .,
                    method = 'lda',
                    data = mnist_27$train )
y_hat <- predict( train_lda,
                  mnist_27$test )
confusionMatrix( data = y_hat,
                 reference = mnist_27$test$y )$overall[ 'Accuracy' ]

# -------       -------       -------       -------       -------       -------

# Case Strudy: More than Three Classes
# In this video, we will give a slightly more complex example, one with three
#    classes instead of two.  We first create a dataset similar to the two or
#    seven dataset.  Except now we have one, twos, and sevens.  We can generate
#    that dataset using this rather complex code.

## read-in data
mnist <- read_mnist()
set.seed( 3456 )
index_127 <- sample( which( mnist$train$labels %in% c( 1, 2, 7 ) ), 2000 )
index_127

y <- mnist$train$labels[ index_127 ]
x <- mnist$train$images[ index_127, ]
index_train <- createDataPartition( y,
                                    p = 0.8,
                                    list = FALSE )
## get the quadrants
row_column <- expand.grid( row = 1:28,
                           col = 1:28 )

# temporary object to help figure out the quadrants
upper_left_ind <- which( row_column$col <= 14 & row_column$row <= 14 )
lower_right_ind <- which( row_column$col > 14 & row_column$row > 14 )
upper_left_ind
lower_right_ind

x <- x > 200

# binarize the values.  Above 200 is ink, below is no ink
x <- cbind( rowSums( x[ , upper_left_ind ] ) / rowSums( x ),
            # proportion of pixels in upper right quadrant
            rowSums( x[ , lower_right_ind ] ) / rowSums( x ) )
train_set <- data.frame( y = factor( y[ index_train ] ),
                         x_1 = x[ index_train, 1 ],
                         x_2 = x[ index_train, 2 ] )
test_set <- data.frame( y = factor( y[ -index_train ] ),
                        x_1 = x[ -index_train, 1 ],
                        x_2 = x[ -index_train, 2 ] )
train_set
test_set

# Once we're done, we obtain training set and a test set.  Here we're showing the
#    data for the training set.

train_set %>%
    ggplot( aes( x_1, x_2, color = y ) ) +
    geom_point()

# You can see the x1 and x2 predictors.  And then in color, we're showing you the
#    different labels, the different categories.
# The ones are in red, the greens are the twos, and the blue points are the sevens.
#    As an example, we'll fit a qda model.  We'll use the caret package.  So all
#    we do is type this piece of code.

train_qda <- train( y ~ .,
                    method = 'qda',
                    data = train_set )
train_qda

# So how do things differ now?  First note that we estimate three conditional
#    probabilities, although they all have to add up to 1.  So if you type predict
#    with type probability, you now get a matrix with three columns, a probability
#    for the ones, a probability for the two, a probability for the sevens.

predict( train_qda,
         test_set,
         type = 'prob' ) %>%
    head()

# We predict the one with the highest probability.  So for the first observation,
#    we would predict a two, .  And now our predictors are one of three classes.
#    If we use the predict function, with the default setting of just giving you
#    the outcome, we get twos, ones, and sevens.

predict( train_qda,
         test_set )

# The confusion matrix is a three-by-three table now because we can make two kinds
#    of mistakes with the ones, two kinds of mistakes with the two, and two kinds
#    of mistakes with the sevens. You can see it here.

confusionMatrix( predict( train_qda,
                          test_set ),
                 test_set$y )

# The accuracy's still at one number because it just basically computes how often
#    we make the correct prediction.  Note that for sensitivity and specificity,
#    we have a pair of values for each class.  This is because to define these
#    terms, we need a binary outcome.  We therefore have three columns, one for
#    each class as a positive and the other two as the negatives.  Finally, we
#    can visualize what parts of the regions are called ones, twos, and seven by
#    simply plotting the estimated conditional probability.

GS <- 150
new_x <- expand.grid( x_1 = seq( min( train_set$x_1 ),
                                 max( train_set$x_1 ),
                                 len = GS ),
                      x_2 = seq( min( train_set$x_2 ),
                                 max( train_set$x_2 ),
                                 len = GS ))
new_x %>%
    mutate( y_hat = predict( train_qda,
                                   new_x ) ) %>%
    ggplot( aes( x_1, x_2, color = y_hat, z = as.numeric( y_hat ) ) ) +
    geom_point( size = 0.5, pch = 16 ) +
    stat_contour( breaks = c( 1.5, 2.5 ), color = 'black' ) +
    guides( colour = guide_legend( override.aes = list( size = 2 ) ) )

# Let's see how it looks like for lda. We can train the model like this. The
#    accuracy is much worse, and it is because our boundary regions have three
#    lines.

train_lda <- train( y ~ .,
                    method = 'lda',
                    data = train_set )
confusionMatrix( predict( train_lda, test_set ), test_set$y )$overal[ 'Accuracy' ]
#0.664

new_x_lda <- expand.grid( x_1 = seq( min( train_set$x_1 ),
                                 max( train_set$x_1 ),
                                 len = GS ),
                      x_2 = seq( min( train_set$x_2 ),
                                 max( train_set$x_2 ),
                                 len = GS ))
new_x_lda %>%
    mutate( y_hat = predict( train_lda,
                             new_x_lda ) ) %>%
    ggplot( aes( x_1, x_2, color = y_hat, z = as.numeric( y_hat ) ) ) +
    geom_point( size = 0.5, pch = 16 ) +
    stat_contour( breaks = c( 1.5, 2.5 ), color = 'black' ) +
    guides( colour = guide_legend( override.aes = list( size = 2 ) ) )


# This is something we can show mathematically.  The results for knn are actually
#    much better.  Look how higher the accuracy is.  And we can also see that the
#    estimated conditional probability is much more flexible, as we can see in
#    this plot.

train_knn <- train( y ~ .,
                    method = 'knn',
                    tuneGrid = data.frame( k = seq( 15, 51, 2 ) ),
                    data = train_set )
confusionMatrix( predict( train_knn, test_set ),
                 test_set$y )$overal[ 'Accuracy' ]

# Note that the reason that qda and, in particularly, lda are not working well
#    is due to lack of fit.  We can see that by plotting the data and noting
#    that at least the ones are definitely not bivariate normally distributed.
# So in summary, generating models can be very powerful but only when we're able
#    to successfully approximate the joint distribution of predictor's condition
#    on each class.

new_x %>%
    mutate( y_hat = predict( train_knn, new_x ) ) %>%
    ggplot( aes( x_1, x_2, color = y_hat, z = as.numeric( y_hat ) ) ) +
    geom_point( size = 0.5, pch = 16 ) +
    stat_contour( breaks = c( 1.5, 2.5 ),color = 'black' ) +
    guides( colour = guide_legend( override.aes = list( size = 2 ) ) )


train_set %>%
    mutate( y = factor( y ) ) %>%
    ggplot( aes( x_1, x_2, fill = y, color=y ) ) +
    geom_point( show.legend = FALSE ) +
    stat_ellipse( type = 'norm' )

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Generative Models

# In the following exercises, we are going to apply LDA and QDA to the tissue_gene_expression dataset. We will start with simple examples based on this dataset and then develop a realistic example.

# Q1
# Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, and a predictor matrix with 10 randomly selected columns using the following code:

# Use the train function to estimate the accuracy of LDA.

set.seed( 1993 )
data( 'tissue_gene_expression' )
ind <- which( tissue_gene_expression$y %in% c( 'cerebellum', 'hippocampus' ) )
y <- droplevels( tissue_gene_expression$y[ ind ] )
x <- tissue_gene_expression$x[ ind, ]
x <- x[ , sample( ncol( x ), 10 ) ]

fit_lda <- train(x, y, method = 'lda')
fit_lda$results['Accuracy']

# What is the accuracy?
# 0.87

# Q2
# In this case, LDA fits two 10-dimensional normal distributions. Look at the fitted model by looking at the finalModel component of the result of train. Notice there is a component called means that includes the estimated means of both distributions. Plot the mean vectors against each other and determine which predictors (genes) appear to be driving the algorithm.

t(fit_lda$finalModel$means) %>% data.frame() %>%
    mutate(predictor_name = rownames(.)) %>%
    ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
    geom_point() +
    geom_text() +
    geom_abline()

# Which TWO genes appear to be driving the algorithm?
# PLCB1
# RAB1B   <-*
# MSH4
# OAZ2    <-*
# SPI1
# SAPCD1
# HEMK1

# Q3
# Repeat the exercise in Q1 with QDA.
#
# Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, and a predictor matrix with 10 randomly selected columns using the following code:

set.seed(1993)
data('tissue_gene_expression')
ind <- which(tissue_gene_expression$y %in% c('cerebellum', 'hippocampus'))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# Use the train function to estimate the accuracy of QDA.

fit_qda <- train(x, y, method = 'qda')
fit_qda$results['Accuracy']

# What is the accuracy?
# 0.81

# Q4
# Which TWO genes drive the algorithm when using QDA instead of LDA?

t(fit_qda$finalModel$means) %>% data.frame() %>%
    mutate(predictor_name = rownames(.)) %>%
    ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
    geom_point() +
    geom_text() +
    geom_abline()

# PLCB1
# RAB1B   <-*
# MSH4
# OAZ2    <-*
# SPI1
# SAPCD1
# HEMK1

# Q5
# One thing we saw in the previous plots is that the values of the predictors correlate in both groups: some predictors are low in both groups and others high in both groups. The mean value of each predictor found in colMeans(x) is not informative or useful for prediction and often for purposes of interpretation, it is useful to center or scale each column. This can be achieved with the preProcessing argument in train. Re-run LDA with preProcessing = 'scale'. Note that accuracy does not change, but it is now easier to identify the predictors that differ more between groups than based on the plot made in Q2.

fit_lda <- train(x, y, method = 'lda', preProcess = 'center')
fit_lda$results['Accuracy']
t(fit_lda$finalModel$means) %>% data.frame() %>%
    mutate(predictor_name = rownames(.)) %>%
    ggplot(aes(predictor_name, hippocampus)) +
    geom_point() +
    coord_flip()


d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

# Which TWO genes drive the algorithm after performing the scaling?
# C21orf62
# PLCB1
# RAB1B
# MSH4
# OAZ2    <-*
# SPI1    <-*
# SAPCD1
# IL18R1

# Q6
# Now we are going to increase the complexity of the challenge slightly: we will consider all the tissue types. Use the following code to create your dataset:

set.seed(1993)
data('tissue_gene_expression')
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

# What is the accuracy using LDA?
fit_lda_q6 <- train(x, y, method = 'lda', preProcess = 'center')
fit_lda_q6$results['Accuracy']

# 0.81

# -------       -------       -------       -------       -------       -------

# Trees Motivation
# We describe how [? methods ?] lda and qda are not meant to be used with datasets
#    that have many predictors.  This is because the number of parameters that we
#    need to estimate becomes too large.
# For example, with the digits example where we have 784 predictors, lda would
#    have to estimate over 600,000 parameters.  With qda, you would have to multiply
#    that by the number of classes, which is 10 here.
# Kernel methods such k-nearest neighbors or local regression do not have model
#    parameters to estimate.  But they also face a challenge when multiple
#    predictors are used due to what is referred to as the curse of dimensionality.
# The dimension here refers to the fact that when we have p predictors, the distance
#    between two observations is computed in p dimensional space.  A useful way
#    to understand the curse of dimensionality is by considering how large we
#    have to make a neighborhood, the neighborhood we used to make the estimates,
#    to include a given percentage of the data.  Remember that with large
#    neighborhoods, our methods lose flexibility.  For example, suppose we have
#    one continuous predictor with equally spaced points in the [? 01 ?] interval,
#    and you wanted to create windows that include 1/10 of the data.
# Then it's easy to see that our windows have to be of size 0.1.
# You can see it in this figure.
# Now, for two predictors, if we decide to keep the neighborhood just a small,
#    10% of each dimensions we only include one point.  If we want to include 10%
#    of the data, then we need to increase the size of each side of the square
#    to the square root of 10 so that the area is 10 out of 100.
#    This is now 0.316.
# In general, to include 10% of the data in a case with p dimensions, we need an
#    interval [? with ?] each side having a size of 0.10 to the 1/p.
#    This proportion gets close to 1, which means we're including practically all
#    the data, and it's no longer smoothing very quickly.
# You can see it in this graph, plotting p versus 0.1 to the 1/p.  So by the time
#    we reach 100 predictors, the neighborhood is no longer very local, as each
#    side covers almost the entire dataset.
# In this video, we introduced a set of elegant and versatile methods that adapt
#    to higher dimensions and also allow these regions to take more complex shapes,
#    while still producing models that are interpretable.  These are very popular
#    well-known and studied methods.  We will focus on regression and decision
#    trees and their extension, random forests.

# -------       -------       -------       -------       -------       -------

# Classification and Regression Trees (CART)
# To [INAUDIBLE] this video, we will use a new dataset that includes the breakdown
#    of the composition of olive into eight fatty acids.  You can get the data
#    like this.

data('olive')
head(olive)

# For illustrative purpose, we'll try to predict the region using the fatty acid
#    composition values as predictors.

names(olive)
table(olive$region)

# It's either Northern Italy, Sardinia, or Southern Italy.  We'll remove the area
#    column because we don't use it as a predictor.

olive <- select(olive, -area)
head(olive)

# Let's very quickly see how we do using k-nearest neighbors.

library(caret)
fit <- caret::train(region ~ .,  method = 'knn', tuneGrid = data.frame(k = seq(1, 15, 2)), data = olive)
ggplot(fit)

# We use the caret package to fit the model, and we get an accuracy of about
#    0.97, pretty good.  However, a bit of data exploration reveals that we should
#    be able to do even better.  For example, if we look at the distribution
#    of each predictor stratified by region, we see that one of the fatty acids
#    is only present in Southern Italy, and then another one separates Northern
#    Italy from Sardinia.

olive %>% gather(fatty_acid, percentege, -region) %>%
    ggplot(aes(region, percentege, fill = region)) +
    geom_boxplot() +
    facet_wrap(~fatty_acid, scales = 'free')

# This implies that we should be able to build an
#    algorithm that perfectly predicts.  We can see this clearly by plotting the
#    values of these two fatty acids.

p <- olive %>%
    ggplot(aes(eicosenoic, linoleic, color = region)) +
    geom_point()
p

# We can, by eye, construct a prediction rule the partitions the predictor space
#    like this.

p + geom_vline(xintercept = 0.065, lty = 2) +
    geom_segment(x = -0.2, y = 10.535, xend = 0.065, yend = 10.535, color = 'black', lty = 2)

# Specifically, we define the following decision rule.  If the first
#    predictor is larger than 0.065, B, predict Southern Italy.  If not, then look
#    at the second predictor.  And if that's larger than 10.535, predict Sardinia,
#    and Northern Italy otherwise.  We can draw this as a decision tree like this.
# Decision trees like this one are often used in practice. For example, to decide
#    if a person at risk of having a heart attack, a doctor will use a decision
#    tree such as this one.  The general idea is to define an algorithm that
#    uses data to create trees such as the ones we've just shown.
# Regression and decision trees operate by predicting an outcome variable y by
#    partitioning the predictor space.  When the outcome is continuous, we call
#    these types of algorithms regression trees.  We'll use a continuous case,
#    the 2008 poll data introduced earlier, to describe the basic idea of how we
#    build these algorithms.  We'll try to estimate the conditional expectation
#    -- we'll call it f of x, the expected value of y given x-- with y, the poll
#    margin, and x, the day.  Here's a graph of the data.

data('polls_2008')
qplot(day, margin, data = polls_2008)

# The general idea here is to build a decision tree.  And at the end of each node,
#    we'll have a different prediction Y hat.  Here is how we do it.
# First we partition the space into j non-overlapping regions, R1, R2, all the
#    way up to Rj.  For every observation that follows within a region, let's say,
#    region Rj, we predict the Y hat with the average of all the training observations
#    in that region.  But how do we decide on the partitions R1, R2 and so on?
#    And how do we decide how many?  Regression trees create partitions recursively.
#    But what does this mean? OK, suppose we already have a partition.
# We then have to decide what predictor j to use to make the next partition
#    and where to make it within that predictor.  So suppose we already have a
#    partition so that every observation i is in exactly one of these partitions.
# For each of these partitions, we will divide further using the following
#    algorithm.  First we need to find a predictor j and a value s that define
#    two new partitions.  Let's call them R1 and R2.  These two partitions will
#    split our observations into the following two sets.
# Then, in each one of these two sets, we will define an average and use these
#    as our predictions.  The averages will be the averages of the observations
#    in each of the two partitions.  So we could do this for many Js and Ss.
#    So how do we pick them?
#    We pick the combination that minimizes the residual sum of squares defined
#    by this formula.  This is then applied recursively.  We keep finding new
#    regions to split into two.
# Once we're done partitioning the predictor space into regions, in each region,
#    a prediction is made by using the observations in that region.  You basically
#    calculate an average.  Let's take a look at what this algorithm does on the
#    2008 presidential election poll data.  We will use the rpart function in the
#    rpart package.  We simply type this.

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# Here there's only one predictor.  So we don't have to decide which predictor j
#    to split by.  We simply have to decide what value s we use to split.
# We can visually see where the splits were made by using this piece of code.

plot(fit, margin = 0.1)
text(fit, cex = 0.5)

# Here's a tree.  The first split is made on day 39.5.  Then one of those two
#    regions is split at day 86.5.  The two resulting new partitions are split
#    on day 49.5 and 117.5 respectively and so on.
# We end up with eight partitions.  The final estimate f hat of x looks like this.

polls_2008 %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_step(aes(day, y_hat), col='red')

# Now, why did the algorithm stop partitioning at eight?  There are some details
#    of the algorithm we did not explain.  Let's explain them now.  Note that every
#    time we split and define two new partitions, our training set residual sum
#    of squares decreases.  This is because with more partitions, our model has
#    more flexibility to adapt to the training data.
# In fact, if you split until every point is its own partition, then the residual
#    sum of squares goes all the way down to zero since the average of one value
#    is that same value.  To avoid this overtraining, the algorithm sets a minimum
#    for how much the residual sum of squares must improve for another partition
#    to be added.
# This parameter is referred to as the Complexity Parameter, or CP.  The residual
#    sum of squares must improve by a factor of CP the new partition to be added.
#    Another aspect of the algorithm we didn't describe is that it sets a minimum
#    number of observations to be partitioned.  In the rpart package, that rpart
#    function has an argument called minsplit that lets you define this.
#    The default is 20.  The algorithm also sets a minimum on the number of
#    observations in each partition.  In the rpart function, this argument is
#    called minbucket.
# So if the optimal split results in a partition with less observation than this
#    minimum, it is not considered.  The default for this parameter is minsplit
#    divided by 3 rounded to the closest integer.  OK, so let's see what happens
#    if we set CP to 0 and minsplit to 2.
#    What will happen then?
#    Well, our prediction is our original data because the tree will keep splitting
#    and splitting until the RSS is minimized to zero.  Here's the data with the
#    resulting fit.

fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_step(aes(day, y_hat), col='red')

#  Now, note that in this methodology, we can also prune trees
#    by snipping off partitions that do not meet a CP criterion.
# So we can grow a tree very, very big and then prune off branches to make a smaller
#    tree. Here is a code for how to do this.

pruned_fit <- prune(fit, cp = 0.01)
polls_2008 %>%
    mutate(y_hat = predict(pruned_fit)) %>%
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_step(aes(day, y_hat), col='red')

# With the code that we just wrote, here is the resulting estimate.  OK, but now
#    is a default value of CP the best one?
#    How do we pick CP?
#    Well, we can use cross-validation, just like with any other tuning parameter.
#    We can use the train function in the caret package, for example.
#    We can write this code, then plot the result, and pick the best CP.

library(caret)
train_rpart <- caret::train(margin ~ .,
                     method = 'rpart',
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)

#    To see the resulting tree that minimizes the mean squared error, we can access
#    it through the component finalmodel.  If we plot it, we can see the tree.
#    Here it is.

plot(train_rpart$finalModel)
text(train_rpart$finalModel)

# And because we only have one predictor, we can actually plot f hat x.
#    Here it is.

polls_2008 %>%
    mutate(y_hat = predict(train_rpart)) %>%
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_step(aes(day, y_hat), col='red')

# You can see that the fit looks reasonable.

# -------       -------       -------       -------       -------       -------

# Classification (Decision) Trees
# When the outcome is categorical, we refer to these methods as classification
#    trees or decision trees.  We use the same partitioning principles, that we
#    use for the continuous case, but with some slight differences to account
#    for the fact that we are now working with categorical data.
# The first difference is that rather than taking the average at the end of each
#    node, now in the partitions, we predict with the class that has the majority
#    vote in each node.  So the class that appears the most in a node, that will
#    be what we predict.
# The second difference is that we can no longer use residual sum of squares to
#    decide on the partition because the outcomes are categorical.  Well, we could
#    use a naive approach, for example, looking four partitions that minimize
#    training error.  Better performing approaches use more sophisticated metrics.
# Two of the more popular ones are the Gini index and entropy.  Let's define those
#    two concepts.  If we define p hat m, k as a proportion of observations in
#    partition m that are of class k, then the Gini index is defined as follows.
# And entropy is defined in the following way.  Both of these metrics seek to
#    partition observations into subsets that have the same class.
# They want what is called purity.  Note that of a partition-- let's call it m--
#     has only one class-- let's say it's the first one-- then p hat of 1 for that
#     partition is equal to 1, while all the other p hats are equal to 0.
# When this happens, both the Gini index and entropy are 0, the smallest value.
# So let's look at an example.  Let's see how classification trees perform on the
#    two or seven example we have examined in previous videos.  This is the code
#    that we would write to fit a tree.

train_rpart <- caret::train(y ~ .,
                           method = 'rpart',
                           tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                           data = mnist_27$train)
plot(train_rpart)

# We then look at the accuracy versus
#    complexity parameter function, and we can pick the best complexity parameter
#    from this plot.
# And now we use that tree and see how well we do.  We see that we achieve an
#    accuracy of 0.82.  We can use this code.

confusionMatrix(predict(train_rpart,
                        mnist_27$test),
                mnist_27$test$y)$overall['Accuracy']

# Note that this is better than logistic regression but not as good as the kernel
#    methods.  If we plot the estimate of the conditional probability obtained
#    with this tree, it shows us the limitations of classification trees.
# Note that with decision trees, the boundary can't be smoothed.  Despite these
#    limitations, classification trees have certain advantages that make them very
#    useful.
# First, they're highly interoperable, even more so than linear regression models
#    or logistic regression models.  They're also easy to visualize if they're
#    small enough.
# Finally, they sometimes model human decision processes.  On the other hand,
#    the greedy approach via recursive partitioning is a bit harder to train than,
#    for example, linear regression or k-nearest neighbors.
# Also, it may not be the best performing method since it's not very flexible,
#    and it's actually quite susceptible to changes in the training data.
# Random forests, explained in the next video, improve on several of these
#    shortcomings.

# -------       -------       -------       -------       -------       -------

# Random Forests
# Random forests are a very popular approach that address the shortcomings of
#    decision trees using a clever idea.  The goal is to improve prediction
#    performance and reduce instability by averaging multiple decision trees, a
#    forest of trees constructed with randomness.
# It has two features that help accomplish this.  The first feature is referred
#    to as bootstrap aggregation, or bagging.  The general scheme for bagging is
#    as follows.
# First, we build many decision trees, T1 through TB, using the training set.
#    We later explain how we're sure they're different.
# Second, for every observation j in the test set, we form a prediction y hat j
#    using tree Tj.  Now, to obtain a final prediction, we combine the predictions
#    for each tree in two different ways, one for continuous outcomes and one for
#    categorical outcomes.  For continuous outcomes, we simply take the average
#    of the y hat j's.  For categorical data, we predict y hat with a majority
#    vote.  The class that appears most across all the trees is the one we predict.
# OK, now, but how do we get many decision trees from a single training set?
# For this, we use the bootstrap.  So to create, let's say, B bootstrap trees,
#    we do the following.  To create tree Tj from a training set of size N, we
#    create a bootstrap training set by sampling N observations from this training
#    set with replacement.  Now we build a decision tree for each one of these
#    bootstrap training sets.  And then we apply the algorithm that we just
#    described to get a final prediction.  Here's the code for applying random
#    forest to the 2008 polls data.
# It's quite simple.  We do it like this.

library(randomForest)
fit <- randomForest(margin ~ ., data = polls_2008)
install.packages(rafalib)
rafalib::mypar(1,1)
plot(fit)

# We can see the algorithm improves as we add more trees.  If you plot the object
#    that comes out of this function like this, we get a plot of the error versus
#    the number of trees that have been created.  In this case, we see that by the
#    time we get to about 200 trees, the algorithm is not changing much.  But note
#    that for more complex problems will require more trees for the algorithm to
#    converge.  Here is the final result for the polls 2008 data.

polls_2008 %>%
    mutate(y_hat = predict(fit, newdata = polls_2008)) %>%
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_line(aes(day, y_hat), col='red')

# Note that the final result is somewhat smooth.  It's not a step function like
#    the individual trees.  The averaging is what permits estimates that are not
#    step functions.  To see this, we've generated an animation to help illustrate
#    the procedure.  In the animated figure, you see each of 50 trees, B equals
#    1 up to 50.  Each one is a bootstrap sample which appears in order.
#    For each one of the bootstrap samples, we see the tree that is fitted to that
#    bootstrap sample.  And then in blue, we see the result of bagging the trees
#    up to that point.  So you can see the blue line changing with time.
#    Now let's look at another example.  Let's fit a random forest to our two or
#    seven digit example.  The code would look like this.

library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)

confusionMatrix(predict(train_rf, mnist_27$test),
                mnist_27$test$y)$overall['Accuracy']

# And here's what the conditional probabilities look like.
plot_cond_prob <- function(p_hat) {
    GS <- 150
    new_x <- expand.grid(x_1 = seq(0, 0.5, len = GS),
                         x_2 = seq(0, 0.5, len = GS))
    new_x %>% mutate(p = p_hat) %>%
        ggplot(aes(x_1, x_2, z = p, fill = p)) +
        geom_raster() +
        scale_fill_gradientn(colors = c('#F8766D', 'white', '#00BFC4')) +
        stat_contour(breaks = c(0.5), color = 'black')
}
plot_cond_prob(predict(train_rf, mnist_27$true_p, type = 'prob')[,2])

# Note that we now have
#    much more flexibility than just a single tree.  This particular random forest
#    is a little bit too wiggly.  We want something smoother.
# However, note that we have not optimized the parameters in any way.  So let's
#    use the caret package to do this.  We can do it using this code.

install.packages('Rborist')
library(Rborist)
fit <- caret::train(y ~ .,
             method = 'Rborist',
             tuneGrid = data.frame(predFixed= 2, minNode = seq(3, 50)),
             data = mnist_27$train)
confusionMatrix(predict(fit, mnist_27$test), mnist_27$test$y)$overall['Accuracy']

# Here we're going to use a different random forest algorithm, Rborist, that is
#    a little bit faster.  And here is the final result. We also see that our
#    accuracy is much improved.

plot_cond_prob(predict(fit, mnist_27$true_p, type = 'prob')[,2])

# So we can control the smoothness of the random forest estimate in several ways.
# One is to limit the size of each node.
# We can require the number of points per node to be larger.  A second feature of
#    random forest that we have not yet described is that we can use a random
#    selection of features to use for the splits.
# Specifically, when building each tree at each recursive partition, we only
#    consider a randomly selected subset of predictors to check for the best split.
#    And every tree has a different random selection of features.  This reduces
#    correlation between trees in the forests, which in turn improves prediction
#    accuracy.
# The argument for this tuning parameter in the random forest function is mtry.
# But each random forest implementation has a different name.  You can look at
#    the help file to figure out which one.  A disadvantage of random forest is
#    that we lose interpretability.  We're averaging hundreds or thousands of
#    trees.  However, there's a measure called variable importance that helps us
#    interpret the results.  Variable [? importance ?] basically tells us how much
#    each predictor influences the final predictions.  We will see an example later.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Trees and Random Forests
# Q1
# Create a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor, using this code:

library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Which code correctly uses rpart to fit a regression tree and saves the result to fit?
# fit <- rpart(y ~ .)
# fit <- rpart(y, ., data = dat)
# fit <- rpart(x ~ ., data = dat)
fit <- rpart(y ~ ., data = dat)   # <-*

# Q2
# Which of the following plots correctly shows the final tree obtained in Q1?
plot(fit)
text(fit)

# Regression tree with 6 total branches. The first split is at x < 0.1261. Left branch then splits at x < -0.815, leading to leftmost branch at 1.093, and its right branch splits at x < -0.39, to values -0.5291 and -0.175. Right branch splits at x < 1.426, leading to its left branch that splits at x < 0.381 (to values 0.2832 and 0.644) and its right branch at 1.486.
# Regression tree with 6 total branches. The first split is at x < 0.2441. Left branch then splits at x < -0.8356, leading to leftmost branch at 0.9345, and its right branch splits at x < -0.252, to values -0.3438 and 0.02802. Right branch splits at x < 1.321, leading to its left branch that splits at x < 0.6797 (to values 0.3147 and 0.741) and its right branch at 1.442.
# Regression tree with 4 total branches. The first split is at x < 0.1261. Left branch then splits at x < -0.3893, leading to leftmost branch at 0.7814 and its right branch at -0.2593. Right branch splits at x < 1.15, leading to its left branch at 0.4897 and its right branch at 1.186.
# Regression tree with 8 total branches. The first split is at x < 0.0506. Left branch then splits at x < -0.9978, leading to leftmost branch split at x < -1.414 (going to 1.443 and -0.8961), and its right branch splits at x < -0.5247 (going to values -0.5449 and -0.1772). Right branch splits at x < 1.05, leading to its left branch that splits at x < 0.3771 (to values 0.202 and 0.5491) and its right branch that splits at x < 1.05 (to values 1.017 and 1.753).   <-*

# Q3
# Below is most of the code to make a scatter plot of y versus x along with the predicted values based on the fit.

dat %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(x, y)) +
#   BLANK
# Which line of code should be used to replace #BLANK in the code above?
 geom_step(aes(x, y_hat), col = 2)  # <-*
# geom_smooth(aes(y_hat, x), col=2)
# geom_quantile(aes(x, y_hat), col=2)
# geom_step(aes(y_hat, x), col=2)

# Q4
# Now run Random Forests instead of a regression tree using randomForest from the __randomForest__ package, and remake the scatterplot with the prediction line. Part of the code is provided for you below.

library(randomForest)
fit <- #BLANK
    dat %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)
# What code should replace #BLANK in the provided code?
randomForest(y ~ x, data = dat)   # <-*
# randomForest(x ~ y, data = dat)
# randomForest(y ~ x, data = data)
# randomForest(x ~ y)

# Q5
# Use the plot function to see if the Random Forest from Q4 has converged or if we need more trees.
#
# Which is the correct plot to assess whether the Random Forest has converged?

rafalib::mypar(1,1)
plot(fit)

# Plot of fit. Error on y-axis; number of trees on x-axis. Error fluctuates wildly between about 0.16 to 0.22 up to 25 trees, then fluctuates less jaggedly from about 0.14 to 0.017 between 25 and 500 trees.
# Plot of fit. Error on y-axis; number of trees on x-axis. Error decreases sharply from about 0.5 to 0.18 by about 50 trees, then fluctuates around 0.082 to 0.083 between 50 and 500 trees.
# Plot of fit. Error on y-axis; number of trees on x-axis. Error decreases sharply from about 0.115 to 0.084 by about 50 trees, then fluctuates around 0.082 to 0.083 between 50 and 500 trees.   <-*
# Plot of fit. Error on y-axis; number of trees on x-axis. Error decreases fluctuates wildly between about 0.13 and 0.17 up to about 50 trees, then fluctuates less jaggedly between about 0.125 and 0.135 between 50 and 500 trees.

# Q6
# It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.

# Part of the code is provided for you below.

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)#BLANK
    dat %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)

# What code should replace #BLANK in the provided code?
# randomForest(y ~ x, data = dat, nodesize = 25, maxnodes = 25)
# randomForest(y ~ x, data = dat, nodes = 50, max = 25)
# randomForest(x ~ y, data = dat, nodes = 50, max = 25)
randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)   # <-*
# randomForest(x ~ y, data = dat, nodesize = 50, maxnodes = 25)

# -------       -------       -------       -------       -------       -------

#Caret Package
#We have already learned about regression, logistic regression, and k-nearest
#   neighbors as machine-learning algorithms.  In later sections, we learn several
#   others.  And this is just a small subset of all the algorithms out there.
# Many of these algorithms are implemented in R. However, they are distributed
#    via different packages, developed by different authors, and often use different
#    syntax.
# The caret package tries to consolidate these differences and provide consistency.
# It currently includes 237 different methods, which are summarized in the following
#    site.  We'll include the link in the courseware.
#    (https://topepo.github.io/caret/available-models.html)
# Note that caret does not automatically install the packages needed to run
#    these methods.
# So to implement a package through caret, you still need to install the library.
# The required package for each method is included in this page.
#    (https://topepo.github.io/caret/train-models-by-tag.html)
# We'll include the link in the courseware.  The caret package also provides a
#    function that performs cross-validation for us.  Here we provide some examples
#    showing how we use this incredibly helpful package.
# We will use the two-or-seven example to illustrate this.  You can load it like
#    this.

data('mnist_27')

# train trade function lets us train different algorithms using similar syntax.
# So for example, we can train a logistic regression model or a k-NN model
#    using very similar syntax, like this.

library(caret)
train_glm <- caret::train(y ~ ., method = 'glm', data = mnist_27$train)
train_knn <- caret::train(y ~ ., method = 'knn', data = mnist_27$train)

# Now,to make predictions, we can use the output of this function directly without
#    needing to look at the specifics of predict.glm or predict.knn.
# Instead, we can learn how to obtain predictions from the predict.train function.
# Once we read this help page, we know how to use a predict function for these
#    objects.  Here is the code to get the predictions for the logistic regression
#    and the k-NN.

y_hat_glm <- predict(train_glm, mnist_27$test, type = 'raw')
y_hat_knn <- predict(train_knn, mnist_27$test, type = 'raw')

# Notice that the syntax is very similar.  We can also very quickly study the
#    confusion matrix.
# For example, we can compare the accuracy of both these methods like this.

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall['Accuracy'] # 0.75
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall['Accuracy'] # 0..84

# -------       -------       -------       -------       -------       -------
# Tuning Parameters with Caret
# When an algorithm includes a tuning parameter, train automatically uses cross-
#    validation to decide among a few default values.  To find out what parameter
#    or parameters are optimized, you can read this page that explains it.
#    (http://topepo.github.io/caret/available-models.html)
# We'll include the link in the courseware. Or study the output of the following code.

getModelInfo('knn')

# The get model info function can be used to get information of the method that
#    you're interested in.  You can do a quick lookup using the model lookup function
#    like this.  When we run this code, we see that for knn, the parameter that's
#    optimized is k.

modelLookup('knn')

# So if we run the function train with default values, you can quickly see the
#    results of the cross-validation using the ggplot function.

train_knn <- caret::train(y ~ ., method = 'knn', data = mnist_27$train)

# You can use the argument highlight to highlight the parameter that optimizes
#    the algorithm.  So you can type this.

ggplot(train_knn, highlight = TRUE)

# By default, the cross-validation is performed by testing on 25 bootstrap samples
#    comprised of 25% of the observations.  Also, for the knn method, the default
#    is to try out k=5, 7, and 9.  We already saw that 9 maximizes this.
# But maybe there's another k that's even better.  So to change this, we need to
#    use the tunegrid parameter in the train function.
# The grid of values that are going to be compared must be supplied by a data
#    frame with the column names as specified by the parameters that you get in
#    the model lookup output.  Here we present an example trying out 30 values
#    between 9 and 67.
# We need to use a column in k, so the data frame we use is going to be this one.
# Now, note that when running this code, we are fitting 30 versions of knn
#    to 25 bootstrap samples, so we're fitting 750 knn models.
# And thus, running this code will take several seconds. Here's the code.

train_knn <- caret::train(y ~ ., method = 'knn',
                          data = mnist_27$train,
                          tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)

# In the plot, we can see the k that maximizes accuracy, but we can also access
#    it using this code.

train_knn$bestTune

# The bestTune component gives us the parameter that maximizes the accuracy.
#    We can also access the best-performing model using this code.

train_knn$finalModel

# Now, if you apply the function predict to the output of the train function,
#    it will use this best-performing model to make predictions.
# Note that the best model was obtained using the training set.  We did not use
#    the test set at all.  The cross-validation was performed on the training set.
# So now, if we want to see the accuracy we obtain on the test set, which hasn't
#    been used, we can use the following code.

confusionMatrix(predict(train_knn,
                        mnist_27$test,
                        type = 'raw'),
                mnist_27$test$y)$overall['Accuracy']

# Sometimes we like to change the way we perform cross-validation.  We might
#    change the method, we might change how we do the partitions, et cetera.
# If we want to do this, we need to use a trainControl function.  So for example,
#    we can make the code that we just showed go a bit faster by using 10-fold
#    cross-validation.
# This means we're going to have 10 validation samples that use 10% of the
#    observations each.  Notice that if we plot the estimated accuracy versus k
#    plot, we notice that the accuracy estimates are more variable than in the
#    previous example.

control <- trainControl(method = 'cv',
                        number = 10,
                        p = .9)
train_knn_cv <- caret::train(y ~ .,
                             method = 'knn',
                             data = mnist_27$train,
                             tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn_cv, highlight = TRUE)

# Now this is expected since we changed a number of samples we use to estimate
#    accuracy.  In the first example, we used 25 bootstrap samples, and in this
#    example, we use 10-fold cross-validation.  One more thing to point out.
# Note that the train function also provides standard deviation values for each
#    parameter that was tested.  This is obtained from the different validation
#    sets.  So we can make a plot like this that shows  the point estimates of
#    the accuracy along with standard deviations.

train_knn$results %>%
    ggplot(aes(x = k, y = Accuracy)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(x = k,
                      ymin = Accuracy - AccuracySD,
                      ymax = Accuracy + AccuracySD))

# To finish this example up, let's notice that the best-fitting knn model
#    approximates the true condition of probability pretty well.
# However, we do see that the boundary is somewhat wiggly. This is because knn,
#    like the basic bin smoother, does not use a smooth kernel.  To improve this,
#    we could try loess.
# By reading through the available models of the caret package, which you can get
#    to through this link, which we include in the course material, we see that
#    we can use the gamLoess method.  Also from the caret documentation link--
#    you can go to it here-- you can see that we need to install the gam package
#    if we have not done so already.  So we will type something like this.

install.packages('gam')
library(gam)

# Then we will see that we have two parameters to optimize if we use this particular
#    method.  You can see this with the model lookup function, like this.

modelLookup('gamLoess')

# For this example, we'll keep the degree fixed at one.  We won't try out degree
#    two.  But to try out different values for the span, we still have to include
#    a column in the table with the named degree.  This is a requirement of the
#    caret package.  So we would define a grid using the expand.grid function,
#    like this.

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

# Now, we use the default cross-validation control parameters, so we type code
#    like this to train our model.

train_loess <- caret::train(y ~ .,
                     method = 'gamLoess',
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

# Then, select the best-performing model, and now we can see the final result.

confusionMatrix(data = predict(train_loess,
                               mnist_27$test),
                reference = mnist_27$test$y)$overall['Accuracy']

# It performs similarly to knn. However, we can see that the conditional
#    probability estimate is indeed smoother than what we get with knn.

plot_cond_prob <- function(p_hat) {
    GS <- 150
    new_x <- expand.grid(x_1 = seq(0, 0.5, len = GS),
                         x_2 = seq(0, 0.5, len = GS))
    new_x %>% mutate(p = p_hat) %>%
        ggplot(aes(x_1, x_2, z = p, fill = p)) +
        geom_raster() +
        scale_fill_gradientn(colors = c('#F8766D', 'white', '#00BFC4')) +
                                         stat_contour(breaks = c(0.5), color = 'black')
}

plot_cond_prob(predict(train_loess, mnist_27$true_p, type = 'prob')[, 2])

# Note that not all parameters in machine-learning algorithms are tuned.
# For example, in regression models or in LDA, we fit the best model using the
#    squares estimates or maximum likelihood estimates.  Those are not tuning
#    parameters.  We obtained those using least squares, or MLE, or some other
#    optimization technique.
# Parameters that are tuned are parameters that we can change and then get
#    an estimate of the model for each one.
# So in k-nearest neighbors, the number of neighbors is a tuning parameter.
# In regression, the number of predictors that we include could be considered a
#    parameter that's optimized.  So in the caret package, in the train function,
#    we only optimize parameters that are tunable.  So it won't be the case that,
#    for example, in regression models, the caret package will optimize the
#    regression coefficients that are estimated.
# Instead, it will just estimate using least squares.  This is an important
#    distinction to make when using the caret package--knowing which parameters
#    are optimized, and which ones are not.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Caret Package
# The exercises in Q1 and Q2 continue the analysis you began in the last set of assessments.

# Q1
# In the exercise in Q6 from Comprehension Check: Trees and Random Forests, we saw that changing nodesize to 50 and setting maxnodes to 25 yielded smoother results. Let's use the train function to help us pick what the values of nodesize and maxnodes should be.
#
# From the caret description of methods, we see that we can't tune the maxnodes parameter or the nodesize argument with randomForests. So we will use the __Rborist__ package and tune the minNode argument. Use the train function to try values minNode <- seq(25, 100, 25). Set the seed to 1.

library(randomForest)
library(tidyverse)
library(Rborist)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
set.seed(1)

fit <- caret::train(y ~ .,
             method = 'Rborist',
             tuneGrid = data.frame(predFixed = 1, minNode = seq(25, 100, 25)),
             data = dat)
fit

# Which value minimizes the estimated RMSE?
# 50

# Q2
# Part of the code to make a scatterplot along with the prediction from the best fitted model is provided below.
#
library(caret)
dat %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(x, y)) +
#     #BLANK
#     Which code correctly can be used to replace #BLANK in the code above?
# geom_step(aes(y_hat, x), col = 2)
 geom_step(aes(x, y_hat), col = 2)   # <-*
# geom_step(aes(x, y), col = 2)
# geom_step(aes(x_hat, y_hat), col = 2)
# geom_smooth(aes(x, y_hat), col = 2)
# geom_smooth(aes(y_hat, x), col = 2)

# The exercises from Q3 onward take you through an analysis using the tissue_gene_expression dataset.
#
# Q3
# Use the rpart function to fit a classification tree to the tissue_gene_expression dataset. Use the train function to estimate the accuracy. Try out cp values of seq(0, 0.1, 0.01). Plot the accuracies to report the results of the best model. Set the seed to 1991.
#
# Which value of cp gives the highest accuracy?
library(caret)
library(dslabs)
set.seed(1991)
data('tissue_gene_expression')

fit <- with(tissue_gene_expression,
            caret::train(x, y, method = 'rpart',
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)
fit$bestTune

# 0.0

# Q4
# Study the confusion matrix for the best fitting classification tree from the exercise in Q3.

confusionMatrix(fit)
# will show the confusion matrix for the classification tree from the tissue gene expression dataset. Looking at the confusion matrix, you can see that placenta is classified somewhat evenly across different tissue types, and in fact, placentas are called endometriums more frequently than they are called placentas.

# What do you observe happening for the placenta samples?
# Placenta samples are all accurately classified.
# Placenta samples are being classified as two similar tissues.
# Placenta samples are being classified somewhat evenly across tissues.   <-*
# Placenta samples not being classified into any of the classes.

# Q5
# Note that there are only 6 placentas in the dataset. By default, rpart requires 20 observations before splitting a node. That means that it is difficult to have a node in which placentas are the majority. Rerun the analysis you did in the exercise in Q3, but this time, allow rpart to split any node by using the argument control = rpart.control(minsplit = 0). Look at the confusion matrix again to determine whether the accuracy increases. Again, set the seed to 1991.

library(caret)
library(dslabs)
set.seed(1991)
data('tissue_gene_expression')

fit_rpart <- with(tissue_gene_expression,
            caret::train(x, y,
                         method = 'rpart',
                         control = rpart.control(minsplit = 0),
                         tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
ggplot(fit_rpart)

fit_rpart$bestTune
confusionMatrix(fit_rpart)

# What is the accuracy now?
#

# Q6
# Plot the tree from the best fitting model of the analysis you ran in Q5.
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

# Which gene is at the first split?
# B3GNT4
# CAPN3
# CES2
# CFHR4
# CLIP3
# GPA33   <-*
# HRH1

# Q7
# We can see that with just seven genes, we are able to predict the tissue type. Now let's see if we can predict the tissue type with even fewer genes using a Random Forest. Use the train function and the rf method to train a Random Forest. Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other values on your own). What mtry value maximizes accuracy? To permit small nodesize to grow as we did with the classification trees, use the following argument: nodesize = 1.
#
# Note: This exercise will take some time to run. If you want to test out your code first, try using smaller values with ntree. Set the seed to 1991 again.

set.seed(1991)
library(randomForest)
fit <- with(tissue_gene_expression,
            caret::train(x, y, method = 'rf',
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)
fit$bestTune$mtry

# What value of mtry maximizes accuracy?
#

# Q8
# Use the function varImp on the output of train and save it to an object called imp.
#
imp <- varImp(fit)
imp
# What should replace #BLANK in the code above?
# Do not include spaces in your answer.
#

# Q9
# The rpart model we ran above produced a tree that used just seven predictors. Extracting the predictor names is not straightforward, but can be done. If the output of the call to train was fit_rpart, we can extract the names like this:
#
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == '<leaf>')]))
tree_terms
# Calculate the variable importance in the Random Forest call for these seven predictors and examine where they rank.

data_frame(term = rownames(imp$importance),
           importance = imp$importance$Overall) %>%
    mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
    filter(term %in% tree_terms)

# What is the importance of the CFHR4 gene in the Random Forest call?
# Enter a number.
#
# What is the rank of the CFHR4 gene in the Random Forest call?
# Enter a number.

# -------       -------       -------       -------       -------       -------

# Case Study: MNIST
# We have learned several machine learning algorithms and demonstrated how to use
#    them with illustrative examples.  But we are now going to try them out on a
#    real example.
# This is a popular data set used in machine learning competitions called the
#    MNIST digits.  We can load the data using the following dslabs package,
#    like this.

mnist <- read_mnist()

# The data set includes two components, a training set and a test set. You can
#    see that by typing this.

names(mnist)

# Each of these components includes a matrix with features in the columns.
# You can access them using code like this.

dim(mnist$train$images)

# It also includes a vector with the classes as integers. You can see that by
#    using this code.

class(mnist$train$labels)
table(mnist$train$labels)

# Because we want this example to run on a small laptop and in less than an hour,
#    we'll consider a subset of the data set.  We will sample 10,000 random rows
#    from the training set and 1,000 random rows from the test set.

set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])

# -------       -------       -------       -------       -------       -------

# Processing MNIST Data
# In machine learning, we often transform predictors before running the machine
#    learning algorithm.  This is actually an important step.  We also remove
#    predictors that are clearly not useful--also an important step.  We call
#    all this pre-processing.  Examples of pre-processing include standardizing
#    the predictors, taking the log transform of some predictors or some other
#    transformation, removing predictors that are highly correlated with others,
#    and removing predictors with very few non-unique values or close to zero
#    variation.
# We're going to show an example of one of these.  The example we're going to
#    look at relates to the variability of the features.  We can see that there
#    are a large number of features with zero variability, or almost zero
#    variability.  We can use this code to compute the standard deviation of each
#    column and then plot them in a histogram.  Here's what it looks like.

library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I('black'))

# This is expected, because there are parts of the image that rarely contain
#    writing, very few dark pixels, so there's very little variation and almost
#    all the values are 0.  The caret package includes a function that
#    recommends features to be removed due to near zero variance.  You can run
#    it like this.

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

# We can see the columns that are removed--they're the yellow ones in this plot--
#    by simply making an image of the matrix.  Once we remove these columns, we
#    end up keeping these many columns. Now, we're ready to fit some models.

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

# -------       -------       -------       -------       -------       -------

# Model Fitting for MNIST Data
# In this video, we're going to actually implement k-nearest neighbors and random
#    forest on the mnist data.  However, before we start, we need to add
#    column names to the feature matrices, as this is a requirement of the caret
#    package.  We can do this using this code.

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

# We're going to add as a name the number of the column.  OK, so let's start with
#    knn.  The first step is to optimize for the number of neighbors.  Now, keep
#    in mind that when we run the algorithm, we will have to compute a distance
#    between each observation in the test set and each observation in the training
#    set.  These are a lot of calculations.
# We will therefore use k-fold cross-validation to improve speed.  So we can use
#    the caret package to optimize our k-nearest neighbor algorithm.

control <- trainControl(method = 'cv',
                        number = 10,
                        p = .9)
train_knn <- caret::train(x[, col_index],
                          y,
                          method = 'knn',
                          tuneGrid = data.frame(k = c(1, 3, 5, 7)),
                          trControl = control)

ggplot(train_knn)

# This will find the model that maximizes the accuracy.  Note that running this code
#    takes quite a bit of time on a standard laptop.  It could take several minutes.
# In general, it is a good idea to test out a piece of code with a small subset
#    of the data first to get an idea of timing, before we start running code that
#    might take hours to run or even days.  So always performs checks or make
#    calculations by hand to make sure that your code isn't going to take too much
#    time.  You want to know--have an idea-- of how long your code will take.
# So one thing you can do is test it out on smaller datasets.  Here we define n
#    and b as the number of rows that we're going to use and b, the number of
#    cross-validation folds that we're going to use.  Then we can start increasing
#    these numbers slowly to get an idea of how long the final code will take.

n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = 'cv', number = b, p = .9)
train_knn <- caret::train(x[index, col_index],
                          y[index],
                          method = 'knn',
                          tuneGrid = data.frame(k = c(3, 5, 7)),
                          trControl = control)

# Now, once we're done optimizing our algorithm, we can fit the entire dataset.
#    So we would code like this.

fit_knn <- knn3(x[, col_index],
                y,
                k = 5)
y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type = 'class')
cm <- confusionMatrix(y_hat_knn,
                      factor(y_test))
cm$overall['Accuracy']

# We see that our accuracy is almost 0.95. From the specificity and sensitivity
#    output coming from the confusion matrix function, we see that the eights are
#    the hardest to detect, and the most commonly incorrect predicted digit seven.

cm$byClass[, 1:2]

# Now, let's see if we can do even better with random forest.  With random forest,
#    computation time is even a bigger challenge than with k-nearest neighbors.
#    For each forest, we need to build hundreds of trees.  We also have several
#    parameters that we can tune.  Here we use the random forest implementation
#    in the Rborist package, which is faster than the one in the random forest
#    package.
# It has less features, but it is faster.  Because with random forest, the fitting
#    is the slowest part of the procedure rather than the predicting, as with knn,
#    we will only use five-fold cross-validation.  We'll also reduce the number
#    of trees that are fit, since we are not yet building our final model.
# Finally, we'll take a random subset of observations when constructing each tree.
# We can change this number with the nSamp argument in the Rborist function.
# So here's a code to optimize around a random forest.

library(Rborist)
control <- trainControl(method = 'cv',
                        number = 5,
                        p = 0.8)
grid <- expand.grid(minNode = c(1, 5),
                    predFixed = c(10, 15, 25, 35, 50))

train_rf <- caret::train(x[, col_index],
                         y,
                         method = 'Rborist',
                         nTree = 50,
                         trControl = control,
                         tuneGrid = grid,
                         nSamp = 5000)

# It takes a few minutes to run.  We can see the final results using ggplot.

ggplot(train_rf)
train_rf$bestTune

# And we can choose the parameters using the best tune component of the training
#    object.  And now we're ready to optimize our final tree.  Now we're going to
#    set the number of trees to a larger number.  We can write this piece of code.

fit_rf <- Rborist(x[, col_index],
                  y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

# Once the code is done running, and it takes a few minutes, we can see, using the
#    confusion matrix function, that our accuracy is above 0.95.

y_hat_rf <- factor(levels(y)[predict(fit_rf,
                                     x_test[, col_index])$yPred])
cm <- confusionMatrix(y_hat_rf,
                      y_test)
cm$overall['Accuracy']

# We have indeed improved over k-nearest neighbors.  Now, let's look at some
#    examples of the original image in the test set in our calls.
# You can see that that first one we called an eight. It's an eight.  The second
#    is also called an eight.  Looks like an eight.  And all of them look like we
#    made the right call.  Not surprising-- we have an accuracy above 0.95.
# Now, note that we have done minimal tuning here.  And with some further tuning,
#    examining more parameters, growing out more trees, we can get even higher
#    accuracy.

# -------       -------       -------       -------       -------       -------

# Variable Importance
# Earlier we described that one of the limitations of random forest is that
#    they're not very interpretable.  However, the concept of variable importance
#    helps a little bit in this regard.  Unfortunately, the current implementation
#    of the Arborist package does not yet support variable importance
#    calculations.  So to demonstrate the concept the variable importance,
#    we're going to use the random forest function in the random forest package.
#    Furthermore, we're not going to filter any columns out of the feature matrix
#    out.  We're going to use them all. So the code will look like this.

library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x,
                   y,
                   ntree = 50)
rf

# Once we run this, we can compute the importance of each feature using the
#    important function.  So we would type something like this.

imp <- importance(rf)
imp

# If you look at the importance, we immediately see that the first few features
#    have zero importance.  They're never used in the prediction algorithm.  This
#    makes sense because these are the features on the edges, the features that
#    have no [? writings ?] in them, no dark pixels in them.
# In this particular example, it makes sense to explore the importance of this
#    feature using an image.  We'll make an image where each feature is plotted
#    in the location of the image where it came from.  So we can use this code.

image(matrix(imp, 28, 28))

# And we see where the important features are. It makes a lot of sense.  They're
#    in the middle, where the writing is.  And you can kind of see the different
#    numbers there-- six, eight, seven.  These are the features that distinguish
#    one digit from another.  An important part of data science is visualizing
#    results to discern why we're failing.  How we do this depends on the
#    application.  For the examples with the digits, we'll find digits for which
#    we were quite certain of a call, but it was incorrect.  We can compare what
#    we got with k-nearest neighbors to what we got with random forest.
# So we can write code like this and then make images of the cases where we made
#    a mistake. Here they are.

p_max <- predict(fit_knn,
                 x_test[, col_index])
p_max <- apply(p_max,
               1,
               max)
ind <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind],
                 decreasing = TRUE)]

# That first one was called a zero. It's actually a two. You can kind of see why.
# The second one was called a four, but it's a six.  That one you definitely see
#    why we made a mistake, et cetera.  But by looking at these images, you might
#    get ideas of how you could improve your algorithm.  We can do the same for
#    random forest.  Here are the top 12 cases where we were very sure it was one
#    digit, when it was, in fact, another.

# -------       -------       -------       -------       -------       -------

# Ensembles
# A very powerful approach in machine learning is the idea of ensembling different
#    machine learning algorithms into one.  Let's explain what we mean by that.
# The idea of an ensemble is similar to the idea of combining data from different
#    pollsters to obtain a better estimate of the true support for different
#    candidates.  In machine learning, one can usually greatly improve the final
#    result of our predictions by combining the results of different algorithms.
# Here we present a very simple example, where we compute new class probabilities
#    by taking the average of the class probabilities provided by random forest
#    and k-nearest neighbors.  We can use this code to simply average these
#    probabilities.

p_rf <- predict(fit_rf,
                x_test[, col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn,
                 x_test[, col_index])
p <- (p_rf + p_knn) / 2
y_pred <- factor(apply(p,
                       1,
                       which.max) - 1)
confusionMatrix(y_pred,
                y_test)

# And we can see that once we do this, when we form the prediction, we actually
#    improve the accuracy over both k-nearest neighbors and random forest.

#    | Model                            | Accuracy |
#    |---                               |--        |
#    | Knn                              | 0.949    |
#    | Random Forest                    | 0.954    |
#    | Ensemble (Knn and Random Forest) | 0.961    |

# Now,notice, in this very simple example, we ensemble just two methods.
#    In practice, we might ensemble dozens or even hundreds of different methods.
#    And this really provides substantial improvements.


# -------       -------       -------       -------       -------       -------

# Comprehension Check: Ensembles
# For these exercises we are going to build several machine learning models for the mnist_27 dataset and then build an ensemble. Each of the exercises in this comprehension check builds on the last.

# Q1
# Use the training set to build a model with several of the models available from the caret package. We will test out all of the following models in this exercise:
#
models <- c('adaboost', 'avNNet',
            'gam', 'gamboost', 'gamLoess', 'gbm', 'glm',
            'knn', 'kknn',
            'lda', 'loclda', 'naive_bayes', 'qda',
            'mlp', 'monmlp',
            'ranger', 'Rborist', 'rf',
            'svmLinear', 'svmRadial', 'svmRadialCost', 'svmRadialSigma',
            'wsrf')
# We have not explained many of these, but apply them anyway using train with all the default parameters. You will likely need to install some packages. Keep in mind that you will probably get some warnings. Also, it will probably take a while to train all of the models - be patient!

# Run the following code to train the various models:
#
library(caret)
library(dslabs)
set.seed(1)
data('mnist_27')

fits <- lapply(models,
               function(model){
                   print(model)
                   caret::train(y ~ .,
                                method = model,
                                data = mnist_27$train)
               })

names(fits) <- models

# Did you train all of the models?
# Yes   <-*
# No

# Q2
# Now that you have all the trained models in a list, use sapply or map to create a matrix of predictions for the test set. You should end up with a matrix with length(mnist_27$test$y) rows and length(models).
#
# What are the dimensions of the matrix of predictions?

pred <- sapply(fits,
               function(object)
                   predict(object,
                           newdata = mnist_27$test))
dim(pred)
pred
# Number of rows:
# 200

# Number of columns:
# 23

# Q3
# Now compute accuracy for each model on the test set. Report the mean accuracy across all models.
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

# Q4
# Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble.

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

# What is the accuracy of the ensemble?
# 84.5

# Q5
# In Q3, we computed the accuracy of each method on the training set and noticed that the individual accuracies varied.

ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

# How many of the individual methods do better than the ensemble?
# 1
# loclda

# Which individual methods perform better than the ensemble?
# Select ALL that apply.
#
# glm
# lda
# naive_bayes
# svmLinear
# gamboost
# gamLoess
# qda
# knn
# kknn
# loclda   <-*
# gam
# rf
# ranger
# wsrf
# Rborist
# avNNet
# mlp
# monmlp
# adaboost
# gbm
# svmRadial
# svmRadialCost
# svmRadialSigma

# Q6
# It is tempting to remove the methods that do not perform well and re-do the ensemble. The problem with this approach is that we are using the test data to make a decision. However, we could use the accuracy estimates obtained from cross validation with the training data. Obtain these estimates and save them in an object. Report the mean accuracy of the new estimates.

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

# What is the mean accuracy of the new estimates?
# 0.811

# Q7
# Now let's only consider the methods with an estimated accuracy of greater than or equal to 0.8 when constructing the ensemble.

ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

# What is the accuracy of the ensemble now?
#

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Dimension Reduction

# Q1
# We want to explore the tissue_gene_expression predictors by plotting them.

data('tissue_gene_expression')
dim(tissue_gene_expression$x)

# We want to get an idea of which observations are close to each other, but, as you can see from the dimensions, the predictors are 500-dimensional, making plotting difficult. Plot the first two principal components with color representing tissue type.

pc <- prcomp(tissue_gene_expression$x)

data.frame(pc_1 = pc$x[, 1],
           pc_2 = pc$x[, 2],
           tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, pc_2, color = tissue)) +
    geom_point()
# Which tissue is in a cluster by itself?
# cerebellum
# colon
# endometrium
# hippocampus
# kidney
# liver   <-*
# placenta

# Q2
# The predictors for each observation are measured using the same device and experimental procedure. This introduces biases that can affect all the predictors from one observation. For each observation, compute the average across all predictors, and then plot this against the first PC with color representing tissue. Report the correlation.

avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs,
           tissue = tissue_gene_expression$y) %>%
    ggplot(aes(avgs, pc_1, color = tissue)) +
    geom_point()
cor(avgs, pc$x[,1])

# What is the correlation?
# 0.597

# Q3
# We see an association with the first PC and the observation averages. Redo the PCA but only after removing the center. Part of the code is provided for you.
#
# #BLANK
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2],
           tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, pc_2, color = tissue)) +
    geom_point()
# Which line of code should be used to replace #BLANK in the code block above?
# x <- with(tissue_gene_expression, sweep(x, 1, mean(x)))
# x <- sweep(x, 1, rowMeans(tissue_gene_expression$x))
# x <- tissue_gene_expression$x - mean(tissue_gene_expression$x)
# x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))   <-*

# Q4
# For the first 10 PCs, make a boxplot showing the values for each tissue.

for(i in 1:10){
    boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

# For the 7th PC, which two tissues have the greatest median difference?
# Select the TWO tissues that have the greatest median difference.
#
# cerebellum
# colon
# endometrium
# hippocampus
# kidney
# liver    <-*
# placenta <-*

# Q5
# Plot the percent variance explained by PC number. Hint: use the summary function.

plot(summary(pc)$importance[3,])

# How many PCs are required to reach a cumulative percent variance explained greater than 50%?
# 3

# -------       -------       -------       -------       -------       -------

# Recommendation Systems
# Recommendation systems use ratings that users have given items to make
#    specific recommendations to users.  Companies like Amazon that sell many
#    products to many customers and permit these customers to rate their
#    products are able to collect massive data sets that can be used to predict
#    what rating a given user will give a specific item.
# Items for which a high rating is predicted for specific users are then
#    recommended to that user.  Netflix uses recommendation systems to predict
#    how many stars a user will give a specific movie.  Here we provide the
#    basics of how these recommendations are predicted, motivated by some of
#    the approaches taken by the winners of the Netflix challenge.
# What's the Netflix challenge?
# On October 2006, Netflix offered a challenge to the data science community.
#    Improve our recommendation algorithm by 10% and win a $1 million.
# In September 2009, the winners were announced. You can follow this link to
#    see the news article.
# You can read a good summary of how the winning algorithm was put together
#    following this link.
# We'll include it in the class material. And a more detailed explanation
#    following this link, also included in the class material.
# Here we show you some of the data analysis strategies used by the winning
#    team.  Unfortunately, the Netflix data is not publicly available.
# But the GroupLens research lab generated their own database with over 20
#    million ratings for over 27,000 movies by more than 138,000 users.
# We make a small subset of this data available via the DS labs package.
#    You can upload it like this.

library(dslabs)
data("movielens")
head(movielens)

# We can see that the movie lens table is tidy formant and contains thousands
#    of rows.  Each row represents a rating given by one user to one movie.
# We can see the number of unique users that provide ratings and for how many
#    unique movies they provided them using this code.

movielens %>%
    summarize(n_users = n_distinct(userId),
              n_movies = n_distinct(movieId))

# If we multiply those two numbers, we get a number much larger than 5 million.
#    Yet our data table has about 100,000 rows.  This implies that not every
#    user rated every movie.  So we can think of this data as a very large
#    matrix with users on the rows and movies on the columns with many empty
#    cells.  The gather function permits us to convert to this format, but if
#    we try to do it for the entire matrix it will crash R. So lets look at a
#    smaller subset.  This table shows a very small subset of seven users and
#    five movies.  You can see the ratings that each user gave each movie
#    and you also see NA's for movies that they didn't watch or they didn't
#    rate.  You can think of the tasking recommendation systems as filling in
#    the NA's in the table we just showed.  To see how sparse the entire matrix
#    is, here the matrix for a random sample of 100 movies and 100 users
#    is shown with yellow indicating a user movie combination for which we have
#    a rating.  All right so let's move on to try to make predictions.  The
#    machine learning challenge here is more complicated than we have studied
#    up to now because each outcome y has a different set of predictors.
# To see this, note that if we are predicting the rating for movie i by user u,
#    in principle, all other ratings related to movie i and by user u may be
#    used as predictors.
# But different users rate a different number of movies and different movies.
# Furthermore, we may be able to use information from other movies that we have
#    determined are similar to movie i or from users determined to be similar
#    to user u.  So in essence, the entire matrix can be used as predictors for
#    each cell.  OK.  So let's get started.  Let's look at some of the general
#    properties of the data to better understand the challenge.
# The first thing we notice is that some movies get rated more than others.
#    Here's the distribution.  This should not surprise us given that there are
#    blockbusters watched by millions and artsy independent movies watched
#    by just a few.

movielens %>%
    count(movieId) %>%
    ggplot(aes(n)) +
    geom_histogram(bins = 30, color = 'black') +
    scale_x_log10() +
    ggtitle('Movies')

# A second observation is that some users are more active than others at rating
#    movies.  Notice that some users have read it over 1,000 movies while
#    others have only rated a handful.  To see how this is a machine learning
#    challenge, note that we need to build an algorithm with data we have
#    collected.  And this algorithm will later be used by others as users look
#    for movie recommendations.

movielens %>%
    count(userId) %>%
    ggplot(aes(n)) +
    geom_histogram(bins = 30, color = "black") +
    scale_x_log10() +
    ggtitle("Users")

# So let's create a test set to assess the accuracy of the models we implement,
#    just like in other machine learning algorithms.  We use the caret package
#    using this code.

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating,
                                  times = 1,
                                  p = 0.2,
                                  list = FALSE)
train_set <- movielens[-test_index,]

# To make sure we don't include users and movies in the test set that do not
#    appear in the training set, we removed these using the semi_join function,
#    using this simple code.

test_set <- movielens[test_index,]
test_set <- test_set %>%
    semi_join(train_set,
              by = 'movieId') %>%
    semi_join(train_set,
              by = 'userId')

# All right, now.  To compare different models or to see how well we're doing
#    compared to some baseline, we need to quantify what it means to do well.
# We need a loss function.  The Netflix challenge used the typical error and
#    thus decided on a winner based on the residual mean squared error on a
#    test set.  So if we define yui as the rating for movie i by user u and y
#    hat ui as our prediction, then the residual mean squared error is defined
#    as follows.
# Here n is a number of user movie combinations and the sum is occurring over
#    all these combinations.  Remember that we can interpret the residual mean
#    squared error similar to standard deviation.  It is the typical error we
#    make when predicting a movie rating.  If this number is much larger than
#    one, we're typically missing by one or more stars rating which is not very
#    good.  So let's quickly write a function that computes this residual means
#    squared error for a vector of ratings and their corresponding predictors.
# It's a simple function that looks like this.

RMSE <- function(true_ratings, predicted_ratings) {
    sqrt(mean(true_ratings - predicted_ratings)^2)
}

# And now we're ready to build models and compare them to each other.

# -------       -------       -------       -------       -------       -------

# Building Recommendation System
# The Netflix challenge winners implemented two general classes of models.
# One was similar to k-nearest neighbors, where you found movies that were
#    similar to each other and users that were similar to each other.
# The other one was based on an approach called matrix factorization.
# That's the one we we're going to focus on here.
# So let's start building these models. Let's start by building the simplest
#    possible recommendation system.  We're going to predict the same rating
#    for all movies, regardless of the user and movie.
# So what number should we predict?
# We can use a model-based approach.
# A model that assumes the same rating for all movies and all users, with all
#    the differences explained by random variation would look something like
#    this.
# Here epsilon represents independent errors sampled from the same distribution
#    centered at zero, and mu represents the true rating for all movies and
#    users.  We know that the estimate that minimizes the residual mean
#    squared error is the least squared estimate of mu.  And in this case,
#    that's just the average of all the ratings.  We can compute it like this.

mu_hat <- mean(train_set$rating)
mu_hat

# That average is 3.54.
# So that is the average rating of all movies across all users.
# So let's see how well this movie does.
# We compute this average on the training data.
# And then we compute the residual mean squared error on the test set data.
# So we're predicting all unknown ratings with this average.
# We get a residual mean squared error of about 1.05.

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

# That's pretty big.
# Now note, if you plug in any other number, you get a higher RMSE.
# That's what's supposed to happen, because we know that the average minimizes
#    the residual mean squared error when using this model.
# And you can see it with this code.

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

# So we get a residual mean squared error of about 1.
# To win the grand prize of $1 million, a participating team had to get to a residual mean squared error of about 0.857.
# So we can definitely do better.
# Now because as we go along we will be comparing different approaches, we're
#    going to create a table that's going to store the results that we
#    obtain as we go along.
# We're going to call it RMSE results.
# It's going to be created using this code.

rmse_results <- data_frame(method = 'Just the average',
                           RMSE = naive_rmse)
rmse_results

# All right.
# So let's see how we can do better.
# We know from experience that some movies are just generally rated higher than
#     others.
# We can see this by simply making a plot of the average rating that each movie
#     got.
# So our intuition that different movies are rated differently is confirmed by
#     data.
# So we can augment our previous model by adding a term, b i, to represent the
#     average rating for movie i.
# In statistics, we usually call these b's, effects.
# But in the Netflix challenge papers, they refer to them as "bias," thus the b
#     in the notation.
# All right.
# We can again use these squares to estimate the b's in the following way,
#    like this.

fit <- lm(rating ~ as.factor(userId), data = movielens)
fit

# However, note that because there are thousands of b's, each movie gets
#    one parameter, one estimate.
# So the lm function will be very slow here.
# So we don't recommend running the code we just showed.
# However, in this particular situation, we know that the least squared
#    estimate, b hat i, is just the average of yui minus the overall mean for
#    each movie, i.
# So we can compute them using this code.

mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu))
mu
movie_avgs

# Note that we're going to drop the hat notation in the code to represent the
#     estimates going forward, just to make the code cleaner.
# So this code completes the estimates for the b's.

movie_avgs %>% qplot(b_i,
                     geom = 'histogram',
                     bins = 10,
                     data = .,
                     color = I('black'))

# We can see that these estimates vary substantially, not surprisingly.
# Some movies are good.
# Other movies are bad.
# Remember, the overall average is about 3.5.
# So a b i of 1.5 implies a perfect five-star rating.
# Now let's see how much our prediction improves once we predict using the
#    model that we just fit.
# We can use this code and see that our residual mean squared error did drop a
#    little bit.

predicted_ratings <- mu + test_set %>%
    left_join(movie_avgs, by='movieId') %>%
    .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# We already see an improvement.
# Now can we make it better?
# All right.
# How about users?
# Are different users different in terms of how they rate movies?
# To explore the data, let's compute the average rating for user, u, for those
#    that have rated over 100 movies.
# We can make a histogram of those values.
# And it looks like this.

train_set %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating)) %>%
    filter(n() >= 100) %>%
    ggplot(aes(b_u)) +
    geom_histogram(bins = 30, color = 'black')

# Note that there is substantial variability across users, as well.
# Some users are very cranky.
# And others love every movie they watch, while others are somewhere in the
#    middle.
# This implies that a further improver to our model may be something like this.
# We include a term, bu, which is the user-specific effect.
# So now if a cranky user--this is a negative bu-- rates a great movie, which
#    will have a positive b i, the effects counter each other, and we may be
#    able to correctly predict that this user gave a great movie a three rather
#    than a five, which will happen.
# And that should improve our predictions.
# So how do we fit this model?
#     Again, we could use lm.
# The code would look like this.

lm(rating ~ as.factor(movieId) + as.factor(userId))

# But again, we won't do it, because this is a big model.
# It will probably crash our computer.
# Instead, we will compute our approximation by computing the overall mean,
#    u-hat, the movie effects, b-hat i, and then estimating the user effects,
#    b u-hat, by taking the average of the residuals obtained after removing
#    the overall mean and the movie effect from the ratings yui.
# The code looks like this.

user_avgs <- test_set %>%
    left_join(movie_avgs, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))

# And now we can see how well we do with this new model by predicting values
#    and computing the residual mean squared error.

predicted_ratings <- test_set %>%
    left_join(movie_avgs, by = 'movieId') %>%
    left_join(user_avgs, by = 'userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred


model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = 'Movie + User Effects Model',
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# We see that now we obtain a further improvement.
# Our residual mean squared error dropped down to about 0.89.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Recommendation Systems

# The following exercises all work with the movielens data, which can be loaded using the following code:
#
library(dslabs)
data("movielens")

# Q1
# Compute the number of ratings for each movie and then plot it against the year the movie came out. Use the square root transformation on the counts.

movielens %>% group_by(movieId) %>%
    summarize(n = n(), year = as.character(first(year))) %>%
    qplot(year, n, data = ., geom = "boxplot") +
    coord_trans(y = "sqrt") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# What year has the highest median number of ratings?
# 1995

# Q2
# We see that, on average, movies that came out after 1993 get more ratings. We also see that with newer movies, starting in 1993, the number of ratings decreases with year: the more recent a movie is, the less time users have had to rate it.
#
# Among movies that came out in 1993 or later, what are the 25 movies with the most ratings per year, and what is the average rating of each of the top 25 movies?

movielens %>%
    filter(year >= 1993) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = 2018 - first(year),
              title = title[1],
              rating = mean(rating)) %>%
    mutate(rate = n/years) %>%
    top_n(25, rate) %>%
    arrange(desc(rate))

# What is the average rating for the movie The Shawshank Redemption?
# 4.49

# What is the average number of ratings per year for the movie Forrest Gump?
# 14.2

# Q3
# From the table constructed in Q2, we can see that the most frequently rated movies tend to have above average ratings. This is not surprising: more people watch popular movies. To confirm this, stratify the post-1993 movies by ratings per year and compute their average ratings. Make a plot of average rating versus ratings per year and show an estimate of the trend.

movielens %>%
    filter(year >= 1993) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = 2017 - first(year),
              title = title[1],
              rating = mean(rating)) %>%
    mutate(rate = n/years) %>%
    ggplot(aes(rate, rating)) +
    geom_point() +
    geom_smooth()

# What type of trend do you observe?
# There is no relationship between how often a movie is rated and its average rating.
# Movies with very few and very many ratings have the highest average ratings.
# The more often a movie is rated, the higher its average rating.   <-*
# The more often a movie is rated, the lower its average rating.

# Q4
# Suppose you are doing a predictive analysis in which you need to fill in the missing ratings with some value.
#
# Given your observations in the exercise in Q3, which of the following strategies would be most appropriate?
# Fill in the missing values with the average rating across all movies.
# Fill in the missing values with 0.
# Fill in the missing values with a lower value than the average rating across all movies.   <-*
# Fill in the value with a higher value than the average rating across all movies.
# None of the above.

# Q5
# The movielens dataset also includes a time stamp. This variable represents the time and data in which the rating was provided. The units are seconds since January 1, 1970. Create a new column date with the date.
#
# Which code correctly creates this new column?
# movielens <- mutate(movielens, date = as.date(timestamp))
movielens <- mutate(movielens, date = as_datetime(timestamp))   # <-*
# movielens <- mutate(movielens, date = as.data(timestamp))
# movielens <- mutate(movielens, date = timestamp)

# Q6
# Compute the average rating for each week and plot this average against day. Hint: use the round_date function before you group_by.

movielens %>% mutate(date = round_date(date, unit = "week")) %>%
    group_by(date) %>%
    summarize(rating = mean(rating)) %>%
    ggplot(aes(date, rating)) +
    geom_point() +
    geom_smooth()

# What type of trend do you observe?
# There is strong evidence of a time effect on average rating.
# There is some evidence of a time effect on average rating.   <-*
# There is no evidence of a time effect on average rating.

# Q7
# Consider again the plot you generated in Q6.
#
# If we define  as the day for user's  rating of movie , which of the following models is most appropriate?
#
# Yu,i = mu + bi + bu + du,i + Eu,i
# Yu,i = mu + bi +bu + du,iB + Eu,i
# Yu,i = mu + bi + bu + du,iBi + Eu,i
# Yu,i = mu + bi + bu + f(du,i) + Eu,i , with  a smooth function of du,   <-*

# Q8
# The movielens data also has a genres column. This column includes every genre that applies to the movie. Some movies fall under several genres. Define a category as whatever combination appears in this column. Keep only categories with more than 1,000 ratings. Then compute the average and standard error for each category. Plot these as error bar plots.

movielens %>% group_by(genres) %>%
    summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
    filter(n >= 1000) %>%
    mutate(genres = reorder(genres, avg)) %>%
    ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
    geom_point() +
    geom_errorbar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Which genre has the lowest average rating?
# Enter the name of the genre exactly as reported in the plot, including capitalization and punctuation.
#

# Q9
# The plot you generated in Q8 shows strong evidence of a genre effect. Consider this plot as you answer the following question.
#
# If we define  as the day for user's  rating of movie , which of the following models is most appropriate?
#
# Yu,i = mu + bi + bu + gu,i + Eu,i
# Yu,i = mu + bi + bu + gu,iB + Eu,i
# Yu,i = mu + bi + bu + SUMk = 1^kxu,iBk + Eu,i , with  if gu,i is genre k  <-*
# Yu,i = mu + bi + bu + f(gu,i), with f a smooth function of gu,i

# -------       -------       -------       -------       -------       -------

# Regularization
# In this video, we're going to introduce the concept of regularization and show
#    how it can improve our results even more.
# This is one of the techniques that was used by the winners of the Netflix
#    challenge. All right. So how does it work? Note that despite the large movie t
#    o movie variation, our improvement in residual mean square error when we just
#    included the movie effect was only about 5%.
# So let's see why this happened. Let's see why it wasn't bigger. Let's explore
#    where we made mistakes in our first model when we only used movies.
# Here are 10 of the largest mistakes that we made when only using the movie
#    effects in our models. Here they are.
# Note that these all seem to be obscure movies and in our model many of them
#    obtained large predictions.
# So why did this happen?
#    To see what's going on, let's look at the top 10 best movies in the top 10
#    worst movies based on the estimates of the movie effect b hat i.
# So we can see the movie titles, we're going to create a database that includes
#    movie ID and titles using this very simple code.
# So here are the best 10 movies according to our estimates.
# America is number one, Love and Human Remains also number one, Infer L number
#    one. Look at the rest of the movies in this table.
# And here are the top 10 worst movies.
# The first one started with Santa with Muscles.
# Now they all have something in common.
# They're all quite obscure.
# So let's look at how often they were rated.
# Here's the same table, but now we include the number of ratings they received
#    in our training set.
# We can see the same for the bad movies.
# So the supposed best and worst movies were rated by very few users, in most cases
#    just one.
# These movies were mostly obscure ones.
# This is because with just a few users, we have more uncertainty, therefore larger
#    estimates of bi, negative or positive, are more likely when fewer users rate
#    the movies.
# These are basically noisy estimates that we should not trust, especially when
#    it comes to prediction.
# Large errors can increase our residual mean squared error, so we would rather
#    be conservative when we're not sure.
# Previously we've learned to compute standard errors and construct confidence
#    intervals to account for different levels of uncertainty.
# However, when making predictions we need one number, one prediction, not an
#    interval.
# For this, we introduce the concept of regularization.
# Regularization permits us to penalize large estimates that come from small
#    sample sizes.
# It has commonalities with the Bayesian approaches that shrunk predictions.
# The general idea is to add a penalty for large values of b to the sum of squares
#    equations that we minimize.
# So having many large b's makes it harder to minimize the equation that we're
#    trying to minimize.
# One way to think about this is that if we were to fit an effect to every rating,
#    we could of course make the sum of squares equation by simply making each b
#    match its respective rating y.
# This would yield an unstable estimate that changes drastically with new instances
#    of y.
# Remember y is a random variable.
# But by penalizing the equation, we optimize to b bigger when the estimate b are
#    far from zero.
# We then shrink the estimates towards zero.
# Again, this is similar to the Bayesian approach we've seen before.
# So this is what we do.
# To estimate the b's instead of minimizing the residual sum of squares as is done
#    by least squares, we now minimize this equation.
# Note the penalty term.
# The first term is just the residual sum of squares and the second is a penalty
#    that gets larger when many b's are large.
# Using calculus, we can actually show that the values of b that minimized equation
#    are given by this formula, where ni is a number of ratings b for movie i.
# Note that this approach will have our desired effect.
# When ni is very large which will give us a stable estimate, then lambda is
#    effectively ignored because ni plus lambda is about equal to ni.
# However, when ni is small, then the estimate of bi is shrunken towards zero.
# The larger lambda, the more we shrink.
# So let's compute these regularized estimates of vi using lambda equals to 3.0.
# Later we see why we picked this number.
# So here is the code.
# To see how the estimates shrink, let's make a plot of the regularized estimate
#    versus the least square estimates with the size of the circle telling us how
#    large ni was.
# You can see that when n is small, the values are shrinking more towards zero.
# All right, so now let's look at our top 10 best movies based on the estimates
#    we got when using regularization.
# Note that the top five movies are now All
# About Eve, Shawshank Redemption, The Godfather, The Godfather
# II, and the Maltese Falcons.
# This makes much more sense.
# We can also look at the worst movies and the worst five are Battlefield Earth,
#    Joe's Apartment, Speed 2, Cross
# Control, Super Mario Bros, and Police Academy 6: City Under Siege.
# Again, this makes sense.
# So do we improve our results?
# We certainly do.
# We get the residual mean squared error all the way down to 0.885 from 0.986.
# So this provides a very large improvement.
# Now note that lambda is a tuning parameter.
# We can use cross-fertilization to choose it.
# We can use this code to do this.
# And we see why we picked 3.0 as lambda.
# One important point.
# Note that we show this as an illustration and in practice, we should be using
#    full cross-validation just on a training set without using the test it until
#    the final assessment.
# We can also use regularization to estimate the user effect.
# The equation we would minimize would be this one now.
# It includes the parameters for the user effects as well.
# The estimates that minimizes can be found similarly to what we do previously.
# Here we again use cross-validation to pick lambda.
# The code looks like this, and we see what lambda minimizes our equation.
# For the full model including movie and user effects, the optimal lambda is 3.75.
# And we can see that we indeed improved our residual mean squared error.
# Now it's 0.881.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Regularization

# The exercises in Q1-Q8 work with a simulated dataset for 100 schools. This pre-exercise setup walks you through the code needed to simulate the dataset.
#
# An education expert is advocating for smaller schools. The expert bases this recommendation on the fact that among the best performing schools, many are small schools. Let's simulate a dataset for 100 schools. First, let's simulate the number of students in each school, using the following code:

set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely independent from size. This is the parameter we want to estimate in our analysis. The true quality can be assigned using the following code:

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste('PS', 1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# We can see the top 10 schools using this code:

schools %>%
    top_n(10, quality) %>%
    arrange(desc(quality))

# Now let's have the students in the school take a test. There is random variability in test taking, so we will simulate the test scores as normally distributed with the average determined by the school quality with a standard deviation of 30 percentage points. This code will simulate the test scores:

set.seed(1)
scores <- sapply(1:nrow(schools),
                 function(i) {
                     scores <- rnorm(schools$size[i],
                                     schools$quality[i],
                                     30)
                     scores
                })
schools <- schools %>% mutate(score = sapply(scores, mean))

# Q1
# What are the top schools based on the average score? Show just the ID, size, and the average score.

first_ten <- schools %>%
    top_n(10, score) %>%
    arrange(desc(score)) %>%
    select(id, size, score)
first_ten

# Report the ID of the top school and average score of the 10th school.
#
# What is the ID of the top school?
#     Note that the school IDs are given in the form "PS x" - where x is a number. Report the number only.
# 67

# What is the average score of the 10th school?
# 88.09490

# Q2
# Compare the median school size to the median school size of the top 10 schools based on the score.

median(schools$size)
schools %>%
    top_n(10, score) %>%
    .$size %>%
    median()

# What is the median school size overall?
# 261

# What is the median school size of the of the top 10 schools based on the score?
# 136

# Q3
# According to this analysis, it appears that small schools produce better test scores than large schools. Four out of the top 10 schools have 100 or fewer students. But how can this be? We constructed the simulation so that quality and size were independent. Repeat the exercise for the worst 10 schools.

median(schools$size)
schools %>%
    top_n(-10, score) %>%
    .$size %>%
    median()

# What is the median school size of the bottom 10 schools based on the score?
# 146

# Q4
# From this analysis, we see that the worst schools are also small. Plot the average score versus school size to see what's going on. Highlight the top 10 schools based on the true quality. Use a log scale to transform for the size.

schools %>% ggplot(aes(size, score)) +
    geom_point(alpha = 0.5) +
    geom_point(data = filter(schools, rank<=10), col = 2)

# What do you observe?
# There is no difference in the standard error of the score based on school size; there must be an error in how we generated our data.
# The standard error of the score has larger variability when the school is smaller, which is why both the best and the worst schools are more likely to be small.   <-*
# The standard error of the score has smaller variability when the school is smaller, which is why both the best and the worst schools are more likely to be small.
# The standard error of the score has larger variability when the school is very small or very large, which is why both the best and the worst schools are more likely to be small.
# The standard error of the score has smaller variability when the school is very small or very large, which is why both the best and the worst schools are more likely to be small.

# Q5
# Let's use regularization to pick the best schools. Remember regularization shrinks deviations from the average towards 0. To apply regularization here, we first need to define the overall average for all schools, using the following code:

overall <- mean(sapply(scores, mean))

# Then, we need to define, for each school, how it deviates from that average.

alpha <- 25
score_reg <- sapply(scores,
                    function(x)  overall + sum(x - overall) / (length(x) + alpha))
schools %>%
    mutate(score_reg = score_reg) %>%
    top_n(10, score_reg) %>%
    arrange(desc(score_reg))

# Write code that estimates the score above the average for each school but dividing by n + alpha  instead of , with n the schools size and alpha a regularization parameters. Try alpha = 25.
#
# What is the ID of the top school with regularization?
# Note that the school IDs are given in the form "PS x" - where x is a number. Report the number only.
# 91

# What is the regularized score of the 10th school?
# 86.9

# Q6
# Notice that this improves things a bit. The number of small schools that are not highly ranked is now lower. Is there a better ? Find the  that minimizes the RMSE = .

alphas <- seq(10,250)
rmse <- sapply(alphas,
               function(alpha){
                   score_reg <- sapply(scores,
                                       function(x) overall + sum(x - overall) / (length(x) + alpha))
                   mean((score_reg - schools$quality)^2)
                })
plot(alphas, rmse)
alphas[which.min(rmse)]

# What value of  gives the minimum RMSE?
# 128

# Q7
# Rank the schools based on the average obtained with the best . Note that no small school is incorrectly included.
#
# What is the ID of the top school now?
# Note that the school IDs are given in the form "PS x" - where x is a number. Report the number only.

alpha <- alphas[which.min(rmse)]
score_reg <- sapply(scores, function(x) overall + sum(x - overall) / (length(x) + alpha))
schools %>%
    mutate(score_reg = score_reg) %>%
    top_n(10, score_reg) %>%
    arrange(desc(score_reg))


# What is the regularized average score of the 10th school now?
#

# Q8
# A common mistake made when using regularization is shrinking values towards 0 that are not centered around 0. For example, if we don't subtract the overall average before shrinking, we actually obtain a very similar result. Confirm this by re-running the code from the exercise in Q6 but without removing the overall mean.

alphas <- seq(10,250)
rmse <- sapply(alphas,
               function(alpha){
                   score_reg <- sapply(scores,
                                       function(x) sum(x) / (length(x) + alpha))
                   mean((score_reg - schools$quality)^2)
               })
plot(alphas, rmse)
alphas[which.min(rmse)]

# What value of  gives the minimum RMSE here?
# 10

# -------       -------       -------       -------       -------       -------

# Matrix Factorization
# Matrix factorisation is a widely used concept in machine learning.
# It is very much related to factor analysis, single value composition, and
#    principal component analysis, or PCA.
# Here we describe the concept in the context of movie recommendation systems.
# We have previously described the following model which accounts for movie and
#    movie differences through the parameters bi, and user reviews or differences
#    through parameters bu.

# Yu,i = mu + bi + bu + Eu,i

# But this model leaves out an important source of variation related to the fact
#    that groups of movies have similar rating patterns and groups of users
#    have similar rating patterns as well.
# We will discover these patterns by studying the residuals obtained after fitting
#    our model.
#    ru,i = yu,i + bi + bu
# These residuals.
# To study these residuals, we will convert the data into a matrix so that each
#    user gets a row and each movie gets a column.
# So yui is the entry in row u and column i.  User u, movie i.
# For illustration purposes, we will only consider a small subset of movies with
#    many ratings and users that have rated many movies.
# We will use this code to generate our training data.

train_small <- movielens %>%
    group_by(movieId) %>%
    filter(n() >= 50 | movieId == 3252) %>%
    ungroup() %>% # 3252 is Scent of a Woman used in example
    group_by(userId) %>%
    filter(n() >= 50) %>%
    ungroup()

y <- train_small %>%
    select(userId, movieId, rating) %>%
    spread(movieId, rating) %>%
    as.matrix()

# To facilitate exploration we add row names and column names.

rownames(y) <- y[, 1]
y <- y[,-1]

movie_titles <- movielens %>%
    select(movieId, title) %>%
    distinct()

colnames(y) <- with(movie_titles,
                    title[match(colnames(y),
                                movieId)])

# The column names will be the movie names.
# And we convert these residuals by removing the column and row averages.
# Here's the code.

y <- sweep(y,
           2,
           colMeans(y,
                    na.rm = TRUE))
y <- sweep(y,
           1,
           rowMeans(y,
                    na.rm = TRUE))

# OK, now.
# If the model we've been using describes all the signal and the extra ones are
#    just noise, then the residuals for different movies should be independent
#    of each other.  But they are not. Here's an example.
# Here's a plot of the residuals for The Godfather and The Godfather II.
# They're very correlated.
# This plot says that users that liked the godfather more than what the model
#    expects them to based on the movie and user effects also like The Godfather
#    II more than expected.

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

# The same is true for The Godfather and Goodfellas.

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

# You can see it in this plot. Although not as strong, there still is a correlation.
#    We see a correlation between other movies as well. For example, here's a
#    correlation between You've Got Mail and Sleepless in Seattle.

m_4 <- "You've Got Mail"
m_5 <- "Sleepless in Seattle"
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

# We can see a pattern.

x <- y[,
       c(m_1, m_2, m_3, m_4, m_5)]
colnames(x)[1:2] <- c("Godfather", "Godfather 2")
cor(x,
    use = "pairwise.complete") %>%
    knitr::kable()

# If we look at the pairwise correlation for these five movies, we can see that
#    there's a positive correlation between the gangster movies
# Godfathers and Goodfellas, and then there's a positive correlation between the
#    romantic comedies You've
# Got Mail and Sleepless in Seattle.
# We also see a negative correlation between the gangster movies and the romantic
#    comedies.
# This means that users that like gangster movies a lot tend to not like romantic
#    comedies and vise versa.
# This result tells us that there is structure in the data that the model does not
#    account for.
# So how do we model this?
#     Here is where we use matrix factorization.
# We're going to define factors.
# Here's an illustration of how we could use some structure to predict the residuals.

q <- matrix(c(1 , 1, 1, -1, -1),
            ncol = 1)
rownames(q) <- c("Godfather",
                 "Godfather 2",
                 m_3,
                 m_4,
                 m_5)
p <- matrix(rep(c(2,0,-2),
                c(3,5,4)),
            ncol = 1)
rownames(p) <- 1:nrow(p)

set.seed(1)
r <- jitter(p %*% t(q))
round(r, 1)

# Suppose the residuals look like this. This is a simulation.  There seems to be
#    a pattern here. It's based on what we saw with the real data.
# There's a gangster movie effect and there's a romantic comedy effect.
# In fact, we see a very strong correlation pattern, which we can see here.

cor(r)
t(q)

# This structure could be explained using the following coefficients.
# We assign a 1 to the gangster movies and a minus one to the romantic comedies.
# In this case, we can narrow down movies to two groups, gangster and romantic comedy.

p

# Note that we can also reduce the users to three groups, those that like gangster
#    movies but hate romantic comedies, the reverse, and those that don't care.
# The main point here is that we can reconstruct this data that has 60 values with
#    a couple of vectors totaling 17 values.
# Those two vectors we just showed can be used to form the matrix with 60 values.
# We can model the 60 residuals with the 17 parameter model like this.
# And this is where the factorization name comes in.
# We have a matrix r and we factorised I used it into two things, the vector p,
#    and the vector q.
# Now we should be able to explain much more of the variance if we use a model
#    like this one.
# Now the structure in our movie data seems to be much more complicated than gangster
#    movie versus romantic comedies.
# We have other factors.
# For example, and this is a simulation, let's suppose we had the movie Scent of
#    a Woman, and now the data looks like this.

set.seed(1)
m_6 <- "Scent of a Woman"
q <- cbind(c(1 , 1, 1, -1, -1, -1),
           c(1 , 1, -1, -1, -1, 1))
rownames(q) <- c("Godfather", "Godfather 2", m_3, m_4, m_5, m_6)
p <- cbind(rep(c(2,0,-2), c(3,5,4)),
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(p) <- 1:nrow(p)

r <- jitter(p %*% t(q), factor=1)
round(r, 1)

# Now we see another factor, a factor that divides users into those that love,
#    those that hate, and those that don't care for Al Pacino.
# The correlation is a bit more complicated now.
# We can see it here.

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
x <- y[,six_movies]
colnames(x)[1:2] <- c("Godfather", "Godfather 2")
cor(x, use="pairwise.complete")

# Now to explain the structure, we need two factors. Here they are.

t(q)

# The first one divides gangster movies from romantic comedies.
# The second factor divide Al Pacino movies and non Al Pacino movies.
# And we also have two sets of coefficients to describe the users.
# You can see it here.

p

# The model now has more parameters, but still less than the original data.
# So we should be able to fit this model using, for example, the least squares
#    method.  However, for the Netflix challenge, they used regularization,
#    and they penalize not just the user and movie effects, but also large values
#    of the factors p or q.
# Now does this simulation match the actual data?
#     Here are the correlation we get for the movies we just showed, but using
#     the actual data.

cor(r)

# Notice that the structure is similar.
# However, if we want to find the structure using the data as opposed to constructing
#    it ourselves as we did in the example, we need to fit models to data.
# So now we have to figure out how to estimate factors from the data as opposed
#    to defining them ourselves.
# One way to do this is to fit models, but we can also use principle component
#    analysis or equivalently, the singular reality composition to estimate factors
#    from data.
# And we're going to show that in the next video.

# -------       -------       -------       -------       -------       -------

# SVD and PCA
# The matrix vectorization decomposition that we showed in the previous video that
#    looks something like this is very much related to singular value composition
#    and PCA.
# Singular value composition and principal component analysis are complicated
#    concepts, but one way to understand them is to think of, for example, singular
#    value decomposition as an algorithm that finds the vectors p and q that
#    permit us to write the matrix of residuals r with m rows and n columns
#    in the following way.
# But with the added bonus that the variability of these terms is decreasing and
#    also that the p's are uncorrelated to each other.
# The algorithm also computes these variabilities so that we can know how much of
#    the matrix's total variability is explained as we add new terms.
# This may permit us to see that with just a few terms, we can explain most of
#    the variability.
# Let's see an example with our movie data.  To compute the decomposition, will
#    make all DNA zero. So we will write this code.

y[is.na(y)] <- 0
pca <- prcomp(y)

# The vectors q are called the principal components and they are stored in this
#    matrix.

dim(pca$rotation)

# While the p vectors which are the user's effects are stored in this matrix.

dim(pca$x)

# The PCA function returns a component with the variability of each of the
#    principal components and we can access it like this and plot it.

plot(pca$sdev)

# We can also see that just with a few of these principal components we already
#    explain a large percent of the data.

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

# So for example, with just 50 principal components we're already explaining about
#    half the variability out of a total of over 300 principal components.
# To see that the principal components are actually capturing something important
#    about the data, we can make a plot of for example, the first two principal
#    components, but now label the points with the movie that each one of those
#    points is related to.

library(ggrepel)

pcs <- data.frame(pca$rotation, name = colnames(y))

highlight <- filter(pcs, PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1)

pcs %>%  ggplot(aes(PC1, PC2)) +
    geom_point() +
    geom_text_repel(aes(PC1,
                        PC2,
                        label = name),
                    data = highlight,
                    size = 2)

# Just by looking at the top three in each direction, we see meaningful patterns.
# The first principle component shows the difference between critically acclaimed
#    movies on one side.
# Here are the one extreme of the principal component.

pcs %>%
    select(name, PC1) %>%
    arrange(PC1) %>%
    slice(1:10)

# You can see Pulp Fiction, Seven, Fargo, Taxi Driver, and Hollywood blockbusters
#    on the other.
# So this principle component has critically acclaimed movies on one side and
#    blockbusters on the other.

pcs %>%
    select(name, PC1) %>%
    arrange(desc(PC1)) %>%
    slice(1:10)

# It's separating out movies that have structure and they're determined by users
#    that like these more than these and others that like these more than that.
# We can also see that the second principle component also seems to capture
#    structure in the data.

pcs %>%
    select(name, PC2) %>%
    arrange(PC2) %>%
    slice(1:10)

# If we look at one extreme of this principle component, we see arts and independent
#    films such as Little Miss Sunshine, the Truman Show, and Slumdog Millionaire.
# When we look at the other extreme, we see what I would call nerd favorites,
#    The Lord of the Rings, Star Wars, The Matrix.

pcs %>%
    select(name, PC2) %>%
    arrange(desc(PC2)) %>%
    slice(1:10)

# So using principal component analysis, we have shown that a matrix factorisation
#    approach can find important structure in our data.
# Now to actually fit the matrix factorization model that we presented earlier
#    that takes into account that there is missing data, that there's missing
#    cells in the matrix, is a bit more complicated.
# For those interested we recommend trying the recommended lab package which fits
#    these models.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Matrix Factorization

# In this exercise set, we will be covering a topic useful for understanding matrix factorization: the singular value decomposition (SVD). SVD is a mathematical result that is widely used in machine learning, both in practice and to understand the mathematical properties of some algorithms. This is a rather advanced topic and to complete this exercise set you will have to be familiar with linear algebra concepts such as matrix multiplication, orthogonal matrices, and diagonal matrices.
#
# The SVD tells us that we can decompose an N x p matrix Y with p < N as Y = IDV^T
#
# with U and V orthogonal of dimensions N x p and D a p x p respectively and  a  diagonal matrix with the values of the diagonal decreasing:
#     d1,1 >= d2,2 >= ... dp.p
#     In this exercise, we will see one of the ways that this decomposition can be useful. To do this, we will construct a dataset that represents grade scores for 100 students in 24 different subjects. The overall average has been removed so this data represents the percentage point each student received above or below the average test score. So a 0 represents an average grade (C), a 25 is a high grade (A+), and a -25 represents a low grade (F). You can simulate the data like this:
#
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3)
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m),
             decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep = "_"),
                 paste(rep("Science",k), 1:k, sep = "_"),
                 paste(rep("Arts",k), 1:k, sep = "_"))

# Our goal is to describe the student performances as succinctly as possible. For example, we want to know if these test results are all just a random independent numbers. Are all students just about as good? Does being good in one subject  imply you will be good in another? How does the SVD help with all this? We will go step by step to show that with just three relatively small pairs of vectors we can explain much of the variability in this 100x24 dataset.

# Q1
# You can visualize the 24 test scores for the 100 students by plotting an image:

my_image <- function(x, zlim = range(x), ...){
        colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
        cols <- 1:ncol(x)
        rows <- 1:nrow(x)
        image(cols, rows, t(x[rev(rows),,drop = FALSE]), xaxt = "n", yaxt = "n",
              xlab = "", ylab = "",  col = colors, zlim = zlim, ...)
        abline(h=rows + 0.5, v = cols + 0.5)
        axis(side = 1, cols, colnames(x), las = 2)
    }

my_image(y)

# How would you describe the data based on this figure?
# The test scores are all independent of each other.
# The students that are good at math are not good at science.
# The students that are good at math are not good at arts.
# The students that test well are at the top of the image and there seem to be three groupings by subject.   <-*
# The students that test well are at the bottom of the image and there seem to be three groupings by subject.

# Q2
# You can examine the correlation between the test scores directly like this:

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Which of the following best describes what you see?
# The test scores are independent.
# Test scores in math and science are highly correlated but scores in arts are not.
# There is high correlation between tests in the same subject but no correlation across subjects.
# There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.   <-*

# Q3
# Remember that orthogonality means that U^TY and V^TV are equal to the identity matrix. This implies that we can also rewrite the decomposition as YV = UD or U^TY = DV^T
#
# We can think of YV and U^TV as two transformations of Y that preserve the total variability of Y since U and V are orthogonal.
#
# Use the function svd to compute the SVD of y. This function will return U, V, and the diagonal entries of D.

s <- svd(y)
names(s)

# You can check that the SVD works by typing:

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# Compute the sum of squares of the columns of Y and store them in ss_y. Then compute the sum of squares of columns of the transformed YV and store them in ss_yv. Confirm that sum(ss_y) is equal to sum(ss_yv).

ss_y <- y^2
ss_yv <- y_svd^2

sum(ss_y)
sum(ss_yv)

# What is the value of sum(ss_y) (and also the value of sum(ss_yv))?
# 175434.6

# Q4
# We see that the total sum of squares is preserved. This is because V is orthogonal. Now to start understanding how YV is useful, plot ss_y against the column number and then do the same for ss_yv.

plot(ss_y)
plot(ss_yv)

# What do you observe?
# ss_y and ss_yv are decreasing and close to 0 for the 4th column and beyond.
# ss_yv is decreasing and close to 0 for the 4th column and beyond.   <-*
# ss_y is decreasing and close to 0 for the 4th column and beyond.
# There is no discernible pattern to either ss_y or ss_yv.

# Q5
# Note that we didn't have to compute ss_yv because we already have the answer. How? Remember that YV = UD and because U is orthogonal, we know that the sum of squares of the columns of UD are the diagonal entries of D squared. Confirm this by plotting the square root of ss_yv versus the diagonal entries of D.

plot(sqrt(ss_yv),
     s$d)
abline(0,1)

# What else is equal to YV?
# D
# U
# UD   <-*
# VUD

# Q6
# So from the above we know that the sum of squares of the columns of Y (the total sum of squares) adds up to the sum of s$d^2 and that the transformation YV gives us columns with sums of squares equal to s$d^2. Now compute the percent of the total variability that is explained by just the first three columns of YV.
#
# What proportion of the total variability is explained by the first three columns of YV?

sum(s$d[1:3]^2) / sum(s$d^2)

# Enter a decimal, not the percentage.
# 0.988

# Q7
# Before we continue, let's show a useful computational trick to avoid creating the matrix diag(s$d). To motivate this, we note that if we write U out in its columns [U1, U2, ..., Up] then UD is equal to UD = [U1d1,1, U2d2,2, ..., Updp,p]
#
# Use the sweep function to compute  without constructing diag(s$d) or using matrix multiplication.

# Which code is correct?
# identical(t(s$u %*% diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))   # <-*
# identical(s$u %*% t(diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
# identical(s$u %*% diag(s$d), sweep(s$u, 2, s, FUN = "*"))

# Q8
# We know that U1d1,1, the first column of UD, has the most variability of all the columns of . Earlier we looked at an image of Y using my_image(y), in which we saw that the student to student variability is quite large and that students that are good in one subject tend to be good in all. This implies that the average (across all subjects) for each student should explain a lot of the variability. Compute the average score for each student, plot it against U1d1,1, and describe what you find.

plot(-s$u[,1]*s$d[1], rowMeans(y))

# What do you observe?
# There is no relationship between the average score for each student and U1d1,1.
# There is a linearly decreasing relationship between the average score for each student and U1d1,1 .
# There is a linearly increasing relationship between the average score for each student and U1d1,1.   <-*
# There is an exponentially increasing relationship between the average score for each student and U1d1,1.
# There is an exponentially decreasing relationship between the average score for each student and U1d1,1.

# Q9
# We note that the signs in SVD are arbitrary because: UDV^T = (-U)D(-V)^T
#
# With this in mind we see that the first column of UD is almost identical to the average score for each student except for the sign.
#
# This implies that multiplying Y by the first column of V must be performing a similar operation to taking the average. Make an image plot of V and describe the first column relative to others and how this relates to taking an average.

my_image(s$v)

# How does the first column relate to the others, and how does this relate to taking an average?
#     The first column is very variable, which implies that the first column of YV is the sum of the rows of Y multiplied by some non-constant function, and is thus not proportional to an average.
# The first column is very variable, which implies that the first column of YV is the sum of the rows of Y multiplied by some non-constant function, and is thus proportional to an average.   <-*
# The first column is very close to being a constant, which implies that the first column of YV is the sum of the rows of Y multiplied by some constant, and is thus proportional to an average.
# The first three columns are all very close to being a constant, which implies that these columns are the sum of the rows of Y multiplied by some constant, and are thus proportional to an average.

# The following four exercises are all ungraded and are provided to give you an additional opportunity to practice working with matrices in a continuation of the exercises with this dataset.
#
# We recommend that you attempt to write the code on your own before hitting "submit" and viewing the answers.
#
# Q10 - UNGRADED
# We already saw that we can rewrite  as U1d1,1 + U2d2,2 + ... + Updp,p
#
# with Uj the j-th column of U. This implies that we can rewrite the entire SVD as:
#  Y = U1d1,1 V1^T + U2d2,2 V2^T + ... + Updp,p Vp^T
#
# with Vj the jth column of V. Plot U1, then plot V1^T using the same range for the y-axis limits, then make an image of U1d1,1 V1^T and compare it to the image of Y. Hint: use the my_image function defined above. Use the drop=FALSE argument to assure the subsets of matrices are matrices.

plot(s$u[,1],
     ylim = c(-0.25, 0.25))
plot(s$v[,1],
     ylim = c(-0.25, 0.25))
with(s,
     my_image((u[, 1, drop = FALSE]*d[1]) %*% t(v[, 1, drop = FALSE])))
my_image(y)

# Q11 - UNGRADED
# We see that with just a vector of length 100, a scalar, and a vector of length 24, we can actually come close to reconstructing the a matrix. This is our first matrix factorization:
# Y = d1,1 U1V1^T
# In the exercise in Q6, we saw how to calculate the percent of total variability explained. However, our approximation only explains the observation that good students tend to be good in all subjects. Another aspect of the original data that our approximation does not explain was the higher similarity we observed within subjects. We can see this by computing the difference between our approximation and original data and then computing the correlations. You can see this by running this code:

resid <- y - with(s,(u[, 1, drop = FALSE]*d[1]) %*% t(v[, 1, drop = FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Now that we have removed the overall student effect, the correlation plot reveals that we have not yet explained the within subject correlation nor the fact that math and science are closer to each other than to the arts. So let's explore the second column of the SVD.
#
# Repeat the previous exercise (Q10) but for the second column: Plot U2, then plot V2^T using the same range for the y-axis limits, then make an image of U2d2,2 V2^T and compare it to the image of resid.

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s,
     my_image((u[, 2, drop = FALSE]*d[2]) %*% t(v[, 2, drop = FALSE])))
my_image(resid)

# Q12 - UNGRADED
# The second column clearly relates to a student's difference in ability in math/science versus the arts. We can see this most clearly from the plot of s$v[,2]. Adding the matrix we obtain with these two columns will help with our approximation:
# y = d1,1 U1 V1^T + d2,2 U2 V2^T
#     We know it will explain sum(s$d[1:2]^2)/sum(s$d^2) * 100 percent of the total variability. We can compute new residuals like this:

resid <- y - with(s,
                  sweep(u[, 1:2],
                        2,
                        d[1:2],
                        FUN = "*") %*% t(v[, 1:2]))
my_image(cor(resid),
         zlim = c(-1,1))
axis(side = 2,
     1:ncol(y),
     rev(colnames(y)),
     las = 2)

# and see that the structure that is left is driven by the differences between math and science. Confirm this by first plotting U3, then plotting V3^T using the same range for the y-axis limits, then making an image of U3 d3,3 V3^T and comparing it to the image of resid.

plot(s$u[,3],
     ylim = c(-0.5, 0.5))
plot(s$v[,3],
     ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop = FALSE]*d[3]) %*% t(v[, 3, drop = FALSE])))
my_image(resid)

# Q13 - UNGRADED
# The third column clearly relates to a student's difference in ability in math and science. We can see this most clearly from the plot of s$v[,3]. Adding the matrix we obtain with these two columns will help with our approximation:
# Y = d1,1 U1 V1^T + d2,2 U2 V2^T + d3,3 U3 V3^T
# We know it will explain: sum(s$d[1:3]^2)/sum(s$d^2) * 100 percent of the total variability. We can compute new residuals like this:

resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# We no longer see structure in the residuals: they seem to be independent of each other. This implies that we can describe the data with the following model:
#  Y = d1,1 U1 V1^T + d2,2 U2 V2^T + d3,3 U3 V3^T + E
# with E a matrix of independent identically distributed errors. This model is useful because we summarize of 100x24 observations with 3 x (100 + 24 + 1) = 375 numbers.
#
# Furthermore, the three components of the model have useful interpretations:
#
# 1 - the overall ability of a student
# 2 - the difference in ability between the math/sciences and arts
# 3 - the remaining differences between the three subjects.
# The sizes d1,1, d2,2 and d3,3 tell us the variability explained by each component. Finally, note that the components dj,j Uj Vj^T are equivalent to the jth principal component.
#
# Finish the exercise by plotting an image of Y, an image of d1,1 U1 V1^T + d2,2 U2 V2^T + d3,3 U3 V^T and an image of the residuals, all with the same zlim.

y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN = "*") %*% t(v[, 1:3]))
my_image(y,
         zlim = range(y))
my_image(y_hat,
         zlim = range(y))
my_image(y - y_hat,
         zlim = range(y))

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Clustering

# These exercises will work with the tissue_gene_expression dataset, which is part of the dslabs package.
#
# Q1
# Load the tissue_gene_expression dataset. Remove the row means and compute the distance between each observation. Store the result in d.

data("tissue_gene_expression")

# Which of the following lines of code correctly does this computation?
# d <- dist(tissue_gene_expression$x)
# d <- dist(rowMeans(tissue_gene_expression$x))
# d <- dist(rowMeans(tissue_gene_expression$y))
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))   # <-*

# Q2
# Make a hierarchical clustering plot and add the tissue types as labels.

h <- hclust(d)
plot(h)

# You will observe multiple branches.
#
# Which tissue type is in the branch farthest to the left?
# cerebellum
# colon
# endometrium
# hippocampus
# kidney
# liver   <-*
# placenta
# unanswered

# Q3
# Run a k-means clustering on the data with K=7. Make a table comparing the identified clusters to the actual tissue types. Run the algorithm several times to see how the answer changes.

cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)

# What do you observe for the clustering of the liver tissue?
#     Liver is always classified in a single cluster.
# Liver is never classified in a single cluster.
# Liver is classified in a single cluster roughly 20% of the time and in more than one cluster roughly 80% of the time.   <-*
# Liver is classified in a single cluster roughly 80% of the time and in more than one cluster roughly 20% of the time.

# Q4
# Select the 50 most variable genes. Make sure the observations show up in the columns, that the predictor are centered, and add a color bar to show the different tissue types. Hint: use the ColSideColors argument to assign colors. Also, use col = RColorBrewer::brewer.pal(11, "RdBu") for a better use of colors.
#
# Part of the code is provided for you here:
#
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
# #BLANK
# Which line of code should replace #BLANK in the code above?
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)   # <-*
# heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(colors))
# heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
# heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))

# -------       -------       -------       -------       -------       -------

