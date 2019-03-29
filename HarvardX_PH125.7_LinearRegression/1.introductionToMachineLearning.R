# Machine Learning

# Notation
# In machine learning, data comes in the form of the outcome we want to predict
#    and the features that we will use to predict the outcome.  We want to build
#    an algorithm that takes feature value as input and returns a prediction for
#    the outcome when we don't know the outcome.
# The machine-learning approach is to train an algorithm using a dataset for which
#    we do know the outcome, and then apply this algorithm in the future to make
#    a prediction when we don't know the outcome.
# Here we'll use Y to denote the outcomes and X's--X1 through XP-- to denote the
#    features.  Know that the features are sometimes referred to as predictors or
#    covariates.  We consider all these to be synonyms.  Prediction problems can
#    be divided into categorical and continuous outcomes.
# For categorical outcomes, Y can be anyone of K classes.  The number of classes
#    can vary greatly across applications.  For example, in the digit reader data,
#    K is equal to 10, with the classes being the digits 0, 1, 2, 3, 4, 5, 6, 7,
#    8, and 9.
# In speech recognition, the outcome are all possible words we're trying to detect.
#    Spam detection has two outcomes-- spam or not spam.  In this course, we
#    denote the k categories with indexes k equals 1 through capital K.
# However, for binary data, we use 0 and 1.  This is for mathematical convenience,
#    something which we demonstrate later. OK.
# The general setup is as follows.  We have a series of features and an unknown
#    outcome we want to predict, so it looks something like this-- five features,
#    unknown outcome.
# To build a model or an algorithm that provides a prediction, for any set of
#    values, X1 equals small x1 all the way up to x5, we collect data for which
#    we do know the outcome, so we get a table like this.  We use a notation y
#    hat to denote the prediction.
# We use the term actual outcome to denote what we ended up observing.  So we
#    want the prediction y hat to match the actual outcome.  The outcome y can be
#    categorical-- which digit, word, spam or no spam, pedestrian, or empty road
#    ahead; or continuous-- movie rating, house price, stock value, distance
#    between driverless car and pedestrian.
# The concept and algorithms we learn here apply to both.  However, there are
#    some differences in how we approach each case, so it is important to distinguish
#    between the two.  When the outcome is categorical, we refer to the machine-
#    learning task as classification.
# Our predictions will be categorical just like our outcomes, and they will be
#    either correct or incorrect.  When the outcome is continuous, we'll refer to
#    the task as prediction.  In this case, our prediction will not be either
#    right or wrong.  Instead, we will make an error which is the difference
#    between the prediction and the actual outcome.
# This terminology can become confusing since we use the term y hat denote our
#    predictions even when it is a categorical outcome.  However, throughout the
#    lecture, the context will make the meaning clear.  But note, that these terms
#    vary among courses, textbooks, and other publications, so don't let that
#    confuse you.
# For example, often, prediction is used for both categorical and continuous,
#    and the word regression is used for the continuous case.  Here, we avoid
#    using regression to avoid confusion with our previous use of the term linear
#    regression.
# Again, in most cases, it will be clear if our outcomes are categorical or
#    continuous, so we will avoid using these terms when possible.

# -------       -------       -------       -------       -------       -------

# An Example
#  Let's consider an example, the first thing that happens to a letter when they
#     are received in the post office is that they are sorted by zip code.
# Originally humans had to sort these by hand.  To do this, they had to read the
#    zip codes on each letter.  Today, thanks to machine learning algorithms,
#    a computer can read zip codes and then a robot sorts the letters.
# In this course, we will learn how to build algorithms that can read a digit.
# The first step in building an algorithm is to understand what are the outcomes
#    and what are the features.  Here are three images of written digits.
# These have already been read by a human and assigned an outcome, Y. These are
#    considered known and serve as the training data.  The images are converted
#    into 28 by 28 images.  So we have 784 pixels.
# And for each pixel we obtain a grayscale intensity between 0, that's white,
#    and 255, that's black, which we consider to be continuous for now.
# We can see these values by remaking the figures like this.
# For each digitized imaged, i, we have a categorical outcome Yi, which can be
#    one of 10 values-- 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
# And we have 784 features.  So we have X1, X2, all the way up to X784.
# We use boldface for the X's, to distinguish the vector of predictors from the
#    individual predictors. When referring to an arbitrary set of features, we
#    drop the index i and use Y and bold X. We use upper case variables because,
#    in general, we think of the predictors as random variables.
# We use lowercase-- for example, we might say upper case X equals lowercase x--
#    to denote observed values.  Although when we code, we stick to lower case.
# The machine learning task is to build an algorithm that returns a prediction
#    for any of the possible values of the features.  Here we will learn several
#    approaches to building these algorithms.  Although at this point it may seem
#    impossible to achieve this, we will start with a simple example and build up
#    our knowledge until we can attack more complex ones.
# To learn the basics concepts we will start with very simple examples with just
#    one predictor, then we'll move to two predictors.  And once we learn these,
#    we will attack more real world
# machine learning challenges.

# -------       -------       -------       -------       -------       -------

# Comprehension Check: Introduction to Machine Learning
# Q1
# "Turbochamp" was one of the first algorithms written by famed British computer scientist, mathematician and crypto-analyst Alan Turing in the late 1940s. The artificial intelligence program was based on programmable rules derived from theory or first principles and could 'think' two moves ahead.
#
# True or False: A key feature of machine learning is that the algorithms are built on data.
# True   <-*
# False


# Q2
# True or False: In machine learning, we build algorithms that take feature values (X) and train a model using known outcomes (Y) that is then used to predict outcomes when presented with features without known outcomes.
# True   <-*
# False


