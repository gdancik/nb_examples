library(readr)
library(dplyr)  
library(ggplot2)
library(e1071)

# load in sample data
survey <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv")


o <- table(survey$CatOrDogPerson)
counts <- data.frame(o)
ggplot(counts) + geom_col(aes(x=Var1, y=Freq, fill = Var1)) +
  ggtitle("Cat or Dog Person") +
  labs(x = "Cat or Dog", y = "Frequency")



# filter survey to keep males and females only (there was one student who
# responded 'Other or prefer not to say')
survey <- filter(survey, Gender != "Other or prefer not to say")


# need to convert character columns to factors for
# naive bayes
survey <- mutate_if(survey, is.character, as.factor)

# fit a naive bayes model that predicts gender based on whether
# someone is a cat or dog person, and that person's college GPA
nb <- naiveBayes(Gender ~ CatOrDogPerson + collegeGPA, data = survey)

# View the conditional probabilities --
# this shows the probabilities for Cat or Dog given female or male;
# for GPA, the mean and standard deviation are provided
nb

# view the prior probabilities a student is a female or male
prop.table(nb$aprior)

#################################################################
# Suppose a person is a Cat person and has a college gpa of 3.0.
# We want to predict whether they are female (F) or male (M).

# Therefore we want to find P(F | Cat person and 3.0 college GPA),
# which is proportional to 
# P(3.0 college GPA | F) * P(Cat | F) * P(F).

# We do the same to find the probability of being a male.
#################################################################


# 1) Find the prior probability that a student is Female, P(F)

p0 <- prop.table(nb$apriori)[1]

# 2) Find P(Cat | F) 

p1 <- nb$tables$CatOrDogPerson[1,1]

# 3) Find P(3.0 college GPA | F). This probability is
#  calculated using the command dnorm(x, mean, sd), where x
#  is the observed value (3.0 in this case), and mean and sd are
#  the mean and standard deviation which you can get from the
#  'nb' object.
f <- nb$tables$collegeGPA[1,]
p2 <- dnorm(3, mean = f[1], sd = f[2])

# 4) Multiply your answers from (1)- (3), and store the answer in
#    a variable named 'pf'. This value is proportional to
#    P(Female | Cat and college GPA of 3.0)

pf <- p0*p1 * p2

# 5) find the prior probability that a student is Male

p0 <- prop.table(nb$apriori)[2]

# 6) Find P (Cat | Male)

p1 <- nb$tables$CatOrDogPerson[2,1]

# 7) Find P(college GPA of 3.0 | Male)

m <- nb$tables$collegeGPA[2,]
p2 <- dnorm(3, mean = m[1], sd = m[2])

# 8) Multiply your answers from (6) and (8), and store your answer
#    in a variable named 'pm'. This value represents
#    P(Male | Cat person and college GPA of 3.0)

pm <- p0*p1*p2



# create a vector containing the female and male probabilities
x <- c(pf, pm)
x

# this should give you:
#Female       Male 
#0.06173074 0.07393142 

#9. Scale your 'x' vector so that the values sum to 1 -- you can do
# this by taking 'x' and dividing it by the sum of 'x'

x / sum(x)

# the code below uses the Naive Bayes model to predict the 
# probability that someone is a female, given that they have
# a 3.0 GPA and are a Cat person; and the probability that they are
# a male. Compare these probabilities to your answer from (9). 
# They should match!
p <- predict(nb, data.frame(collegeGPA = 3, CatOrDogPerson = 'Cat'), type = 'raw')
p
