#Project
library(tree)

#import dataset using from text (base) and make sure heading is on yes
#cleaned_student_por <- read.csv("cleaned_student_por.csv", sep="")


#Setting Training Sets
set.seed(1)
training_sets = list()
for( i in 1:10) {
  training_sets[[i]] = sample(nrow(cleaned_student_por), 0.8*nrow(cleaned_student_por))  
}

#Determining attributes to use

#determining if a tree created from data will need pruning
tree = tree(G3~., cleaned_student_por)
plot(tree)
text(tree, pretty = 0, cex = 0.7)

pruning_tree = cv.tree(tree)
pruning_tree
plot(pruning_tree, type = 'b')
min_dev = min(pruning_tree$dev)
pruned.tree = prune.tree(tree, best = pruning_tree$size[which(pruning_tree$dev == min_dev)])
plot(pruned.tree)
text(pruned.tree, pretty = 0, cex = 0.7)


#Regression Tree
tree = tree(G3~., cleaned_student_por, subset = training_sets[[1]] )
plot(tree)
text(tree, pretty = 0, cex = 0.7)

pred_tree = predict(tree, newdata = cleaned_student_por[-training_sets[[1]],])
mean((pred_tree - cleaned_student_por[-training_sets[[1]], 'G3'])^2)

Student = cleaned_student_por[, c('age', 'studytime','traveltime','failures', 'famrel',
                                  'goout', 'Dalc', 'Walc', 'health', 'absences', 'G1', 'G2', 'G3')]

#converting Mjob to binary attributes
Student$MTeacherJob = ifelse(cleaned_student_por$Mjob == "teacher", 1, 0)
Student$MHealthJob = ifelse(cleaned_student_por$Mjob == "health", 1, 0)
Student$MServiceJob = ifelse(cleaned_student_por$Mjob == "services", 1, 0)
Student$MHomeJob = ifelse(cleaned_student_por$Mjob == "at_home", 1, 0)
Student$MOtherJob = ifelse(cleaned_student_por$Mjob == "other", 1, 0)

#converting Fjob to binary attributes
Student$FTeacherJob = ifelse(cleaned_student_por$Fjob == "teacher", 1, 0)
Student$FHealthJob = ifelse(cleaned_student_por$Fjob == "health", 1, 0)
Student$FServiceJob = ifelse(cleaned_student_por$Fjob == "services", 1, 0)
Student$FHomeJob = ifelse(cleaned_student_por$Fjob == "at_home", 1, 0)
Student$FOtherJob = ifelse(cleaned_student_por$Fjob == "other", 1, 0)

#converting reason to binary attributes
Student$RHome = ifelse(cleaned_student_por$reason == "home", 1, 0)
Student$RReputation = ifelse(cleaned_student_por$reason == "reputation", 1, 0)
Student$RCourse = ifelse(cleaned_student_por$reason == "course", 1, 0)
Student$ROther = ifelse(cleaned_student_por$reason == "other", 1, 0)

#converting guardian  to binary attributes
Student$GMother = ifelse(cleaned_student_por$guardian == "mother", 1, 0)
Student$GFather = ifelse(cleaned_student_por$guardian == "father", 1, 0)
Student$GOther = ifelse(cleaned_student_por$guardian == "other", 1, 0)

#converting binary factors
Student$school = ifelse(cleaned_student_por$school == "MS", 1, 0) #GP = 0, MS = 1
Student$sex = ifelse(cleaned_student_por$sex == "M", 1, 0) #F = 0, M = 1
Student$famsize = ifelse(cleaned_student_por$famsize == "GT3", 1, 0) #Less or equal to 3 (LE3) = 0, Greater than 3(GT3) = 1
Student$address = ifelse(cleaned_student_por$address == "U", 1, 0)  #Rural (R) = 0, Urban (U) = 1
Student$Pstatus = ifelse(cleaned_student_por$Pstatus == "T", 1, 0) #Alone (A) = 0, Together (T) = 1
# Yes = 1, No = 0
Student$schoolsup = ifelse(cleaned_student_por$schoolsup == "yes", 1, 0)
Student$famsup = ifelse(cleaned_student_por$famsup == "yes", 1, 0)
Student$activities = ifelse(cleaned_student_por$activities == "yes", 1, 0)
Student$nursery = ifelse(cleaned_student_por$nursery == "yes", 1, 0)
Student$higher = ifelse(cleaned_student_por$higher == "yes", 1, 0)
Student$internet = ifelse(cleaned_student_por$internet == "yes", 1, 0)
Student$romantic = ifelse(cleaned_student_por$romantic == "yes", 1, 0)
Student$paid = ifelse(cleaned_student_por$paid == "yes", 1, 0)

#Linear Regression
attach(Student)
lm.fit <-lm(G3~., data=Student)
summary(lm.fit)

lm_errors = vector()
signi = c('G1', 'G2', 'MHealthJob', 'MServiceJob', 'failures', 'RCourse', 'traveltime')
for(i in 1:10){
  linear_regression = lm(G3~G1+ G2 + MHealthJob + MServiceJob + failures + RCourse + traveltime, data = Student, 
                         subset = training_sets[[i]])
  lm_pred = predict(linear_regression, newdata = Student[-training_sets[[i]],signi])
  lm_errors = c(lm_errors,mean((lm_pred - cleaned_student_por[-training_sets[[i]], 'G3'])^2))
}

lm_errors

rt_errors = vector()
for(i in 1:10)
{
  tree = tree(G3~., cleaned_student_por, subset = training_sets[[i]] )
  pred_tree = predict(tree, newdata = cleaned_student_por[-training_sets[[i]],])
  rt_errors = c(rt_errors, mean((pred_tree - cleaned_student_por[-training_sets[[i]], 'G3'])^2))
}

rt_errors
