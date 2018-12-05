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

Student = cleaned_student_por

#converting Mjob to binary attributes
Student$MTeacherJob = ifelse(Student$Mjob == "teacher", 1, 0)
Student$MHealthJob = ifelse(Student$Mjob == "health", 1, 0)
Student$MServiceJob = ifelse(Student$Mjob == "services", 1, 0)
Student$MHomeJob = ifelse(Student$Mjob == "at_home", 1, 0)
Student$MOtherJob = ifelse(Student$Mjob == "other", 1, 0)

#converting Fjob to binary attributes
Student$FTeacherJob = ifelse(Student$Fjob == "teacher", 1, 0)
Student$FHealthJob = ifelse(Student$Fjob == "health", 1, 0)
Student$FServiceJob = ifelse(Student$Fjob == "services", 1, 0)
Student$FHomeJob = ifelse(Student$Fjob == "at_home", 1, 0)
Student$FOtherJob = ifelse(Student$Fjob == "other", 1, 0)

#converting reason to binary attributes
Student$RHome = ifelse(Student$reason == "home", 1, 0)
Student$RReputation = ifelse(Student$reason == "reputation", 1, 0)
Student$RCourse = ifelse(Student$reason == "course", 1, 0)
Student$ROther = ifelse(Student$reason == "other", 1, 0)

#converting guardian  to binary attributes
Student$GMother = ifelse(Student$guardian == "mother", 1, 0)
Student$GFather = ifelse(Student$guardian == "father", 1, 0)
Student$GOther = ifelse(Student$guardian == "other", 1, 0)

#converting binary factors
Student$school = ifelse(Student$school == "MS", 1, 0) #GP = 0, MS = 1
Student$sex = ifelse(Student$sex == "M", 1, 0) #F = 0, M = 1
Student$famsize = ifelse(Student$famsize == "GT3", 1, 0) #Less or equal to 3 (LE3) = 0, Greater than 3(GT3) = 1
Student$address = ifelse(Student$address == "U", 1, 0)  #Rural (R) = 0, Urban (U) = 1
Student$Pstatus = ifelse(Student$Pstatus == "T", 1, 0) #Alone (A) = 0, Together (T) = 1
# Yes = 1, No = 0
Student$schoolsup = ifelse(Student$schoolsup == "yes", 1, 0)
Student$famsup = ifelse(Student$famsup == "yes", 1, 0)
Student$activities = ifelse(Student$activities == "yes", 1, 0)
Student$nursery = ifelse(Student$nursery == "yes", 1, 0)
Student$higher = ifelse(Student$higher == "yes", 1, 0)
Student$internet = ifelse(Student$internet == "yes", 1, 0)
Student$romantic = ifelse(Student$romantic == "yes", 1, 0)
Student$paid = ifelse(Student$paid == "yes", 1, 0)

#Linear Regression
attach(Student)
lm.fit <-lm(G3~school+sex+age+address+famsize+Pstatus+
            Medu+Fedu+MTeacherJob+MHealthJob+MServiceJob+
            MHomeJob+MOtherJob+FTeacherJob+FHealthJob+FServiceJob+
            FHomeJob+FOtherJob+RHome+RCourse+RReputation+ROther+GMother+GFather+ 
            GOther+traveltime+studytime+
            failures+schoolsup+famsup+paid+activities+nursery+romantic+famrel+
            freetime+goout+Dalc+Walc+health+absences+G1+G2, data=Student)
summary(lm.fit)

