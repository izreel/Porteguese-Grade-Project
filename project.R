#Project
library(tree)

cleaned_student_por <- read.csv("cleaned_student_por.csv", sep="")


#Setting Training Sets
set.seed(1)
training1 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))
training2 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))
training3 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))
training4 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))
training5 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))
training6 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))
training7 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))
training8 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))
training9 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))
training10 = sample(1:nrow(cleaned_student_por), .8*nrow(cleaned_student_por))


#Determining attributes to use

#Regression Tree
tree = tree(G3~., cleaned_student_por, subset = training1 )
plot(tree)
text(tree, pretty = 0, cex = 0.7)

pred_tree = predict(tree, newdata = cleaned_student_por[-training1,])
mean((pred_tree - cleaned_student_por[-training1, 'G3'])^2)

tree = tree(G3~., cleaned_student_por, subset = training2 )
plot(tree)
text(tree, pretty = 0, cex = 0.7)

pred_tree = predict(tree, newdata = cleaned_student_por[-training2,])
mean((pred_tree - cleaned_student_por[-training1, 'G3'])^2)

tree = tree(G3~., cleaned_student_por, subset = training3 )
plot(tree)
text(tree, pretty = 0, cex = 0.7)

pred_tree = predict(tree, newdata = cleaned_student_por[-training3,])
mean((pred_tree - cleaned_student_por[-training1, 'G3'])^2)

#Linear Regression