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
tree = prune.tree(tree, best = pruning_tree$size[which(pruning_tree$dev == min_dev)])
plot(tree)
text(tree, pretty = 0, cex = 0.7)


#Regression Tree
tree = tree(G3~., cleaned_student_por, subset = training_sets[[1]] )
plot(tree)
text(tree, pretty = 0, cex = 0.7)

pred_tree = predict(tree, newdata = cleaned_student_por[-training_sets[[1]],])
mean((pred_tree - cleaned_student_por[-training_sets[[1]], 'G3'])^2)


#Linear Regression