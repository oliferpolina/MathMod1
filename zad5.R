iris
names(iris)
iris$Petal.Length
setosa=c(iris$Petal.Length[1:50])
setosa
versicolor=c(iris$Petal.Length[51:100])
versicolor
virginica=c(iris$Petal.Length[101:150])
mean(setosa)
mean(versicolor)
mean(virginica)
Species=c(mean(setosa),mean(versicolor),mean(virginica))
sort(Species)
f=factor(sort(Species))
f
names(f)<- c("setosa", "versicolor", "virginica")
f