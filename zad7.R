iris
names(iris)
x=iris$Sepal.Length[1:50]
y=iris$Petal.Length[1:50]
plot(x,y,main="The dependence of the length of the sepal length of petal to form setosa", xlab="Sepal.Length",ylab="$Petal.Length", col="purple")
x=NULL
y=NULL
x=iris$Sepal.Length[51:101]
y=iris$Petal.Length[51:101]
plot(x,y,main="The dependence of the length of the sepal length of petal to form versicolor", xlab="Sepal.Length",ylab="$Petal.Length", col="red")
x=NULL
y=NULL
x=iris$Sepal.Length[101:150]
y=iris$Petal.Length[101:150]
plot(x,y,main="The dependence of the length of the sepal length of petal to form virginica", xlab="Sepal.Length",ylab="$Petal.Length", col="orange")
x=NULL
y=NULL

