#CHANGE ATTRIBUTE NAMES
colnames(IRIS) = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Class")

#DEFINE COLORS
colors = c("purple", "green","blue")
colors <- colors [as.numeric(IRIS$Class)]

#DEFINE SHAPES
shapes = c(15, 16, 17)
shapes <- shapes[as.numeric(IRIS$Class)]

#PLOT DATA
plot(x= IRIS$Sepal.Length, y = IRIS$Sepal.Width,frame = FALSE, 
     xlab = "Sepal Length", ylab= "Sepal Width", main = "Sepal Length vs Sepal Width", col = colors , pch = shapes)

#CREATE SCATTERPLOT LEGEND
legend("topright", legend = levels(IRIS$Class), col= c("purple","green","blue") , pch = c(15, 16, 17) )


