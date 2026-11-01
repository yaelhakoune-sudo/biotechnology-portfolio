#create the matrix
weight_matrix<-matrix(
  c(23.5, 25.1, 23.8,
    25.0, 26.0, 24.8,
    25.4, 26.1, 24.2,
    25.8, 26.3, 25.1),
  nrow=4, 
  ncol=3, 
  byrow=TRUE, 
  dimnames= list(
    c("Day_1","Day_2","Day_3","Day_4"),
    c("Mouse_1","Mouse_2","Mouse_3")
    )
  )
print(weight_matrix)

#Calculate the min. of the weught of mouse 1&2 
min_mouse1<- min(weight_matrix[,"Mouse_1"])
min_mouse2<- min(weight_matrix[,"Mouse_2"])
min_mouse1
min_mouse2

# conditions of min. weight
if (min_mouse1>=24.5 && min_mouse2>=24.5){
  print("Both mice are within the normal range")
} else if (min_mouse1>= 24.5 || min_mouse2>= 24.5){
  print("Only one mouse is within the normal range")
}else {print("Neither mouse is within the normal rang")}
  
#update by removing one gram in each
weight_matrix <- weight_matrix - 1
weight_matrix

#create a full name
first_name <- "Tom"
last_name <- "Soffer"

full_name<- paste(first_name, last_name)
print(full_name)

#create a list
experiment<- list(species="Mouse",
                  weight_matrix= weight_matrix, 
                  days=4,
                  researcher= full_name)
print(experiment)

#remove the days
experiment$days<-NULL

