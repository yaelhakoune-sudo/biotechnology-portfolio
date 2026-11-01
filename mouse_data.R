getwd()
setwd("/Users/yaelhakoune/R")

#Read the file, change the name 
mouse_data<- read.csv("mouse_data_updated.csv")
#Show the fisrt part of the file as a row
mouse_data<- read.csv("mouse_data_updated.csv",row.names = 1)

#Present the first 10 rows of the given data
head(mouse_data,10)

#Check the the data with str()
str(mouse_data)

#Switch the row  Group as factor
mouse_data$Group<-factor(mouse_data$Group)

#Show the levels of the row Group
levels(mouse_data$Group)

#Calculate the average length of the mouses from the Lngth_cm
mean(mouse_data$Length_cm)

#Take only the data of the mouses from the control group ad save with a newc name
control_mice<-subset(mouse_data,Group== "Control")
control_mice

#Take only the data of the group Treatment that weight more than 22gr ad save t as heavy_treatmet
heavy_treatment<-subset(mouse_data, Weight_g > 22)
heavy_treatment

#Sort the data from the lightest to heaviest weight and save it as sorted_weight
sorted_weight<-mouse_data[order(mouse_data$Weight_g),]
sorted_weight

#Sort the data by Sex so that the male will come first and then the females and sort them by Genex from the hightest to lowest
sorted_sex_gene<-mouse_data[order(mouse_data$Sex=="F",-mouse_data$GeneX_expr),]
sorted_sex_gene

#Sort the data by Day and in each day the weight is mentioned from the heaviest to lightest
sorted_day_weight<-mouse_data[order(mouse_data$Day, mouse_data$Weight_g),]
sorted_day_weight

#Create a new Data Frame wich includes only the Group, Day, Sex from the GeneX highest than 8
high_GeneX_mice<-subset(mouse_data,GeneX_expr > 8, select= c(Group, Day, Sex))
write.csv(high_GeneX_mice,"high_GeneX_mice.csv", row.names= FALSE)
high_GeneX_mice
