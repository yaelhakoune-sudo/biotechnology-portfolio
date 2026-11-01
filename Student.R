#Define a list of a student
student<-list(
  name = "Yael",
  last_name = 'Hakoune',
  age = 23,
  exam_grade = 89
)
student$exam_grade
#Pint the name of a student
student[1]

#Remove the age of the student
student[-3]

#Add a new factor "hw_grade" contains the score of homework 96
student$hw_grade<- 96
print(student)

#Add another factor that contains conditios
student$is_passed <- if(student$exam_grade >60 & student$hw_grade > 60){
  TRUE
}else{
  FALSE
}
print(student)

#Check if the value 100 is mentioned
100%in% student

#Make another list
extra_info <- list(
  hobby = "dance",
  city ="Jerusalem"
)

#Combine the two list into one list
student_full<-c(student, extra_info)
print(student_full)

#Check the amount of factors in the list
length(student_full)
