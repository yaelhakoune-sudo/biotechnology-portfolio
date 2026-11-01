#Identify the wanted file and skip the first 3 rows in the files data
df <- read.csv("/Users/yaelhakoune/Downloads/mouse_data.csv", skip = 3)
getwd()

#Verify the first 3 rows if it did the run
print(df)

#Add a folder named R and Print
getwd()
#Change the organised folder where the R is
setwd("/Users/yaelhakoune/R")
getwd()

#Read the file again by the name of the file only, skip 3 rows and i dentify it
df <- read.csv("mouse_data.csv",
               skip = 3,
               row.names = "Mouse_ID")
#Present the first 6 rows and the last 6 rows
head(df,6)
tail(df,6)

#Summarize the datas
summary(df)

#Check column and the rows data correctly , if not change it
str(df)

#The value of the weight should be positive, if not, change it to NA
df$Weight_g[df$Weight_g < 0] <- NA

#Verify if the updated file is in the folder
write.csv(df,
          "mouse_data_updated.csv",
          row.names = TRUE)
file.exists("mouse_data_updated.csv")
