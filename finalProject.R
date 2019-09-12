#authored by: Skalony Konstantin
#ID: 310496781
#Date: 22.08.2019
##################################################################################
library(dplyr)
library(factoextra)
library(FactoMineR)
library(data.table)
library(caret)
library(ggplot2)
##################################################################################
#adjusting the data function   -   MOVE ALL VALUES TO NUMBERS
prepare.data <- function(Data)
{
  #female is zero
  Data <- Data %>%
    mutate(sex = ifelse(sex == "F",0,1))
  #urban is zero
  Data <- Data %>%
    mutate(address = ifelse(address == "U",0,1))
  Data <- Data %>%
    mutate(higher = ifelse(higher == "no",0,1))
  Data <- Data %>%
    mutate(internet = ifelse(internet == "no",0,1))
  Data <- Data %>%
    mutate(romantic = ifelse(romantic == "no",0,1))
  Data <- Data %>%
    mutate(nursery = ifelse(nursery == "no",0,1))
  Data <- Data %>%
    mutate(activities = ifelse(activities == "no",0,1))
  Data <- Data %>%
    mutate(schoolsup = ifelse(schoolsup == "no",0,1))
  Data <- Data %>%
    mutate(famsup = ifelse(famsup == "no",0,1))
  
  #multiply grades by 5 to get a 0-100 scale
  Data$G3 = Data$G3*5
  
  return(Data)
}
####################################################################################
#ggplot function - makes a ggplot with lm
my.ggplot <- function(raw.data)
{
  p1 = ggplot(raw.data, aes( raw.data$edu , raw.data$G3))+
    geom_point(col="red") + geom_smooth(method = 'lm', col= 'black')+
    labs(title = "---", x = "parents edu", y = "Score" )
  
  plot(p1)
}
####################################################################################
#barplot function - makes a barplot of total students with certain level parent
#while shows next to it the same students only with a certain grade
#str1 : title name
#x : grade to 
my.barplot <- function(data , str1 , x)
{
  par(mar=c(2, 2, 2, 2))
  
  barplot(c(data[edu == 1, sum(G3>=x)],data[edu == 1, sum(G3>=0)])
          , names.arg = c("passed ","total in course with parent lvl 1"), ylab="# of students", main = str1,col = c("green","orange"))
  
  barplot(c(data[edu == 2, sum(G3>=x)],data[edu == 2, sum(G3>=0)])
          , names.arg = c("passed ","total in course with parent lvl 2"), ylab="# of students", main = str1, col= c("green","orange"))
  
  barplot(c(data[edu == 3, sum(G3>=x)],data[edu == 3, sum(G3>=0)])
          , names.arg = c("passed ","total in course with parent lvl 3"), ylab="# of students", main = str1, col= c("green","orange"))
  
  barplot(c(data[edu == 4, sum(G3>=x)],data[edu == 4, sum(G3>=0)])
          , names.arg = c("passed ","total in course with parent lvl 4"), ylab="# of students", main = str1, col= c("green","orange"))
}
####################################################################################
#pca function
my.pca <- function(Data)
{
  pca.data=PCA(Data,graph = FALSE)
  p=fviz_pca_var(pca.data,labelsize = 4,col.var="contrib"
                 ,gradient.cols = c("blue","red","green"),repel = TRUE) 
  
  plot(p)
}
####################################################################################
#correlation function - finds the correlation betwwen all col. to the final grade one
my.cor <- function(Data)
{
  DT = suppressWarnings(sapply(Data, as.numeric))
  DT = as.data.frame(DT)
  Cordata = cor(DT)
  # correlation with the final grade
  Cordata = Cordata[,"G3"] 
  Cordata = sort(Cordata,decreasing = TRUE)  #sorting values from big to small
  View(Cordata)
}
####################################################################################


#LOADING THE DATA
#math course data
dtMath = fread("C:/Users/Admin/Desktop/student-mat.csv")
Math.data= as.data.table(dtMath)

#lang course data
dtLang = fread("C:/Users/Admin/Desktop/student-por.csv")
Lang.data= as.data.table(dtLang)

#removing unrelated data
Math.data[,31:32 := NULL]
Lang.data[,31:32 := NULL]
Math.data[,1 := NULL]
Lang.data[,1 := NULL]
Math.data[,8:11 := NULL]
Lang.data[,8:11 := NULL] 
Math.data[,4:5 := NULL]
Lang.data[,4:5 := NULL] 
Math.data[,paid := NULL]
Lang.data[,paid := NULL]

#prepare the data for work
Math.data = prepare.data(Math.data)
Lang.data = prepare.data(Lang.data)
Combined.data = rbind(Math.data,Lang.data)
##################################################################################
#CHOOSE A METHOD TO COMBINE Fedu Medu TO ONE COL.
#method #1 >avg of the parents education counts
methodNo1.combined = as.data.table(Combined.data )
methodNo1.combined$edu = (Combined.data$Fedu + Combined.data$Medu) /2
methodNo1.combined[,Fedu := NULL]
methodNo1.combined[,Medu := NULL]

#my.cor(methodNo1.combined)
##################################################################################
#method #2 > the higher educated parent counts
methodNo2.combined = as.data.table(Combined.data )
methodNo2.combined$edu = pmax.int(Combined.data$Fedu , Combined.data$Medu)
methodNo2.combined[,Fedu := NULL]
methodNo2.combined[,Medu := NULL]

#my.cor(methodNo2.combined)
##################################################################################
##################################################################################
#CHOOSING methos number 2
Combined.data = methodNo2.combined
Math.data = as.data.table(Math.data )
Math.data$edu = pmax.int(Math.data$Fedu , Math.data$Medu)
Math.data[,Fedu := NULL]
Math.data[,Medu := NULL]
Lang.data = as.data.table(Lang.data )
Lang.data$edu = pmax.int(Lang.data$Fedu , Lang.data$Medu)
Lang.data[,Fedu := NULL]
Lang.data[,Medu := NULL]

##################################################################################
#RAW data
raw.data = Combined.data

#-----------INITIAL DATA REVIEW WITH THE FUNCTIONS

#my.cor(raw.data)
#my.pca(raw.data)

#my.cor(Math.data)
#my.cor(Lang.data)

#my.ggplot(raw.data)
#my.ggplot(Math.data)
#my.ggplot(Lang.data)

##################################################################################
#prepare tables for work (DIVIDE TO SMALLER GROUPS)
math.18Plus =Math.data[which(Math.data$age>=18),] # all students 18 plus in math course
math.Under18 =Math.data[which(Math.data$age<18),] # all students under 18

Lang.18Plus =Lang.data[which(Lang.data$age>=18),] # all students 18 plus in lang. course
Lang.Under18 =Lang.data[which(Lang.data$age<18),] # all students under 18

##################################################################################
##################################################################################
#<<<<<<<<<<<<<<   USING MY BAR PLOT TO GET MORE INITIAL INFORMAITION

#my.barplot(Math.data , "Math Data - score over 55", 55)

#my.barplot(Lang.data , "Language Data - score over 55", 55)

#my.barplot(Math.data , "Math Data - score over 80", 80)

#my.barplot(Lang.data , "Language Data - score over 80", 80)

##################################################################################
##################################################################################
#>>>>>>>>>>   math course by gender and age
#>>>>>>> APPLING MY FUNCTIONS TO ALL DIFFENT GROUPS AND CHECK THE CHANGES

Math.Boys.under18 = math.Under18[which(math.Under18$sex== 1),]
#my.cor(Math.Boys.under18)
#my.pca(Math.Boys.under18)
#my.ggplot(Math.Boys.under18)
#my.barplot(Math.Boys.under18 , "Math.Boys.under18 Data - score over 80", 80)

Math.Boys18.Plus = math.18Plus[which(math.18Plus$sex== 1),]
#my.cor(Math.Boys18.Plus)
#my.pca(Math.Boys18.Plus)
#my.ggplot(Math.Boys18.Plus)
#my.barplot(Math.Boys18.Plus , "Math.Boys18.Plus Data - score over 80", 80)

Math.Girls.under18 = math.Under18[which(math.Under18$sex== 0),]
#my.cor(Math.Girls.under18)
#my.pca(Math.Girls.under18)
#my.ggplot(Math.Girls.under18)
#my.barplot(Math.Girls.under18 , "Math.Girls.under18 Data - score over 80", 80)

Math.Girls18.Plus = math.18Plus[which(math.18Plus$sex== 0),]
#my.cor(Math.Girls18.Plus)
#my.pca(Math.Girls18.Plus)
#my.ggplot(Math.Girls18.Plus)
#my.barplot(Math.Girls18.Plus , "Math.Girls18.Plus Data - score over 80", 80)

##################################################################################
#>>>>>>>>>>   language course by gender and age
#>>>>>>> APPLING MY FUNCTIONS TO ALL DIFFENT GROUPS AND CHECK THE CHANGES

Lang.Boys18.Plus = Lang.18Plus[which(Lang.18Plus$sex== 1),]
#my.cor(Lang.Boys18.Plus)
#my.pca(Lang.Boys18.Plus)
#my.ggplot(Lang.Boys18.Plus)
#my.barplot(Lang.Boys18.Plus , "Lang.Boys18.Plus Data - score over 80", 80)

Lang.Boys.under18 = Lang.Under18[which(Lang.Under18$sex== 1),]
#my.cor(Lang.Boys.under18)
#my.pca(Lang.Boys.under18)
#my.ggplot(Lang.Boys.under18)
#my.barplot(Lang.Boys.under18 , "Lang.Boys.under18 Data - score over 80", 80)

Lang.Girls18.Plus = Lang.18Plus[which(Lang.18Plus$sex== 0),]
#my.cor(Lang.Girls18.Plus)
#my.pca(Lang.Girls18.Plus)
#my.ggplot(Lang.Girls18.Plus)
#my.barplot(Lang.Girls18.Plus , "Lang.Girls18.Plus Data - score over 80", 80)

Lang.Girls.under18 = Lang.Under18[which(Lang.Under18$sex== 0),]
#my.cor(Lang.Girls.under18)
#my.pca(Lang.Girls.under18)
#my.ggplot(Lang.Girls.under18)
#my.barplot(Lang.Girls.under18 , "Lang.Girls.under18 Data - score over 80", 80)

##################################################################################
















