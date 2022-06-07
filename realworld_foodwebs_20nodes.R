library(dplyr)
rm(list=ls())
getwd()
setwd('C:/data/Hsi')
load("EmpiricalFoodWebs.RData")
Brose_data <- read.csv('Brose_foodwebdata.csv')

Select_Brose_data <- filter(Brose_data,interaction.type%in%c('predacious','herbivorous','fungivorous'))

length(unique(Select_Brose_data$foodweb.name)) #290

foodweb.name1 <- unique(Select_Brose_data$foodweb.name)

foodweb_30nodes1 <- data.frame()

for(i in 1:290){
  if(length(unique(c(filter(Select_Brose_data,foodweb.name%in%foodweb.name1[i])$res.taxonomy,filter(Select_Brose_data,foodweb.name%in%foodweb.name1[i])$con.taxonomy)))>=20){
    foodweb_30nodes1 <- rbind(foodweb_30nodes1,filter(Select_Brose_data,foodweb.name%in%foodweb.name1[i]))
  }
}

foodweb.name2 <- unique(foodweb_30nodes1$foodweb.name) #241

foodweb_30nodes2 <- data.frame()

for(i in 1:241){
  if(sum(na.omit(filter(foodweb_30nodes1,foodweb.name%in%foodweb.name2[i])$res.metabolic.type=='primary producer'))>0){
    foodweb_30nodes2 <- rbind(foodweb_30nodes2,filter(foodweb_30nodes1,foodweb.name%in%foodweb.name2[i]))
  }else{
    foodweb_30nodes2 <- foodweb_30nodes2
  }
}

foodweb.name3 <- unique(foodweb_30nodes2$foodweb.name) #229

foodweb_30nodes3 <- data.frame()
for(i in 1:229){
  if(sum(is.na(c(filter(foodweb_30nodes2,foodweb.name%in%foodweb.name3[i])$con.mass.mean.g.,filter(foodweb_30nodes2,foodweb.name%in%foodweb.name3[i])$res.mass.mean.g.)))==0){
    foodweb_30nodes3 <- rbind(foodweb_30nodes3,filter(foodweb_30nodes2,foodweb.name%in%foodweb.name3[i]))
  }else{
    foodweb_30nodes3 <- foodweb_30nodes3
  }
}

foodweb.name4 <- unique(foodweb_30nodes3$foodweb.name) #229

foodweb_30nodes4 <- data.frame()

for(i in 1:229){
  if(sum(c(filter(foodweb_30nodes3,foodweb.name%in%foodweb.name4[i])$con.mass.mean.g.,filter(foodweb_30nodes3,foodweb.name%in%foodweb.name4[i])$res.mass.mean.g.)<0) == 0 ){
    foodweb_30nodes4 <- rbind(foodweb_30nodes4,filter(foodweb_30nodes3,foodweb.name%in%foodweb.name4[i]))
  }else{
    foodweb_30nodes4 <- foodweb_30nodes4
  }
}

foodweb.name5 <- unique(foodweb_30nodes4$foodweb.name) #153

foodweb_30nodes5 <- data.frame()

for(i in 1:153){
  if(sum(na.omit(filter(foodweb_30nodes4,foodweb.name%in%foodweb.name5[i])$interaction.classification == 'ibi'))>0.5*dim(filter(foodweb_30nodes4,foodweb.name%in%foodweb.name5[i]))[1]){
    foodweb_30nodes5 <- rbind(foodweb_30nodes5,filter(foodweb_30nodes4,foodweb.name%in%foodweb.name5[i]))
  }else{
    foodweb_30nodes5 <- foodweb_30nodes5
  }
}

length(unique(foodweb_30nodes5$foodweb.name)) #110

realworld_foodwebs_20nodes <- unique(foodweb_30nodes5$foodweb.name)
save(realworld_foodwebs_20nodes,file = 'realworld_foodwebs_20nodes.RData')
