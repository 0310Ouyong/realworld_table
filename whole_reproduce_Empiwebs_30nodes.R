library(dplyr)
rm(list=ls())
getwd()
setwd('C:/data/Hsi')
load("EmpiricalFoodWebs.RData")
load('realworld_foodwebs_30nodes.RData')
Brose_data <- read.csv('Brose_foodwebdata.csv')

foodweb_name <- realworld_foodwebs_30nodes

rep_webs <- as.data.frame(matrix(nrow=74,ncol=32))
names(rep_webs) <- c('web.name','eco.type','num.sp','num.B','num.I','num.T','num.I.no.res','num.link',
                     'num.ibi.link','num.TI','num.TB','num.II','num.IB','num.2D','num.3D','num.Gra','num.Sit',
                     'num.Act','Prop.T','Prop.TI','Prop.TB','Prop.II','Prop.IB','Prop.3D','Prop.Act',
                     'T.score','R.C.slope','R.C.cor','R.C.R2','R.C.slope.R2','Prop.C.big.R.small','mean.mass')
rep_webs[,1] <- foodweb_name

#Select food-web data
Select_Brose_data <- filter(Brose_data,foodweb.name%in%foodweb_name)
Select_Brose_data <- filter(Select_Brose_data,interaction.type%in%c('predacious','herbivorous','fungivorous'))

basal_resources <- list()
intermediate_consumers <- list()
top_consumers <- list()

for(i in 1 :74){
  basal_resources[[i]] <- unique(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),res.metabolic.type%in%'primary producer')$res.taxonomy)
  intermediate_consumers[[i]] <- unique(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),!res.metabolic.type%in%'primary producer')$res.taxonomy)
  top_consumers[[i]] <- unique(setdiff(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$con.taxonomy,filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$res.taxonomy))
}

for(i in 1:74){
  rep_webs[i,2] <- unique(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$ecosystem.type)
  rep_webs[i,3] <- length(unique(c(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$con.taxonomy,filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$res.taxonomy)))
  rep_webs[i,4] <- length(unique(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),res.metabolic.type%in%'primary producer')$res.taxonomy))
  rep_webs[i,5] <- length(unique(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),!res.metabolic.type%in%'primary producer')$res.taxonomy))
  rep_webs[i,6] <- length(unique(setdiff(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$con.taxonomy,filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$res.taxonomy)))
  rep_webs[i,7] <- length(unique(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),!res.metabolic.type%in%'primary producer')$res.taxonomy))-length(intersect(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$res.taxonomy,filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$con.taxonomy))
  rep_webs[i,8] <- dim(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]))[1]
  rep_webs[i,9] <- dim(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),interaction.classification%in%'ibi'))[1]
  rep_webs[i,10] <- dim(filter(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),!res.metabolic.type%in%'primary producer'),con.taxonomy%in%top_consumers[[i]]))[1]
  rep_webs[i,11] <- dim(filter(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),res.metabolic.type%in%'primary producer'),con.taxonomy%in%top_consumers[[i]]))[1]
  rep_webs[i,12] <- dim(filter(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),!res.metabolic.type%in%'primary producer'),con.taxonomy%in%intermediate_consumers[[i]]))[1]
  rep_webs[i,13] <- dim(filter(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),res.metabolic.type%in%'primary producer'),con.taxonomy%in%intermediate_consumers[[i]]))[1]
  rep_webs[i,14] <- sum(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$interaction.dimensionality%in%'2D')
  rep_webs[i,15] <- sum(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$interaction.dimensionality%in%'3D  ')
  rep_webs[i,16] <- sum(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$res.movement.type%in%'sessile'&!filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$con.movement.type%in%'sessile')
  rep_webs[i,17] <- sum(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$con.movement.type%in%'sessile')
  rep_webs[i,18] <- dim(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]))[1]-sum(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$con.movement.type%in%'sessile'|filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$res.movement.type%in%'sessile')
  rep_webs[i,19] <- rep_webs[i,6]/rep_webs[i,3]
  rep_webs[i,20] <- rep_webs[i,10]/rep_webs[i,8]
  rep_webs[i,21] <- rep_webs[i,11]/rep_webs[i,8]
  rep_webs[i,22] <- rep_webs[i,12]/rep_webs[i,8]
  rep_webs[i,23] <- rep_webs[i,13]/rep_webs[i,8]
  rep_webs[i,24] <- rep_webs[i,15]/rep_webs[i,8]
  rep_webs[i,25] <- rep_webs[i,18]/rep_webs[i,8]
  rep_webs[i,26] <- rep_webs[i,19]-0.5*rep_webs[i,19]*(rep_webs[i,11]/(rep_webs[i,6]*rep_webs[i,4]))
  rep_webs[i,27] <- summary(lm(formula=log10(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),interaction.classification%in%'ibi')$res.mass.mean.g.)~log10(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),interaction.classification%in%'ibi')$con.mass.mean.g.)))[[4]][2]
  rep_webs[i,28] <- cor(log10(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),interaction.classification%in%'ibi')$res.mass.mean.g.),log10(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),interaction.classification%in%'ibi')$con.mass.mean.g.))
  rep_webs[i,29] <- summary(lm(formula=log10(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),interaction.classification%in%'ibi')$res.mass.mean.g.)~log10(filter(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i]),interaction.classification%in%'ibi')$con.mass.mean.g.)))[[8]]
  rep_webs[i,30] <- rep_webs[i,27]*rep_webs[i,29]
  rep_webs[i,31] <- (sum(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$con.mass.mean.g. > filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])$res.mass.mean.g.)-(rep_webs[i,8]-rep_webs[i,9]))/rep_webs[i,9]
}

rep_webs[is.na(rep_webs)] <- 0
mean.mass <- c()
for(i in 1:74){
  data1 <- as.data.frame(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])[,c(7,19)])
  names(data1) <- c('taxonomy','mass')
  data2 <- as.data.frame(filter(Select_Brose_data,foodweb.name%in%foodweb_name[i])[,c(21,33)])
  names(data2) <- c('taxonomy','mass')
  data3 <- rbind(data1,data2)
  data3 <- data3[!duplicated(data3[,1]),]
  mean.mass <- c(mean.mass,mean(data3$mass))
}
rep_webs[,32] <- mean.mass/1000
save(rep_webs,file = 'whole_reproduce_Empiwebs_30nodes.RData')
