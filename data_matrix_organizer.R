#Funtions to organize the data matrix to conduct the statistical analyses by placing species relative densities in columns

setwd("C:/Users/gmazz/OneDrive/Documentos/Diversity experiment")

spp<-read.table("sp_names_abrev.txt",h=T)
composition<-read.table("compositions.txt",h=T)
R_pos<-read.table("posicoes_composicoes_graus_wgs.txt",h=T, dec=",")

spp_comp<-strsplit(as.character(composition[,1]),split="+", fixed=T)
names(spp_comp)<-as.numeric(composition[,1])

spp_community<-matrix(0,nr=155,nc=16)
colnames(spp_community)<-spp$Abrev

for(i in 1:nrow(R_pos)){ #i = plots
  compo<-R_pos$composition[i]
  if(compo==1) next
  else{
    gg<-spp_comp[[which(names(spp_comp) %in% compo)[1]]]
    spp_community[i,gg]<-1/length(gg) #Relative density
  }
}

R_pos$facilitation<-colSums(sppFaci*t(spp_community))

R_pos<-data.frame(R_pos,spp_community)

write.table(R_pos,file="data_DivExp.txt",row.names=F)
