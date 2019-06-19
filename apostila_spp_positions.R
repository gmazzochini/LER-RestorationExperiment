# Code to create the field apostille with plots compositions and species positions inside the plots

spp<-read.table("/Users/gmazzochini/Dropbox/Public/sp_names_abrev.txt",h=T)
composition<-read.table("/Users/gmazzochini/Dropbox/Public/compositions.txt",h=T)
R_pos<-read.table("/Users/gmazzochini/Dropbox/Public/posicoes_composicoes_graus_wgs.txt", dec=",", h=T)
composition$Ncompo<-as.numeric(composition[,1])
composition$nsp<-unlist(lapply(strsplit(as.character(composition[,1]),split="+", fixed=T), length))
composition$nsp[composition$Ncompo==1]<-0

h=2
y<-rep(h*0:3,t=8)+rep(c(0,h/2),e=4)+.5
x<-rep(seq(0,13,sqrt((h^2)-((h/2)^2))),e=4)+.5

spp_comp<-strsplit(as.character(composition[,1]),split="+", fixed=T)
names(spp_comp)<-as.numeric(composition[,1])

spp_community<-matrix(0,nr=155,nc=16)
colnames(spp_community)<-spp$Abrev

R_pos<-data.frame(R_pos,spp_community)

spp_pos<-matrix(NA,nr=155,nc=32)

for(i in 1:155){
  spp_plot<-unlist(strsplit(as.character(unique(composition$composition[which(composition$Ncompo==R_pos$compositions[i])])),split="+",fixed=T))
  spp_plot<-spp$Abrev[spp$Abrev %in% spp_plot]

  if(length(spp_plot)==0){
    spp_pos[i,]<-as.character(rep("0",32))
  }
  
  if(length(spp_plot)==1){
    spp_pos[i,]<-as.character(rep(spp_plot,32))
  }
  
  if(length(spp_plot)==2){
    spp_pos[i,]<-rep(c(as.character(sample(spp_plot)),
                       as.character(sample(spp_plot)),
                       as.character(sample(spp_plot)),
                       as.character(sample(spp_plot)),
                       as.character(sample(spp_plot)),
                       as.character(sample(spp_plot)),
                       as.character(sample(spp_plot)),
                       as.character(sample(spp_plot))),t=2)
  }
  
  if(length(spp_plot)==4){
    spp_pos[i,]<-rep(c(as.character(sample(spp_plot)),
                       as.character(sample(spp_plot)),
                       as.character(sample(spp_plot)),
                       as.character(sample(spp_plot))),t=2)
  }
  if(length(spp_plot)==8){
    spp_pos[i,]<-rep(c(as.character(sample(spp_plot)),
                       as.character(sample(spp_plot))),t=2)
  }
  if(length(spp_plot)==16){
    spp_pos[i,]<-rep(as.character(sample(spp_plot)),t=2)
  }
  
}



pdf(height=11.69291, width=8.26772, file="/Users/gmazzochini/Dropbox/Public/apostila_campo.pdf")
par(mar=c(5,10,2,1), mfrow=c(3,1))


for(i in 1:155){
  if("0"%in%unique(spp_pos[i,])){
    plot(y ~ x, frame=F,xaxt="n",yaxt="n",ylim=c(0,9),xlab="",ylab="",type="n",xlim=c(0,18))
    points(y ~ x,cex=1.5, pch=16)
    rect(0,0,max(x)+.5,max(y)+.5,lwd=2,border="grey85")
    text(x,y+0.3,spp_pos[i,], cex=.8)
    text(x,y-0.3,1:32, cex=1)
    axis(1, at=unique(round(x,2)), pos=0, cex.axis=1)
    axis(2, at=unique(round(y,2)), pos=0, las=2, cex.axis=1)
    mtext(paste("Parcela",i, "(Controle)", sep=" "),3, at=6, cex=1.5, line=-1)
  }
  else{
    if(length(unique(spp_pos[i,]))==1){
      plot(y ~ x, frame=F,xaxt="n",yaxt="n",ylim=c(0,9),xlab="",ylab="",type="n",xlim=c(0,18))
      points(y ~ x,cex=1.5, pch=16)
      rect(0,0,max(x)+.5,max(y)+.5,lwd=2,border="grey85")
      text(x,y+0.3,spp_pos[i,], cex=.8)
      text(x,y-0.3,1:32, cex=1)
      axis(1, at=unique(round(x,2)), pos=0, cex.axis=1)
      axis(2, at=unique(round(y,2)), pos=0, las=2, cex.axis=1)
      text(14,8,32, cex=1.7)
      text(16,8,unique(spp_pos[i,]), cex=1.7)
      mtext(paste("Parcela",i, "(1 sp)", sep=" "),3, at=6, cex=1.5, line=-1)
    }
    if(length(unique(spp_pos[i,]))==2){
      plot(y ~ x, frame=F,xaxt="n",yaxt="n",ylim=c(0,9),xlab="",ylab="",type="n",xlim=c(0,18))
      points(y ~ x,cex=1.5, pch=16)
      rect(0,0,max(x)+.5,max(y)+.5,lwd=2,border="grey85")
      text(x,y+0.3,spp_pos[i,], cex=.8)
      text(x,y-0.3,1:32, cex=1)
      axis(1, at=unique(round(x,2)), pos=0, cex.axis=1)
      axis(2, at=unique(round(y,2)), pos=0, las=2, cex.axis=1)
      text(14,c(8,7.5),16, cex=1.7)
      text(16,c(8,7.5),unique(spp_pos[i,]), cex=1.7)
      mtext(paste("Parcela",i, "(2 sp)", sep=" "),3, at=6, cex=1.5, line=-1)
    }
    if(length(unique(spp_pos[i,]))==4){
      plot(y ~ x, frame=F,xaxt="n",yaxt="n",ylim=c(0,9),xlab="",ylab="",type="n",xlim=c(0,18))
      points(y ~ x,cex=1.5, pch=16)
      rect(0,0,max(x)+.5,max(y)+.5,lwd=2,border="grey85")
      text(x,y+0.3,spp_pos[i,], cex=.8)
      text(x,y-0.3,1:32, cex=1)
      axis(1, at=unique(round(x,2)), pos=0, cex.axis=1)
      axis(2, at=unique(round(y,2)), pos=0, las=2, cex.axis=1)
      text(14,c(8,7.5,7,6.5),8, cex=1.7)
      text(16,c(8,7.5,7,6.5),unique(spp_pos[i,]), cex=1.7)
      mtext(paste("Parcela",i, "(4 sp)", sep=" "),3, at=6, cex=1.5, line=-1)
    }
    
    if(length(unique(spp_pos[i,]))==8){
      plot(y ~ x, frame=F,xaxt="n",yaxt="n",ylim=c(0,9),xlab="",ylab="",type="n",xlim=c(0,18))
      points(y ~ x,cex=1.5, pch=16)
      rect(0,0,max(x)+.5,max(y)+.5,lwd=2,border="grey85")
      text(x,y+0.3,spp_pos[i,], cex=.8)
      text(x,y-0.3,1:32, cex=1)
      axis(1, at=unique(round(x,2)), pos=0, cex.axis=1)
      axis(2, at=unique(round(y,2)), pos=0, las=2, cex.axis=1)
      text(14,c(8,7.5,7,6.5,6,5.5,5,4.5),4, cex=1.7)
      text(16,c(8,7.5,7,6.5,6,5.5,5,4.5),unique(spp_pos[i,]), cex=1.7)
      mtext(paste("Parcela",i, "(8 sp)", sep=" "),3, at=6, cex=1.5, line=-1)
    }
    
    if(length(unique(spp_pos[i,]))==16){
      plot(y ~ x, frame=F,xaxt="n",yaxt="n",ylim=c(0,9),xlab="",ylab="",type="n",xlim=c(0,18))
      points(y ~ x,cex=1.5, pch=16)
      rect(0,0,max(x)+.5,max(y)+.5,lwd=2,border="grey85")
      text(x,y+0.3,spp_pos[i,], cex=.8)
      text(x,y-0.3,1:32, cex=1)
      axis(1, at=unique(round(x,2)), pos=0, cex.axis=1)
      axis(2, at=unique(round(y,2)), pos=0, las=2, cex.axis=1)
      text(14,c(8,7.5,7,6.5,6,5.5,5,4.5,4,3.5,3,2.5,2,1.5,1,0.5),2, cex=1.7)
      text(16,c(8,7.5,7,6.5,6,5.5,5,4.5,4,3.5,3,2.5,2,1.5,1,0.5),unique(spp_pos[i,]), cex=1.7)
      mtext(paste("Parcela",i, "(16 sp)", sep=" "),3, at=6, cex=1.5, line=-1)
    }
  }
}
dev.off()
