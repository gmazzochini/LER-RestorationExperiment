set.seed(356)

richness=rep(c(0,1,2,4,8,16),t=c(5,48,48,24,12,15))
#pool<-LETTERS[1:16]

pool<-c("MimTen","HanImp","AspPyr","ComLep","PipSti","AmbCea","CynFle","CombLep",
        "LibFer","BauChe","PoiGar","PseMar","CocVit","ZizJoa","AnaCol","SebMac")


composition<-vector("character",150)

#Observed facilitation index of the species planted in the experiment
sppFaci<-c(0.130,0.101,0.187,0.057,0.058,0.068,0.060,0.060,0.033,
           0.047,0.022,0.006,-0.001,-0.009,-0.063,-0.069)
names(sppFaci)<-pool



#Defining compositions based on observed facilitation
compo0<-rep("0",5)
compo1<-rep(pool,e=3)
groups<-vector("list",4)
groups[[1]]<-pool[1:4] ; groups[[2]]<-pool[5:8] ; groups[[3]]<-pool[9:12] ; groups[[4]]<-pool[13:16]
compo2b<-rep(apply(matrix(unlist(lapply(groups, sample)),nc=8),2,function(x) paste(x,collapse="+")),e=3)
compo4b<-rep(apply(matrix(pool,nc=4),2,function(x) paste(x,collapse="+")),e=3)
compo8b<-rep(apply(matrix(pool,nc=2),2,function(x) paste(x,collapse="+")),e=3)
compo16<-rep(paste(pool,collapse="+"),15)

#Randomizing independent compositions for each richness level with three replications for each composition
d=T
while(d == T){
  compo2a<-apply(matrix(sample(pool),nc=2),1,function(x) paste(x))
  compo4a<-apply(matrix(sample(pool),nc=4),1,function(x) paste(x))
  compo8a<-apply(matrix(sample(pool),nc=8),1,function(x) paste(x))
  
  comps<-sapply(unique(compo2b), function(x) strsplit(x,split="+",fixed=T))
  a<-logical(length(comps))
  for(i in 1:length(comps)){
   a[i]<-any(colSums(matrix(compo2a %in%  comps[[i]],nr=2))==2)
  }

  comps<-sapply(unique(compo4b), function(x) strsplit(x,split="+",fixed=T))
  b<-logical(length(comps))
  for(i in 1:length(comps)){
    b[i]<-any(colSums(matrix(compo4a %in%  comps[[i]],nr=4))>2)
  }

  comps<-sapply(unique(compo8b), function(x) strsplit(x,split="+",fixed=T))
  c<-logical(length(comps))
  for(i in 1:length(comps)){
    c[i]<-any(colSums(matrix(compo8a %in%  comps[[i]],nr=8))>4)
  }
  
  d<-any(a,b,c)
}

compo2a<-rep(apply(compo2a,2,function(x) paste(x,collapse="+")),e=3)
compo4a<-rep(apply(compo4a,2,function(x) paste(x,collapse="+")),e=3)
compo8a<-rep(apply(compo8a,2,function(x) paste(x,collapse="+")),e=3)

composition<-c(compo0,compo1,compo2a,compo2b,compo4a,compo4b,compo8a,compo8b,compo16)

#write.table(cbind(composition), file="C:/Users/João/OneDrive/Documentos/Diversity experiment/compositions.txt", row.names=F)

#Effect of species on biomass of the community is proportional to species facilitaiton index
sppBiomass<-round(abs(rnorm(16,mean=1000*c(exp(sppFaci-min(sppFaci))^2),sd=150)),1)
names(sppBiomass)<-pool


biomass<-vector("numeric",length(composition))
sppMatrix<-matrix(0,nr=length(composition),nc=16,dimnames=list(1:length(composition),pool))

for(i in 1:length(composition)){ #i = plots
  compo<-composition[i]
  if(compo=="0") biomass[i]<-abs(rnorm(1,500,300))
  else{
    sppcompo<-unlist(strsplit(compo,"+",fixed=T))  #Species present in the plot
    sppMatrix[i,sppcompo]<-1/length(sppcompo) #Relative density
    #Biomass is a function of richness and species effects on biomass (SppBiomass)
    biomass[i]<-rnorm(1,500*log(richness[i]),300)+sum(rnorm(richness[i],sppBiomass[sppcompo],300)*sppMatrix[i,sppcompo])
  }
}


### Analyses
data <- data.frame(biomass,richness,composition,sppMatrix)

#Potential plot Facilitation is calculated as a weighted mean of facilitation index of species in a plot
data$facilitation <- colSums(sppFaci*t(sppMatrix))

plot(richness ~ facilitation, data,  frame=F, yaxt="n", xlim=c(-0.1,0.2), ylab="Richness", xlab="Facilitation")  #No correlation between Facilitation and Richness
axis(2, at=richness[-c(1:3)])
cor.test(data$richness,data$facilitation)

library(lme4)

#Analyses using Potential Plot Facilitation as additional covariate
#Full model with facilitation
m1 <- lmer(biomass ~ log(richness+1) * facilitation + (1|compositions) , data=data)
summary(m1)
plot(m1)

windows(200,100)
par(mfrow=c(1,2))
plot(biomass ~ richness,data, xaxt="n", frame=F, xlab="Richness")
axis(1,at=data$richness)
plot(biomass ~ facilitation,data, frame=F, xlab="Facilitation")


#Model without Richness
m2 <- lmer(biomass ~ facilitation + (1|compositions) , data=data)
anova(m1,m2)  #Testing richness effect with LRT

#Model without Facilitation
m3 <- lmer(biomass ~ log(richness+1) + (1|composition) , data=data)
anova(m1,m3)  #Testing facilitation effect with LRT

#Model without interaction
m4 <- lmer(biomass ~ log(richness+1) + facilitation + (1|composition) , data=data)
anova(m1,m4)  #Testing facilitation effect with LRT



#Analyses using species identity
#Full model
m1_identity <- lmer(biomass ~ log(richness+1) + sppMatrix + (1|composition) , data=data)
summary(m1_identity, corr=F)
plot(m1_identity)

#Extract individual species effect of functioning
sppEffect <- fixef(m1_identity)[-c(1:2)]

#Correlation between species effect on ecosystem functioning and species facilitation index
summary(lm(sppEffect ~ sppFaci))
plot(sppEffect ~ sppFaci)

