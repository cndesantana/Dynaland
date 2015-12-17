getExtinctionRate<-function(dat,i,nGen,sprate,S,J){
   extrate<-array(0,nGen);
   for(k in 2:nGen){
      newspecies  <- dat$lastspecies[k] - dat$lastspecies[k-1];
      deltaGamma  <- dat$Gamma[k] - dat$Gamma[k-1];
      lostspecies <- newspecies - deltaGamma;
      extrate[k]  <- lostspecies/dat$Gamma[k-1];
   }
   return(extrate/(S*J));
}


plotrichness<-function(dat,filename){
   allcolors<-c("black","red","green","blue","orange","purple","cyan","brown","lightblue","lightgreen","pink","gray75")
   nGen<-max(dat$G)+1;
   myrc<-as.numeric(names(table(dat$rc)))
   mycolors<-allcolors[1:length(myrc)];
   png(filename,width=1980,height=1280,res=300);
   i<-1;
   myx<-dat$G[((i-1)*nGen)+(1:nGen)];
   myy<-dat$Gamma[((i-1)*nGen)+(1:nGen)];
   plot(myx,myy,pch=i,col=mycolors[i],ylim=c(0,300),xlab="Generations",ylab="Richness",main="Richness\nStatic landscape")
   for(i in 2:length(myrc)){
       myx<-dat$G[((i-1)*nGen)+(1:nGen)];
       myy<-dat$Gamma[((i-1)*nGen)+(1:nGen)];
       points(myx,myy,pch=i,col=mycolors[i])
       points(myx,myy,pch=i,col=mycolors[i])
       points(myx,myy,pch=i,col=mycolors[i])
       points(myx,myy,pch=i,col=mycolors[i])
   }
   par(xpd="TRUE");
   legend("topright",legend=signif(myrc,5),col=mycolors,lwd=1,pch=(1:length(myrc)),title="critical radius");
   dev.off();
}

plotextinctionrate<-function(dat,filename){
   allcolors<-c("black","red","green","blue","orange","purple","cyan","brown","lightblue","lightgreen","pink","gray75")
   nGen<-max(dat$G)+1;
   sprate<-dat$vr[1];
   S<-dat$S[1];
   J<-dat$J[1];
   myrc<-as.numeric(names(table(dat$rc)))
   mycolors<-allcolors[1:length(myrc)];
   png(filename,width=1980,height=1280,res=300);
   i<-1;
   myx<-dat$G[((i-1)*nGen)+(1:nGen)]+1;
   myy<-getExtinctionRate(dat[((i-1)*nGen)+(1:nGen),],i,nGen,sprate,S,J);
   plot(myx,myy,pch=i,col=mycolors[i],ylim=c(0,sprate/30),xlab="Generations",ylab="Ext. rate",main="Extinction rate\nStatic landscape")
   for(i in 2:length(myrc)){
       myx<-dat$G[((i-1)*nGen)+(1:nGen)];
       myy<-getExtinctionRate(dat[((i-1)*nGen)+(1:nGen),],i,nGen,sprate,S,J);
       points(myx,myy,pch=i,col=mycolors[i])
       points(myx,myy,pch=i,col=mycolors[i])
       points(myx,myy,pch=i,col=mycolors[i])
       points(myx,myy,pch=i,col=mycolors[i])
   }
   par(xpd="TRUE");
   legend("topleft",legend=signif(myrc,5),col=mycolors,lwd=1,pch=(1:length(myrc)),title="critical radius");
   dev.off();
}

args <- commandArgs(trailingOnly = TRUE)
dat<-read.csv(args,sep=" ")

split_filename<-unlist(strsplit(args,'[.]'))
inputfile<-paste(".",paste(paste(split_filename[2],split_filename[3],sep='_'),split_filename[4],sep="."),sep="");
splitinput<-unlist(strsplit(inputfile,'_'))
sufix<-paste(paste(splitinput[2],splitinput[3],splitinput[4],splitinput[5],unlist(strsplit(splitinput[6],'[.]'))[1],sep="_"),".png",sep="");
richfile<-paste("./Richness_",sufix,sep="");
plotrichness(dat,richfile);
extratefile<-paste("./ExtinctionRate_",sufix,sep="");
plotextinctionrate(dat,extratefile);
