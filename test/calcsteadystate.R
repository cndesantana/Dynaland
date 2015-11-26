calcConvergence<-function(subset){
   conv = sd(subset)/mean(subset)
   return(conv)
}

plotConvergence<-function(inputfilename,dat){
   maxrich<-max(as.numeric(dat$Gamma));
   maxgen<-max(as.numeric(dat$G));
   myr0<-as.numeric(names(table(dat$r0)))
   outputfilename<-paste(inputfilename,".convergence.png",sep="");
   png(outputfilename,width=1980,height=1280,res=300);
   for(i in 1:length(myr0)){
      mydat<-dat;
      pos<-which(mydat$r0==myr0[i])
      mydat<-mydat[pos,];

      newpos<-which(mydat$Ncomponents==1);
      mydat<-mydat[newpos,];
      ndat<-length(mydat[,1])      
      mybeg<-1;myend<-500; 
      myconvergence<-list();
      
      while(myend<=ndat){
         myconvergence<-c(as.numeric(myconvergence),calcConvergence(mydat$G[mybeg:myend]));
         mybeg<-mybeg+1;
         myend<-myend+1;
      }  
      nwindows<-length(myconvergence);
      if(i==1){
         cat("plotting plot...",sep="\n");
         plot(1:nwindows,as.numeric(myconvergence),xlab="Generations",ylab="sd/mean",main="Convergence",type="b",col=i,pch=i,log="xy")
      }else{
         cat("plotting points...",sep="\n");
         points(1:nwindows,as.numeric(myconvergence),col=i,type="b",pch=i)
      }
   }
   par(xpd="TRUE");
   legend("bottomleft",legend=signif(myr0,2),col=(1:length(myr0)),lwd=1,pch=(1:length(myr0)),title="mean radius");
   dev.off();
   mydate<-format(Sys.time(), "%H_%M_%S_%Y");system(paste("sh sendmail.sh Figure","_",mydate," charles.santana@gmail.com ",outputfilename,sep=""));
}

plotSteadystate<-function(inputfilename,dat){
   maxrich<-max(as.numeric(dat$Gamma));
   maxgen<-max(as.numeric(dat$G));
   myr0<-as.numeric(names(table(dat$r0)))
   outputfilename<-paste(inputfilename,".steadystate.png",sep="");
   png(outputfilename,width=1980,height=1280,res=300);
   for(i in 1:length(myr0)){
   	pos<-which(dat$r0==myr0[i])
   	mydat<-dat[pos,]
    newpos<-which(mydat$Ncomponents==1);
    mydat<-mydat[newpos,];
   	mydat$G<-mydat$G+1;
   	if(i==1){
                   plot(log10(mydat$G),mydat$Gamma,xlab="log10(Generations)",ylab="Richness",col=i,type="b",main="Steady State Study\n (A = 0, f = 0)",xlim=c(0,log10(maxgen)),ylim=c(0,maxrich),pch=i)
   	}else{
   		points(log10(mydat$G),mydat$Gamma,col=i,type="b",pch=i)
   	}
   }
   par(xpd="TRUE");
   legend("bottomleft",legend=signif(myr0,2),col=(1:length(myr0)),lwd=1,pch=(1:length(myr0)),title="mean radius");
   dev.off();
}

#args <- commandArgs(trailingOnly = TRUE)
#inputfilename<-args[1];
inputfilename<-"SitesOutputs_Static3.txt"
dat<-read.csv(inputfilename,sep=" ")

plotSteadystate(inputfilename,dat);
plotConvergence(inputfilename,dat);
