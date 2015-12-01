calcConvergence<-function(subset){
   conv = sd(subset)/mean(subset)
   cat(paste(sd(subset),mean(subset),conv,sep=" "),sep="\n",file="./log.dat",append=TRUE);
   return(conv)
}

plotAreaRichness<-function(inputfilename,dat,f,A){
   outputfilename<-paste(inputfilename,".area_richness.png",sep="");
   mydat<-dat;
   pos<-which(mydat$G >= (max(mydat$G)/2))#only for the last generation time
   mydat<-mydat[pos,];
   png(outputfilename,width=1980,height=1280,res=300);
   plot(mydat$Nverts,mydat$GammaPerComponent,log="x",xlab="Size of the Component",ylab="Richness of the component",main=paste("Area vs. Richness\n (A = ",signif(A,2),", f = ",signif(f,2),")",sep=""),ylim=c(0,400))
   dev.off()
}


plotConvergence<-function(inputfilename,dat,f,A){
   maxrich<-max(as.numeric(dat$Gamma));
   maxgen<-max(as.numeric(dat$G));
   myr0<-unique(sort(dat$r0));
   outputfilename<-paste(inputfilename,".convergence.png",sep="");
   png(outputfilename,width=1980,height=1280,res=300);
   mycolor<-1;
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
         myconvergence<-c(as.numeric(myconvergence),calcConvergence(mydat$Gamma[mybeg:myend]));
         mybeg<-mybeg+1;
         myend<-myend+1;
      }  
      nwindows<-length(myconvergence);
      if(i==1){
         cat("plotting plot...",sep="\n");
         plot(1:nwindows,as.numeric(myconvergence),xlab="Generations",ylab="sd/mean",main=paste("Convergence\n (A = ",signif(A,2),", f = ",signif(f,2),")",sep=""),type="p",col=mycolor,pch=i,log="x",ylim=c(0,0.5))
         mycolor<-mycolor+1;
      }else{
         cat("plotting points...",sep="\n");
         if(sum(mydat$Nedges)>0){
              points(1:nwindows,as.numeric(myconvergence),col=mycolor,type="p",pch=i)
              points(log10(mydat$G),mydat$Gamma,col=mycolor,type="p",pch=i)
              mycolor<-mycolor+1;
         }
      }
   }
   if(mycolor <= 6)#if there are not so much values for the legend
   { 
      par(xpd="TRUE");
      legend("topright",legend=signif(myr0,2),col=(1:length(myr0)),lwd=1,pch=(1:length(myr0)),title="mean radius");
   }
   dev.off();
#   mydate<-format(Sys.time(), "%H_%M_%S_%Y");system(paste("sh sendmail.sh ConvFigure","_",mydate," charles.santana@gmail.com ",outputfilename,sep=""));
}

plotSteadystate<-function(inputfilename,dat,f,A){
   maxrich<-max(as.numeric(dat$Gamma));
   maxgen<-max(as.numeric(dat$G));
   myr0<-unique(sort(dat$r0));
   outputfilename<-paste(inputfilename,".steadystate.png",sep="");
   png(outputfilename,width=1980,height=1280,res=300);
   mycolor<-1;
   for(i in 1:length(myr0)){
      pos<-which(dat$r0==myr0[i])
      mydat<-dat[pos,]
      newpos<-which(mydat$Ncomponents==1);
      mydat<-mydat[newpos,];
      mydat$G<-mydat$G+1;
      if(i==1){
         plot(log10(mydat$G),mydat$Gamma,xlab="log10(Generations)",ylab="Richness",col=mycolor,type="p",main=paste("Steady State Study\n (A = ",signif(A,2),", f = ",signif(f,2),")",sep=""),xlim=c(0,log10(maxgen)),ylim=c(0,400),pch=i)
         mycolor<-mycolor+1;
      }else{
         if(sum(mydat$Nedges)>0){
            points(log10(mydat$G),mydat$Gamma,col=mycolor,type="p",pch=i)
            mycolor<-mycolor+1;
         }
      }
   }
   if(mycolor <= 6)#if there are not so much values for the legend
   { 
        par(xpd="TRUE");
        legend("topleft",legend=signif(myr0,2),col=(1:length(myr0)),lwd=1,pch=(1:length(myr0)),title="mean radius");
   }
#   mydate<-format(Sys.time(), "%H_%M_%S_%Y");system(paste("sh sendmail.sh SteadyStateFig","_",mydate," charles.santana@gmail.com ",outputfilename,sep=""));
   dev.off();
}

args <- commandArgs(trailingOnly = TRUE)
inputfilename<-args[1];

dat<-read.csv(inputfilename,sep=" ")

myf<-unique(sort(dat$f));   

for(i in 1:length(myf)){
  mydat<-dat[which(dat$f==myf[i]),]
  myA<-mydat$A[1];
  plotSteadystate(paste(inputfilename,"_f_",myf[i],sep=""),mydat,myf[i],myA);
  plotConvergence(paste(inputfilename,"_f_",myf[i],sep=""),mydat,myf[i],myA);
  plotAreaRichness(paste(inputfilename,"_f_",myf[i],sep=""),mydat,myf[i],myA);
}
