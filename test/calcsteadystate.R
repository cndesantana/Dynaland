calcConvergence<-function(subset){
   conv = sd(subset)/mean(subset)
   cat(paste(sd(subset),mean(subset),conv,sep=" "),sep="\n",file="./log.dat",append=TRUE);
   return(conv)
}

plotConvergence<-function(inputfilename,dat,f){
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
         cat(paste("plotting plot...",i,sep=""),sep="\n");
         plot(1:nwindows,as.numeric(myconvergence),xlab="Generations",ylab="sd/mean",main="Convergence",type="b",col=mycolor,pch=i,log="x",ylim=c(0,0.3))
         mycolor<-mycolor+1;
      }else{
         if(sum(mydat$Nedges)>0){
                 cat(paste("plotting points...",i,sep=""),sep="\n");
                 points(1:nwindows,as.numeric(myconvergence),col=mycolor,type="b",pch=i)
   	 	 points(log10(mydat$G),mydat$Gamma,col=mycolor,type="b",pch=i)
                 mycolor<-mycolor+1;
   	 }
      }
   }
   par(xpd="TRUE");
   if(mycolor <= 6)#if there are not so much values for the legend
   { 
      legend("topright",legend=signif(myr0[1:(mycolor-1)],2),col=(1:length(myr0[1:(mycolor-1)])),lwd=1,pch=(1:length(myr0[1:(mycolor-1)])),title="mean radius");
   }
   else{
      nelements<-length(myr0);
      poselements<-seq(from=1,to=nelements,length.out=6);
      legend("topright",legend=signif(myr0[poselements],2),col=poselements,lwd=1,pch=poselements,title="mean radius");
   }
   dev.off();
#   mydate<-format(Sys.time(), "%H_%M_%S_%Y");system(paste("sh sendmail.sh ConvFigure","_",mydate," charles.santana@gmail.com ",outputfilename,sep=""));
}

plotSteadystate<-function(inputfilename,dat,f){
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
            plot(log10(mydat$G),mydat$Gamma,xlab="log10(Generations)",ylab="Richness",col=mycolor,type="b",main=paste("Steady State Study\n (A = 0, f = ",signif(f,2),")",sep=""),xlim=c(0,log10(maxgen)),ylim=c(0,400),pch=i)
            mycolor<-mycolor+1;
   	}else{
            if(sum(mydat$Nedges)>0){
   	    	points(log10(mydat$G),mydat$Gamma,col=mycolor,type="b",pch=i)
                    mycolor<-mycolor+1;
   	    }
   	}
   }
   abline(h=250,col="black",lty="dashed",lwd=1);
#   par(xpd="TRUE");
#   if(mycolor <= 6)#if there are not so much values for the legend
#   { 
#      legend("topleft",legend=signif(myr0[1:(mycolor-1)],2),col=(1:length(myr0[1:(mycolor-1)])),lwd=1,pch=(1:length(myr0[1:(mycolor-1)])),title="mean radius");
#   }
#   else{
#      nelements<-length(myr0);
#      poselements<-seq(from=1,to=nelements,length.out=6);
#      legend("topleft",legend=signif(myr0[poselements],2),col=poselements,lwd=1,pch=poselements,title="mean radius");
#   }
#   mydate<-format(Sys.time(), "%H_%M_%S_%Y");system(paste("sh sendmail.sh SteadyStateFig","_",mydate," charles.santana@gmail.com ",outputfilename,sep=""));
   dev.off();
}

args <- commandArgs(trailingOnly = TRUE)
inputfilename<-args[1];

dat<-read.csv(inputfilename,sep=" ")

myf<-unique(sort(dat$f));   

for(i in 1:length(myf)){
  mydat<-dat[which(dat$f==myf[i]),]
  plotSteadystate(paste(inputfilename,"_f_",myf[i],sep=""),mydat,myf[i]);
  plotConvergence(paste(inputfilename,"_f_",myf[i],sep=""),mydat,myf[i]);
}
