plotRichness<-function(inputfilename,dat){
   maxrich<-max(as.numeric(dat$Mr_Gamma)[-c(1,2,3)]);
   minrich<-min(as.numeric(dat$Mr_Gamma)[-c(1,2,3)]);
   maxr0<-max(as.numeric(dat$r0));
   outputfilename<-paste(inputfilename,".richness.png",sep="");
   png(outputfilename,width=1980,height=1280,res=300);
   plot(log10(dat$r0)[-c(1,2,3)],dat$Mr_Gamma[-c(1,2,3)],xlab="log10(r0)",ylab="Richness",col=1,type="b",main="Richness\n (A = 0, f = 0)",ylim=c(minrich,maxrich),pch=1)
   dev.off();
}

args <- commandArgs(trailingOnly = TRUE)
inputfilename<-args[1];

dat<-read.csv(inputfilename,sep=" ")

plotRichness(inputfilename,dat);
