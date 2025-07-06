exp.design.table<-function(dir.exp.inputs,Limits.File,sample.size,Policies.File,Policy.Switch,Climate.File,Climate.Switch)
{

Limits<-read.csv(paste(dir.exp.inputs,Limits.File,sep="")) #type1->constant,type2->factorial, type3->latinhypercube
Limits$Description<-NULL
Limits$Comments<-NULL
Policies<-read.csv(paste(dir.exp.inputs,Policies.File,sep=""))
Climate<-read.csv(paste(dir.exp.inputs,Climate.File,sep=""))

#Separate variables into the three different strings
Constants<-Limits[Limits$Type==1,"VarName"]
Factorial<-Limits[Limits$Type==2,"VarName"]
LatinHyperCube<-Limits[Limits$Type==3,"VarName"]

#Create factorial part of the experimental design

Create.factorial.exp<-function(Limits)
{
  if (length(Factorial)==0)
   {
     c()
   }else{
     Limits.Factorial<-Limits[Limits$Type==2,]
     row.names(Limits.Factorial)<-Limits.Factorial$VarName
     Limits.Factorial<-Limits.Factorial[,c("Min","Max","Gradient")]
     Limits.Factorial$Gradient<-ifelse(Limits.Factorial$Gradient>1,Limits.Factorial$Gradient,2) #this allows the instruction below to use gradient=1 as input and work ok
     l<-lapply(data.frame(t(Limits.Factorial)),function (x) {seq(x[1],x[2],by=(x[2]-x[1])/(x[3]-1))}) #if in doubt about the function look at the form of data.frame(t(Limits.Factorial))
     return(expand.grid(l))
   }
}
Exp.design.factorial<-Create.factorial.exp(Limits)

#Create Latin hyper cube part of experimental design

Create.LHC.exp<-function(Limits)
{
  if (length(LatinHyperCube)==0)
  {
   c()
  }else{
   library(lhs)
   sample<-randomLHS(sample.size,length(LatinHyperCube))
   Limits.LHC<-Limits[Limits$Type==3,]
   row.names(Limits.LHC)<-Limits.LHC$VarName
   Limits.LHC<-Limits.LHC[,c("Min","Max")]
   Limits.LHC$Row.Number<-c(1:nrow(Limits.LHC))
   lhc.p1<-as.data.frame(apply(Limits.LHC,1,function (x){sample[,x[3]]*(x[2]-x[1])+x[1]}))
   lhc.p1$LHC.Type<-"Sample"
   #add extreme values to sample table
   lhc.p2<-Limits[Limits$Type==3,]
   row.names(lhc.p2)<-lhc.p2$VarName
   lhc.p2<-lhc.p2[,c("Min","Max","Gradient")]
   lhc.p2$Gradient<-2
   lhc.p2$Gradient<-ifelse(lhc.p2$Gradient>1,lhc.p2$Gradient,2) #this allows the instruction below to use gradient=1 as input and work ok
   lhc.p2<-lapply(data.frame(t(lhc.p2)),function (x) {seq(x[1],x[2],by=(x[2]-x[1])/(x[3]-1))}) #if in doubt about the function look at the form of data.frame(t(lhc.p2))
   lhc.p2<-expand.grid(lhc.p2)
   lhc.p2$LHC.Type<-"Extremes"
   #rbind both tables
   lhc<-rbind(lhc.p1,lhc.p2)
   return(lhc)
  }
}
Exp.design.LHC<-Create.LHC.exp(Limits)

#Create constant part of the experimental design
Create.Constant.exp<-function(Limits)
{
   if (length(Constants)==0)
  {
    c()
  }else{
   Limits.Constant<-Limits[Limits$Type==1,]
   varnames<-Limits.Constant$VarName
   Limits.Constant<-data.frame(t(Limits.Constant[,"Cte"]))
   colnames(Limits.Constant)<-varnames
   return(Limits.Constant)
  }
}
Exp.design.Constant<-Create.Constant.exp(Limits)

#Join the three experimental design tables to create total number of futures
 Exp.design<-if(length(Factorial)==0 && length(LatinHyperCube)==0){Exp.design.Constant}else{if(length(Factorial)==0){Exp.design.LHC}else{if(length(LatinHyperCube)==0){Exp.design.factorial}else{merge(Exp.design.LHC,Exp.design.factorial)}}}
 Exp.design<-if(length(Factorial)==0 && length(LatinHyperCube)==0){Exp.design}else{if(length(Constants)==0){Exp.design}else{merge(Exp.design,Exp.design.Constant)}}

#Finally add reference Future.ID
 #Exp.design.reference<-Limits
 #varnames<-Exp.design.reference$VarName
 #Exp.design.reference<-data.frame(t(Exp.design.reference[,"Cte"]))
 #colnames(Exp.design.reference)<-varnames

#rbind with rest of experiment
# Exp.design<-rbind(Exp.design,Exp.design.reference)

#Join with climate scenarios to create total number of futures
Climate<-if(Climate.Switch==FALSE) {Climate[1,]}else{Climate}
Exp.design<-merge(Exp.design,Climate)
Exp.design$Future.ID<-as.numeric(row.names(Exp.design))

#Join total number of futures with policies to create total number of runs
Policies<-if(Policy.Switch==FALSE) {Policies[1,]}else{Policies}
Exp.design<-merge(Exp.design,Policies)
Exp.design$Run.ID<-as.numeric(row.names(Exp.design))
return(Exp.design)
}
