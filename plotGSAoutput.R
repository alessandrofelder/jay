library(ggplot2)

cbbPalette <-
  c("#E69F00",
    "#56B4E9",
    "#009E73",
    "#CC79A7") #subset of colour-blind friendly palette from cookbook-r.com. Order is ocre, light-blue, dark-green, dark-pink. Thanks!

setwd("/home/alessandro/Documents/data/Magpie/")

result.variables <-  c("mean.deflection","maximum.deflection","buckling.factor","length","mean.deflection.at.mid.point","deflection.at.mid.point","normalized.mean.deflection","normalized.maximum.deflection");

result.variable.units <- c("[mm]","[mm]","","[m]","[mm]","[mm]","","")
naiveResults <- read.csv("naive-space-frame-results.csv")
naiveResults[,"length"] <- naiveResults[,"length"]/1000;#convert to meters
naiveResults$normalized.maximum.deflection <- naiveResults[,"maximum.deflection"]/naiveResults[,"length"]
naiveResults$normalized.mean.deflection <- naiveResults[,"mean.deflection"]/naiveResults[,"length"]
varying.variables <- c("anisotropy.ratio","size.ratio","align.weight")

for(i in 1:length(varying.variables))
{
  csv.files <- c("varyAnisotropy.csv","varySize.csv","varyAlign.csv")
  GSAresults <- read.csv(csv.files[i])
  GSAresults[,"length"] <- GSAresults[,"length"]/1000#convert to meters
  
  GSAresults$normalized.maximum.deflection <- GSAresults[,"maximum.deflection"]/GSAresults[,"length"]
  GSAresults$normalized.mean.deflection <- GSAresults[,"mean.deflection"]/GSAresults[,"length"]
  
  covarIndex1 <- ((i+1) %% length(varying.variables))
  covarIndex2 <- ((i+2) %% length(varying.variables))
  if(covarIndex1==0) covarIndex1<-length(varying.variables)
  if(covarIndex2==0) covarIndex2<-length(varying.variables)
  
  covar1Max <- max(GSAresults[,varying.variables[covarIndex1]])
  covar2Max <- max(GSAresults[,varying.variables[covarIndex2]])
  covar1Min <- min(GSAresults[,varying.variables[covarIndex1]])
  covar2Min <- min(GSAresults[,varying.variables[covarIndex2]])
  
  study1 <- GSAresults[((GSAresults[,varying.variables[covarIndex1]]==covar1Max)&(GSAresults[,varying.variables[covarIndex2]]==covar2Max)),]
  study2 <- GSAresults[((GSAresults[,varying.variables[covarIndex1]]==covar1Max)&(GSAresults[,varying.variables[covarIndex2]]==covar2Min)),]
  study3 <- GSAresults[((GSAresults[,varying.variables[covarIndex1]]==covar1Min)&(GSAresults[,varying.variables[covarIndex2]]==covar2Max)),]
  study4 <- GSAresults[((GSAresults[,varying.variables[covarIndex1]]==covar1Min)&(GSAresults[,varying.variables[covarIndex2]]==covar2Min)),]
  
  for(j in 1:length(result.variables))
  {
    GSAplot <- ggplot() + xlab(gsub("\\."," ",varying.variables[i]))+ylab(paste0(gsub("\\."," ",result.variables[j])," ",result.variable.units[j]))
    GSAplot <-
      GSAplot + theme_bw(base_size = 20) + theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key.size = unit(3, 'lines'))+guides(color=guide_legend(override.aes=list(size=4.5)))
    
    studylabels <- c(
      paste0(gsub("\\."," ",varying.variables[covarIndex1])," = ",covar1Max,"\n",gsub("\\."," ",varying.variables[covarIndex2])," = ",covar2Max),
      paste0(gsub("\\."," ",varying.variables[covarIndex1])," = ",covar1Max,"\n",gsub("\\."," ",varying.variables[covarIndex2])," = ",covar2Min),
      paste0(gsub("\\."," ",varying.variables[covarIndex1])," = ",covar1Min,"\n",gsub("\\."," ",varying.variables[covarIndex2])," = ",covar2Max),
      paste0(gsub("\\."," ",varying.variables[covarIndex1])," = ",covar1Min,"\n",gsub("\\."," ",varying.variables[covarIndex2])," = ",covar2Min))
    
    GSAplot <- GSAplot + geom_point(data = study1, aes(x = study1[,varying.variables[i]], y=study1[,result.variables[j]],color=studylabels[1],size=3))
    GSAplot <- GSAplot + geom_point(data = study2, aes(x = study2[,varying.variables[i]], y=study2[,result.variables[j]],color=studylabels[2],size=3))
    GSAplot <- GSAplot + geom_point(data = study3, aes(x = study3[,varying.variables[i]], y=study3[,result.variables[j]],color=studylabels[3],size=3))
    GSAplot <- GSAplot + geom_point(data = study4, aes(x = study4[,varying.variables[i]], y=study4[,result.variables[j]],color=studylabels[4],size=3))
    
    
    if(result.variables[j]!="length")
    {
      GSAplot <- GSAplot + theme(legend.position="none")
    }
    
    if(result.variables[j]!="length" & result.variables[j]!="buckling.factor")
    {
      GSAplot <- GSAplot + theme(axis.title.x=element_blank())
    }
    
    GSAplot <- GSAplot + geom_hline(yintercept=naiveResults[1,result.variables[j]])
    GSAplot <- GSAplot + geom_hline(yintercept=naiveResults[2,result.variables[j]],linetype = 2)# 101 metres, dashed line
    GSAplot <- GSAplot + geom_hline(yintercept=naiveResults[3,result.variables[j]],linetype = 3)# 123 metres, dotted line
    if(varying.variables[i]!="align.weight")
    {
      GSAplot <- GSAplot + scale_x_reverse()
    }
    GSAplot <- GSAplot + guides(size=FALSE)
    GSAplot <- GSAplot + scale_colour_manual(name="combination", values = cbbPalette)
    
    ggsave(
      filename = paste0(
        "plots/magpie-result-",
        gsub("\\.","-",varying.variables[i]),"-",
        gsub("\\.","-",result.variables[j]),
        ".png"),
        width = 24,
        height = 12,
        units = "cm"
      )
  }
}












