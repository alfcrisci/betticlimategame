#############################################################################################
# Check Libraries

# install.packages("modeest")
# install.packages("HMM")
# install.packages("markovchain")
# install.packages("its")
# install.packages("rgexf")
# install.packages("igraph")
# install.packages("Gmisc")
# install.packages("corrplot")
#############################################################################################
# Load libraries

library(lubridate)
library(zoo)
library(xts)
library(markovchain) 
library(its) 
library(modeest) 
library(rgexf)
library(igraph)
library(Gmisc)
library(corrplot)
library(classInt)
library(reshape)
library(reshape2)
#############################################################################################
# Setup working directory

setwd("D:/lav_betti_indices")

source("betti_utils.r")


#############################################################################################
# Data preparation weather type Domain 00 Cost733 - 

wt_8=read.table("data/date_wt_PCT8.txt",sep=" ")
names(wt_8)<-c("Data","WT")
wt_8$Data=as.Date(dmy(wt_8$Data))
wt_8$Mese=month(wt_8$Data)
wt_8$Year=year(wt_8$Data)
wt_8$WTs=paste0("PCT_",wt_8$WT)

######################################################################################################
# Run lengths analisys


WT.rle <- rle(wt_8$WT)

maxWT=tapply(WT.rle$lengths, WT.rle$values, max)
medianWT=tapply(WT.rle$lengths, WT.rle$values, median)
meanWT=tapply(WT.rle$lengths, WT.rle$values, mean)
mcPCT8<- markovchainFit(data=wt_8$WTs,name="Weather_type_PCT8",method="mle")$estimate
g_PCT8=as(mcPCT8,"igraph")

######################################################################################################


wt_monthly_f=as.matrix(t(table(wt_8$WTs,format(wt_8$Data,"%Y-%m-01"))))
WT_df=data.frame(Year=year(ymd(rownames(wt_monthly_f))),Mese=month(ymd(rownames(wt_monthly_f))),wt_monthly_f[,1:8])

######################################################################################################
# Amo load 

AMO<-read_AMO("data/AMO_latest.txt")
AMO_df=data.frame(Year=year(date_decimal(AMO$yr_frac)),Mese=month(date_decimal(AMO$yr_frac)),amo=AMO$AMO)

AMO_WT_df=merge(AMO_df,WT_df)

AMO_WT_df$amo_q <- as.factor(findInterval(AMO_WT_df$amo, classIntervals(AMO_WT_df$amo, n=5, style="quantile")$brks))



res_median=aggregate(.  ~ AMO_WT_df$amo_q +Mese, data = AMO_WT_df, FUN = median)[,4:13]
res_max=aggregate(.  ~ AMO_WT_df$amo_q +Mese, data = AMO_WT_df, FUN = max)[,4:13]
res_mode=aggregate(.  ~ AMO_WT_df$amo_q +Mese, data = AMO_WT_df, FUN = mode)[,4:13]
				
#############################################################################################
# Siberian high
 
sib_index=read.csv("data/sib_index.csv")
names(sib_index)<-c("Data","Sib")
sib_index$Data=as.Date(mdy(sib_index$Data))
sib_index$Mese=month(sib_index$Data)
sib_index$Year=year(sib_index$Data)

################################################################################################

tg_tosc=readLines("data/tg_tosc_fin.csv")
tg_tosc_df <- read.table(textConnection(tg_tosc), na.string="-9999",header=F, skip=0)
names(tg_tosc_df)<-c("Data","tg_tosc")
tg_tosc_df$Data=as.Date(ymd(tg_tosc_df$Data))
tg_tosc_df$Mese=month(tg_tosc_df$Data)
tg_tosc_df$Year=year(tg_tosc_df$Data)


rr_tosc=readLines("data/rr_tosc_fin.csv")
rr_tosc_df <- read.table(textConnection(rr_tosc), na.string="-9999",header=F, skip=0)
names(rr_tosc_df)<-c("Data","rr_tosc")
rr_tosc_df$Data=as.Date(ymd(rr_tosc_df$Data))
rr_tosc_df$Mese=month(rr_tosc_df$Data)
rr_tosc_df$Year=year(rr_tosc_df$Data)


pp_tosc=readLines("data/pp_tosc_fin.csv")
pp_tosc_df <- read.table(textConnection(pp_tosc), na.string="-9999",header=F, skip=0)
names(pp_tosc_df)<-c("Data","pp_tosc")
pp_tosc_df$Data=as.Date(ymd(pp_tosc_df$Data))
pp_tosc_df$Mese=month(pp_tosc_df$Data)
pp_tosc_df$Year=year(pp_tosc_df$Data)

################################################################################################

clim_tosc_df=merge(tg_tosc_df,rr_tosc_df)
clim_tosc_df=merge(clim_tosc_df,pp_tosc_df)
clim_tosc_WT_df=merge(clim_tosc_df,wt_8)


################################################################################################
# annual testing

res_totale=combn(as.character(unique(clim_tosc_WT_df$WTs)), 2, FUN = function(y) t.test(tg_tosc ~ WTs, clim_tosc_WT_df[clim_tosc_WT_df$WTs %in% y ,]), simplify = FALSE)

################################################################################################
# annual testing

clim_tosc_WT_split=split(clim_tosc_WT_df,clim_tosc_WT_df$Mese)
res_monthly_tg=as.list(1:12)
res_monthly_rr=as.list(1:12)
res_monthly_pp=as.list(1:12)

for ( i in seq(clim_tosc_WT_split)){ 
                                    if ( i==7) { clim_tosc_WT_split[[i]]=clim_tosc_WT_split[[i]][which(clim_tosc_WT_split[[i]]$WTs!="PCT_6"),]}
                                    if ( i==10) { clim_tosc_WT_split[[i]]=clim_tosc_WT_split[[i]][which(clim_tosc_WT_split[[i]]$WTs!="PCT_6"),]}
                                    
									
									res_monthly_tg[[i]]=combn(as.character(unique(clim_tosc_WT_split[[i]]$WTs)), 2, FUN = function(y) t.test(tg_tosc ~ WTs, clim_tosc_WT_split[[i]][clim_tosc_WT_split[[i]]$WTs %in% y ,]), simplify = FALSE)
                                    res_monthly_rr[[i]]=combn(as.character(unique(clim_tosc_WT_split[[i]]$WTs)), 2, FUN = function(y) t.test(rr_tosc ~ WTs, clim_tosc_WT_split[[i]][clim_tosc_WT_split[[i]]$WTs %in% y ,]), simplify = FALSE)
                                    res_monthly_pp[[i]]=combn(as.character(unique(clim_tosc_WT_split[[i]]$WTs)), 2, FUN = function(y) t.test(pp_tosc ~ WTs, clim_tosc_WT_split[[i]][clim_tosc_WT_split[[i]]$WTs %in% y ,]), simplify = FALSE)
                               
								   }

################################################################################################
saveRDS(res_monthly_tg,"res_monthly_tg.rds")
saveRDS(res_monthly_rr,"res_monthly_rr.rds")
saveRDS(res_monthly_pp,"res_monthly_pp.rds")
saveRDS(res_totale,"res_totale.rds")

################################################################################################
res_tg_wt=aggregate(tg_tosc ~ WTs+Mese, data = na.omit(clim_tosc_WT_df), FUN = mean)
res_tg_wt_table <- dcast(res_tg_wt, WTs ~ Mese, value.var="tg_tosc")

res_rr_wt=aggregate(rr_tosc ~ WTs+Mese, data = na.omit(clim_tosc_WT_df), FUN = mean)
res_rr_wt_table <- dcast(res_rr_wt, WTs ~ Mese, value.var="rr_tosc")

res_pp_wt=aggregate(pp_tosc ~ WTs+Mese, data = na.omit(clim_tosc_WT_df), FUN = mean)
res_pp_wt_table <- dcast(res_pp_wt, WTs ~ Mese, value.var="pp_tosc")




saveRDS(clim_tosc_WT_df,"clim_tosc_WT_df.rds")

################################################################################################

res_tg_month=aggregate(tg_tosc  ~ Mese+Year, data = na.omit(clim_tosc_WT_df), FUN = mean)

res_rr_month=aggregate(rr_tosc ~ Mese+Year, data = na.omit(clim_tosc_WT_df), FUN = mean)

res_pp_month=aggregate(pp_tosc ~ Mese+Year, data = na.omit(clim_tosc_WT_df), FUN = mean)

clim_tosc=merge(res_tg_month,res_rr_month)
clim_tosc=merge(clim_tosc,res_pp_month)


AMO_WT_df=merge(AMO_df,WT_df)
AMO_WT_SIB_df=merge(AMO_WT_df,sib_index)
AMO_WT_SIB_clim_df=merge(AMO_WT_SIB_df,clim_tosc)
AMO_WT_SIB_clim_df$Data=NULL

AMO_WT_SIB_clim_df$amo_L1=c(AMO_WT_SIB_clim_df$amo[2:length(AMO_WT_SIB_clim_df$amo)],NA)
AMO_WT_SIB_clim_df$amo_L2=c(AMO_WT_SIB_clim_df$amo[3:length(AMO_WT_SIB_clim_df$amo)],c(NA,NA))
AMO_WT_SIB_clim_df$amo_L3=c(AMO_WT_SIB_clim_df$amo[4:length(AMO_WT_SIB_clim_df$amo)],c(NA,NA,NA))
AMO_WT_SIB_clim_df$Sib_L1=c(AMO_WT_SIB_clim_df$amo[2:length(AMO_WT_SIB_clim_df$Sib)],NA)
AMO_WT_SIB_clim_df$Sib_L2=c(AMO_WT_SIB_clim_df$amo[3:length(AMO_WT_SIB_clim_df$Sib)],c(NA,NA))
AMO_WT_SIB_clim_df$Sib_L3=c(AMO_WT_SIB_clim_df$amo[4:length(AMO_WT_SIB_clim_df$Sib)],c(NA,NA,NA))

saveRDS(AMO_WT_SIB_clim_df,"AMO_WT_SIB_clim_df.rds")

AMO_WT_SIB_clim_mon_split=split(AMO_WT_SIB_clim_df,AMO_WT_SIB_clim_df$Mese)
saveRDS(AMO_WT_SIB_clim_mon_split,"AMO_WT_SIB_clim_mon_split.rds")

################################################################################################



for (i in 1:12) {

if ( i==7) { AMO_WT_SIB_clim_mon_split[[i]]$PCT_6=NULL
             M <- cor(na.omit(AMO_WT_SIB_clim_mon_split[[i]][,3:20]))
			 png(paste0("correlation_plot_",i,".png"),width = 1000, height = 1000, units = "px", pointsize = 12)
             corrplot.mixed(M)
             dev.off()
			 next
		   }
		   
if ( i==8) { AMO_WT_SIB_clim_mon_split[[i]]$PCT_6=NULL
             M <- cor(na.omit(AMO_WT_SIB_clim_mon_split[[i]][,3:20]))
			 png(paste0("correlation_plot_",i,".png"),width = 1000, height = 1000, units = "px", pointsize = 12)
             corrplot.mixed(M)
             dev.off()
			 next
		   }
		   
if ( i==9) { AMO_WT_SIB_clim_mon_split[[i]]$PCT_6=NULL
             M <- cor(na.omit(AMO_WT_SIB_clim_mon_split[[i]][,3:20]))
			 png(paste0("correlation_plot_",i,".png"),width = 1000, height = 1000, units = "px", pointsize = 12)
             corrplot.mixed(M)
             dev.off()
			 next
		   }

if ( i==10) { AMO_WT_SIB_clim_mon_split[[i]]$PCT_6=NULL
             M <- cor(na.omit(AMO_WT_SIB_clim_mon_split[[i]][,3:20]))
			 png(paste0("correlation_plot_",i,".png"),width = 1000, height = 1000, units = "px", pointsize = 12)
             corrplot.mixed(M)
             dev.off()
			 next
		   }
		   
M <- cor(na.omit(AMO_WT_SIB_clim_mon_split[[i]][,3:21]))

png(paste0("correlation_plot_",i,".png"),width = 1000, height = 1000, units = "px", pointsize = 12)
corrplot.mixed(M)
dev.off()
}

#############################################################################################
# Reference
#
# Generating a Markov chain vs. computing the transition matrix http://freakonometrics.hypotheses.org/6803
# http://web.mit.edu/~r/current/arch/i386_linux26/lib/R/library/IRanges/html/Rle-class.html
# http://stackoverflow.com/questions/16112162/graph-flow-chart-of-transition-from-states
# http://gforge.se/2013/06/visualizing-transitions-with-the-transitionplot-function/
# http://linkedscience.org/tools/sparql-package-for-r/sparql-package-for-r-gephi-movie-star-graph-visualization-tutorial/
# http://stackoverflow.com/questions/6289538/aggregate-a-dataframe-on-a-given-column-and-display-another-column

############################################################################################
# Supplementary code
# df <- read.table(text="Group   var1    var2    var3    var4    var5
# 1           3   5   7   3   7
# 1           3   7   5   9   6
# 1           5   2   6   7   6
# 1           9   5   7   0   8
# 1           2   4   5   7   8
# 1           2   3   1   6   4
# 2           4   2   7   6   5
# 2           0   8   3   7   5
# 2           1   2   3   5   9
# 2           1   5   3   8   0
# 2           2   6   9   0   7
# 2           3   6   7   8   8
# 2           10  6   3   8   0", header = TRUE)


# t(sapply(df[-1], function(x) unlist(t.test(x~df$Group)[c("estimate","p.value","statistic","conf.int")])))

# estimate.mean in group 1 estimate.mean in group 2   p.value statistic.t conf.int1 conf.int2
# var1                 4.000000                 3.000000 0.5635410   0.5955919 -2.696975  4.696975
# var2                 4.333333                 5.000000 0.5592911  -0.6022411 -3.104788  1.771454
# var3                 5.166667                 5.000000 0.9028444   0.1249164 -2.770103  3.103436
# var4                 5.333333                 6.000000 0.7067827  -0.3869530 -4.497927  3.164593
# var5                 6.500000                 4.857143 0.3053172   1.0925986 -


# http://stackoverflow.com/questions/19032228/r-use-of-t-test-function-in-data-frame-or-matrix4

# mydf <- data.frame(
  # group = rep(letters[1:4], each = 10),
  # result = c(1:10, 5:14, 11:20, 15:24)
# )
# mydf

# You can use combn to create the "pairs" of each group to use t.test on.

# combn(as.character(unique(mydf$group)), 2, 
      # FUN = function(y) t.test(result ~ group, 
                               # mydf[mydf$group %in% y ,]), 
      # simplify = FALSE)

# As for extracting separate vectors, I think that a list of vectors might be more convenient, for which you can use split:

# x <- split(mydf$result, mydf$group)

# share|improve this answer
	
# answered Sep 26 '13 at 15:45
# Ananda Mahto
# 75.5k864134
	
# add a comment
# up vote 0 down vote
	

# with(subset(df, group %in% c("a", "b")),
     # t.test(value ~ factor(group))