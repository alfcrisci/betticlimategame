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
AMO_WT_SIB_clim_df=readRDS("AMO_WT_SIB_clim_df.rds")

AMO_WT_SIB_clim_df$Sib_L2=c(AMO_WT_SIB_clim_df$amo[3:length(AMO_WT_SIB_clim_df$Sib)],c(NA,NA))
AMO_WT_SIB_clim_df$Sib_L3=c(AMO_WT_SIB_clim_df$amo[4:length(AMO_WT_SIB_clim_df$Sib)],c(NA,NA,NA))


AMO_WT_SIB_clim_mon_split=split(AMO_WT_SIB_clim_df,AMO_WT_SIB_clim_df$Mese)


saveRDS(AMO_WT_SIB_clim_mon_split,"last_clim_mon_split.rds")




res_monthly_tg.rds
res_monthly_rr.rds
res_monthly_pp.rds
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