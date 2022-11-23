#df <- read.table("../../20190105/data/IBRL/PMFres/08.RMSEhard.csv",sep=";", dec=",")
df <- read.table("../data/IBRL/PMFres/RMSEhard.csv",sep=",", dec=".")

library("reshape2")
library("ggplot2")
names(df)=1:17
df2 = melt(df) 

names(df2)[1] = "cluster"
df2$cluster <- as.factor(df2$cluster)

dev.off()
svg("boxplotPMFhard.svg",width=14,height=7)
bp <- ggplot(df2, aes(x=cluster, y=value, fill=cluster)) + 
  geom_boxplot(notch=T)+
  labs(title="",x="# clusters", y = "RMSE")

bp + theme_classic(base_size = 18)
dev.off()

dev.off()
svg("boxplotPMFhard-iii.svg",width=14,height=7)
bp <- ggplot(df2, aes(x=cluster, y=value, fill=cluster)) + 
  geom_boxplot(notch=T)+
  labs(title="",x="# clusters", y = "RMSE")

bp + theme_classic(base_size = 32)
dev.off()


dev.off()
svg("boxplotPMFhard.svg",width=14,height=7)
bp <- ggplot(df2, aes(x=cluster, y=value, fill=cluster)) + 
  geom_boxplot(notch=T)+
  labs(title="",x="# clusters", y = "RMSE")

bp + theme_classic(base_size = 32)
dev.off()
