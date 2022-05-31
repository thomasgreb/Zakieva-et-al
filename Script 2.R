library(ggplot2)
library(agricolae)
setwd("~/PhD/Results")
data <- read.table("evaluation-model_20210910.csv", header=TRUE, sep=",", dec=".")
data$gt_file <- as.factor(data$gt_file)
str(data)
data$model <- factor(data$model,levels=c("confocal_2D_unet_bce_dice_ds2x", "confocal_PNAS_2d", "hypocotyl_20210311", "Model1_20210816", "Model2_20210816", "full_20210818", "model_20210910"))
plot.design(data)
hist(data$adapted_rand)
hist(data$split)
hist(data$merge)

#Normality

datas <- read.table("evaluation-model_20210910b.csv", header=TRUE, sep=",", dec=".")
shapiro.test(datas$confocal_ovules_2D_A)
shapiro.test(datas$confocal_ovules_2D_S)
shapiro.test(datas$confocal_ovules_2D_M) 
shapiro.test(datas$confocal_PNAS_2d_A) #
shapiro.test(datas$confocal_PNAS_2d_S) #
shapiro.test(datas$confocal_PNAS_2d_M)
shapiro.test(datas$full_20210818_A) #
shapiro.test(datas$full_20210818_S)
shapiro.test(datas$full_20210818_M)
shapiro.test(datas$hypocotyl_20210311_A)
shapiro.test(datas$hypocotyl_20210311_S)
shapiro.test(datas$hypocotyl_20210311_M)
shapiro.test(datas$Model1_20210816_A)
shapiro.test(datas$Model1_20210816_S)
shapiro.test(datas$Model1_20210816_M)
shapiro.test(datas$Model2_20210816_A)
shapiro.test(datas$Model2_20210816_S)
shapiro.test(datas$Model2_20210816_M)
shapiro.test(datas$model_20210910_A)
shapiro.test(datas$model_20210910_S)
shapiro.test(datas$model_20210910_M)

#adapted_rand
plot_adapted_rand <- ggplot(data = data, aes(model, adapted_rand))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()
plot_adapted_rand

print(pairwise.t.test(data$adapted_rand, data$model, p.adj="none"))

tmodel_adapted_rand <- aov(adapted_rand~model, data = data)
ttable_adapted_rand <- HSD.test(tmodel_adapted_rand,"model")
print(ttable_adapted_rand$groups)

wilcox.test(datas$confocal_PNAS_2d_A, datas$model_20210910_A, exact = FALSE)
wilcox.test(datas$full_20210818_A, datas$model_20210910_A, exact = FALSE)

#VOI-Split
plot_split <- ggplot(data = data, aes(model, split))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()
plot_split

print(pairwise.t.test(data$split, data$model, p.adj="none"))

tmodel_split <- aov(split~model, data = data)
ttable_split <- HSD.test(tmodel_split,"model")
print(ttable_split$groups)

wilcox.test(datas$confocal_PNAS_2d_S, datas$model_20210910_S, exact = FALSE)

#VOI-Merge
plot_merge <- ggplot(data = data, aes(model, merge))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()
plot_merge

print(pairwise.t.test(data$merge, data$model, p.adj="none"))

tmodel_merge <- aov(merge~model, data = data)
ttable_merge <- HSD.test(tmodel_merge,"model")
print(ttable_merge$groups)
