uvm_counts<-read.table("/Users/wireless/Downloads/UVM_count_data.txt", stringsAsFactors=FALSE, header=TRUE, sep="\t")
uvm_clin<-read.table("/Users/wireless/Downloads/uvm_clinical_data.txt", stringsAsFactors=FALSE, header=TRUE, sep="\t")
#uvm_clin<-uvm_clin[-1,]
#let's take a quick first look at our datasets to get a sense of what we're working with!
dim(uvm_counts)
dim(uvm_clin)
str(uvm_counts)
str(uvm_clin)

#here's a quick example on how to subset your dataframe
#to get the first 10 rows and first two columns of the clinical data:

a<-uvm_clin[1:10,1:2]
uvm_clin[1:4,1:3]

a<-uvm_clin$gender
a
install.packages("ggplot2")
library("ggplot2")

#let's have a quick example of how to generate a scatter plot in R with ggplot2
ggplot(uvm_counts, aes(x=uvm_counts$DNMT3A, y=uvm_counts$DNMT3B)) + geom_point()

#here's a barplot representing eye color across the uvm cohort
ggplot(uvm_clin, aes(uvm_clin$eye_color))+geom_bar()
#histogram
ggplot(uvm_clin, aes(uvm_clin$age_at_initial_pathologic_diagnosis)) + geom_histogram()
#boxplot
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=uvm_clin$age_at_initial_pathologic_diagnosis))+geom_boxplot()
#violin plot
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=uvm_clin$age_at_initial_pathologic_diagnosis))+geom_violin()
#density plot
ggplot(uvm_clin, aes(uvm_clin$age_at_initial_pathologic_diagnosis)) + 
  geom_density()

#let's save this plot in pdf
pdf(file="eye_color.pdf")
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=uvm_clin$age_at_initial_pathologic_diagnosis))+geom_boxplot()
dev.off()

#let's make a dendrogram!

uvm_counts_t<-t(uvm_counts)

sub_UVM <- uvm_counts_t[rowMeans(uvm_counts_t) > 0,]
vars <- apply(sub_UVM,1,var)
select <- order(vars,decreasing = T)[1:100]
sub_UVM <- sub_UVM[select,]
#to cluster:
clust_UVM<-hclust(dist(t(sub_UVM)))
plot(as.dendrogram(clust_UVM))

#cladogram

install.packages("ape") 
library("ape")

plot(as.phylo(clust_UVM), type = "cladogram" , cex = 0.6, 
     label.offset = 0.5)
dev.off()
plot(as.phylo(clust_UVM), type = "fan")


library(survival)
install.packages("survminer")
library(survminer)
library(dplyr)

install.packages("survival")
library(survival)
                 install.packages("survminer")
                 library(survminer)
                 install.packages("dplyr")
                 library(dplyr)
                 

data(ovarian)
#make the treatments (info in rx) into factors, labeled A and B
ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
#make a survival object, with info on survival time (futime) and whether the patient's info is censored (fustat)
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
#now fit survival info to a curve, stratifying info based on which treatment the patient got
fit1 <- survfit(surv_object ~ rx, data = ovarian)
#now let's use ggsurvplot function to plot the survival curves!
ggsurvplot(fit1, data = ovarian, pval = TRUE)


uvm_clin$gender<-factor(uvm_clin$gender, levels = c("FEMALE", "MALE"), labels = c("F", "M"))
uvm_clin$NSD1_status<-"LOW"
for(i in 1:nrow(uvm_counts)){
  if(uvm_counts$NSD1[i]>=3000){
    uvm_clin$NSD1_status[i]<-"MEDIUM"
  }
  if(uvm_counts$NSD1[i]>=3158.5){
    uvm_clin$NSD1_status[i]<-"HIGH"
  }
}
uvm_clin$NSD1_status<-factor(uvm_clin$NSD1_status, levels = c("HIGH", "MEDIUM", "LOW"), labels = c("H", "M", "L"))

#if there was an futime column, it would replace it with
#days_to_death. In this case, given that futime doesn't yet 
#exist in uvm_clin, it is being generated and added as the
#last column of uvm_clin
uvm_clin$futime<-uvm_clin$days_to_death
#making a fustat column filled with 1s. 1 means that the
#patient is not censored
uvm_clin$fustat<-c(1)
for(i in 1:nrow(uvm_clin)){
  if(uvm_clin$futime[i]=="[Not Applicable]"){
    uvm_clin$futime[i]<-uvm_clin$days_to_last_followup[i]
    uvm_clin$fustat[i]<-0
  }
}
surv_object <- Surv(time = as.numeric(uvm_clin$futime), event = uvm_clin$fustat)


fit1<- survfit(surv_object ~ gender, data = uvm_clin)
fit1<- survfit(surv_object ~ NSD1_status, data = uvm_clin)
#now let's use ggsurvplot function to plot the survival curves!
ggsurvplot(fit1, data = ovarian, pval = TRUE)
ggsurvplot(fit1, data = uvm_clin, pval = TRUE)

# from the chord diagram demo here: https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html
install.packages("circlize")
library(circlize)
set.seed(999)
mat = matrix(sample(18, 18), 3, 6) 
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)
mat

df = data.frame(from = rep(rownames(mat), times = ncol(mat)),
                to = rep(colnames(mat), each = nrow(mat)),
                value = as.vector(mat),
                stringsAsFactors = FALSE)
df

chordDiagram(mat)

#considering color:
#here is an example of uniform color scheme:
ggplot(uvm_clin, aes(uvm_clin$eye_color))+geom_bar(fill="red")
ggplot(uvm_clin, aes(uvm_clin$eye_color))+geom_bar(color="red")
#what about for scatter plots? What should we do to get a scatterplot with red dots?
#fill or color?
ggplot(uvm_counts, aes(x=uvm_counts$DNMT3A, y=uvm_counts$DNMT3B)) + geom_point(fill="red")
ggplot(uvm_counts, aes(x=uvm_counts$DNMT3A, y=uvm_counts$DNMT3B)) + geom_point(color="red")

#let's revisit the eye colour plot and try to make it a bit more user friendly using the color scheme!
#here is the plain one, that doesn't have any changes made to the color scheme
ggplot(uvm_clin, aes(uvm_clin$eye_color))+geom_bar()
#is it easy to see at a glance what each of these means and compare?
#let's change the colors to reflect the eye color of the bar!
ggplot(uvm_clin, aes(uvm_clin$eye_color))+geom_bar(fill=c("gray", "black", "blue", "brown", "green"))
#okay, great! But the colours are not quite what I want them to be (brown looks a bit too red to me!)
#let's use the table of ggplot colours to improve this!
ggplot(uvm_clin, aes(uvm_clin$eye_color))+geom_bar(fill=c("gray", "black", "cornflowerblue", "sienna4", "forestgreen"))
#much better!
#Bonus: how do we only represent the known eye colours (omitting the NA and Unk)?
a<-c(which(uvm_clin$eye_color=="[Not Available]"), which(uvm_clin$eye_color=="[Unknown]"))
ggplot(uvm_clin[-a,], aes(uvm_clin[-a,]$eye_color))+geom_bar(fill=c("cornflowerblue", "sienna4", "forestgreen"))

#
ggplot(uvm_clin, aes(as.numeric(uvm_clin$age_at_initial_pathologic_diagnosis)))+geom_histogram()


#boxplot example
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=as.numeric(uvm_clin$age_at_initial_pathologic_diagnosis)))+geom_boxplot()
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=as.numeric(uvm_clin$age_at_initial_pathologic_diagnosis), fill=uvm_clin$eye_color))+geom_boxplot()+scale_fill_brewer()
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=as.numeric(uvm_clin$age_at_initial_pathologic_diagnosis)))+geom_boxplot()


#density plot

ggplot(uvm_clin, aes(as.numeric(uvm_clin$age_at_initial_pathologic_diagnosis)))+geom_density()


#bar plot, changing fill
ggplot(uvm_clin, aes(uvm_clin$eye_color))+geom_bar(fill="red")

#example of brewer use
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=as.numeric(uvm_clin$age_at_initial_pathologic_diagnosis)))+geom_boxplot(fill=uvm_clin$eye_color)+scale_fill_brewer()
ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width, color=iris$Species))+geom_point()+scale_color_brewer(palette="Paired")
#revisiting the pathologic stage
ggplot(uvm_clin, aes(pathologic_stage, fill=uvm_clin$pathologic_stage)) + geom_bar()+scale_fill_brewer(palette="Reds")

#viridis
install.packages("viridis")
library(viridis)
ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width, color=iris$Sepal.Length))+geom_point()+scale_color_viridis()

#sizes
ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width, color=iris$Sepal.Length))+geom_point(size=2)+scale_color_viridis()
ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width, color=iris$Sepal.Length))+geom_point(size=4)+scale_color_viridis()

#boxplot, editing titles and axes
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=as.numeric(uvm_clin$age_at_initial_pathologic_diagnosis)))+geom_boxplot()
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=as.numeric(uvm_clin$age_at_initial_pathologic_diagnosis)))+geom_boxplot(fill=c("gray", "black", "cornflowerblue", "sienna4", "forestgreen"))
#adding title
ggplot(uvm_clin, aes(x=uvm_clin$eye_color, y=as.numeric(uvm_clin$age_at_initial_pathologic_diagnosis)))+geom_boxplot(fill=c("gray", "black", "cornflowerblue", "sienna4", "forestgreen"))+ggtitle("Age at Initial Diagnosis, stratified by Eye Colour")


#cleaning up the example plot
ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width, color=iris$Species))+geom_point()+scale_color_brewer(palette="Paired")

#background
ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width, color=iris$Species))+geom_point()+scale_color_brewer(palette="Paired")+theme_minimal()

ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width, color=iris$Species))+geom_point()+scale_color_brewer(palette="Paired")+theme_light()

#getting rid of the legend
ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width, color=iris$Species))+geom_point()+scale_color_brewer(palette="Paired")+theme_light()+theme(legend.position = "none")

#making up some dataset
a<-c(1,2,1,3,2,1,1,2)
b<-c(10,13,12,10,14,13,15)
ab<-c(rep("A", 8), rep("B", 7))
abc<-c(a,b)
d<-cbind.data.frame(abc, ab)
ggplot(d, aes(x=ab,y=d$abc))+geom_boxplot()
ggplot(d, aes(x=ab,y=d$abc))+geom_violin()
ggplot(d, aes(x=ab,y=d$abc))+geom_boxplot()+geom_jitter()
ggplot(d, aes(x=ab,y=d$abc))+geom_jitter(position=position_jitter(0.1))
ggplot(d, aes(x=ab,y=d$abc))+geom_violin()+geom_jitter(position=position_jitter(0.1))


#facet grid demo:

a<-ggplot(uvm_clin, aes(x=uvm_clin$new_tumor_event_after_initial_treatment))+geom_bar()
a+facet_grid(cols = vars(eye_color))

#let's add some colors to make this a bit easier!
a<-ggplot(uvm_clin, aes(x=uvm_clin$new_tumor_event_after_initial_treatment, fill=uvm_clin$eye_color))+geom_bar()
a+facet_grid(cols = vars(eye_color))+ scale_fill_manual(values=c("Black", "grey", "cornflowerblue", "sienna4", "forestgreen"))
#linear regression
a<-ggplot(uvm_counts, aes(x=uvm_counts$DNMT3A, y=uvm_counts$DNMT3B)) + geom_point()+ stat_smooth(method = lm)
model <- lm(uvm_counts$DNMT3B ~ uvm_counts$DNMT3A, data = uvm_counts)
summary(model)
#correlation
cor(uvm_counts$DNMT3B, uvm_counts$DNMT3A)




#let's make a heatmap!

sub_UVM <- uvm_counts[rowMeans(uvm_counts) > 0,]
vars <- apply(sub_UVM,1,var)
select <- order(vars,decreasing = T)[1:80]
sub_UVM <- sub_UVM[select,]

clust_UVM<-hclust(dist(sub_UVM))
install.packages('pheatmap')
library("pheatmap")
pheatmap(sub_UVM)
install.packages("heatmaply")
library(heatmaply)
heatmaply(sub_UVM)

install.packages("plotly")
library(plotly)
a<-ggplot(uvm_counts, aes(x=uvm_counts$DNMT3A, y=uvm_counts$DNMT3B, color=rownames(uvm_counts))) + geom_point()+theme(legend.position = "none") + stat_smooth(method = lm)
ggplotly(a)

devtools::install_github("octaviamd/interactmapper")
library(interactmapper)

#example of use of interactmapper function, interact_multi
interact_multi(iris[,1:4], iris$Species, iris[,1:2], "UMAP", "viridis", "Species", c("Sepal Length", "Sepal Width"))
interact_multi(uvm_counts, as.factor(uvm_clin$new_tumor_event_after_initial_treatment), uvm_clin$eye_color, "UMAP", "inferno", "New Tumor Event", "Eye Color")


par(mfrow(2,2))  


uvm_clin<-uvm_clin[ order(match(uvm_clin[,1], rownames(uvm_counts))), ]
write.table(uvm_clin, "/Users/wireless/Downloads/uvm_clinical_data.txt", sep="\t")

