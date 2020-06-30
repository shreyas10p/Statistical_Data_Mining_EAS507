library(kohonen)

SwissBankNotes[1:100,]    # Data Set in Global Environment

x11()
#box plot
boxplot(SwissBankNotes)

#principal components of real fake and combined currency
pc_real <- prcomp(SwissBankNotes[1:100,], center = TRUE, scale = TRUE)
pc_fake <- prcomp(SwissBankNotes[100:200,], center = TRUE, scale = TRUE)
pc_combined <- prcomp(SwissBankNotes, center = TRUE, scale = TRUE)

#plots
x11()
plot(pc_real)

x11()
plot(pc_fake)

x11()
plot(pc_combined)

summary(pc_real)
summary(pc_fake)
summary(pc_combined)

PCAcolors <- c("#66c2a5","#fc8d62","#8da0cb")

names(pc_combined)
PCA_scores = pc_combined$x
PCA_loading = pc_combined$rotation

#scores plot with variables
x11()
plot(PCA_scores[1:100,1:2],  # x and y data
     pch=21,           # point shape
     col="#66c2a5",    # point border color
     bg="#fc8d62",     # point color
     cex=1.5,          # point size
     main="Scores",     # title of plot
     ylim=range(c(-3,3))
     )
par(new=TRUE)
plot(PCA_scores[100:200,1:2],  # x and y data
     pch=21,           # point shape
     col="#037894",    # point border color
     bg="#9216ef",     # point color
     cex=1.5,          # point size
     main="Scores",     # title of plot
     ylim=range(c(-3,2)),axes = FALSE, xlab = "", ylab = ""
     )

par(new=TRUE)
plot(PCA_loading[,1:2],   # x and y data
     pch=21,              # point shape
     bg="black",          # point color
     cex=1,               # point size
     ylim=range(c(-3,2)),axes = FALSE, xlab = "", ylab = ""
     )
text(PCA_loading[,1:2],             # sets position of labels
     labels=rownames(PCA_loading)   # print labels
)
legend("topright",                                # position of legend
       legend=c('first 100 notes','100 to 200 notes','Loadings'),                       # legend display
       pch=21,                                    # point shape
       pt.bg=c("#fc8d62","#9216ef","black"),    # point colors
       pt.cex=1.5,                                # point size
       col = c("#66c2a5","#037894","black")    # point border color
       
)

#biplot
x11()
biplot(pc_combined,col=c("blue","black"))
