library(igraphdata)
library(igraph)
data("yeast")
data("karate")
data("kite")

set.seed(10)


karate
E(karate)

###  -------------Creating noisy Datasets------------------------ 
remove.edges <- function(g, n) {
    noisy_k <- delete.edges(g, E(g)[sample(seq_along(E(g)), round(n*gsize(g)/100))])
    return(noisy_k)
}
###-------------------------------------------------------

#---------get deleted edges--------------------------------
diffrence.edges <- function(a,b){
  differen <- difference(E(a),intersection(E(b),E(a)))
  return(differen)
}
#------------------------------------------------------------


#-------------------------function to plot hrg graph-----------
fit.hierar_r_graph <- function(x){
  f_hrg<-fit_hrg(x)
  print(f_hrg, level=5)
  ihrg <- as.igraph(f_hrg)
  ihrg$layout <- layout.reingold.tilford
  x11()
  plot(ihrg, vertex.size=5, edge.arrow.size=0.4,edge.width=1,edge.arrow.width=0.0000002)
  return(f_hrg)
}
#--------------------------------------------------------------

#-----------------------function to plot dendogram-----------
dendrogram.plot <- function(y){
  x11()
  plot_dendrogram(y)
}
#-----------------------------------------------------------

#------------edge prediction-------------------------------
edges.prediction <- function(n,m){
  pred <- predict_edges(n,hrg=m,start=TRUE)
  x11()
  plot(pred$prob)
  return(pred)
}
#-------------------------------------------------------------

#-----------Add predicted edges------------------------------
add.predicted_edges <- function(r,pred){
  E(r)$color <- "gray"
  lay <- layout_nicely(r)
  g2 <- add_edges(r, t(pred$edges[1:5, ]), color = "red")
  
  x11()
  plot(g2)
}
#--------------------------------------------------------------

noise_karate <- remove.edges(karate,5)
E(karate-noise_karate)
diffrn <- diffrence.edges(karate,noise_karate)
noise_karate_hrg <- fit.hierar_r_graph(noise_karate)
dendrogram.plot(noise_karate_hrg)
karate_pre_edge <- edges.prediction(noise_karate,noise_karate_hrg)
karate_pre_edge$edges[1:10,]
add.predicted_edges(noise_karate,karate_pre_edge)

## Add four more predicted edges, color orange
#g3 <- add_edges(g2, t(pred$edges[6:9,]), color = "blue")
#x11()
#plot(g3, layout = lay)

###-------------------Question 1(b)--------------------------------------------------
data(kite)
E(kite)
noise_yeast <- remove.edges(kite,5)
y_diffrn <- diffrence.edges(kite,noise_yeast)
noise_yeast_hrg <- fit.hierar_r_graph(noise_yeast)
dendrogram.plot(noise_yeast_hrg)
yeast_pre_edge <- edges.prediction(noise_yeast,noise_yeast_hrg)
add.predicted_edges(noise_yeast,yeast_pre_edge)


####--------------------Question 1(c)-------------------------------
noise15_karate <- remove.edges(karate,15)   # 15% of the edges deleted karate
E(karate-noise15_karate)
diffrn <- diffrence.edges(karate,noise15_karate)
noise15_karate_hrg <- fit.hierar_r_graph(noise15_karate)
dendrogram.plot(noise15_karate_hrg)
kar_15_pre_edge <- edges.prediction(noise15_karate,noise15_karate_hrg)
add.predicted_edges(noise15_karate,kar_15_pre_edge)


noise40_karate <- remove.edges(karate,40)   # 40% of the edges deleted karate
diffrn <- diffrence.edges(karate,noise40_karate)
noise40_karate_hrg <- fit.hierar_r_graph(noise40_karate)
dendrogram.plot(noise40_karate_hrg)
kar_40_pre_edge <- edges.prediction(noise40_karate,noise40_karate_hrg)
add.predicted_edges(noise40_karate,kar_40_pre_edge)

noise15_kite <- remove.edges(kite,15)   # 15% of the edges deleted kite
diffrn <- diffrence.edges(kite,noise15_kite)
noise15_kite_hrg <- fit.hierar_r_graph(noise15_kite)
dendrogram.plot(noise15_kite_hrg)
kite_15_pre_edge <- edges.prediction(noise15_kite,noise15_kite_hrg)
add.predicted_edges(noise15_kite,kite_15_pre_edge)

noise40_kite <- remove.edges(kite,40)   # 40% of the edges deleted kite
diffrn <- diffrence.edges(kite,noise40_kite)
noise40_kite_hrg <- fit.hierar_r_graph(noise40_kite)
dendrogram.plot(noise40_kite_hrg)
kite_40_pre_edge <- edges.prediction(noise40_kite,noise40_kite_hrg)
add.predicted_edges(noise40_kite,kite_40_pre_edge)
