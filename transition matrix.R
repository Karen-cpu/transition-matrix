install.packages("igraph")

library(igraph)



Gamma = rbind(c(0.000,0.000, 1.00, 0.000),
              c(0.000, 0.433, 0.21, 0.357),
              c(0.371,0.126, 0.00,  0.503),
              c(0.814, 0.00, 0.00, 0.186)
              )

draw_TD = function(Gamma)
{  
  u = dim(Gamma)[1]
  Gamma = round(Gamma,3)
  colnames(Gamma) =paste0('State',1:u)
  rownames(Gamma) =paste0('State',1:u)
  
  t = seq(1, 0, length = u+1)+0.25
  x = 1*cos(2*pi*t)[1:u]
  y = 1*sin(2*pi*t)[1:u]
  X = cbind(x=x, y=y)
  
  net = graph.adjacency(Gamma, mode = 'directed', weighted = TRUE, diag = TRUE, add.rownames = TRUE)
  
  plot.igraph(net,
              layout = X,
              vertex.color='grey90',
              vertex.label = V(net)$name,
              vertex.size = 50,
              edge.curved = 0.3,
              edge.label = E(net)$weight,
              edge.color = 'black',
              margin = 0.05,
              edge.loop.angle = -pi/4)
}

draw_TD(Gamma)









