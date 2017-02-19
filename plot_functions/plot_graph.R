
# ######################################
# Plot actors in the graph
# ######################################

# Function needs to call ggplot2 and igraph

######################
# Input
######################
# graph_def: graph to be plotted
# history: history of actors 
# time_c: time to plot
# threshold_c: threshold to determine similarity between groups and colour edges accordingly.
# layout_chosen: 'lattice' or 'small_world'

######################
# Output
#####################
# Returns a plot


######################
# Define Function
#####################

plot_graph_culture<-function(graph_def,history,time_c, threshold_c, layout_chosen, nodesize){

n_agents<-length(history[[time_c]]$actors)

labels_actors<-1: n_agents
for(i in 1: n_agents){
	i2<-if(i<n_agents)i+1 else n_agents
	for(j in i2: n_agents){
	b1<-history[[time_c]]$actors[[i]]$attributes
    b2<-history[[time_c]]$actors[[j]]$attributes
    if(sum(b1==b2)>= threshold_c){
    	labels_actors[j]<-labels_actors[i]
}
    
}}



n_edges<-length(E(graph_def))
edge_label<-rep('',n_edges)
edge_width<-rep(1,n_edges)

for(i in 1:n_edges){
temp_edge<-E(graph_def)[i]
temp_node1<-ends(graph_def, temp_edge)[1]
temp_node2<-ends(graph_def, temp_edge)[2]
if(labels_actors[temp_node1]==labels_actors[temp_node2])
edge_label[i]<-'gray90'
else
edge_label[i]<-'black'
edge_width[i]<-4
}



E(graph_def)$color<-edge_label
E(graph_def)$width <-edge_width

graph_def<-set_vertex_attr(graph_def, 'attr1', index = V(graph_def), labels_actors )

attrleft<-unique(V(graph_def)$attr1)
n_attrleft<-length(attrleft)
V(graph_def)$attr1<-V(graph_def)$attr1+500
attrleft<-attrleft +500
for(i in 1: n_attrleft){
vertex_color<-replace(V(graph_def)$attr1,V(graph_def)$attr1==attrleft[i],i)
V(graph_def)$attr1[V(graph_def)$attr1==attrleft[i]]<-i
}
n_communities<-length(unique(labels_actors))


# Small world lazyout

  l_sw<-matrix(0,nrow= n_agents,ncol=2)
for(i in 1: n_agents){
  l_sw[i,]<-c(cos(2*(i-1)*pi/n_agents),2*sin(2*(i-1)*pi/n_agents))


}

if(layout_chosen=='lattice'){
plot(graph_def, vertex.size= nodesize, vertex.label= V(graph_def)$attr1, vertex.label.color='black', edge.width=1.4, vertex.label.cex=.5, vertex.color= vertex_color, edge.width=E(graph_def)$width, main=paste0('Number of Cultures: ', n_communities),layout=layout_on_grid, xlab=paste0('Period: ', time_c))
}

if(layout_chosen=='small_world'){
plot(graph_def, vertex.size=nodesize, vertex.label= V(graph_def)$attr1, vertex.label.color='black', edge.width=1.4, vertex.label.cex=.5, vertex.color= vertex_color, edge.width=E(graph_def)$width, main=paste0('Number of Cultures: ', n_communities),layout= l_sw, xlab=paste0('Period: ', time_c))
}

  if(layout_chosen=='scale_free'){
  	
    degre_size<-degree(graph_def)*nodesize
   plot(graph_def, vertex.size=degre_size, vertex.label= V(graph_def)$attr1, vertex.label.color='black', edge.width=1.4, vertex.label.cex=.5, vertex.color= vertex_color, edge.width=E(graph_def)$width, main=paste0('Number of Cultures: ', n_communities), xlab=paste0('Period: ', time_c))
  }

}