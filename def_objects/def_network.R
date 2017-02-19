
# ######################################
# Define network
# ######################################

# Function needs to call ggplot2 and igraph

######################
# Input
######################
# N: number of nodes for erdos-renyi and small world. For grid Number of nodes equall N^dimension
# p: probability: works for erdos-reny and small world 
# neig: neighbourhod: works for grid and small world
# dimension: Works for grid and small world
# type: 'random'-erdos-renyi, 'grid','small_world'

######################
# Output
#####################
# Returns a network


######################
# Define Function
####################

select_network<-function(N,p, neig, dimension, type, power, m){
	
	
	if(type=='random'){
 graph_def<-erdos.renyi.game(N,p)		
		return(graph_def)
}
	if(type=='lattice'){
		if(dimension==1){
	graph_def <- make_lattice( N, dim= dimension)
			return(graph_def)}
		if(dimension==2){
	graph_def <- make_lattice( c(N,N), dim= dimension)
		return(graph_def)}
		else warning('Dimension has to be equal 1 or 2')

	}
	if(type=='small_world'){
	graph_def <-watts.strogatz.game(dim= 1,size=N,nei= neig,p=p)
		return(graph_def)

	}
	
	if(type=='scale_free'){
	barabasi.game(N, power =power , m =m , directed = F)
	 }
	
	else{
		warning('Please choose:random, latttice, small_world or scale_free')
	}
	
	
}