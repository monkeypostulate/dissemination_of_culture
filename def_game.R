
# ######################################
# Define Game
# ######################################

# Function needs to call function
# Def agents_culture

######################
# Input
######################
# Time_T: Number of Periods the game is played
# n_attr: Number of attributes
# n_oj: number of triats per attribute
# def_graph: graph in which actors play

######################
# Output
#####################
# Returns the history of agents playing the dissemination game in a graph


######################
# Define Function
#####################
culture_game<-function(T_time,n_attr,n_obje, graph_def)
{

  	
agents_def<-list()
# Number of agents is equal to the number of nodes in the network.
n_agents<-length(V(graph_def))
for(i in 1: n_agents){
agents_def[[i]]<-agents_culture(i,sample(1:n_obje,n_attr, replace=T))
}

graph_def<-set_vertex_attr(graph_def,'id',index=V(graph_def),1: n_agents)


# ##################################
# Track history
# ##################################
history<-list()
history$time<-list()
history$time$actors<-list()

  withProgress(message = 'Simulation running', value = 0, min=0, max=1, {
# ##################################
# Simulations
# ##################################
for(time_t  in 1:T_time){
#
history$time[[time_t]]<-list()
history$time[[time_t]]$actors<-agents_def
#Choose 
select_node<-sample(1: n_agents,1)
neig<-neighbors(graph_def, select_node)
neig<-as.numeric(neig)
if(length(neig)!=0){
select_node2<-sample(neig,1, replace=F)


# Probability to share 
com_attr<-agents_def[[select_node]]$attributes==agents_def[[select_node2]]$attributes
p_sh<-sum(com_attr)/n_attr

if(p_sh<1 & p_sh>0){
	k<-rep(0, n_attr)
	for(i in 1:n_attr){
		if(!com_attr[i]) k[i]<-i
	}
k<-k[k!=0]
k<-sample(k,1)
agents_def[[select_node]]$attributes[k]<-agents_def[[select_node2]]$attributes[k]
}
}

percentage<-round(time_t/T_time,1)
incProgress(amount=percentage, detail = paste("Progress", percentage))
 
         }
         })
        
return(history$time)
}

