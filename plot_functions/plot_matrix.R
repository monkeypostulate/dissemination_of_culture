time_chosen<-1
agents_chosen<-history$time[[time_chosen]]$actors



similarity_mat<-list()
similarity_mat[[time_chosen]]<-matrix(NA,nrow= n_agents,ncol= n_agents)
thres<-.5

colnames(similarity_mat[[time_chosen]])<-1: n_agents
rownames(similarity_mat[[time_chosen]])<-1: n_agents

for(i in 1: n_agents){
	nei_nodes<-neighbors(graph_def, i)
	for(j in nei_nodes){
	temp_sim<-sum(agents_chosen[[i]]$attributes==agents_chosen[[j]]$attributes)/n_attr
	similarity_mat[[time_chosen]][i,j]<-temp_sim
	}
}

for(i in 1: n_agents){
	for(j in 1: n_agents){
similarity_mat[[time_chosen]][i,j]<-if(is.na(similarity_mat[[time_chosen]][i,j])) NA  else {if(similarity_mat[[time_chosen]][i,j]<.5 ) 0 else 1	
	}
}}


print(levelplot(similarity_mat[[time_chosen]], col.regions=c('red','blue')),colorkey=F)