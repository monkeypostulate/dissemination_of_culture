
# ######################################
# Plot the trend of the number of cultures over time
# ######################################

# Function needs to call ggplot2

######################
# Input
######################
# threshold_c: threshold: choose similarity between actors. Choose threshold_c=n_obj
# history: actors over time
# n_attr: number of attributes
# n_obje: number of objects

######################
# Output
#####################
# Returns a plot


######################
# Define Function
######################
plot_number_cultures<-function(threshold_c, history, n_attr, n_obje){

time_t<-length(history)
different_c<-rep(0, time_t)
labels_actors<-list()

n_agents<-length(history[[1]]$actors)

  withProgress(message = 'Simulation running', value = 0, min=0, max=1, {
for(time_c in 1:time_t){
labels_actors[[time_c]]<-1: n_agents
time_plot<-1: time_t		
# if(time_c %%10==1){
for(i in 1: n_agents){
	i2<-if(i<n_agents)i+1 else n_agents
	for(j in i2: n_agents){
	b1<-history[[time_c]]$actors[[i]]$attributes
    b2<-history[[time_c]]$actors[[j]]$attributes
    if(sum(b1==b2)>= threshold_c){
    	labels_actors[[time_c]][j]<-labels_actors[[time_c]][i]
    }
}}
different_c[time_c]<-length(unique(labels_actors[[time_c]]))

percentage<-round(time_c/time_t,1)
incProgress(amount=percentage, detail = paste("Progress", percentage))

}
})

time_plot<-time_plot[different_c!=0]
different_c <-different_c[different_c!=0]

data_plotted<-matrix(0,nrow=length(time_plot), ncol=2)
data_plotted[,1]<-time_plot
data_plotted[,2]<-different_c
data_plotted<-data.frame(data_plotted)
names(data_plotted)<-c('time','n_com')


max_communities<-max(data_plotted[,'n_com'])  
min_communities<-min(data_plotted[,'n_com']) 
ggplot(data_plotted)+geom_point(aes(x= time,y=n_com), colour='red')+geom_line(aes(x= time,y=n_com), colour='red')+ylim(0, max_communities)+ggtitle(paste0('Number of attributes: ',n_attr,'. Number of Objects: ',n_obje))+ylab('Number of Cultures')+xlab('Time')+geom_hline(yintercept= min_communities)+theme(   axis.text = element_text(size = 14),
    legend.key = element_rect(fill = "gray80"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, face="bold"))+
    annotate("text", x = max_communities, y = min_communities+2, label = min_communities, size=6)
}