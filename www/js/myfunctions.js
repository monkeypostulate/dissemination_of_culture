  
  
  

  	 	       	 function runsimulation() {
      	 	  simulation=simulation+1;
        Shiny.onInputChange("simulation", simulation);    
                    } ;
                    
               	 function plotnet() {
      	 	  plotnetwork=plotnetwork+1;
        Shiny.onInputChange("plot_network", plotnetwork);    
                    } ;          
                    
                   	 function plotnumcult() {
      	 	  plotnumcults=plotnumcults+1;
        Shiny.onInputChange("plot_numcultures", plotnumcults);    
                    } ;   

                     function choosemodel() {
    var x = document.getElementById("selectmodel").value;
    var z = document.getElementById("swparameter");
    var z2 = document.getElementById("sfreeparameter");
    var z3 = document.getElementById("latticeparameter");
     
    if (x === 'Small World') {
        z.style.display = 'block';
    } else {
        z.style.display = 'none';}

    if (x === 'Scale Free') {
        z2.style.display = 'block';
    } else {
        z2.style.display = 'none';}
        
            if (x === 'Lattice') {
        z3.style.display = 'block';
    } else {
        z3.style.display = 'none';} 
        } ;    

 	 function runregressions() {
     	 	 input3=1;
      	 	 Shiny.onInputChange("simulation2", input3);  
  input3=null;
                    };
                    
                    function displayinfo2() {
    var x = document.getElementById('info2');
    if (x.style.display === 'none') {
        x.style.display = 'block';
    } else {
        x.style.display = 'none';
    }
};


