
HTMLWidgets.widget({
  name: "alluvial_as_plotly",
  type: "output",

  initialize: function(el, width, height) {
    return {};
  },

  resize: function(el, width, height, instance) {
    if (instance.autosize) {
      var width = instance.width || width;
      var height = instance.height || height;
      Plotly.relayout(el.id, {width: width, height: height});
    }
  },  
  
  renderValue: function(el, x, instance) {
    
    var data = [];
    var layout = [];
    var myPlot = document.getElementById(el.id);
    
    for( var key of Object.keys(x.traces)){
      data.push( x.traces[key] );
    }
    
      
    layout = x.layout;

    Plotly.plot(el.id, data, x.layout);
    
    myPlot.on('plotly_hover', function(data){
      
      var curve = data.points[0].curveNumber;
      
      if(  curve == 0 ){
      
        for ( var i_data in data.points){
        
          var point = data.points[i_data].pointNumber;
      
          for(var i in x.map_curve[point] ){
            
            if( x.map_type[point][i] == 'scatter'){
            
              Plotly.restyle(el.id, {fillcolor : x.parcats_cols[point]
                                      // changing of the marker color takes too long
                                      //, line : { color : x.parcats_cols[point] }
                                      //, marker : { color : x.parcats_cols[point] }
                                      }
                             , parseInt(x.map_curve[point][i]) );
            }
            
            if( x.map_type[point][i] == 'bar'){
            
              Plotly.restyle(el.id, { marker : { color : x.parcats_cols[point] }
                                      }
                             , parseInt(x.map_curve[point][i]) );
            }
            
          }
        
        }
      }

    });
    
    myPlot.on('plotly_unhover', function(data){
      
      var curve = data.points[0].curveNumber;

      if( curve == 0){
      
        for( var i_data in data.points){
        
          var point = data.points[i_data].pointNumber;
    
          for(var i in x.map_curve[point] ){
            
            if( x.map_type[point][i] == 'scatter'){
            
              Plotly.restyle(el.id, {fillcolor :  x.map_color[point][i]
                                      //, line : { color :  x.hist_cols[x.map[point][i] - 1] }
                                      //, marker : {  color :  x.hist_cols[x.map[point][i] - 1] }
                                      }
                                      , parseInt(x.map_curve[point][i]) );
            }
            
            if( x.map_type[point][i] == 'bar'){
            
              Plotly.restyle(el.id, { marker : {  color :  x.map_color[point][i] }
                                      }
                                      , parseInt(x.map_curve[point][i]) );
            }
          }
        
        }
      }


    });

  }
});