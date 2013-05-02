/*
 * Data in this format
 *
var data_A = {
		"title": "Muscle vs Protein Plot",
		"series_names": ["Muscle","Protein"],
		"yaxis_name":"Strength (kg/s)",
		"xaxis_name": "Time",
		"values": [{x:new Date(1908,0,1),y1:400,y2:300}
		,{x:new Date(1918,1,1),y1:700,y2:600}
		,{x:new Date(1928,2,1),y1:900,y2:1100}
		,{x:new Date(1938,3,1),y1:1300,y2:1200}
		,{x:new Date(1948,4,1),y1:1700,y2:2000}
		,{x:new Date(1958,5,1),y1:1900,y2:2100}
		,{x:new Date(1968,6,1),y1:2000,y2:2400}
		,{x:new Date(1978,7,1),y1:2500,y2:2600}
		,{x:new Date(1988,8,1),y1:2700,y2:3300}
		,{x:new Date(1998,9,1),y1:3000,y2:3500}
		,{x:new Date(2008,10,1),y1:3800,y2:3700}]
}
 */
function Plots(){
	this.PLOT_WIDTH=720;
	this.PLOT_HEIGHT=480;
}
/**
 * div_id is the id of the div to place the plot into
 * data is the time series data structure
 * diff is the flag to calculate and display difference
 * sdate and edate are optional date parameters to display a slice of the data
 * bars is a data structure to draw background bars
 */
Plots.prototype.time_series_plot = function (div_id, data, diff, sdate, edate, bars){
    if (sdate==null){
    	sdate = data.values[0].x;
    }
    if (edate==null){
    	edate=data.values[data.values.length-1].x;
    }
/* Sizing and scales. */
var w = this.PLOT_WIDTH,
    h = this.PLOT_HEIGHT,
    h2= diff ? 150 :0 ,
    x = pv.Scale.linear(data.values, 
    		function(d) {
    			if (d.x>=sdate && d.x<=edate){
    				return new Date(d.x);
    			} else {
    				return sdate;
    			}
    			}).range(0, w),
    y = pv.Scale.linear(data.values, 
    	    function(d) {
    			if (d.x>=sdate && d.x<=edate) {
    				return Math.min(isNaN(d.y1)?0:d.y1,isNaN(d.y2)?0:d.y2)*0.95
    	    	} else { 
    	    		return null; 
    	    	}}, 
    	    function(d) {
    	    		if (d.x>=sdate && d.x<=edate) {
    	    			return Math.max(isNaN(d.y1)?0:d.y1,isNaN(d.y2)?0:d.y2)*1.10;
    	    		} else {
    	    			return null;
    	    		}})
    	    .range(h2, h+h2);
   	if (diff){
   		diff_data = data.values.map(function(d) {
   			if (d.x>=sdate && d.x<=edate) {
   				if (isNaN(d.y1) || isNaN(d.y2)){
   					return 0;
   				}else{
   					return d.y2-d.y1;
   				}
   			} else {
   				return null;
   			}
   			});
    	yd = pv.Scale.linear(diff_data).range(0, h2-50);
    }

var curves=[{"color":"green", "width":1}
	,{"color":"blue", "width":1, "dashArray": "10,3"}
	,{"color":"red", "width":1}
	]

/* The root panel. */
var vis = new pv.Panel()
	.canvas(div_id)
    .width(w)
    .height(h+h2+50)
    .bottom(50)
    .left(70)
    .right(70)
    .top(50);

if (bars){
	function truncate(val, range){
		var min = range[0];
		var max = range[range.length-1];
		var rval = Math.max(min, Math.min(max,val));
		return rval;
	}
	vis.add(pv.Bar).data(bars)
		.bottom(h2).height(h)
		.left(function(d) {return truncate(x(d.start), x.range());})
		.width(function(d) {return truncate(x(d.end), x.range())-truncate(x(d.start), x.range());})
		.fillStyle(function(d){return d.color}).strokeStyle(function(d){return d.color});
}
/* X-axis ticks. */
vis.add(pv.Rule)
    	.data(x.ticks())
    	.left(x)
    	.strokeStyle("#eee")
	.add(pv.Rule)
    	.bottom(-10+h2)
    	.height(5)
    	.strokeStyle("#000")
    .anchor("bottom").add(pv.Label)
    	.text(x.tickFormat);

/* X-axis label */
vis.add(pv.Label)
	.left(w/2)
	.bottom(-40)
	.text(data.xaxis_name)
	.font("18px sans-serif")
	.textAlign("center");

/* Y-axis ticks. */
  vis.add(pv.Rule)
    .data(y.ticks(8))
    .bottom(y)
    .strokeStyle("#ddd")
  .anchor("left").add(pv.Label)
	    .text(y.tickFormat);
 
/* Y-axis label */
vis.add(pv.Label)
	.top(h/2)
	.left(-45)
	.text(data.yaxis_name)
	.font("18px sans-serif")
	.textAngle(-Math.PI/2)
	.textAlign("center");

if (diff){
	/* Y-axis for diff ticks. */
	  vis.add(pv.Rule)
	    .data(yd.ticks(8))
	    .bottom(yd)
	    .strokeStyle(function(d) {return d==0 ? "LightCoral" : "#eee"})
	  .anchor("left").add(pv.Label)
		    .text(yd.tickFormat)
		    .textStyle("red");
	 
	/* Y-axis for diff label */
	vis.add(pv.Label)
		.bottom(h2-50)
		.right(w/2)
		.text("Difference")
		.font("12px sans-serif")
		.textAlign("center")
		.textStyle("red");
}
/* Line 1 */
vis.add(pv.Line)
    .data(data.values)
    .interpolate("step-before")
    .segmented(true)
    .visible(function(d) { if (d.x>=sdate && d.x<=edate) {return isNaN(d.y1)? false: true}})
    .left(function(d) { if (d.x>=sdate && d.x<=edate) return x(d.x)})
    .bottom(function(d) {if (d.x>=sdate && d.x<=edate) return y(d.y1)})
    .lineWidth(curves[0].width)
    .strokeStyle(curves[0].color)
    .dashArray(curves[0].dashArray);
    
/* Line 2 */
vis.add(pv.Line)
    .data(data.values)
    .interpolate("step-before")
    .segmented(true)
    .visible(function(d) {if (d.x>=sdate && d.x<=edate) return isNaN(d.y2)? false: true})
    .left(function(d) {if (d.x>=sdate && d.x<=edate) return x(d.x)})
    .bottom(function(d) {if (d.x>=sdate && d.x<=edate) return y(d.y2)})
    .lineWidth(curves[1].width)
    .strokeStyle(curves[1].color)
    .dashArray(curves[1].dashArray);

/* Line for diff */
if (diff){
vis.add(pv.Line)
    .data(data.values)
    .interpolate("step-before")
    .segmented(true)
    .visible(function(d) {if (d.x>=sdate && d.x<=edate) return isNaN(diff_data[this.index])? false: true})
    .left(function(d) {if (d.x>=sdate && d.x<=edate) return x(d.x)})
    .bottom(function(d) {if (d.x>=sdate && d.x<=edate) return yd(diff_data[this.index]);})
    .lineWidth(curves[2].width)
    .strokeStyle(curves[2].color)
    .dashArray(curves[2].dashArray);
}
/* Title */
vis.add(pv.Label)
	.right(function(d) {return w/2})
	.top(-15)
	.textAlign("center")
	.font("28px sans-serif")
	.text(data.title)
	
/* Legend */
vis.add(pv.Rule)
	.left(12).top(20).width(40)
    .lineWidth(curves[0].width)
    .strokeStyle(curves[0].color)
    .dashArray(curves[0].dashArray)
	.anchor("right").add(pv.Label).text(data.series_names[0]);

vis.add(pv.Rule)
	.left(12).top(32).width(40)
    .lineWidth(curves[1].width)
    .strokeStyle(curves[1].color)
    .dashArray(curves[1].dashArray)
	.anchor("right").add(pv.Label).text(data.series_names[1]);
/* Render */
vis.render();
return vis;
}

/**
 * div_id is the id of the div to place the plot into
 * data is the time series data structure
 * diff is the flag to calculate and display difference
 * sdate and edate are optional date parameters to display a slice of the data
 * bars is a data structure to draw background bars
 */
Plots.prototype.multi_time_series_plot = function (div_id, data, diff, sdate, edate, bars){
    Array.prototype.sumabs = function() {
        return (!this.length) ? 0 : this.slice(1).sumabs() +
          (this[0]>0 ? this[0] : Math.abs(this[0]));
    };
	Array.prototype.max = function() {
        var max = isNaN(this[0])?-9999999:this[0], len = this.length;
        for (var i = 1; i < len; i++) if (this[i] > max) max = this[i];
        return max;
    }
    Array.prototype.min = function() {
        var min = isNaN(this[0])?99999999:this[0], len = this.length;
        for (var i = 1; i < len; i++) if (this[i] < min && isNaN(this[i])==false) min = this[i];
        return min;
    }

/* Data properties */
    var y0= 0, y1= 0, y2= 0;
	if (data.values.length > 0){
	    if (typeof data.values[0].y0 != "undefined") y0 = 1;
	    if (typeof data.values[0].y1 != "undefined") y1 = 1;
	    if (typeof data.values[0].y2 != "undefined") y2 = 1;
    }
  	var ts=[], i=0, cou1=0, cou2=0 ;
    if (y0==1) {ts[i]={"type":"dot","series":"y0","name":data.series_names[0]}; i++;}
    if (y1==1 && cou1==0) {ts[i]={"type":"line","series":"y1","name":data.series_names[1]}; cou1++; i++;}
    if (y2==1 && cou2==0) {ts[i]={"type":"line","series":"y2","name":data.series_names[2]}; cou2++; i++;}
    if (sdate==null){
    	sdate = data.values[0].x;
    }
    if (edate==null){
    	edate=data.values[data.values.length-1].x;
    }
/* Sizing and scales. */
var w = this.PLOT_WIDTH,
    h = this.PLOT_HEIGHT,
    h2= diff ? 150 :0,
	diff_data = [];
	if (y0==1 || y1==1 || y2==1){
        x = pv.Scale.linear(data.values, 
    		function(d) {
    			if (d.x>=sdate && d.x<=edate){
    				return new Date(d.x);
    			} else {
    				return sdate;
    			}
    			}).range(0, w),
        y = pv.Scale.linear(data.values, 
    	    function(d) {
    			if (d.x>=sdate && d.x<=edate) {
    				return Math.min(isNaN(d.y0)?0:d.y0,isNaN(d.y1)?0:d.y1,isNaN(d.y2)?0:d.y2)*0.95
    	    	} else { 
    	    		return null; 
    	    	}}, 
    	    function(d) {
    	    	if (d.x>=sdate && d.x<=edate) {
    	    		return Math.max(isNaN(d.y0)?0:d.y0,isNaN(d.y1)?0:d.y1,isNaN(d.y2)?0:d.y2)*1.10;
    	    	} else {
    	    		return null;
    	    	}})
    	    .range(h2, h+h2);
    }

   	if (diff){
	    if (y0==1 && y1==1 && y2==1) {               // case 5
   		    diff_data[0] = data.values.map(function(d) {
   			  if (d.x>=sdate && d.x<=edate) {return d.y1 - d.y0;}
   			  else {return null;}
   			});
   		    diff_data[1] = data.values.map(function(d) {
   			  if (d.x>=sdate && d.x<=edate) {return d.y2 - d.y0;}
   			  else {return null;}
   			});			
		}else{
   	 	    diff_data[0] = data.values.map(function(d) {
   			  if (d.x>=sdate && d.x<=edate) {
			    if (y0==0 && y1==1 && y2==1)  {return d.y2 - d.y1; }        //case 3
                else if (y0==1 && y1==1 && y2==0) {return d.y1 - d.y0; }    //case 4
                else { return 0;}
   		 	  } else {
   				return null;
   			  }
   			});
		}	
		if (y0==1 && y1==1 && y2==1){ // case 5
		  var diff_concat = diff_data[0].concat(diff_data[1]);
		  if (diff_data[0].sumabs()==0 && diff_data[1].sumabs()==0) yd = pv.Scale.linear([-0.01,0.01]).range(0, h2-50); /* add this line to fix the problem of x axis for zero array */
		  else { yd = pv.Scale.linear([diff_concat.min(),diff_concat.max()]).range(0, h2-50);}
		}else{
		  var diff_arr=diff_data[0];
    	  yd = pv.Scale.linear([diff_arr.min(),diff_arr.max()]).range(0, h2-50);
		  if (diff_data[0].sumabs()==0) yd = pv.Scale.linear([-0.01,0.01]).range(0, h2-50); /* add this line to fix the problem of x axis for zero array */
		}
    }
var curves=[
    {"color":"blue", "width":1, "dashArray": "10,3"}
    ,{"color":"green", "width":1}
	,{"color":"blue", "width":1, "dashArray": "10,3"}
	,{"color":"red", "width":1}
	]
var dots=[
     {"color":"orange","size":"2"}
	,{"color":"pink","size":"2"}
    ]
var diffline=[
    {"color":"red", "width":1}
    ,{"color":"RoyalBlue", "width":1}
	,{"color":"LimeGreen", "width":1}
]	

/* The root panel. */
var vis = new pv.Panel()
	.canvas(div_id)
    .width(w)
    .height(h+h2+50)
    .bottom(50)
    .left(70)
    .right(70)
    .top(50);

if (bars){
	function truncate(val, range){
		var min = range[0];
		var max = range[range.length-1];
		var rval = Math.max(min, Math.min(max,val));
		return rval;
	}
	vis.add(pv.Bar).data(bars)
		.bottom(h2).height(h)
		.left(function(d) {return truncate(x(d.start), x.range());})
		.width(function(d) {return truncate(x(d.end), x.range())-truncate(x(d.start), x.range());})
		.fillStyle(function(d){return d.color}).strokeStyle(function(d){return d.color});
}
/* X-axis ticks. */
vis.add(pv.Rule)
    	.data(x.ticks())
    	.left(x)
    	.strokeStyle("#eee")
	.add(pv.Rule)
    	.bottom(-10+h2)
    	.height(5)
    	.strokeStyle("#000")
    .anchor("bottom").add(pv.Label)
    	.text(x.tickFormat);

/* X-axis label */
vis.add(pv.Label)
	.left(w/2)
	.bottom(-40)
	.text(data.xaxis_name)
	.font("18px sans-serif")
	.textAlign("center");

/* Y-axis ticks. */
  vis.add(pv.Rule)
    .data(y.ticks(8))
    .bottom(y)
    .strokeStyle("#ddd")
  .anchor("left").add(pv.Label)
	    .text(y.tickFormat);
 
/* Y-axis label */
vis.add(pv.Label)
	.top(h/2)
	.left(-45)
	.text(data.yaxis_name)
	.font("18px sans-serif")
	.textAngle(-Math.PI/2)
	.textAlign("center");

if (diff){
	/* Y-axis for diff ticks. */
	  vis.add(pv.Rule)
	    .data(yd.ticks(8))
	    .bottom(yd)
	    .strokeStyle(function(d) {return d==0 ? "LightCoral" : "#eee"})
	  .anchor("left").add(pv.Label)
		    .text(yd.tickFormat)
		    .textStyle("red");
	 
	/* Y-axis for diff label */
	vis.add(pv.Label)
		.bottom(h2-50)
		.right(w/2)
		.text("Difference")
		.font("12px sans-serif")
		.textAlign("center")
		.textStyle("red");
}

for (i=0;i<ts.length;i++){
   var ts_series= ts[i].series;
   if (ts[i].type=='dot'){
    vis.add(pv.Dot)
	    .data(function() {return data.values})
		.visible(function(d){if (d.x>=sdate && d.x<=edate) return isNaN(d.y0)? false: true;})
	    .left(function(d) {if (d.x>=sdate && d.x<=edate) return x(d.x)})
	    .bottom(function(d) {if (d.x>=sdate && d.x<=edate) return y(d.y0)})
	    .shapeRadius(dots[i].size)
	    .strokeStyle(dots[i].color)
	    .fillStyle("yellow");      
   }
   if (ts[i].type=='line' && ts[i].series=="y1"){
    vis.add(pv.Line)
        .data(function() {return data.values})
        .interpolate("step-before")
        .segmented(true)
        .visible(function(d){if (d.x>=sdate && d.x<=edate) { return isNaN(d.y1)? false: true;}})
        .left(function(d) { if (d.x>=sdate && d.x<=edate) return x(d.x)})
        .bottom(function(d) {if (d.x>=sdate && d.x<=edate) {return y(d.y1);}})
        .lineWidth(curves[i].width)
        .strokeStyle(curves[i].color)
        .dashArray(curves[i].dashArray);   
   }
   if (ts[i].type=='line' && ts[i].series=="y2"){
    vis.add(pv.Line)
        .data(function() {return data.values})
        .interpolate("step-before")
        .segmented(true)
        .visible(function(d){if (d.x>=sdate && d.x<=edate) { return isNaN(d.y2)? false: true;}})
        .left(function(d) { if (d.x>=sdate && d.x<=edate) return x(d.x)})
        .bottom(function(d) {if (d.x>=sdate && d.x<=edate) {return y(d.y2);}})
        .lineWidth(curves[i].width)
        .strokeStyle(curves[i].color)
        .dashArray(curves[i].dashArray);   
   }
}

/* Line for diff */
if (diff){
  if (y0==1 && y1==1 && y2==1){
  vis.add(pv.Line)
    .data(data.values)
    .interpolate("step-before")
    .segmented(true)
    .visible(function(d) {if (d.x>=sdate && d.x<=edate) return isNaN(diff_data[1][this.index])? false: true})
    .left(function(d) {if (d.x>=sdate && d.x<=edate) return x(d.x)})
    .bottom(function(d) {if (d.x>=sdate && d.x<=edate) return yd(diff_data[1][this.index]);})
    .lineWidth(diffline[1].width)
    .strokeStyle(diffline[1].color)
    .dashArray(diffline[1].dashArray);
  }
vis.add(pv.Line)
    .data(data.values)
    .interpolate("step-before")
    .segmented(true)
    .visible(function(d) {if (d.x>=sdate && d.x<=edate) return isNaN(diff_data[0][this.index])? false: true})
    .left(function(d) {if (d.x>=sdate && d.x<=edate) return x(d.x)})
    .bottom(function(d) {if (d.x>=sdate && d.x<=edate) return yd(diff_data[0][this.index]);})
    .lineWidth(diffline[0].width)
    .strokeStyle(diffline[0].color)
    .dashArray(diffline[0].dashArray);	
}

/* Title */
vis.add(pv.Label)
	.right(function(d) {return w/2})
	.top(-15)
	.textAlign("center")
	.font("28px sans-serif")
	.text(data.output)
	
/* Legend */
for (i=0;i<ts.length;i++){
  if (ts[i].type=='dot'){
    vis.add(pv.Dot)
	    .left(12).top((i+1)*10).width(40)
        .strokeStyle(dots[i].color)
		.shapeRadius(dots[i].size)
	    .anchor("right").add(pv.Label).text(ts[i].name);
  }
  if (ts[i].type=='line'){
    vis.add(pv.Rule)
        .left(12).top((i+1)*10).width(40)
        .lineWidth(curves[i].width)
        .strokeStyle(curves[i].color)
        .dashArray(curves[i].dashArray)
	    .anchor("right").add(pv.Label).text(ts[i].name);  
  }
}
if (diff){      /* add legend to diff  */
  if (y1==1 && (y0==1 || y2==1)){
    for (i=0;i<diff_data.length;i++){
	  vis.add(pv.Rule)
	      .left(12).bottom(100-i*10).width(40)
		  .lineWidth(diffline[i].width).strokeStyle(diffline[i].color)
		  .anchor("right").add(pv.Label).text(ts[i+1].name+" - "+ts[0].name);
	}
  }
}
/* Render */
vis.render();
return vis;
}
/**
 * Exceedance plot with reversed axis from 100% to 0%
 * 
 * var data_B = { "title": "Car vs Motorcycle Speeds on the Autobahn",
 * "series_names": ["Car","Motorcycle"], "yaxis_name":"Speed (mph)",
 * "xaxis_name": "Percent time at or above", "values": [{x:100,y1:10,y2:10}
 * ,{x:90,y1:20,y2:30} ,{x:80,y1:30,y2:44} ,{x:70,y1:55,y2:65}
 * ,{x:60,y1:65,y2:75} ,{x:50,y1:70,y2:85} ,{x:40,y1:90,y2:95}
 * ,{x:30,y1:120,y2:110} ,{x:20,y1:140,y2:120} ,{x:10,y1:150,y2:125}
 * ,{x:0,y1:160,y2:130}], }
 * 
 * @param div_id
 * @param data
 * @return
 */
Plots.prototype.exceedance_plot = function(div_id, data){
	/* Sizing and scales. */
	var w = this.PLOT_WIDTH,
	    h = this.PLOT_HEIGHT,
	    x = pv.Scale.linear(100,0).range(0, w),
	    y = pv.Scale.linear(data.values, function(d) {return Math.min(d.y1,d.y2)}, function(d) {return Math.max(d.y1,d.y2)}).range(0, h);

	var curves=[{"color":"red", "width":3},{"color":"blue", "width":3, "dashArray": "10,3"}]
	            var curves = [ {
		"color" : "green",
		"width" : 2
	}, {
		"color" : "blue",
		"width" : 2,
		"dashArray" : "10,3"
	}, {
		"color" : "red",
		"width" : 2
	} ]

	/* The root panel. */
	var vis = new pv.Panel()
		.canvas(div_id)
	    .width(w)
	    .height(h)
	    .bottom(50)
	    .left(60)
	    .right(10)
	    .top(50);
	/* Border around plot */
	vis.add(pv.Area)
		.data([0,1])
		.bottom(0)
		.height(h)
		.left(function(d) {return d*w})
		.fillStyle(null)
		.strokeStyle("#ddd");
	/* X-axis ticks. */
	vis.add(pv.Rule)
	    	.data(x.ticks())
	    	.visible(function(d) {return d >= 0})
	    	.left(x)
	    	.strokeStyle("#eee")
		.add(pv.Rule)
	    	.bottom(-10)
	    	.height(5)
	    	.strokeStyle("#000")
	    .anchor("bottom").add(pv.Label)
	    	.text(function(d) {return x.tickFormat(d)+'%'});

	/* X-axis label */
	vis.add(pv.Label)
		.left(w/2)
		.bottom(-40)
		.text(data.xaxis_name)
		.font("18px sans-serif")
		.textAlign("center");

	/* Y-axis ticks. */
	  vis.add(pv.Rule)
	    .data(y.ticks(5))
	    .bottom(y)
	    .strokeStyle("#999")
	  .anchor("left").add(pv.Label)
		    .text(y.tickFormat);
	 
	/* Y-axis label */
	vis.add(pv.Label)
		.top(h/2)
		.left(-40)
		.text(data.yaxis_name)
		.font("18px sans-serif")
		.textAngle(-Math.PI/2)
		.textAlign("center");

	/* Line 1 */
	vis.add(pv.Line)
	    .data(data.values)
	    .interpolate("linear")
	    .left(function(d) {return x(d.x)})
	    .bottom(function(d) {return y(d.y1)})
	    .lineWidth(curves[0].width)
	    .strokeStyle(curves[0].color)
	    .dashArray(curves[0].dashArray);
	    
	/* Line 2 */
	vis.add(pv.Line)
	    .data(data.values)
	    .interpolate("linear")
	    .left(function(d) {return x(d.x)})
	    .bottom(function(d) {return y(d.y2)})
	    .lineWidth(curves[1].width)
	    .strokeStyle(curves[1].color)
	    .dashArray(curves[1].dashArray);
	/* Title */
	vis.add(pv.Label)
		.right(function(d) {return w/2})
		.top(-15)
		.textAlign("center")
		.font("28px sans-serif")
		.text(data.title)
	/* Legend */
	vis.add(pv.Rule)
		.left(12).top(20).width(40)
	    .lineWidth(curves[0].width)
	    .strokeStyle(curves[0].color)
	    .dashArray(curves[0].dashArray)
		.anchor("right").add(pv.Label).text(data.series_names[0]);
	vis.add(pv.Rule)
		.left(12).top(32).width(40)
	    .lineWidth(curves[1].width)
	    .strokeStyle(curves[1].color)
	    .dashArray(curves[1].dashArray)
		.anchor("right").add(pv.Label).text(data.series_names[1]);
	/* Render */
	vis.render();
	return vis;
}
/**
 * var data = { "title": "Stage vs Flow", "series_names": ["dS","dQ"],
 * "yaxis_names": "dS", "xaxis_names": "dQ", "values": [[],[],[] };
 */
Plots.prototype.time_series_derivative_plot = function(div_id, data){
/* Sizing and scales. */
var w = this.PLOT_WIDTH,
    h = this.PLOT_HEIGHT,
    x = pv.Scale.linear(data.values,
    		function(d) {return Math.min(d.y1) - 0.05*Math.abs(d.y1)}, 
			function(d) {return Math.max(d.y1) + 0.05*Math.abs(d.y1)})
			.range(0, w),
    y = pv.Scale.linear(data.values, 
			function(d) {return Math.min(d.y2) - 0.05*Math.abs(d.y2)}, 
			function(d) {return Math.max(d.y2) + 0.05*Math.abs(d.y2)})
    	    .range(0, h);

/* Interaction state. Focus scales will have domain set on-render. */
 var h2=30;
 var c = pv.Scale.linear(data.values,function(d){return d.y3}).range("red", "yellow");

var curves=[{"color":"green", "width":1}
	,{"color":"blue", "width":1, "dashArray": "10,3"}
	]

/* The root panel. */
var vis = new pv.Panel()
	.canvas(div_id)
    .width(w)
    .height(h+h2+50)
    .bottom(50)
    .left(70)
    .right(70)
    .top(50);
vis.x=x;
vis.y=y;
vis.c=c;
/* X-axis ticks. */
vis.add(pv.Rule)
    	.data(x.ticks())
    	.left(x)
    	.strokeStyle("#eee")
	.add(pv.Rule)
    	.bottom(-10)
    	.height(5)
    	.strokeStyle("#000")
    .anchor("bottom").add(pv.Label)
    	.text(x.tickFormat);

/* X-axis label */
vis.add(pv.Label)
	.left(w/2)
	.bottom(-40)
	.text(data.xaxis_name)
	.font("18px sans-serif")
	.textAlign("center");

/* Y-axis ticks. */
  vis.add(pv.Rule)
    .data(y.ticks(8))
    .bottom(y)
    .strokeStyle("#ddd")
  .anchor("left").add(pv.Label)
	    .text(y.tickFormat);
  /* Y-axis line */
  vis.add(pv.Rule)
    .bottom(0)
    .left(x(0))
    .strokeStyle("#333")
  /* X-axis line */
  vis.add(pv.Rule)
    .bottom(y(0))
    .left(0)
    .right(0)
    .strokeStyle("#333")
    /* origin label */
 vis.add(pv.Label)
    .left(x(0))
    .bottom(y(0))
    .text("0,0")
 vis.color_scale = c;
 dc=c.domain();
 vis.add(pv.Bar)
    .data(pv.range(dc[0],dc[1],(dc[1]-dc[0])/30))
    .left(function() {return this.index * 4})
    .width(4)
    .height(20)
    .top(50)
    .fillStyle(c)
    .anchor("top")
    .add(pv.Label)
    .textMargin(20)
    .visible(function(d){ return this.index==0 || this.index==29;})
    .text(function(d) { 
    	if (this.index==0){
    		return pv.Format.number().format(dc[0]);
    	} else if (this.index == 29){
    		return pv.Format.number().format(dc[1]);
    	} else {
    		return "";
    	}
    	});
    ;
/* Y-axis label */
vis.add(pv.Label)
	.top(h/2)
	.left(-45)
	.text(data.yaxis_name)
	.font("18px sans-serif")
	.textAngle(-Math.PI/2)
	.textAlign("center");
/* Dots on */
  vis.add(pv.Dot)
    .data(data.values)
    .left(function(d) { return x(d.y1)})
    .bottom(function(d) { return y(d.y2)})
    .strokeStyle(function(d){ return c(d.y3)})
    .fillStyle(function(d){ return c(d.y3)})
    .title(function(d) { return "("+d.y1+", "+d.y2+", "+d.y3+")";});

/*
 * Line 1 vis.add(pv.Line) .data(data.values) .segmented(true)
 * .visible(function(d) {return isNaN(d.y1)? false: true}) .left(function(d)
 * {return x(d.y1)}) .bottom(function(d) {return y(d.y2)})
 * .lineWidth(curves[0].width) .strokeStyle(curves[0].color)
 * .dashArray(curves[0].dashArray);
 */
/* Title */
vis.add(pv.Label)
	.right(function(d) {return w/2})
	.top(-15)
	.textAlign("center")
	.font("28px sans-serif")
	.text(data.title)
	
/* Render */
vis.render();

return vis;
}
/**
 * Creates a visual editor for a xsection with a profile background and points
 * representing depth soundings in the vicinity of the xsection
 * 
 * xsection is an array of objects {x: <distance along the xsection>, y: <depth
 * of the xsection>} The xsection will change as a result of editing. profile is
 * an array of objects of the same type as xsection. This points are for display
 * as an initial set of profile points
 * 
 * points are an array of objects {x: <distance along the xsection>, y: <depth
 * of the point>, z: <distance perpendicular to the xsection>}
 */
Plots.prototype.xsection_editor = function(div_id,xsection_points,profile_points,points){
	
	var w = 896,
		w2 = 50,
	    h = 586,
	    h2 = 50,
	    i = 3,
	    interpolate = "linear";
	var data = {
			xaxis_name: "Length(ft)",
			yaxis_name: "Elevation(ft)"
	};
	var xvals = pv.map(profile_points,function(d){ return d.x;});
	var yvals = pv.map(profile_points, function(d){return d.y;});
	var x = pv.Scale.linear(pv.min(xvals), pv.max(xvals)).range(w2,w-w2);
	var y = pv.Scale.linear(pv.min(yvals), pv.max(yvals)).range(h-h2,h2);
	var xsection = xsection_points.map(function(d){return {x: x(d.x),y: y(d.y)}});
	var ds = pv.Scale.linear(0,100,200,300,400).range("black","darkblue","dodgerblue","azure","lightgray");
	var vis = new pv.Panel()
		.canvas(div_id)
	    .width(w+w2)
	    .height(h+h2)
	    .fillStyle("#fff")
	    .strokeStyle("#ccc")
	    .lineWidth(4)
	    .antialias(false)
	    .margin(70)
	    .right(100)
	    .event("mousedown", function() {
	        var m = this.mouse();
	        xsection_points.push({x: x.invert(m.x), y: y.invert(m.y)});
	        return (i = xsection.push(m) - 1, this)}
	    );

	/* X-axis ticks. */
	vis.add(pv.Rule)
	    	.data(x.ticks())
	    	.left(x)
	    	.strokeStyle("#eee")
		.add(pv.Rule)
	    	.bottom(-10+h2)
	    	.height(5)
	    	.strokeStyle("#000")
	    .anchor("bottom").add(pv.Label)
	    	.text(x.tickFormat);

	/* X-axis label */
	vis.add(pv.Label)
		.left(w/2)
		.bottom(-40)
		.text(data.xaxis_name)
		.font("18px sans-serif")
		.textAlign("center");

	/* Y-axis ticks. */
	  vis.add(pv.Rule)
	    .data(y.ticks(8))
	    .top(y)
	    .strokeStyle("#ddd")
	  .anchor("left").add(pv.Label)
		    .text(y.tickFormat);
	 
	/* Y-axis label */
	vis.add(pv.Label)
		.top(h/2)
		.left(-45)
		.text(data.yaxis_name)
		.font("18px sans-serif")
		.textAngle(-Math.PI/2)
		.textAlign("center");
	/* add color legend */
	vis.add(pv.Panel)
		.add(pv.Label)
		.text("Distance from X Section")
		.textAngle(-Math.PI/2)
		.right(30)
		.top(150)
		.add(pv.Bar)
		.data(pv.range(0,400,5))
		.right(5)
		.top(function() { return this.index * 2 })
		.height(2)
		.width(20)
		.fillStyle(function(d) {return ds(d)})
	/* data points */
	vis.add(pv.Dot)
		.data(function() {return points})
		.left(function(d) {return x(d.x)})
		.top(function(d) {return y(d.y)})
		.shapeRadius(function(d) {return 5})
		.strokeStyle(function(d){return ds(d.z)})
		.fillStyle(function(d) {return this.strokeStyle().alpha(0.2);});
	/* add profile points */
	vis.add(pv.Dot)
		.data(function() {return profile_points})
		.left(function(d) {return x(d.x)})
		.top(function(d) {return y(d.y)})
		.shapeRadius(5)
		.strokeStyle("blue")
		.fillStyle("blue");
	/* xsection line */
	vis.add(pv.Line)
	    .data(function() {return xsection})
	    .left(function(d) {return d.x})
	    .top(function(d) {return d.y})
	    .interpolate(function() {return interpolate})
	    .segmented(function() { return true})
	    .strokeStyle("blue")
	    .tension(0.5)
	    .lineWidth(2)
	    .cursor("crosshair")
	    .event("mousedown", function(){
	        var m=this.mouse();
	        var p = xsection.splice(this.index+1,0,m);
	        xsection_points.splice(this.index+1,0,{x:x.invert(m.x),y:y.invert(m.y)});
	    	vis.render();
	    	return this;
	    	});
	/* xsection points */
	vis.add(pv.Dot)
	    .data(function() {return xsection})
	    .left(function(d) {return d.x})
	    .top(function(d) {return d.y})
	    .shapeRadius(7)
	    .cursor("move")
	    .strokeStyle(function() {return i == this.index ? "#ff7f0e" : "#1f77b4"})
	    .fillStyle(function() {return this.strokeStyle().alpha(.2)})
	    .event("mousedown", pv.Behavior.drag())
	    .event("dragstart", function() {return (i = this.index, this)})
	    .event("dragend", function(d){ i=this.index; var xp = xsection_points[i]; var p=xsection[i]; xp.x=x.invert(p.x); xp.y=y.invert(p.y);})
	    .event("drag", vis);

	vis.render();

	pv.listen(window, "mousedown", function() {return self.focus()});
	pv.listen(window, "keydown", function(e) {
	  // code 8 is backspace, code 46 is delete
	  if ((e.keyCode == 8 || e.keyCode == 46) && (i >= 0)) {
		xsection_points.splice(i,1);
	    xsection.splice(i--, 1);
	    vis.render();
	    e.preventDefault();
	  }
	});


}

var plots = new Plots();
