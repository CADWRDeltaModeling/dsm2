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
Plots.prototype.time_series_plot = function (div_id, data, diff, sdate, edate){
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

/* Border around plot 
vis.add(pv.Area)
	.data([0,1])
	.bottom(0)
	.height(h)
	.left(function(d) {return d*w})
	.fillStyle(null)
	.strokeStyle("#000")
	.lineWidth(0.25);
	*/
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
	
/* Legend*/
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
 * Exceedance plot with reversed axis from 100% to 0%
 * 
 * var data_B = {
		"title": "Car vs Motorcycle Speeds on the Autobahn",
		"series_names": ["Car","Motorcycle"],
		"yaxis_name":"Speed (mph)",
		"xaxis_name": "Percent time at or above",
		"values": [{x:100,y1:10,y2:10}
		,{x:90,y1:20,y2:30}
		,{x:80,y1:30,y2:44}
		,{x:70,y1:55,y2:65}
		,{x:60,y1:65,y2:75}
		,{x:50,y1:70,y2:85}
		,{x:40,y1:90,y2:95}
		,{x:30,y1:120,y2:110}
		,{x:20,y1:140,y2:120}
		,{x:10,y1:150,y2:125}
		,{x:0,y1:160,y2:130}],
}
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
	/* Legend*/
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
 * var data = {
 "title": "Stage vs Flow",
 "series_names": ["dS","dQ"],
 "yaxis_names": "dS",
 "xaxis_names": "dQ",
 "values": [[],[],[]
};
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

/* Line 1
vis.add(pv.Line)
    .data(data.values)
    .segmented(true)
    .visible(function(d) {return isNaN(d.y1)? false: true})
    .left(function(d) {return x(d.y1)})
    .bottom(function(d) {return y(d.y2)})
    .lineWidth(curves[0].width)
    .strokeStyle(curves[0].color)
    .dashArray(curves[0].dashArray);
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

var plots = new Plots();
