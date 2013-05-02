

    if (!Array.prototype.indexOf) {
        Array.prototype.indexOf = function(elt /*, from*/){
          var len = this.length;
          var from = Number(arguments[1]) || 0;
          from = (from < 0) ? Math.ceil(from): Math.floor(from);
          if (from < 0)
               from += len;
          for (; from < len; from++){
            if (from in this && this[from] === elt)
                return from;
          }
          return -1;
        };
    }

    function getInternetExplorerVersion()
    {
      var rv = -1; // Return value assumes failure.
      if (navigator.appName == 'Microsoft Internet Explorer'){
         var ua = navigator.userAgent;
         var re  = new RegExp("MSIE ([0-9]{1,}[\.0-9]{0,})");
         if (re.exec(ua) != null)
           rv = parseFloat( RegExp.$1 );
    }
    return rv;
    }

    function roll_over(img_name, img_src) {document[img_name].src = img_src;}

    /* to replace the data JavaScript file */
	function createjscssfile(filename, filetype){
      if (filetype=="js"){ //if filename is a external JavaScript file
       var fileref=document.createElement('script')
       fileref.setAttribute("type","text/javascript")
       fileref.setAttribute("src", filename)
      }
      else if (filetype=="css"){ //if filename is an external CSS file
       var fileref=document.createElement("link")
       fileref.setAttribute("rel", "stylesheet")
       fileref.setAttribute("type", "text/css")
       fileref.setAttribute("href", filename)
      }
      return fileref
    }
    function replacejscssfile(oldfilename, newfilename, filetype){
     var targetelement=(filetype=="js")? "script" : (filetype=="css")? "link" : "none" //determine element type to create nodelist using
     var targetattr=(filetype=="js")? "src" : (filetype=="css")? "href" : "none" //determine corresponding attribute to test for
     var allsuspects=document.getElementsByTagName(targetelement)
     for (var i=allsuspects.length; i>=0; i--){ //search backwards within nodelist for matching elements to remove
      if (allsuspects[i] && allsuspects[i].getAttribute(targetattr)!=null && allsuspects[i].getAttribute(targetattr).indexOf(oldfilename)!=-1){
       var newelement=createjscssfile(newfilename, filetype)
       allsuspects[i].parentNode.replaceChild(newelement, allsuspects[i])
      }
     }
    }
	/* to extract and parse the date */
    function extract_date(date_str){
        date_fields=date_str.split(",");
		b=shift_month(date_fields);
		return new Date(b[0],b[1],b[2]);
        //return new Date(date_fields[0],date_fields[1],date_fields[2]);
    }
    function to_date_comma(calendar_date){
        fi = calendar_date.split("/");
        return fi[2]+","+fi[0]+","+fi[1];
    }
    function to_date_str(comma_date){
        fi = comma_date.split(",");
        return fi[1]+"/"+fi[2]+"/"+fi[0];    
    }
    function chk_type(){
     if (document.getElementById('ta').value=='STAGE' && document.getElementById('data-conversion').value=='daily_avg')
       document.getElementById('warning').innerHTML='Daily Average Stage is meaningless! Please select daily max/min for plotting.';
	 else
	   document.getElementById('warning').innerHTML='';
    }
	function change_period(){ 
	   reload_js();
       setTimeout("clear_and_draw(extract_date(to_date_comma($('#SDate').val())),extract_date(to_date_comma($('#EDate').val())))",1000);
    }
    function get_obj_size(obj){
       var size = 0, key;
       for (key in obj) {
         if (obj.hasOwnProperty(key)) size++;
       }
       return size;
	}
	function downloadcsv(output){
	    //data="";
        //popdata(data);
		alert("under construction");
	}
	function popdata(data){
	    window.location='data:text/csv;charset=utf8,' + encodeURIComponent(data);
        return true;	
	}
	function shift_month(date_fields){
       if (date_fields[1]>1) return [date_fields[0],date_fields[1]-1,date_fields[2]];
	   else return [date_fields[0]-1,12,date_fields[2]];
	}
    function dont_sort(data_arr){
        var dat=new Array();
        nlist=outlist.length;
        n=data.length;
        k=0;
        for(i=0;i<nlist;i++){
          for(j=0;j<n;j++){
            if(outlist[i][0]==data[j].title && outlist[i][1]==data[j].data_type){
               dat[k]=data[j];
               k++;
            }
          }
        }
        return dat;
    }
	/* for Google Map */
    function initialize(e) {
      tab_name = document.getElementById('ta').value; 
      e.innerHTML=document.getElementById("map_canvas"+tab_name).style.display==''?'View Map':'Hide Map';
      document.getElementById("map_canvas"+tab_name).style.display=document.getElementById("map_canvas"+tab_name).style.display==''?'none':'';
	  document.getElementById("map_"+tab_name).style.display=document.getElementById("map_"+tab_name).style.display==''?'none':'';
      var map = new GMap2(document.getElementById("map_canvas"+tab_name));
      map.setCenter(new GLatLng(38.16, -121.6), 10);
      map.setUIToDefault();
      var bounds = map.getBounds();
      var southWest = bounds.getSouthWest();
      var northEast = bounds.getNorthEast();
      var lngSpan = northEast.lng() - southWest.lng();
      var latSpan = northEast.lat() - southWest.lat();
      var baseIcon = new GIcon(G_DEFAULT_ICON);
	  baseIcon.shadow = "http://www.google.com/mapfiles/shadow50.png";
	  
     function createMarker(point, index, info) {
      //var letter = String.fromCharCode("A".charCodeAt(0) + index);
       if(Math.abs(index)>=100) size=35;
       else if(Math.abs(index)>=80 && Math.abs(index)<100) size=30;
       else if(Math.abs(index)>=60 && Math.abs(index)<80) size=25;
       else if(Math.abs(index)>=40 && Math.abs(index)<60) size=20;
       else if(Math.abs(index)>=20 && Math.abs(index)<40) size=15;
	   else if(Math.abs(index)>=10 && Math.abs(index)<20) size=10;
	   else if(Math.abs(index)>=0 && Math.abs(index)<10) size=5;
       else size=0;
       baseIcon.iconSize = new GSize(size, size);
       baseIcon.shadowSize = new GSize(1,1);
       baseIcon.iconAnchor = new GPoint(1, 1);	  
       var letteredIcon = new GIcon(baseIcon);
       if (index>=0) letteredIcon.image = "js/icon16.png";
	   else letteredIcon.image = "js/icon49.png";
       markerOptions = { icon:letteredIcon, title:info };
       var marker = new GMarker(point, markerOptions);
       GEvent.addListener(marker, "click", function() {
       marker.openInfoWindowHtml('<a href="#fig_'+info+'" font size=2>' + info + '</font></a><br>Percentage RMS Diff: '+index+'%');
       });
       return marker;
      }
      period_index = document.getElementById('time-window-select').selectedIndex;
      for(i=0; i < data_list.length; i++) {
	   if (data_list[i].data_type==tab_name && data_list[i].latitude!='nan') {	 
         var latlng = new GLatLng(data_list[i].latitude,data_list[i].longitude);
         map.addOverlay(createMarker(latlng,data_list[i].diff[period_index].perc_rmse,data_list[i].output));
	   }
      }
    }
    /* this function is tempporarily used for Asilomar presentation in order to 
	   show Google Map without internet connection. */	
    function show_img_for_asilomar(e){
      tab_name = document.getElementById('ta').value; 
	  document.getElementById("map_canvas"+tab_name).innerHTML='<img src="js/gmap.JPG">';
      e.innerHTML=document.getElementById("map_canvas"+tab_name).style.display==''?'View Map':'Hide Map';
      document.getElementById("map_canvas"+tab_name).style.display=document.getElementById("map_canvas"+tab_name).style.display==''?'none':'';
	  document.getElementById("map_"+tab_name).style.display=document.getElementById("map_"+tab_name).style.display==''?'none':'';  
	}	
