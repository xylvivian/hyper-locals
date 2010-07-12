function initialize() {
  var map;
  var current_pos_marker;
  var myLatLng = new google.maps.LatLng(-25.363882,131.044922);
  var myOptions = {
    zoom: 4,
    center: myLatLng,
    mapTypeId: google.maps.MapTypeId.ROADMAP,
    disableDefaultUI: true
  }

  map = new google.maps.Map($("#map_canvas")[0], myOptions);
  
  $.ajaxSetup({cache: false});

  var ws = new WebSocket("ws://127.0.0.1:10000/");

  ws.onopen = function(evt) { alert("Connection open ..."); };
  ws.onmessage = function(evt) { alert( "Received Message: "  +  evt.data); };
  ws.onclose = function(evt) { alert("Connection closed."); };

  ws.send("DERP");
}
