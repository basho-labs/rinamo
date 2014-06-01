/*
 * websockets.js
 */

_.templateSettings = {
  evaluate    : /\[\[(.+?)\]\]/g,
  interpolate : /\{\{(.+?)\}\}/g
};

// uses underscore.js for templating
var apply_template = function (input) {
  var template = _.template('<div class="row" mrn="{{mrn}}"><div class="column w145">{{patient_name}}</div><div class="column w206">{{room}}</div><div class="column w161">{{dob}}</div><div class="column w79">{{gender}}</div><div class="column w164">{{admit_physician}}</div><div class="column w198">{{update_time}}</div></div>');
  return template(input);
};

function getParameterByName(name) {
    var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
   return match && decodeURIComponent(match[1].replace(/\+/g, ' '));
}

var ws = null;
var list = getParameterByName('list');
if (typeof(LIST_ID) != "undefined" && LIST_ID != "")
  list = LIST_ID;
if (list == null) {
  // default to development environment, randy.secrist@gmail.com account.
  list = 'VMIk2s9hmUv1VuSvDq2ySxv1Zv'
}

var close_connection = function () {
  console.log("closed connection")
  if (ws) {
    ws.send(JSON.stringify({
      "type" : "leave",
      "data" : {
        "list" : list
      }
    }));
  }
};

var open_connection = function () {
  console.log("join")
  if (ws) {
    ws.send(JSON.stringify({
      "type" : "join",
      "data" : {
        "list" : list
      }
    }));
  }
}

ws_host = 'wss://mhealth-router.roder-faceplant.com';

$(function () {
  var $list_mgr = $("#hospital_manager");
  var in_mhealth = !!$.fn.list_view_row;

  if (typeof(WebSocket) == "undefined") {
    alert("WebSockets aren't supported in your browser!");
  }
  else {
    ws = new WebSocket(ws_host);

    ws.onopen = function (evt) {
      open_connection()
    }

    ws.onmessage = function (evt) {
      msg = JSON.parse(evt.data);

      // $("div[mrn='456']").find('li').contents().get(0).nodeValue = "New Name"
      // $("ul[id='patients']").find("div[mrn='789']").remove()

      switch(msg.type) {
        case 'join':
          break;
        case 'leave':
          break;
        case 'say':
          console.log(msg.data)
          if (msg.data.message != null) {
            var $row = $list_mgr.find(".row[mrn='" + msg.data.message.mrn + "']");
            // row not found, so new patient on the list
            if ($row.length == 0) {
              $row = $(apply_template(msg.data.message));
              $list_mgr.append($row);
              if (in_mhealth)
                $row.list_view_row();
              $row.hide().animate({opacity: 'toggle', height: 'toggle'}, 500, function(){
              });
            }
            // update existing
            else {
              if (in_mhealth)
                $row.list_view_row("update", $(apply_template(msg.data.message)).html());
            }
          }
          break;
        case 'keep-alive':
          console.log('keep-alive')
          break;
        case 'error':
          break;
        default:
          alert("Unknown message received from server: " + evt.type);
      }
    }

    ws.onclose = function (evt) {
      console.log("closed socket")
      //ws = null;
      //ws = new WebSocket(ws_host);
      //open_connection()
    }

    ws.onerror = function(evt) { console.log("error!") };

  }

});
