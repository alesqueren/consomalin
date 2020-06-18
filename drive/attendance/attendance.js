// open a browser at https://www.google.fr/search?q=auchan+drive+balma
// copy/paste this script in a browser to get the mongo request

// Get JQuery
var jq = document.createElement('script');
jq.src = "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js";
document.getElementsByTagName('head')[0].appendChild(jq);
//jQuery.noConflict();


var hour=new Array(17);
hour[0] = "06:00:00"
hour[1] = "07:00:00"
hour[2] = "08:00:00"
hour[3] = "09:00:00"
hour[4] = "10:00:00"
hour[5] = "11:00:00"
hour[6] = "12:00:00"
hour[7] = "13:00:00"
hour[8] = "14:00:00"
hour[9] = "15:00:00"
hour[10] = "16:00:00"
hour[11] = "17:00:00"
hour[12] = "18:00:00"
hour[13] = "19:00:00"
hour[14] = "20:00:00"
hour[15] = "21:00:00"
hour[16] = "22:00:00"
hour[17] = "23:00:00"


function parse() {

  var res = {}
  var sel = $("div.lubh-bar:eq(0)").parent()[0].classList[0]
  var max = $("div." + sel + ":eq(0)").innerHeight()
  $("div." + sel).each(function (i, elem) {
    $(elem).find("div").each(function (j, elem) {
      var value = $(this).innerHeight() / max;
      if (value) {
        if (! res[i+1]) {
          res[i+1] = {};
        }
        res[i+1][hour[j]] = value;
      }
    });
  });

  console.log("db.attendance.insert({_id: \"balma\", " + JSON.stringify(res).substring(1) + ")");
}

setTimeout(parse, 500);
