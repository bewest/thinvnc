var baseUrl;
var inputComputer;
var inputFullColor;
var inputControlMode;
var inputConnect;
var authPending = true;
var CFInstall;

function connect() {
    var fullColor = inputFullColor.checked;
    var mouseControl = inputControlMode.checked;
    var fcQ = 85;
    var fcPF = 0;
    if (!fullColor) {
      fcQ = 50;
      fcPF = 1;
    }
    var cmd = escape("cmd=connect");
    cmd += escape("&destAddr=" + inputComputer.value);
    cmd += escape("&mouseControl=" + mouseControl);
    cmd += escape("&kbdControl=" + mouseControl);
    cmd += escape("&quality=" + fcQ);
    cmd += escape("&pixelFormat=" + fcPF);
    cmd += escape("&remotePointer=" + "false");
    sendLocalCmd(cmd);
}

function sendServerCmd(cmd, query) {
    var url = baseUrl + cmd + "?" + query + "&id=";
    try {
        $.getJSON(url, function(obj) {
            var id = obj.id;
            if (!obj.status) {
                if (obj.errormsg != undefined)
                    alert(obj.errormsg);
                return;
            }
            window.location.href = '/';
        });
    }
    catch (e) {
        alert(e);
    }
}

function sendLocalCmd(query) {
	return sendServerCmd("lcmd", query);
}

function onChromeFrameMissing() {
    alert("Internet Explorer doesn't support HTML5.\nIf you want to use ThinVNC with Internet Explorer, install Google ChromeFrame.");
    window.location.href = "http://google.com/chromeframe";
}

$(document).ready(function() {
    if (document.location.pathname != '/') {
        window.location.replace('/');
        return;
    }
    baseUrl = document.location.pathname;
    if (baseUrl.charAt(baseUrl.length - 1) != '/') {
        baseUrl = baseUrl + '/';
    }
    inputComputer = document.getElementById("computer");
    inputFullColor = document.getElementById("fullColor");
    inputControlMode = document.getElementById("controlMode");
    inputConnect = document.getElementById("connect");

    if (CFInstall != undefined) {
        CFInstall.check({
              preventPrompt : true,
              onmissing: onChromeFrameMissing
        });
    }

    $('#connect').click(function(event) {
        connect();
    });

});
