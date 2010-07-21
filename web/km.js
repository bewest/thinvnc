var mouseX = -1;
var mouseY = -1;
var mouseMoved = false;

function clearMouse() {
    mouseMoved = false;
}

function sendKey(key, action) {
    if (sessionStatus.active && sessionStatus.kbdControl) {
        var url = baseUrl + "cmd?cmd=keyb&key=" + key + "&action=" + action + "&id=" + sessionStatus.id;
        $.get(url, function (data) { })
    }
}

function sendMouse(x, y, button, action) {
    if (sessionStatus.active && sessionStatus.mouseControl) {
        clearMouse();
        if (supportsScale) {
            x = Math.round((x - (getBodyWidth() - sessionStatus.viewWidth * scale) / 2) / scale);
            y = Math.round((y - (getBodyHeight() - sessionStatus.viewHeight * scale) / 2) / scale);
        } else {      
            x = Math.round(x / scale) - getDeltaX();
            y = Math.round(y / scale) - getDeltaY();
        }
        x = x + sessionStatus.viewLeft;
        y = y + sessionStatus.viewTop;
        var url = baseUrl + "cmd?cmd=mouse&x=" + x + "&y=" + y  + "&btn=" + button + "&action=" + action + "&id=" + sessionStatus.id;
        $.get(url, function (data) { })
    }
}

function sendMouseMove() {
    if (sessionStatus.active && sessionStatus.mouseControl) {
        if (mouseMoved) {
            sendMouse(mouseX,mouseY,0,"move");
        }
        setTimeout(sendMouseMove, 100);
    }
}

function hookKM() {
    $("#desk").mousedown(function (e) {
        sendMouse(e.pageX, e.pageY, e.button, "down");
        e.stopPropagation();
        e.preventDefault();
    });

    $("#desk").mouseup(function (e) {
        sendMouse(e.pageX, e.pageY, e.button, "up");
        e.stopPropagation();
        e.preventDefault();
    });

    $("#desk").mousemove(function (e) {
        if ((mouseX != e.pageX) || (mouseY != e.pageY)) {
            mouseX = e.pageX;
            mouseY = e.pageY;
            mouseMoved = true
        }
    });

    $(document).keydown(function (e) {
        sendKey(e.keyCode, "down");
        e.stopPropagation();
        e.preventDefault();
    });

    $(document).keyup(function (e) {
        sendKey(e.keyCode, "up");
        e.stopPropagation();
        e.preventDefault();
    });

    $("#desk").bind("contextmenu", function (e) {
        return false;
    });
}


