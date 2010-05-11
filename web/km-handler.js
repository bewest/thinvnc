var mouseX = -1;
var mouseY = -1;
var mouseMoved = false;

function clearMouse() {
    mouseMoved = false;
}

function sendMouse(x, y,button, action) {
    clearMouse();
    var url = baseUrl + "mouse?x=" + (x - getDeltaX()) + "&y=" + (y- getDeltaY()) + "&btn=" + button + "&action=" + action;
    $.get(url, function (data) { })
}

function sendMouseMove() {
    if (mouseMoved) {
        sendMouse(mouseX,mouseY,0,"move");
    }
    setTimeout(sendMouseMove, 100);
}

function hookKM() {
    $("#desk").mousedown(function (e) {
        sendMouse(e.pageX, e.pageY, e.button, "down");
        event.stopPropagation();
        event.preventDefault();
    });

    $("#desk").mouseup(function (e) {
        sendMouse(e.pageX, e.pageY, e.button, "up");
        event.stopPropagation();
        event.preventDefault();
    });

    $("#desk").mousemove(function (e) {
        if ((mouseX != e.pageX) || (mouseY != e.pageY)) {
            mouseX = e.pageX;
            mouseY = e.pageY;
            mouseMoved = true
        }
    });

    $("#desk").bind("contextmenu", function (e) {
        return false;
    });
}


