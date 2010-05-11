var running;
var basUrl;
var deskDiv;

function getDeltaX() {
    return deskDiv.offsetLeft;
}

function getDeltaY() {
    return deskDiv.offsetTop;
}

function createCanvas(win) {
    var canvas = document.createElement("canvas");
    canvas.visibility = 'visible';
    canvas.display = 'block';
    canvas.style.position = 'absolute';
    canvas.style.left = (win.left + getDeltaX()) + 'px';
    canvas.style.top = (win.top + getDeltaY()) + 'px';
    canvas.style.zIndex = win.zidx;
    canvas.width = win.width;
    canvas.height = win.height;
    canvas.id = "canvas" + win.hwnd;
    document.body.appendChild(canvas);
    return canvas;
}

function processWindow(win) {
    var canvasid = "canvas" + win.hwnd;
    var canvas = document.getElementById(canvasid);
    if (!canvas) {
        canvas = createCanvas(win);
    }

    if ((win.width == 0) || (win.height == 0)) {
        canvas.style.visibility = "hidden";
        canvas.style.zIndex = -1;
    } else {
        if ((win.width != canvas.width) ||
			(win.height != canvas.height)) {
            canvas.width = win.width;
            canvas.height = win.height;
        }
        canvas.style.left = (win.left + getDeltaX()) + 'px';
        canvas.style.top = (win.top + getDeltaY()) + 'px';
        canvas.style.visibility = "visible";
        canvas.style.zIndex = win.zidx;
    }

    if (win.imgs != null) {
        var context = canvas.getContext('2d');
        if (!context || !context.drawImage) {
            alert("no hay canvas");
            return;
        };

        $.each(win.imgs, function (i, imgpart) {
            var img = new Image();
            img.id = "imgcanvas";
            img.style.display = "none";
            img.onload = function () {
                context.drawImage(img, imgpart.x, imgpart.y, img.width, img.height);
            }
            img.src = imgpart.img;
        })
    }
};

function stop() {
    running = false;
}

function reload(reset) {
    if (reset) running = true;
    if (!running) return;

    baseUrl = document.location.pathname;
    if (baseUrl.charAt(baseUrl.length - 1) != '/') {
        baseUrl = baseUrl + '/';
    }

    var url = baseUrl + "json";
    if (reset) url = url + "?reset";
    $.getJSON(url, function (obj) {
        try {
            $.each(obj.windows, function (i, win) {
                if (win.width > deskDiv.clientWidth) {
                    deskDiv.style.width = win.width + 'px';
                    deskDiv.style.marginLeft = (-win.width / 2) + 'px';
                }
                if (win.height > deskDiv.clientHeight) {
                    deskDiv.style.height = win.height + 'px';
                    deskDiv.style.marginTop = (-win.height / 2) + 'px';
                }
            })

            $.each(obj.windows, function (i, win) {
                processWindow(win);
            })

            var canvases = document.getElementsByTagName('canvas');
            for (var i = canvases.length - 1; i >= 0; i--) {
                var found = false;
                var canvas = canvases[i];
                $.each(obj.windows, function (i, win) {
                    var canvasid = "canvas" + win.hwnd;
                    if (canvas.id == canvasid) {
                        found = true;
                    }
                })
                if (!found) {
                    canvas.style.display = "none";
                    document.body.removeChild(canvas);
                }
            }
            setTimeout("reload(false)", 10);
        }
        catch (err) {
        }
    });
}

$(document).ready(function () {
    deskDiv = document.getElementById("desk");
    hookKM();
    reload(true);
    setTimeout(sendMouseMove, 100);
});
