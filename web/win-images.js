
    var running;
    var basUrl;
    var deskDiv;

    function getDeltaX() {
        return 0;// deskDiv.offsetLeft;
    }

    function getDeltaY() {
        return 0; //deskDiv.offsetTop;
    }


    function createImg(win, imgpart, div) {
        var img = document.createElement("img");
        img.visibility = 'visible';
        img.display = 'block';
        img.style.position = 'absolute';
        img.style.zIndex = win.zidx;
        img.left = imgpart.x;
        img.top = imgpart.y;
        img.style.left = imgpart.x + 'px';
        img.style.top = imgpart.y + 'px';
        div.appendChild(img);
        return img;
    }
    function createCanvas(win) {
        var canvas = document.createElement("div");
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
                if ((win.width < canvas.width) ||
                    (win.height < canvas.height)) {
                    $('img', '#' + canvas.id).each(function (index, item) {
                        if (((item.left + item.width) > win.width) ||
                            ((item.top + item.height) > win.height)) {
                            var aux = "rect(" + item.top + "px " + (win.width - item.left) + "px " + (win.height - item.top) + "px " + item.left + "px)";
                            item.style.clip = aux;
                        }
                    })
                }
                canvas.width = win.width;
                canvas.height = win.height;
            }
            canvas.style.left = (win.left + getDeltaX()) + 'px';
            canvas.style.top = (win.top + getDeltaY()) + 'px';
            canvas.style.visibility = "visible";
            canvas.style.zIndex = win.zidx;
        }

        if (win.imgs != null) {
            $.each(win.imgs, function (i, imgpart) {
                var img = null;
                $('img', '#' + canvas.id).each(function (index, item) {
                    if ((item.left >= imgpart.x) && (item.top >= imgpart.y) &&
                        ((item.width + item.left) <= (imgpart.w + imgpart.x)) &&
                        ((item.height + item.top) <= (imgpart.y + imgpart.h))) {
                        if ((item.left == imgpart.x) && (item.top == imgpart.y) &&
                            (item.left == imgpart.x) && (item.top == imgpart.y)) {
                            img = item;
                        } else {
                            canvas.removeChild(item);
                        }
                    }
                });
                if (!img) img = createImg(win, imgpart, canvas);
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
                    processWindow(win);
                })

                var canvases = document.getElementsByTagName('div');
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
                        canvas.style.visibility = "hidden";
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
