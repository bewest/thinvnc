var running;
var basUrl;
var deskDiv;
var toolbar;
var ie;
var iphone;
var supportsCanvas;
var supportsZoom;
var supportsScale;
var scale = 1;
var scaled = true;
var startPending = true;
var jsonTimeoutValue = 5000;
var jsonTimeout;

sessionStatus = {
    id: 0,
    active: false,
    monitor: 0,
    monitorCount: 0,
    viewLeft: 0,
    viewTop: 0,
    viewWidth: 0,
    viewHeight: 0,
    mouseControl: false,
    kbdControl: false,
    quality: 90,
    pixelFormat: 0,
    grayscale: false,
    remotePointer: false
}

function getBodyHeight() {
    if (!ie) return window.innerHeight;
    else return document.body.offsetHeight;
}

function getBodyWidth() {
    if (!ie) return window.innerWidth;
    else return document.body.offsetWidth;
}

function getDeltaX() {
    var ww = getBodyWidth();
    if (ww < sessionStatus.viewWidth * scale) {
        return 0;
    }
    else {
        if (supportsScale) {
            return Math.round((ww - sessionStatus.viewWidth) / 2)
        } else if (ie) {
            return Math.round((ww - sessionStatus.viewWidth * scale) / 2);
        } else return Math.round((ww - sessionStatus.viewWidth * scale) / (2 * scale));
    }
}
function getDeltaY() {
    var wh = getBodyHeight();
    var top = 0;
    if ((wh - top) < sessionStatus.viewHeight * scale) {
        return top;
    }  
    else {
        if (supportsScale) {
            return Math.round(((wh - top) - sessionStatus.viewHeight) / 2) + top;
        } else if (ie) {
            return Math.round((wh - sessionStatus.viewHeight * scale) / 2)  + top;
        } else return Math.round(((wh - top) - sessionStatus.viewHeight * scale) / (2 * scale)) + top;
    }
}

function getScale() {
    if (!scaled) return 1;

    var top = 0;
    var c1 = (getBodyHeight() - top) / sessionStatus.viewHeight;
    var c2 = getBodyWidth() / sessionStatus.viewWidth;
    if ((c1 > 1) && (c2 > 1)) return 1;
    else {
        if (c2 < c1) return c2;
        else return c1;
    }
}

function zoomDesktop() {
    scale = getScale();
        
    deskDiv.style.marginLeft = getDeltaX() + 'px';
    deskDiv.style.marginTop = getDeltaY() + 'px';
    if (supportsScale) {
        deskDiv.style.MozTransform = "scale(" + scale + ")"; 
    } else deskDiv.style.zoom = scale;
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

function createCanvasIE(win) {
    var canvas = document.createElement("div");
    canvas.visibility = 'visible';
    canvas.display = 'block';
    canvas.style.position = 'absolute';
    canvas.style.left = (win.left - sessionStatus.viewLeft + getDeltaX()) + 'px';
    canvas.style.top = (win.top - sessionStatus.viewTop + getDeltaY()) + 'px';
    canvas.style.zIndex = win.zidx;
    canvas.width = win.width;
    canvas.height = win.height;
    canvas.id = "canvas" + win.hwnd;
    deskDiv.appendChild(canvas);
    return canvas;
}

function discardElement(element) {
    var garbageBin = document.getElementById('IELeakGarbageBin');
    if (!garbageBin) {
        garbageBin = document.createElement('DIV');
        garbageBin.id = 'IELeakGarbageBin';
        garbageBin.style.display = 'none';
        document.body.appendChild(garbageBin);
    }

    // move the element to the garbage bin
    garbageBin.appendChild(element);
    garbageBin.innerHTML = '';
}

function processWindowIE(win) {
    var canvasid = "canvas" + win.hwnd;
    var canvas = document.getElementById(canvasid);
    if (!canvas) {
        canvas = createCanvasIE(win);
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
        canvas.style.left = (win.left-sessionStatus.viewLeft) + 'px';
        canvas.style.top = (win.top-sessionStatus.view) + 'px';
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
                        discardElement(item);
                        canvas.removeChild(item);
                    }
                }
            });
            if (!img) img = createImg(win, imgpart, canvas);
            img.src = imgpart.img;
        })
    }
};

function createCanvas(win) {
    var canvas = document.createElement("canvas");
    if (typeof(G_vmlCanvasManager) != 'undefined')  // explorercanvas
        canvas = G_vmlCanvasManager.initElement(canvas);
    
    canvas.visibility = 'visible';
    canvas.display = 'block';
    canvas.style.position = 'absolute';
    canvas.style.left = (win.left-sessionStatus.viewLeft)+'px';
    canvas.style.top = (win.top-sessionStatus.viewTop)+'px';
    canvas.style.zIndex = win.zidx;
    canvas.width = deskDiv.offsetWidth;
    canvas.height = deskDiv.offsetHeight;
    canvas.id = "canvas" + win.hwnd;
    deskDiv.appendChild(canvas);
    return canvas;
}

function processWindow(win) {
    var canvasid = "canvas" + win.hwnd;
    var canvas = document.getElementById(canvasid);
    if (!canvas) {
        canvas = createCanvas(win);
    }

    deskDiv.style.marginLeft = getDeltaX() + 'px';
    deskDiv.style.marginTop = getDeltaY() + 'px';
    if ((win.width == 0) || (win.height == 0)) {
        canvas.style.visibility = "hidden";
        canvas.style.zIndex = -1;
    } else {
        canvas.style.left = (win.left-sessionStatus.viewLeft) + 'px';
        canvas.style.top = (win.top-sessionStatus.viewTop) + 'px';
        canvas.style.clip = 'rect(0px,' + win.width + 'px,' + win.height + 'px,0px)';

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
                context.drawImage(img, imgpart.x , imgpart.y, img.width, img.height);
            }
            img.src = imgpart.img;
        })
    }
};

function sendServerCmd(cmd, query) {
    var url = baseUrl + cmd + "?" + query + "&id=" + sessionStatus.id;
    try {
        $.getJSON(url, function(obj) {
            sessionStatus.id = obj.id;
            if (sessionStatus.id == '-1') {
                window.location.href = '/';
                return;
            }
            sessionStatus.active = obj.active;
            sessionStatus.monitor = obj.monitor;
            sessionStatus.monitorCount = obj.monitorCount;
            sessionStatus.viewLeft = obj.viewLeft;
            sessionStatus.viewTop = obj.viewTop;
            sessionStatus.viewWidth = obj.viewWidth;
            sessionStatus.viewHeight = obj.viewHeight;
            sessionStatus.mouseControl = obj.mouseControl;
            sessionStatus.kbdControl = obj.kbdControl;
            sessionStatus.quality = obj.quality;
            sessionStatus.pixelFormat = obj.pixelFormat;
            sessionStatus.grayscale = obj.grayscale;
            sessionStatus.remotePointer = obj.remotePointer;

            deskDiv.style.marginLeft = getDeltaX() + 'px';
            deskDiv.style.marginTop = getDeltaY() + 'px';
            deskDiv.style.width = sessionStatus.viewWidth + 'px';
            deskDiv.style.height = sessionStatus.viewHeight + 'px';
            deskDiv.style.clip = 'rect(0px,' + sessionStatus.viewWidth + 'px,' + sessionStatus.viewHeight + 'px,0px)';


            if (sessionStatus.active) {
                document.getElementById("state").textContent = 'pause';
                $('#state').removeClass('resume').addClass('pause');
                reload();
            } else {
                document.getElementById("state").textContent = 'resume';
                $('#state').removeClass('pause').addClass('resume');
            }

            if (obj.ticket!='') {
                $('#mouse').addClass('hidden');
                $('#monitor').addClass('hidden');
                $('#cursor').addClass('hidden');
                $('#state').addClass('hidden');
            } else {
                if (sessionStatus.active && sessionStatus.mouseControl) {
                    $('#mouse').addClass('pressed');
                    setTimeout(sendMouseMove, 100);
                } else $('#mouse').removeClass('pressed');

                if (sessionStatus.active && sessionStatus.remotePointer) {
                    $('#cursor').addClass('pressed');
                } else $('#cursor').removeClass('pressed');

                if (sessionStatus.monitorCount>1) {
                    $('#monitor').removeClass('hidden');
                }
            }

            if (sessionStatus.pixelFormat==1) {
                document.getElementById("colors").textContent = 'FULL COLOR';
            } else {
                document.getElementById("colors").textContent = '256 COLOR';
            }

            
            resetToolbarPosition();
            if (startPending) setTimeout(start, 1);
        });
    }
    catch (e) {
        alert(e);
    }
}

function sendCmd(query) {
	return sendServerCmd("cmd", query);
}

function sendLocalCmd(query) {
	return sendServerCmd("lcmd", query);
}

function sendParams (mouseControl,remotePointer,pixelFormat){
    sendCmd("cmd=params&mouseControl=" + mouseControl + "&kbdControl=" + mouseControl + "&remotePointer=" + remotePointer+ "&pixelFormat=" + pixelFormat);
}

function toogleMouseControl() {
    sendParams(!sessionStatus.mouseControl,sessionStatus.remotePointer,sessionStatus.pixelFormat);
}

function toogleRemotePointer() {
    sendParams(sessionStatus.mouseControl, !sessionStatus.remotePointer,sessionStatus.pixelFormat);
}

function swicthPixelFormat() {
    var pf = 0;
    if (sessionStatus.pixelFormat==0) pf = 1;
    sendParams(sessionStatus.mouseControl, sessionStatus.remotePointer,pf);
}

function setMonitor(m) {
    if (m>=sessionStatus.monitorCount) m = -1;
    sendCmd("cmd=params&monitor=" + m);
}

function disconnect() {
    sendLocalCmd("cmd=disconnect");
}

function stop() {
    sendCmd("cmd=stop");
}

function start() {
    startPending = false;
    var cmd = "cmd=start"
    if (ie) cmd = cmd + "&embeddedImage=false";
    cmd = cmd + "&mouseControl=" + sessionStatus.mouseControl + "&kbdControl=" + sessionStatus.mouseControl + "&remotePointer=" + sessionStatus.remotePointer;
    sendCmd(cmd);
}

function setDestAddr(destAddr) {
    var cmd = escape("cmd=params&destAddr=" + destAddr);
    sendCmd(cmd);
}

function refresh() {
    sendCmd("cmd=refresh");
}

function queryStatus() {
    sendLocalCmd("cmd=queryStatus");
}

function onJsonTimeout() {
//    alert('timeout');
    setTimeout(reload, 1);
}

function reload() {
    scale = getScale();
    var url = baseUrl + "json?id=" + sessionStatus.id;
    clearTimeout(jsonTimeout);
    jsonTimeout = setTimeout(onJsonTimeout,jsonTimeoutValue);
    $.getJSON(url, function (obj) {
        try {
            if (obj.status==9) {
                alert("This session has been terminated");
                disconnect();
                return;
            }

            if ((obj==null) ||(obj.status==2)) {
                setTimeout(reload, 1);
                return;
            }

            $.each(obj.windows, function (i, win) {
                if (supportsCanvas) processWindow(win);
                else processWindowIE(win);
            })

            for (var i = deskDiv.children.length - 1; i >= 0; i--) {

                var found = false;
                var canvas = deskDiv.children[i];

                $.each(obj.windows, function (i, win) {
                    var canvasid = "canvas" + win.hwnd;
                    if (canvas.id == canvasid) {
                        found = true;
                    }
                })
                if (!found) {
                    canvas.style.display = "none";
                    canvas.innerHTML = '';
                    deskDiv.removeChild(canvas);
                }
            }

            if (sessionStatus.remotePointer) {
                deskDiv.style.cursor = 'url(point.cur)';
            }

            if (!sessionStatus.remotePointer && !sessionStatus.mouseControl) {
                deskDiv.style.cursor = 'default';
            }

            if (sessionStatus.mouseControl && !sessionStatus.remotePointer) {
                deskDiv.style.cursor = obj.cursor;
            }
            zoomDesktop();
            if (obj.status==3) {
                start();
            }
            else if (sessionStatus.active)setTimeout(reload, 1);
        }
        catch (err) {
            if (sessionStatus.active) {
                setTimeout(reload, 1);
            }
        }
    });
}

function resetToolbarPosition() {
    var ww = getBodyWidth();
    toolbar.style.left = ((ww - toolbar.offsetWidth) / 2)+'px';
    toolbarHandle.style.top = (toolbar.offsetHeight + toolbar.offsetTop - 1)+'px';
    toolbarHandle.style.left = (toolbar.offsetLeft + toolbar.offsetWidth - 18) + 'px';
}

function hideToolbar() {
    toolbar.style.top = -toolbar.offsetHeight + 'px';
    resetToolbarPosition();
    zoomDesktop();
}

function showToolbar() {
    toolbar.style.top = 0 + 'px';
    resetToolbarPosition();
    zoomDesktop();
}

$(document).ready(function() {
    var agent = navigator.userAgent.toLowerCase();
    iphone = ((agent.indexOf('iphone')!=-1) || (agent.indexOf('ipod')!=-1) || (agent.indexOf('ipad')!=-1));

    deskDiv = document.getElementById("desk");
    toolbar = document.getElementById("toolbar");     

    supportsZoom = ((deskDiv.style.MozTransform == undefined) && !iphone);
    supportsCanvas = !!document.createElement('canvas').getContext;
    supportsScale = ((deskDiv.style.MozTransform !== undefined) && !iphone);
    ie = !supportsCanvas;
    
    resetToolbarPosition();

    baseUrl = document.location.pathname;
    if (baseUrl.charAt(baseUrl.length - 1) != '/') {
        baseUrl = baseUrl + '/';
    }

    var pinned = 0;
    $('#toolbarHandle').click(function(event) {
        pinned = pinned ^ 1;

        if (toolbar.offsetTop < 0) {
            showToolbar();
        } else {
            hideToolbar();
        }
    });

    $('#mouse').click(function(event) {
        toogleMouseControl();
    });

    $('#cursor').click(function(event) {
        toogleRemotePointer();
    });

    $('#refresh').click(function(event) {
        start();
    });

    $('#monitor').click(function(event) {
        setMonitor(sessionStatus.monitor+1);
    });
    $('#monitor').addClass('hidden');

    $('#scale').click(function(event) {
        scaled = !scaled;
        if (scaled) $('#scale').addClass('pressed');
        else $('#scale').removeClass('pressed');
        zoomDesktop();
    });

    $('#state').click(function(event) {
        if (sessionStatus.active) stop();
        else start();
    });

    $('#colors').click(function(event) {
        swicthPixelFormat();
    });

    $('#disconnect').click(function(event) {
        disconnect();
    });

    if (!supportsScale && !supportsZoom) {
        scaled = 0;
        $('#scale').addClass('hidden');
    } else {
        scaled = 1;
        $('#scale').addClass('pressed');

        $(window).bind('resize', function() {
            resetToolbarPosition();
            zoomDesktop();
        });
    }

    hookKM();
    $.ajaxSetup({
      timeout: jsonTimeoutValue
    });    
    window.setTimeout('queryStatus();', 1);
});
