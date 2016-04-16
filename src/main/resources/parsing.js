function source_html(path) {
    return "source_" + path + ".html";
}

var query = window.location.search.replace('?', '');
var keyvals = query.split('&');
var options = {};
var i;
for (i = 0; i < keyvals.length; i++) {
    var keyval = keyvals[i].split('=');
    options[decodeURIComponent(keyval[0])] = decodeURIComponent(keyval[1]);
}

var inst = [];
if (typeof(options.inst) != "undefined") {
    inst = options.inst.split("/");
}

var slicelet = slice;

var humanInst = '<a href="source_' + slicelet.file + '.html">' + slicelet.file + '</a>';
var instPath = null;
for (i = 0; i < inst.length; i++) {
    if (mode === 'source') {
        slicelet = slicelet.children[inst[i]];
    } else if (mode === 'annotation') {
        if (i == inst.length - 1) {
            slicelet = slicelet.annotations[inst[i]];
        } else {
            slicelet = slicelet.children[inst[i]];
        }
    }
    if (instPath == null) {
        instPath = inst[i];
    } else {
        instPath = instPath + '/' + inst[i];
    }
    // Should really escape the file names!
    if (i == inst.length - 1) {
        if (mode === 'source') {
            humanInst = humanInst + ":" + inst[i] + "/" + slicelet.file;
        } else {
            humanInst = humanInst + ":" + inst[i];
        }
    } else {
        humanInst = humanInst + ":" + inst[i] + "/" + '<a href="' + source_html(slicelet.file) + '?inst=' + instPath + '">' + slicelet.file + '</a>';
    }
}

document.getElementById("inst").innerHTML = humanInst;
