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

var sliceDetails = slice;
var humanInst = '<a href="source_' + slice.file + '.html">' + slice.file + '</a>';
var instPath = null;
for (i = 0; i < inst.length; i++) {
    sliceDetails = sliceDetails.children[inst[i]];
    if (instPath == null) {
        instPath = inst[i];
    } else {
        instPath = instPath + '/' + inst[i];
    }
    // Should really escape the file names!
    humanInst = humanInst + ":" + inst[i] + "/" + '<a href="' + source_html(sliceDetails.file) + '?inst=' + instPath + '">' + sliceDetails.file + '</a>';
}

document.getElementById("inst").innerHTML = humanInst;


var sliceLines = sliceDetails.lines;

var source = document.getElementById("source").innerHTML;
var lines = source.split('\n');

for (i = 0; i < sliceLines.length; i++) {
    var lineNo = sliceLines[i] - 1; // 1 indexing to 0 indexing
    lines[lineNo] = '<span class="in-slice">' + lines[lineNo] + '</span>';
}

var links = [];
for (i = 1; i < lines.length+1; i++) {
    links[i] = "";
}
for (var childPos in sliceDetails.children) {
    if (sliceDetails.children.hasOwnProperty(childPos)) {
        var childPosSplit = childPos.split('_');
        var line = parseInt(childPosSplit[0]);
        var name = childPosSplit[1];
        var linkInstPath;
        if (instPath == null) {
            linkInstPath = childPos;
        } else {
            linkInstPath = instPath + '/' + childPos;
        }
        links[line] += '<a href="' + source_html(sliceDetails.children[childPos].file) + '?inst=' + linkInstPath + '">' + name + '</a> ';
    }
}
var margin = [];
for (i = 1; i < lines.length+1; i++) {
    margin.push('<span>' + links[i] + i + '</span>');
}
var sourcePre = document.getElementById("source-pre");
sourcePre.innerHTML = '<code class="numbering">' + margin.join('') + '</code>' + sourcePre.innerHTML + '<span style"clear:both;"></span>';

slicedSource = lines.join('\n');

document.getElementById("source").innerHTML = slicedSource;
