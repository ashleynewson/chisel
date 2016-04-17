var sliceLines = slicelet.lines;

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
for (var childPos in slicelet.children) {
    if (slicelet.children.hasOwnProperty(childPos)) {
        var childPosSplit = childPos.split('_');
        var line = parseInt(childPosSplit[0]);
        var name = childPosSplit[1];
        var linkInstPath;
        if (instPath == null) {
            linkInstPath = childPos;
        } else {
            linkInstPath = instPath + '/' + childPos;
        }
        links[line] += '<a href="' + source_html(slicelet.children[childPos].file) + '?inst=' + linkInstPath + '">' + name + '</a> ';
    }
}
for (var annotationPos in slicelet.annotations) {
    if (slicelet.annotations.hasOwnProperty(annotationPos)) {
        var line = parseInt(annotationPos.substring(0, annotationPos.indexOf('_')));
        var name = annotationPos.substring(annotationPos.indexOf('_')+1);
        var linkInstPath;
        if (instPath == null) {
            linkInstPath = annotationPos;
        } else {
            linkInstPath = instPath + '/' + annotationPos;
        }
        links[line] += '<a href="annotation.html?inst=' + linkInstPath + '">' + name + '</a> ';
    }
}
var margin = [];
for (i = 1; i < lines.length+1; i++) {
    margin.push('<span>' + links[i] + i + '</span>');
}

var sourcePre = document.getElementById("source-pre");
sourcePre.innerHTML = '<code class="numbering">' + margin.join('') + '</code>' + sourcePre.innerHTML + '<span style="clear:both;"></span>';

slicedSource = lines.join('\n');

document.getElementById("source").innerHTML = slicedSource;
