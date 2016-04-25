var links = [];

function addLink(line, html) {
    if (typeof(links[line]) != "undefined") {
        links[line] += html;
    } else {
        links[line] = html;
    }
}

function getLinks(line) {
    if (typeof(links[line]) != "undefined") {
        return links[line];
    } else {
        return "";
    }
}

// extraLinks are for items without line association
var extraLinks = "";

var sliceLines = [];

for (var childPos in slicelet.children) {
    if (slicelet.children.hasOwnProperty(childPos)) {
        var childPosSplit = childPos.split('_');
        var name = childPosSplit[1];
        var linkInstPath;
        if (instPath == null) {
            linkInstPath = childPos;
        } else {
            linkInstPath = instPath + '/' + childPos;
        }
        var link = '<a href="' + source_html(slicelet.children[childPos].file) + '?inst=' + linkInstPath + '">' + name + '</a> ';
        if (childPosSplit[0] === "?") {
            extraLinks += link;
        } else {
            var line = parseInt(childPosSplit[0]);
            addLink(line, link);
            if (slicelet.children[childPos].in) {
                sliceLines.push(line);
            }
        }
    }
}
for (var annotationPos in slicelet.annotations) {
    if (slicelet.annotations.hasOwnProperty(annotationPos)) {
        var name = annotationPos.substring(annotationPos.indexOf('_')+1);
        if (name == "") {
            name = "?";
        }
        var linkInstPath;
        if (instPath == null) {
            linkInstPath = annotationPos;
        } else {
            linkInstPath = instPath + '/' + annotationPos;
        }
        var lineStr = annotationPos.substring(0, annotationPos.indexOf('_'));
        var link = '<a href="annotation.html?inst=' + linkInstPath + '">' + name + '</a> ';
        if (lineStr === "?") {
            extraLinks += link;
        } else {
            var line = parseInt(lineStr);
            addLink(line, link);
            if (slicelet.annotations[annotationPos].in) {
                sliceLines.push(line);
            }
        }
    }
}

// var sliceLines = slicelet.lines;

var source = document.getElementById("source").innerHTML;
var lines = source.split('\n');

for (i = 0; i < sliceLines.length; i++) {
    var lineNo = sliceLines[i] - 1; // 1 indexing to 0 indexing
    lines[lineNo] = '<span class="in-slice">' + lines[lineNo] + '</span>';
}

var margin = [];
for (i = 1; i < lines.length+1; i++) {
    margin.push('<span>' + getLinks(i) + i + '</span>');
}

var sourcePre = document.getElementById("source-pre");
sourcePre.innerHTML = '<code class="numbering">' + margin.join('') + '</code>' + sourcePre.innerHTML + '<span style="clear:both;"></span>';

if (extraLinks != "") {
    var content = document.getElementById("content");
    content.innerHTML += '<p>The following items could not be associated with lines in the file:<br>' + extraLinks + '</p';
}

slicedSource = lines.join('\n');

document.getElementById("source").innerHTML = slicedSource;
