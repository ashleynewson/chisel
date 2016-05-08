var links = [];

function addLink(line, html, name) {
    if (typeof(links[line]) != "undefined") {
        links[line].push({name: name, html: html});
    } else {
        links[line] = [{name: name, html: html}];
    }
}

function compareNames(a, b) {
    var x=a.name;
    var y=b.name;
    var shortest = x.length < y.length ? x.length : y.length;
    var longest  = x.length > y.length ? x : y;
    var lastNonNumber = -1;
    var i;
    for (i = 0; i < shortest; i++) {
        if (isNaN(parseInt(x[i]))) {
            if (x[i] === y[i]) {
                lastNonNumber = i;
            } else {
                // They are unrelated
                return x<y?-1:x>y?1:0;
            }
        }
    }
    for (; i < longest.length; i++) {
        if (isNaN(parseInt(longest[i]))) {
            // They are unrelated
            return x<y?-1:x>y?1:0;
        }
    }
    // They are similar, but have a different numeric ending.
    if (x.length === y.length) {
        return x<y?-1:x>y?1:0;
    } else {
        return x.length < y.length ? -1 : 1;
    }
}

function getLinks(line) {
    if (typeof(links[line]) != "undefined") {
        var order_links = links[line].sort(compareNames);
        var linkStr = "";
        for (var i = 0; i < order_links.length; i++) {
            linkStr += order_links[i].html
        }
        return linkStr;
    } else {
        return "";
    }
}

// extraLinks are for items without line association
var extraLinks = "";

var sliceLines = [];

for (var childPos in slicelet.children) {
    if (slicelet.children.hasOwnProperty(childPos)) {
        var child = slicelet.children[childPos];
        var name = childPos.substring(childPos.indexOf('_')+1);
        var linkInstPath;
        if (instPath == null) {
            linkInstPath = childPos;
        } else {
            linkInstPath = instPath + '/' + childPos;
        }
        var lineStr = childPos.substring(0, childPos.indexOf('_'));
        var link = '<a' + (child.in ? ' class="in"' : '') + ' href="' + source_html(slicelet.children[childPos].file) + '?inst=' + linkInstPath + '">' + name + '</a> ';
        if (lineStr === "?") {
            extraLinks += link;
        } else {
            var line = parseInt(lineStr);
            addLink(line, link, name);
            if (child.in) {
                sliceLines.push(line);
            }
        }
    }
}
for (var annotationPos in slicelet.annotations) {
    if (slicelet.annotations.hasOwnProperty(annotationPos)) {
        var annotation = slicelet.annotations[annotationPos];
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
        var classes = [];
        if (annotation.in) {
            classes.push("in");
        }
        if (annotation.hide) {
            classes.push("hide");
        }
        var link = '<a' + (classes.length > 0 ? ' class="' + classes.join(' ') + '"' : '') + ' title="' + annotation.name + '"' + ' href="annotation.html?inst=' + linkInstPath + '">' + name + '</a> ';
        if (lineStr === "?") {
            extraLinks += link;
        } else {
            var line = parseInt(lineStr);
            addLink(line, link, name);
            if (annotation.in && !annotation.hide) {
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
    lines[lineNo] = '<span class="in">' + lines[lineNo] + '</span>';
}

var margin = [];
for (i = 1; i < lines.length+1; i++) {
    margin.push('<span class="line"><span class="hideable">' + getLinks(i) + '</span><a name="' + i + '">' + i + '</a></span>');
}

var sourcePre = document.getElementById("source-pre");
sourcePre.innerHTML = '<code class="numbering"><span id="margin">' + margin.join('') + '</span></code>' + sourcePre.innerHTML + '<span style="clear:both;"></span>';

var content = document.getElementById("content");
if (extraLinks != "") {
    content.innerHTML += '<span id="extralinks"><p>The following items could not be associated with lines in the file:<br>' + extraLinks + '</p></span>';
}

function showhide_details() {
    var show = document.getElementById("showhide").checked;
    document.getElementById("margin").className = show ? "" : "hidespan";
    document.getElementById("extralinks").style = show ? "" : "display:none;";
}
content.innerHTML += '<form class="no-print">List nodes and child modules <input id="showhide" type="checkbox" checked onchange="showhide_details()"></form>';

slicedSource = lines.join('\n');

document.getElementById("source").innerHTML = slicedSource;
