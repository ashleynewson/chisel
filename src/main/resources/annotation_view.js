function getBits(binData, first, until) {
    bits = Array(until - first);
    for (var i = first; i < until; i++) {
        bits[i - first] = (binData.charCodeAt(i / 8) >> (i % 8)) & 1;
    }
    return bits;
}

function highlightBits(state, mask) {
    bitString = "";
    complexMask = false;
    for (var i = 0; i < state.length; i++) {
        if (mask[i] === 0) {
            complexMask = true;
            break;
        }
    }
    if (complexMask) {
        for (var i = 0; i < state.length; i++) {
            if (mask[i] === 1) {
                bitString += "<b>" + (state[i]).toString() + "</b>";
                // bitString += (state[i]).toString();
            } else {
                bitString += (state[i]).toString();
            }
        }
    } else {
        bitString += "<b>";
        for (var i = 0; i < state.length; i++) {
            bitString += (state[i]).toString();
        }
        bitString += "</b>"
    }
    return bitString;
}

function highlightMask(mask) {
    bitString = "";
    complexMask = false;
    for (var i = 0; i < mask.length; i++) {
        if (mask[i] === 0) {
            complexMask = true;
            break;
        }
    }
    if (complexMask) {
        for (var i = 0; i < mask.length; i++) {
            if (mask[i] === 1) {
                bitString += "<b>*</b>";
                // bitString += (state[i]).toString();
            } else {
                bitString += "*";
            }
        }
    } else {
        bitString += "<b>" + ("*".repeat(mask.length)) + "</b>"
    }
    return bitString;
}

var title = document.getElementById("title");
var annotationPre = document.getElementById("annotation-pre");

if (slicelet.type == "bool") {
    title.innerHTML = slicelet.name;
    annotationPre.innerHTML = "In slice?: " + (slicelet.in ? "Yes" : "No");
} else if (slicelet.type == "data") {
    title.innerHTML = slicelet.name;
    output = "";
    state = window.atob(slicelet.state);
    mask = window.atob(slicelet.mask);

    var width = slicelet.width;
    var size = slicelet.size;
    for (var word = 0; word < size; word++) {
        output += highlightBits(getBits(state, word * width, (word+1) * width), getBits(mask, word * width, (word+1) * width)) + "\n"
    }

    var margin = [];
    for (i = 0; i < size; i++) {
        margin.push('<a name="' + i.toString(16) + '">0x' + i.toString(16) + '</a>');
    }
    annotationPre.innerHTML = '<code class="numbering">' + margin.join('') + '</code><code class="dump">' + output + '</code><span style="clear:both;"></span>';
} else if (slicelet.type == "mask") {
    title.innerHTML = slicelet.name;
    output = "";
    mask = window.atob(slicelet.mask);

    var width = slicelet.width;
    var size = slicelet.size;
    for (var word = 0; word < size; word++) {
        output += highlightMask(getBits(mask, word * width, (word+1) * width)) + "\n"
    }

    var margin = [];
    for (i = 0; i < size; i++) {
        margin.push('<a name="' + i.toString(16) + '">0x' + i.toString(16) + '</a>');
    }
    annotationPre.innerHTML = '<code class="numbering">' + margin.join('') + '</code><code class="dump">' + output + '</code><span style="clear:both;"></span>';
}
