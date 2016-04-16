function getBits(binData, first, until) {
    bits = Array(until - first);
    for (var i = first; i < until; i++) {
        bits[i - first] = (binData.charCodeAt(i / 8) >> (i % 8)) & 1;
    }
    return bits;
}

function highlightBits(state, mask) {
    bitString = "";
    for (var i = 0; i < state.length; i++) {
        if (mask[i] === 1) {
            bitString += "<b>" + (state[i]).toString() + "</b>";
            // bitString += (state[i]).toString();
        } else {
            bitString += (state[i]).toString();
        }
    }
    return bitString;
}

if (slicelet.type == "mem") {
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
    var annotationPre = document.getElementById("annotation-pre");
    annotationPre.innerHTML = '<code class="numbering">' + margin.join('') + '</code><code class="dump">' + output + '</code><span style="clear:both;"></span>';
}
