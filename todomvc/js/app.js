(function (window) {
    'use strict';
    
    // Your starting point. Enjoy the ride!
    
    window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};
    jsb.ws.onmessage = (evt) => eval(evt.data);
    jsb.debug = true;
    jsb.tick = 0;
    jsb.uniq = 0;
    jsb.render = (t,o) => {
        jsb.tick = t;
        jsb.view = o;
        console.log(o);
	  // For debugging
//	  jsb.renders.generic(document.querySelector('#target'),o);
	  // Main rendering
	  document.querySelector('#raw').innerHTML = "<code>" + JSON.stringify(o) + "</code>"	   
    }
    jsb.enter = (ix) => {
	let value = document.querySelector('.new-todo');
	jsb.event({id:jsb.view.createOnEnter.id,value:value.value,tick:jsb.tick});
	value.value = "";
    }
    // Generic for debugging. Will need to encapsulate.
    document.addEventListener("DOMContentLoaded",() => {
	console.log("DOM Loaded");
	document.querySelector('.new-todo').addEventListener("keydown",(e) => {
	    if (e.keyCode == 13) {
		jsb.enter(null);
	    }
	});
    })
    
    jsb.renders = {};
    jsb.renders.Array = (el,o) => {
	console.log("Array",el,o);
	var q = ':scope>ul>li'; // Does not work in IE/Edge
	var ch = el.querySelectorAll(q);
	if (ch.length != o.length) {
	    // No need to do this if the list size is the same
	    var t = document.querySelector('#list-template').innerHTML;
	    el.innerHTML = _.template(t)({list:o});
	    ch = el.querySelectorAll(q)
	}
	_.each(ch,(el,i) => { jsb.renders.generic(el,o[i]) })
    }
    jsb.renders.Object = (el,o) => {
	console.log("Object",el,o);
	el.innerHTML = "<table style=\"border: 0; padding: 0; border-spacing:0; border-collapse:collapse\"><tbody></tbody></table>";
        let tab = el.querySelector("tbody");
        let ch = "{";
        let last = "";
        for (let a in o) last = a;
        for (let a in o) {
            let uniq = jsb.uniq++
            tab.insertAdjacentHTML('beforeEnd',"<tr>" +
				   "<td valign=\"top\" style=\"padding:0\"><code>" + ch + "</code></td>" +
				   "<th align=\"right\" valign=\"top\" style=\"padding:0\"><code>\"<span id=\"uniq-X-" + uniq + "\"></span>\":&nbsp;</code></th>" +
				   "<td id=\"uniq-" + uniq + "\" style=\"padding:0\"></td>" +
				   "</tr>");
            el.querySelector("#uniq-X-" + uniq).textContent = a;
            let sub = el.querySelector("#uniq-" + uniq);             
            jsb.renders.generic(sub,o[a]);
            ch = ",";
        }
        tab.insertAdjacentHTML('beforeEnd',"<tr><td valign=\"top\"><code>}</code></td></tr>");
    }
    jsb.renders.unit = (el,o) => {
        el.innerHTML = "<button></button>&nbsp;<code style=\"font-size: xx-small\"></code>"
        let button = el.querySelector("button");
	el.querySelector("button").textContent = "Click";
        button.addEventListener("click",function(e) {
            console.log("click",e);
            jsb.event({id:o.id,value:[],tick:jsb.tick})
        })
	el.querySelector("code").textContent = JSON.stringify(o);
    }
    jsb.renders.bool = (el,o) => {
        el.innerHTML = "<button></button><button></button>&nbsp;<code style=\"font-size: xx-small\"></code>"
	el.querySelectorAll("button")[0].textContent = "True";
        el.querySelectorAll("button")[0].addEventListener("click",function(e) {
            jsb.event({id:o.id,value:true,tick:jsb.tick})
        })
	el.querySelectorAll("button")[1].textContent = "False";
        el.querySelectorAll("button")[1].addEventListener("click",function(e) {
            jsb.event({id:o.id,value:false,tick:jsb.tick})
        })
	el.querySelector("code").textContent = JSON.stringify(o);
    }
    jsb.renders.text = (el,o) => {
        el.innerHTML = "<button></button><input></input>&nbsp;<code style=\"font-size: xx-small\"></code>"
	el.querySelector("button").textContent = "Text";
        let input = el.querySelector("input")
        el.querySelector("button").addEventListener("click",function(e) {
            jsb.event({id:o.id,value:input.value,tick:jsb.tick})
        })
	el.querySelector("code").textContent = JSON.stringify(o);
    }
    jsb.renders.double = (el,o) => {
        el.innerHTML = "<button></button><input type=\"number\"></input>&nbsp;<code style=\"font-size: xx-small\"></code>"
	el.querySelector("button").textContent = "Double";
	el.querySelector("code").textContent = JSON.stringify(o);
    }
    jsb.renders.generic = (el,o) => {
	console.log("generic",el,o);
	if (o instanceof Array) {
	    return jsb.renders.Array(el,o);
	}
	if (!o) {
            el.innerHTML = "<code>" + JSON.stringify(o) + "</code>"
            return;
	}
	if (o.type && jsb.renders[o.type] instanceof Function) {
	    return jsb.renders[o.type](el,o);
	}
	if (o instanceof Object) {
	    return jsb.renders.Object(el,o);
	}
	el.innerHTML = "<code>" + JSON.stringify(o) + "</code>"
	return;
    }
    jsb.renders.Double = (el,o) => {
        var q = 'div.slider';
	var ch = el.querySelectorAll(q);
	if (ch.length == 0) {
	    // Only render HTML the first time around
	    var t = document.querySelector('#slider-template').innerHTML;
	    el.innerHTML = _.template(t)(o);
            el.querySelector('input.slider').oninput = function(e) {
		jsb.event({slide:parseInt(this.getAttribute("data-event-id")),
			   value:parseFloat(this.value),
			   tick:jsb.tick})
	    }
	}
        el.querySelector('input.slider').value = o.value;
        el.querySelector('p.the-text').textContent = o.value;
        el.querySelector('progress.progress').setAttribute('value',o.value);
	el.querySelector('input.slider').setAttribute("data-event-id",o.event);
    }
    
})(window);
