(function (window) {
    'use strict';
    
    // Your starting point. Enjoy the ride!
    
    window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};
    jsb.ws.onmessage = (evt) => eval(evt.data);
    jsb.debug = false;
    jsb.tick = -1;
    jsb.render = (t,o,m) => {
        jsb.tick = t;
        jsb.view = o;
	let tmpl = document.querySelector('#todos-list-template').innerHTML;
	let el = document.querySelector('#todos-list');
	// Brute force the list
	el.innerHTML = _.template(tmpl)({tasks:o.tasks,visibility:o.visibility});
	// Update other features
	let items = o.tasks.length;
	let left  = _.filter(o.tasks,(i) => !i.completed.value).length;
	let completed = items - left
	document.querySelector(".clear-completed")
	    .setAttribute("earthquake-clickbox",o.deletecomplete);
	document.querySelector(".clear-completed").style.display =
	    completed == 0?"none":null;
	document.querySelector(".footer").style.display =
	    items == 0?"none":null;
	document.querySelector(".main").style.display =
	    items == 0?"none":null;
	document.querySelector(".todo-count").innerHTML =
	    "<strong>" + left + "<strong> item" + (left == 1?"":"s") + " left"
	_.map(document.querySelectorAll(".filters a"),i => {
	    if (i.innerText == o.visibility) {
		i.classList.add("selected")
	    } else {
		i.classList.remove("selected")
	    }
	})
	document.querySelector(".main input").checked = o.checkAll.value
	document.querySelector(".main input")
	    .setAttribute("earthquake-checkbox",
			  o.checkAll.recv.toString()
			 )

    }
    // Add an event listener that runs down the
    // target's path, to find the first selector match.
    jsb.on = (eventName,selector,callback) => {
	document
	    .querySelector('body')
	    .addEventListener(eventName,(e) => {
		console.log("on",eventName,selector,e);
		let el = _.find(e.path,(i) =>
				typeof(i.matches) == "function" &&
				i.matches(selector));
		// We bind this to the element, aka jquery.
		el && callback.call(el,e,el);
	    }, false)
    }
    document.addEventListener("DOMContentLoaded",() => {
	window.addEventListener("hashchange",(e) => {
	    let route = null;
	    switch (window.location.hash) {
	    case "":
	    case "#/":
		route = "All"; break;
	    case "#/active":
		route = "Active"; break;
	    case "#/completed":
		route = "Completed"; break;
	    default:
	    }
	    route && jsb.event({id:jsb.view.router, value: route, tick:jsb.tick})
	})
	jsb.on("keydown","input.new-todo",(e,el) => {
	    if (e.keyCode == 13) {
		jsb.event({id:jsb.view.createOnEnter,
			   value:el.value,
			   tick:jsb.tick});
		el.value = "";
	    }	    
	})
	jsb.on("keydown","li.editing > input[earthquake-textbox]",(e,el) => {
	    console.log("keydown",el,e,e.keyCode);
	    if (e.keyCode == 13) {
		jsb.event({id:parseInt(el.getAttribute('earthquake-textbox')),		
			   value:el.value,
			   tick:jsb.tick});
		el.parentNode.classList.remove("editing");
	    }
	    if (e.keyCode == 27) {
		// Restore value and edit status
		el.value = el.getAttribute('earthquake-value')
		el.parentNode.classList.remove("editing");
	    }
	})
	jsb.on("focusout","li.editing > input[earthquake-textbox]",(e,el) => {
	    jsb.event({id:parseInt(el.getAttribute('earthquake-textbox')),		
		       value:el.value,
		       tick:jsb.tick});
	    el.parentNode.classList.remove("editing");
	})
	jsb.on("click","input[earthquake-checkbox]",(e,el) => {
	    jsb.event({id:parseInt(el.getAttribute('earthquake-checkbox')),
		       value:el.checked,
		       tick:jsb.tick
		      })
	})
	jsb.on("click","button[earthquake-clickbox]",(e,el) => {
	    jsb.event({id:parseInt(el.getAttribute('earthquake-clickbox')),
		       value:[],
		       tick:jsb.tick
		      })
	})
	jsb.on("dblclick","li[data-item]",(e,el) => {
	    // We enable this editbox locally.
	    // Any server-side changes will
	    // revert this edit without saving.
	    el.classList.add("editing");
	    let input = el.querySelector("input[earthquake-textbox]")
	    input.focus();
	    input.setSelectionRange(input.value.length,input.value.length)
	})
    });
    
})(window);
