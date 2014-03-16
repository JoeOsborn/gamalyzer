var chunks = location.pathname.split("/");
var id = chunks[1];
var lev = chunks[2];
var performUndos = chunks[3] != "split";
var gameCode;
var baseState;
var frames = [];

var frameContainer = document.createElement("div");
frameContainer.id = "pzl_last_container";

var externalsReady = false;
var internalsReady = false;

//qua http://stackoverflow.com/a/11123701

// Load <script> elements for all uris
// Invoke the whenDone callback function after the last URI has loaded
function loadScripts(uris,whenDone) {
	if(!uris.length) {
		whenDone && whenDone();
	} else {
		for (var wait=[],i=uris.length;i--;) {
			var tag  = document.createElement('script');
			tag.type = 'text/javascript';
			tag.src  = uris[i];
			if (whenDone){
				wait.push(tag);
				tag.onload = maybeDone;
				tag.onreadystatechange = maybeDone; // For IE8-
			}
			document.head.appendChild(tag);
		}
	}
	function maybeDone() {
		if (this.readyState===undefined || this.readyState==='complete'){
			// Pull the tags out based on the actual element in case IE ever
			// intermingles the onload and onreadystatechange handlers for the same
			// script block before notifying for another one.
			for (var i=wait.length;i--;) {
				if (wait[i]==this) {
					wait.splice(i,1);
				}
				if (!wait.length) {
					whenDone();
				}
			}
		}
	}
}

function consolePrint(msg) {
	console.log(msg);
}

function setGameState(s,cmd) {
	//nop
	return;
}

loadScripts([
	"/resources/js/puzzlescript/globalVariables.js",
	"/resources/js/puzzlescript/colors.js",
	"/resources/js/puzzlescript/debug_replay.js",
	"/resources/js/puzzlescript/codemirror.js",
	"/resources/js/puzzlescript/parser.js",
	"/resources/js/puzzlescript/compiler.js"
], function() {
	externalsReady = true;
	tryToRun();
});

function makeFrame(cb) {
	var i = frames.length;
	var visFrame = document.createElement("IFRAME");
	visFrame.setAttribute("src", "/resources/html/pzl-last.html");
	visFrame.setAttribute("id", "pzl_last_"+i);
	visFrame.classList.add("pzl_last_vis_frame");
	visFrame.classList.add("pzl_last_unneeded");
	frames.push(visFrame);
	console.log("listen for LOAD");
	function initializeFrame(evt) {
		console.log("Load with gs "+baseState);
		visFrame.contentWindow.pzl_last.init(
			baseState,
			lev,
			performUndos
		);
		cb(visFrame);
	};
	if(visFrame.addEventListener){
		visFrame.addEventListener('load', initializeFrame, true);
	} else {
		visFrame.attachEvent('onload', initializeFrame);
	}
	frameContainer.appendChild(visFrame);
}

var dirNameInput = {
    'up':0,
    'left':1,
    'down':2,
    'right':3,
    'action':4
}

document.addEventListener("DOMContentLoaded", function(event) {
	document.body.appendChild(frameContainer);
	internalsReady = true;
	tryToRun();
});

function tryToRun() {
	if(!externalsReady || !internalsReady) { return; }
	console.log("ID is "+id+":"+lev);
	var httpClient = new XMLHttpRequest();
	httpClient.open('GET', "/resources/puzzlescript/"+id+".txt");
	httpClient.onreadystatechange = function() {
		if(httpClient.readyState != 4) {
			return;
		}
		gameCode = httpClient.responseText;
		baseState = compile(["restart"],gameCode);
		var pc = gamalyzer.ui.core.pivotCount();
		var remaining = pc;
		for(var i = 0; i < pc; i++) {
			makeFrame(function(_f) {
				remaining--;
				console.log("Need "+remaining+" more");
				if(remaining <= 0) {
					gamalyzer.ui.core.addSummarizer("pzl_last", updateFrames);
				}
			});
		}
	}
	httpClient.send();
};

function updateFrames(traces, tmin, tmax) {
	for(var i = 0; i < frames.length; i++) {
		var f = frames[i];
		if(i < traces.length) {
			if(f.classList.contains("pzl_last_unneeded")) {
				f.classList.remove("pzl_last_unneeded");
			}
			var visW = f.contentWindow;
			visW.pzl_last.update(traces[i].inputs.map(glz_to_pzl), tmin, tmax);
		} else {
			if(!f.classList.contains("pzl_last_unneeded")) {
				f.classList.add("pzl_last_unneeded");
			}
		}
	}
}

function glz_to_pzl(input) {
	var det = input.det;
	var vals = input.vals;
	var firstVal = input.vals[0];

	var move;
	//{vals:[...,player], det:[proc, movegroup, movechoice]}
	//system actions:
	//randomEntIdx:Idx
	//randomDir:dir
	//randomRuleIdx:idx
	//checkpoint
	//win
	if(det[2] == "select_entity") {
		return "randomEntIdx:"+firstVal;
	} else if(det[2] == "select_direction") {
		return "randomDir:"+dirNameMask[firstVal];
	} else if(det[2] == "select_rule") {
		return "randomRuleIdx:"+firstVal;
	} else if(det[2] == "checkpoint") {
		return "checkpoint";
	} else if(det[2] == "win") {
		return "win";
		//player actions:
		//quit
		//undo
		//restart
		//wait (autotick)
		//0 up
		//1 left
		//2 down
		//3 right
		//4 act
	} else if(det[1][0] == "move" && det[2] == "quit") {
		return "quit";
	} else if(det[1][0] == "move" && det[2] == "undo") {
		if(vals.length > 0) {
			//FIXME: ignoring multi-undo since it means the end of a trail. is that OK?
			return "wait";
		} else {
			return "undo";
		}
	} else if(det[1][0] == "move" && det[2] == "restart") {
		return "restart";
	} else if(det[1][0] == "move" && det[2] == "move") {
		if(firstVal == "wait") {
			return "wait";
		} else {
			return dirNameInput[firstVal];
		}
	} else {
		throw "Unrecognized input "+parts;
	}
}
