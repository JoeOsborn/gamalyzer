var chunks = location.pathname.split("/");
var id = chunks[1];
var lev = chunks[2];
var performUndos = chunks[3] != "split";
var gameCode;
var baseState;
var frames = [];
var xmlns = "http://www.w3.org/2000/svg";
var svg, frameLine;

var tickMillis = 500;

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
	visFrame.setAttribute("src", "/resources/html/pzl-seq.html");
	visFrame.setAttribute("id", "pzl_seq_"+i);
	visFrame.classList.add("pzl_seq_vis_frame");
	visFrame.classList.add("pzl_seq_unneeded");
	frames.push(visFrame);
	console.log("listen for LOAD");
	function initializeFrame(evt) {
		console.log("Load with gs "+baseState);
		visFrame.contentWindow.pzl_seq.init(
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
	internalsReady = true;
	document.body.appendChild(frameContainer);
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
					gamalyzer.ui.core.addSummarizer("pzl_seq", updateFrames);
					window.setInterval(tickFrames, tickMillis);
				}
			});
		}
	}
	httpClient.send();
};

var displayTraces = [];
var maxFrame = -1;
var minFrame = -1;
var currentFrame = -1;

function updateFrames(traces, tmin, tmax) {
	minFrame = tmin-1;
	maxFrame = tmax;
	currentFrame = minFrame;
	displayTraces = traces.map(function(t) {
		return t.inputs.map(glz_to_pzl).slice(0,tmax+1);
	});
	resetFrames();
}

function resetFrames() {
	for(var i = 0; i < frames.length; i++) {
		var f = frames[i];
		if(i < displayTraces.length) {
			if(f.classList.contains("pzl_seq_unneeded")) {
				f.classList.remove("pzl_seq_unneeded");
			}
			var visW = f.contentWindow;
			visW.pzl_seq.reset_to(minFrame < 0 ? [] : displayTraces[i].slice(0,minFrame+1));
		} else {
			if(!f.classList.contains("pzl_seq_unneeded")) {
				f.classList.add("pzl_seq_unneeded");
			}
		}
	}
	currentFrame = minFrame;
	moveFrameLine();
}

function tickFrames() {
	if(currentFrame >= maxFrame) {
		resetFrames();
	} else {
		currentFrame++;
		for(var i = 0; i < displayTraces.length; i++) {
			var f = frames[i];
			var visW = f.contentWindow;
			var trace = displayTraces[i];
			if(currentFrame >= 0 && currentFrame < trace.length) {
				visW.pzl_seq.tick(trace[currentFrame],true);
			}
		}
		moveFrameLine();
	}
}

function moveFrameLine() {
	if(!svg) {
		svg = document.getElementById("svg");
	}
	if(!svg) { return; }
	if(!frameLine) {
		frameLine = document.createElementNS(xmlns,"line");
		frameLine.classList.add("pzl_seq_frame_line");
		frameLine.setAttribute("x1", 0);
		frameLine.setAttribute("x2", "400px");
		frameLine.setAttribute("y1", 0);
		frameLine.setAttribute("y2", 0);
		frameLine.setAttribute("stroke-width", 2);
		frameLine.setAttribute("stroke", "red");
		svg.appendChild(frameLine);
	}
	var visibleLine = currentFrame;
//	console.log('show line at '+visibleLine+' for min ' + minFrame + ' max '+ maxFrame);
	var y = gamalyzer.ui.core.tToY(visibleLine);
	frameLine.setAttribute("y1", y);
	frameLine.setAttribute("y2", y);
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
