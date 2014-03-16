var chunks = location.pathname.split("/");
var id = chunks[1];
var lev = chunks[2];
var gameCode;
var visFrame = document.createElement("IFRAME");
visFrame.setAttribute("src", "/resources/html/pzl-last.html");
visFrame.setAttribute("id", "pzl_last");
visFrame.setAttribute("class", "vis_frame");
visFrame.setAttribute("style", "width:320px; height:260px;");

var dirNameInput = {
    'up':0,
    'left':1,
    'down':2,
    'right':3,
    'action':4
}

function glz_to_plz(input) {
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

document.addEventListener("DOMContentLoaded", function(event) {
	document.body.appendChild(visFrame);
	var visW = visFrame.contentWindow;
	console.log("ID is "+id+":"+lev);
	var httpClient = new XMLHttpRequest();
	httpClient.open('GET', "/resources/puzzlescript/"+id+".txt");
	httpClient.onreadystatechange = function() {
		if(httpClient.readyState != 4) {
			return;
		}
		gameCode = httpClient.responseText;
		visW.document.addEventListener("DOMContentLoaded", function(evt) {
			visW.pzl_last.init(
				gameCode,
				lev
			);
		});
		gamalyzer.ui.core.addSummarizer("pzl_last", function(traces, tmin, tmax) {
			visW.pzl_last.update(traces[0].inputs.map(glz_to_plz), tmin, tmax);
		});
	}
	httpClient.send();
});

