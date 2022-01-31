function TRIALpr(trialNrIn){
	// time reference for the first trial
	t = [Date.now()];

	// array to store the response times 
	respTime = [];

	// array to control when a box is clickable
	clickEnabled = [1];

	trialNr = [trialNrIn];
	TO = setTimeout(timedout, 2000,trialNr);
	tipo = [1];	

}

function TRIALprConf(trialNrIn){
	// time reference for the first trial
	t = [Date.now()];

	// array to store the response times 
	respTime = [];

	// array to control when a box is clickable
	clickEnabled = [1];

	trialNr = [trialNrIn];
	TO = setTimeout(timedout, 4000,trialNr);
	tipo = [3];	

}

function TRIAL(trialNrIn){
	// time reference for the first trial
	t = [Date.now()];

	// array to store the response times 
	respTime = [];

	// array to control when a box is clickable
	clickEnabled = [1];

	trialNr = [trialNrIn];	
	
	TO = setTimeout(timedout, 2000,trialNr);

	tipo = [2];
}

function TRIALConf(trialNrIn){
	// time reference for the first trial
	t = [Date.now()];

	// array to store the response times 
	respTime = [];

	// array to control when a box is clickable
	clickEnabled = [1];

	trialNr = [trialNrIn];	
	
	TO = setTimeout(timedout, 4000,trialNr);

	tipo = [4];
}


function onOptionClick(elmtG, boxNr, t, respTime, clickEnabled, trialNr, tipo){
	/* this function handles actions if input is given */

	if (clickEnabled[clickEnabled.length - 1] === 1){

		// get reaction Time
		var clickedRT = Date.now();

		// disable reaction to clicks
		clickEnabled.push(0);

		// first compute the response time
		respTime.push((clickedRT - t[t.length - 1]));

		// select element to change parameters
		var element = document.getElementById(elmtG);

		// change background color of selected element
		element.style.backgroundColor = "#A9A9A9";

		Shiny.onInputChange("selected", boxNr);
		Shiny.onInputChange("respTime", respTime);

		if (tipo == 1) {
			clearTimeout(TO);
			Shiny.onInputChange("trialNrpr", trialNr);
		} else if (tipo == 2){
			clearTimeout(TO);
			Shiny.onInputChange("trialExpNr", trialNr);
		}
		else if (tipo == 3){
			clearTimeout(TO);
			Shiny.onInputChange('trialConfNrpr', trialNr)
		}
		else if (tipo == 4){
			clearTimeout(TO);
			Shiny.onInputChange('trialConfNr', trialNr)
		}
	}
}


document.onkeydown = function(e) {
       switch (e.keyCode) {
        case 37:
		onOptionClick('opt1', 1, t, respTime, clickEnabled, trialNr, tipo);
		break;   
		case 40:
	 	onOptionClick('opt2', 2, t, respTime, clickEnabled, trialNr, tipo);
	 	break;   
        case 39:
		onOptionClick('opt3', 3, t, respTime, clickEnabled, trialNr, tipo);
		break;   
    }
};

function timedout(trialNr){
	clickEnabled = [0];	
	alertTimeout("Paso mucho tiempo",1500,tipo);		
};


function waiter(waitNr,trialITI,tipoITI){	
		if (tipoITI == 1) {
			setTimeout(function(){			
				Shiny.onInputChange("waitTrng", waitNr);
			}, trialITI);

		} else if (tipoITI == 2) {
			setTimeout(function(){	
				Shiny.onInputChange("waitExp", waitNr);
			}, trialITI);
		}
}


function alertTimeout(mymsg,mymsecs,tipo)
{
 var myelement = document.createElement("div");
myelement.setAttribute("style","background-color: Tomato;color:black; width: 180px;height: 100px;position: absolute;top:0;bottom:138px;left:0;right:0;margin:auto;border: 0px solid black;font-family:arial;font-size:22px;font-weight:bold;display: flex; align-items: center; justify-content: center; text-align: center; box-shadow: 0 2.8px 2.2px rgba(0, 0, 0, 0.034),  0 6.7px 5.3px rgba(0, 0, 0, 0.048),  0 12.5px 10px rgba(0, 0, 0, 0.06),  0 22.3px 17.9px rgba(0, 0, 0, 0.072),  0 41.8px 33.4px rgba(0, 0, 0, 0.086),  0 100px 80px rgba(0, 0, 0, 0.12);");
 myelement.innerHTML = mymsg;
 setTimeout(function(){
  myelement.parentNode.removeChild(myelement);
	Shiny.onInputChange("selected", -1);
	Shiny.onInputChange("respTime", 0);
		if (tipo == 1) {			
			Shiny.onInputChange("trialNrpr", trialNr);
		} else if (tipo == 2){			
			Shiny.onInputChange("trialExpNr", trialNr);
		}
		else if (tipo == 3){
			clearTimeout(TO);
			Shiny.onInputChange('trialConfNrpr', trialNr)
		}
		else if (tipo == 4){
			clearTimeout(TO);
			Shiny.onInputChange('trialConfNr', trialNr)
		}	
 },mymsecs);
 document.body.appendChild(myelement);
}