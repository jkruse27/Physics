function follow(e) {	
	s.style.left = (e.pageX - 5) + 'px';
	s.style.top = (e.pageY - 5) + 'px';
}

function mousePos(e){
	var t = document.getElementById("canvas");
	var x = t.getContext("2d");
	x.arc(e.pageX - 5, e.pageY - 5, 10,0, Math.PI*2);
	x.fillStyle = "black";
	x.fill();
	x.closePath();
}

document.addEventListener('click', mousePos, true);

var s = document.createElement('div');
s.style.position = 'absolute';
s.style.margin = '0';
s.style.padding = '5px';
s.style.border = '1px solid red';

window.onload = function() {
	document.body.appendChild(s);
	console.log(event.pageX, event.pageY);
	document.body.onmousemove = follow;
}

