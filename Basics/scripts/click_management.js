function first_button(){
	alert("You pressed the first button!!!");
}

let x = 0;
let y = 100;
function add_circle(){
	var e = document.getElementById("background");
	var cont = e.getContext("2d");
	cont.arc(x, y, 10, 0, Math.PI*2);
	cont.fillStyle = "black";
	cont.fill();
	x = x + 50;
	y = y + 50;
}
