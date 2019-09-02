
function submited(){
	var e = document.getElementsByName("username")[0];
	var f = document.getElementsByName("psw")[0];

	if(e.value === "halter123" && f.value === "1q2w3e4r5t"){
		window.location = "file:///home/debora/learning-javascript/Basics/parabens.html";
	}
	else{
		var x = document.getElementById("textinho");
		x.textContent = "Usuário ou senha incorretos";
	}
	
	var k = document.getElementsByName("gender");

	for( i in k){
		if(i.checked){
			console.log(i.value);
		}
	}
}

