function multiply(s1, s2){
	var res = [];	
	try{
		if(s1[0].length != s2.length){
			throw "DifferentSizeException";
		}
		let sum = 0;
		for(i = 0; i < s2.length; i++){
			var vector = [];
			for(j = 0; j < res[i].length; j++){
				for(k = 0; k < s1[i].length; k++){
					sum = sum + (s1[i][k]*s2[k][j]);		
				}
				vector[j] = sum;
				sum = 0;
			}
			res[i] = vector;
		}
		
	}catch(e){
		logMyErrors(e);
	}

	return res;
}

function add(m1, m2){

	if (m1.length != m2.length){
		throw "DifferentSizeException";
	}
	else{
		try {
			for(i = 0; i < m1.length; i++){
				for(j = 0; j < m1[i].length; j++){
					m1[i][j] = m1[i][j] + m2[i][j]; 
				}
			}
		}
		catch(e){
			logMyErrors(e);
		}
	}
	return m1;
}

function subtract(m1, m2){
	
	if (m1.length != m2.length){
		throw "DifferentSizeException";
	}
	else{
		try {
			for(i = 0; i < m1.length; i++){
				for(j = 0; j < m1[i].length; j++){
					m1[i][j] = m1[i][j] - m2[i][j]; 
				}
			}
		}
		catch(e){
			logMyErrors(e);
		}
	}
	return m1;

}

function scalarProduct(m1, m2){
	var res = 0;
	if (m1.length != m2.length){
		throw "DifferentSizeException";
	}
	else{
		try {
			for(i = 0; i < m1.length; i++){
				res = res + (m1[i]*m2[i]); 
			}
		}
		catch(e){
			logMyErrors(e);
		}
	}
	return res;

}

let m = [1, 2, 6, 12];
let n = [4, 8, 4, 1];
var k = scalarProduct(m, n);
let a = document.querySelectorAll('h1');


k = k.toString();
console.log(k);
a[0].textContent = 'Produto Escalar: ' + k;
a[1].textContent = 'Brincando de Javascript';
