class Circle{

	constructor(x = 0, y = 0, vx = 0, vy = 0, ax = 0, ay = 0, r = 1, m = 1){
		this.x_position = x;
		this.y_position = y;
		this.x_velocity = vx;
		this.y_velocity = vy;
		this.x_acceleration = ax;
		this.y_acceleration = ay;
		this.radius = r;
		this.mass = m;
	}

	setXP(new_value){
		x_position = new_value;
	}

	setYP(new_value){
		y_position = new_value;
	}

	setXV(new_value){
		x_velocity = new_value;
	}

	setYV(new_value){
		y_velocity = new_value;
	}


	setXA(new_value){
		x_acceleration = new_value;
	}

	setXA(new_value){
		y_acceleration = new_value;
	}

}

class Physics{
	
	constructor(canvas, acceleration = 9.81){
		var date = new Date();
		this.d = date;
		this.acceleration = acceleration;
		this.t = 0.017;
		this.gameArea = canvas;
		this.context = this.gameArea.getContext("2d");
	}

	calculatePosition(object, time){
		object.x_position = object.x_position + (object.x_velocity*time) + ((object.x_acceleration*time*time)/2);
		object.y_position = object.y_position + (object.y_velocity*time) + (((object.y_acceleration + this.acceleration)*time*time)/2);
		return object;
	}

	calculateVelocity(object, time){
		object.y_velocity = object.y_velocity + ((this.acceleration + object.y_acceleration) * time);
		object.x_velocity = object.x_velocity + (object.x_acceleration * time);
		return object;
	}
	
	isInteracting(obj1, obj2){
		var ret = false;
		if(((obj1.x_position+obj1.radius)<=(obj2.x_position-obj2.radius))&&((obj1.y_position+obj1.radius)<=(obj2.y_position-obj2.radius))){
			ret = true;
		}
		if(((obj2.x_position+obj2.radius)<=(obj1.x_position-obj1.radius))&&((obj2.y_position+obj2.radius)<=(obj1.y_position-obj1.radius))){
			ret = true;
		}
		if(((obj1.x_position+obj1.radius)<=(obj2.x_position-obj2.radius))&&((obj2.y_position+obj2.radius)<=(obj1.y_position-obj1.radius))){
			ret = true;
		}
		if(((obj2.x_position+obj2.radius)<=(obj1.x_position-obj1.radius))&&((obj1.y_position+obj1.radius)<=(obj2.y_position-obj2.radius))){
			ret = true;
		}

		return ret;
	}

	update(c1){
		c1 = this.calculatePosition(c1, this.t);
		c1 = this.calculateVelocity(c1, this.t);
		this.context.arc(c1.x_position, c1.y_position, c1.radius, 0, 2*Math.PI);
		this.context.fillStyle = "black";
		this.context.fill();
		return c1
	}

	clear(){
		this.context.clearRect(0, 0, 1000, 1000);
		this.context.beginPath();
	}

}


var canvas = document.getElementById("game");
var c1 = new Circle(100, 100, 100, 0, 0, 0, 10, 20);
var c2 = new Circle(200, 100, -100, 0, 0, 0, 10, 20)
var phys = new Physics(canvas);

var id = setInterval(iteration, 17);

function iteration(){
	phys.clear();
	phys.update(c1);
	phys.update(c2);
	console.log(phys.isInteracting(c2,c1));
}

