MODULE physics

	! Constants
	REAL, PARAMETER :: g = 8.8876827E-10 ! In (au)^3(days)^-2(Earths mass)^-1
	INTEGER, PARAMETER :: dt = 1 	! In days 
	INTEGER, PARAMETER :: num_elements = 9

	! Body description
	TYPE Body
		CHARACTER(LEN=20) :: name
		REAL :: mass ! Relative to Earths
		REAL, DIMENSION(3) :: pos, vel, acc
	END TYPE
	
	CONTAINS

	! Recalculates the positions of the body from its velocity 
	! and initial position
	SUBROUTINE getPosition(planet)
		
		IMPLICIT NONE

		TYPE(Body) :: planet
		INTEGER i

		DO i = 1, 3		
			planet%pos(i) = planet%pos(i) + (planet%vel(i)*dt)
		END DO

	END SUBROUTINE getPosition 

	! Recalculates the velocities of the body from its acceleration 
	! and initial velocity
	SUBROUTINE getVelocity(planet)
		
		IMPLICIT NONE

		TYPE(Body) :: planet
		INTEGER :: i
		
		DO i = 1, 3		
			planet%vel(i) = planet%vel(i) + (planet%acc(i)*dt)
		END DO
		
	END SUBROUTINE getVelocity

	! Updates the accelerations of each element in the system 
	! through their interactions using Newton Laws of Gravitation
	SUBROUTINE getAcceleration(system)
		
		IMPLICIT NONE
		
		TYPE(Body), DIMENSION(num_elements) :: system
		INTEGER :: i, j, k
		REAL :: force, distance, force_x, force_y, force_z

		CALL cleanAccelerations(system)

		DO i = 1, num_elements
			DO j = i+1, num_elements
				distance = ((system(i)%pos(1)-system(j)%pos(1))**2+&
					    (system(i)%pos(2)-system(j)%pos(2))**2+&
					    (system(i)%pos(3)-system(j)%pos(3))**2)
				force = (g*(system(i)%mass)*(system(j)%mass))/(distance**0.5)

				force_x = force*(system(j)%pos(1)-system(i)%pos(1))
				force_y = force*(system(j)%pos(2)-system(i)%pos(2))
				force_z = force*(system(j)%pos(3)-system(i)%pos(3))
	
				system(i)%acc(1) = system(i)%acc(1) + (force_x/(system(i)%mass))
				system(j)%acc(1) = system(j)%acc(1) - (force_x/(system(j)%mass))

				system(i)%acc(2) = system(i)%acc(2) + (force_y/(system(i)%mass))
				system(j)%acc(2) = system(j)%acc(2) - (force_y/(system(j)%mass))

				system(i)%acc(3) = system(i)%acc(3) + (force_z/(system(i)%mass))
				system(j)%acc(3) = system(j)%acc(3) - (force_Z/(system(j)%mass))
			END DO
		END DO	

	END SUBROUTINE getAcceleration

	! Resets all accelerations to 0 
	SUBROUTINE cleanAccelerations(system)
		
		IMPLICIT NONE
		
		TYPE(Body), DIMENSION(num_elements) :: system
		INTEGER :: i

		DO i = 1, num_elements
			system(i)%acc = (/0,0,0/)
		END DO	

	END SUBROUTINE cleanAccelerations

END MODULE physics


PROGRAM solarsystem
	USE physics

	IMPLICIT NONE

	INTEGER :: simulation_time, current_time = 0	
	INTEGER :: i	

	TYPE(Body) :: sun, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune
	TYPE(Body), DIMENSION(num_elements) :: system

	PRINT *, "How many days do you want to simulate?"
	READ *, simulation_time

	! Setting Initial Conditions

	sun%name = "Sun"
	sun%mass = 333000
	sun%pos = (/-5.697149190139680E-3,6.687054319491834E-3,7.678597011434522E-5/)
	sun%vel = (/-7.658704054018353E-6,-4.578191601872708E-6,2.239049320860100E-07/)
	sun%acc = (/0,0,0/)

	mercury%name = "Mercury"
	mercury%mass = 0.0553
	mercury%pos = (/-3.605819634631013E-1,9.654498548371143E-2,3.997382057659207E-2/)
	mercury%vel = (/-1.274181104180660E-2,-2.607368899625286E-2,-9.618937105003081E-4/)
	mercury%acc = (/0,0,0/)

	venus%name = "Venus"
	venus%mass = 0.815
	venus%pos = (/6.792714852032707E-1,2.421024443386603E-1,-3.621961123445487E-2/)
	venus%vel = (/-6.650480387887962E-3,1.903322149818538E-2,6.448119750038513E-4/)
	venus%acc = (/0,0,0/)

	earth%name = "Earth"
	earth%mass = 1
	earth%pos = (/8.621088354773110E-1,-5.129180815619578E-1,9.600281448317349E-5/)
	earth%vel = (/8.549570846580982E-3,1.469895244162376E-2,-3.526463495765726E-7/)
	earth%acc = (/0,0,0/)

	mars%name = "Mars"
	mars%mass = 0.107
	mars%pos = (/1.346998699112539,-2.827891907236710E-1,-3.917280549445472E-2/)
	mars%vel = (/3.456679504049717E-3,1.487476592478984E-2,2.270378852417711E-4/)
	mars%acc = (/0,0,0/)

	jupiter%name = "Jupiter"
	jupiter%mass = 317.83
	jupiter%pos = (/2.194228912483959,-4.640839434476283,-2.984010836489017E-2/)
	jupiter%vel = (/6.729928690619951E-3,3.583695512817071E-3,-1.653948245511375E-4/)
	jupiter%acc = (/0,0,0/)

	saturn%name = "Saturn"
	saturn%mass = 95.162
	saturn%pos = (/4.895808732208842,-8.717040700207031,-4.333660839039700E-2/)
	saturn%vel = (/4.554455709810249E-3,2.716742276323908E-3,-2.284333301851185E-4/)
	saturn%acc = (/0,0,0/)

	uranus%name = "Uranus"
	uranus%mass = 14.536
	uranus%pos = (/1.566944459455544E+1,1.208475729849421E+1,-1.581162858251014E-1/)
	uranus%vel = (/-2.430698175832576E-3,2.931158212264652E-3,4.240546236021609E-5/)
	uranus%acc = (/0,0,0/)

	neptune%name = "Neptune"
	neptune%mass = 17.147
	neptune%pos = (/2.938139753440592E+1,-5.635759499285917,-5.610657096339521E-1/)
	neptune%vel = (/5.708362534528513E-4,3.101410747425306E-3,-7.741821372056113E-5/)
	neptune%acc = (/0,0,0/)

	system = (/sun, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune/)

	DO WHILE (current_time <= simulation_time)
		CALL getAcceleration(system)
		DO i = 1, num_elements
			CALL getVelocity(system(i))
			CALL getPosition(system(i))

			PRINT *, system(i)%name, " ", system(i)%pos(1), " " , system(i)%pos(2), " ", system(i)%pos(3)
		END DO
		current_time = current_time + dt
	END DO
END PROGRAM solarsystem
