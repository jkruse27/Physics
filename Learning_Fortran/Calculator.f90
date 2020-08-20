PROGRAM calculator

	IMPLICIT NONE

	! Declaring variables
	REAL :: a, b, res
	CHARACTER :: op
	LOGICAL :: invalid

	! Reading the values and operation

	invalid = .true.	

	DO WHILE (invalid)
	
		PRINT *, "Enter the operation:"
		READ *, op
		
		IF (op /= '+' .and. op /= '*' .and. op /= '/' .and. op /= '-' ) THEN
			PRINT *, "Invalid Entry!"
		ELSE
			invalid = .false.
		END IF

	END DO

	PRINT *, "Enter the first number:"
	READ *, a

	PRINT *, "Enter the second number:"
	READ *, b
	 
	SELECT CASE (op)
		CASE ("+")
			res = a+b
			PRINT *, a, "+", b, "=", res  
		CASE ("-")
			res = a-b
			PRINT *, a, "-", b, "=", res  
		CASE ("*")
			res = a*b
			PRINT *, a, "*", b, "=", res  
		CASE ("/")
			res = a/b
			PRINT *, a, "/", b, "=", res  
	END SELECT

END PROGRAM calculator
