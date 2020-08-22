#define POPULATION_SIZE 100
#define WORD_SIZE 20
#define MUTATIONS 2

PROGRAM genetic
	
	IMPLICIT NONE	
	
	! Declaring variables
	INTEGER, DIMENSION(POPULATION_SIZE) :: main_fitness
	INTEGER, DIMENSION(3) :: main_selection
	INTEGER :: i, individual_size, counter, iterations

	CHARACTER (LEN=WORD_SIZE) :: target_string
	CHARACTER (LEN=WORD_SIZE), DIMENSION(POPULATION_SIZE) :: population
	CHARACTER :: answer	

	! Choosing the target string
	PRINT *, "Choose the target string (20 chars max):"
	READ *, target_string
	
	PRINT *, "Fittest ", "Offspring ", "Target"
	
	individual_size = LEN(TRIM(target_string))

	! Creating the population	
	population = generate_initial_population(individual_size)

	! Calculating the fitness and selecting the fittest and least fittest individuals
	main_fitness = fitness(population, target_string, individual_size)
	main_selection = selection(main_fitness)

	counter = 0
	iterations = 0

	! Iterating through generations until the target string is reached
	DO WHILE (target_string /= population(main_selection(1)))
		IF (counter == 100) THEN
			counter = 0

			PRINT *, "Keep Going? (y/n)"
			READ *, answer
			
			IF (answer == 'n') THEN
				EXIT
			END IF
		END IF	
	
		population = crossover(population, main_selection, individual_size)
		population = mutation(population, main_selection, individual_size)

		PRINT *, population(main_selection(1)), population(main_selection(3)), target_string
	
		main_fitness = fitness(population, target_string, individual_size)
		main_selection = selection(main_fitness)	

		counter = counter + 1	
		iterations = iterations + 1
	END DO	
	
	PRINT *, " "
	PRINT *, "Final String: ", population(main_selection(1))
	PRINT *, "Iterations: ", iterations
CONTAINS


! Function that generates the initial population
! Receives the number of individuals in the population and the size of each individual
FUNCTION generate_initial_population (individual_size) RESULT (population)
	
	IMPLICIT NONE
	
	INTEGER :: individual_size
	INTEGER :: i, j
	INTEGER, DIMENSION(3) :: selection
	
	CHARACTER (LEN=WORD_SIZE), DIMENSION(POPULATION_SIZE) :: population
	CHARACTER(LEN=WORD_SIZE) :: string

	DO i = 1, POPULATION_SIZE
		string = ''
		DO j = 1, individual_size
			string = string(1:j)//ACHAR(97+MOD(IRAND(), 26))
		END DO
		population(i) = string(1:individual_size)
	END DO
	
END FUNCTION generate_initial_population

! Function that calculates the fitness
FUNCTION fitness (population, target_string, individual_size)
	
	IMPLICIT NONE

	INTEGER, DIMENSION(POPULATION_SIZE) :: fitness
	INTEGER :: individual_size
	INTEGER :: i, j

	CHARACTER (LEN=WORD_SIZE), DIMENSION(POPULATION_SIZE) :: population
	CHARACTER (LEN=WORD_SIZE) :: target_string

	DO i = 1, POPULATION_SIZE
		fitness(i) = 0
		DO j = 1, individual_size
			IF (population(i)(j:j) == target_string(j:j)) THEN
				fitness(i) = fitness(i) + 1
			END IF
		END DO
	END DO

END FUNCTION fitness

! Returns the 2 fittest and the least fittest in the fitness array
! Selection Process
FUNCTION selection (fitness)
	
	IMPLICIT NONE

	INTEGER, DIMENSION(POPULATION_SIZE) :: fitness
	INTEGER, DIMENSION(3) :: selection
	INTEGER :: i	

	selection(1) = 1
	selection(2) = 2
	selection(3) = 3

	DO i = 1, POPULATION_SIZE
		IF (fitness(i) > fitness(selection(1))) THEN
			selection(2) = selection(1)
			selection(1) = i
		ELSE IF (fitness(i) > fitness(selection(2))) THEN
			selection(2) = i
		ELSE IF (fitness(i) < fitness(selection(3))) THEN
			selection(3) = i
		END IF
	END DO

END FUNCTION selection

! Function that performs crossovers
FUNCTION crossover (population, selection, individual_size) RESULT (population1)

	IMPLICIT NONE

	INTEGER, DIMENSION(3) :: selection
	INTEGER :: individual_size
	INTEGER :: i, random, fit1, fit2

	CHARACTER (LEN=WORD_SIZE), DIMENSION(POPULATION_SIZE) :: population, population1
	CHARACTER(LEN=WORD_SIZE) :: offspring1, offspring2

	random = 1+MOD(IRAND(), individual_size)	

	offspring1 = TRIM(population(selection(2))(1:random))//population(selection(1))(random+1:individual_size)
	offspring2 = TRIM(population(selection(1))(1:random))//population(selection(2))(random+1:individual_size)

	offspring1 = offspring1(1:individual_size)
	offspring2 = offspring2(1:individual_size)

	DO i = 1, individual_size
		IF (offspring1(i:i) == target_string(i:i)) THEN
			fit1 = fit1 + 1
		END IF
		IF (offspring2(i:i) == target_string(i:i)) THEN
			fit2 = fit2 + 1
		END IF
	END DO

	IF (fit1 < fit2) THEN
		population(selection(3)) = offspring2
	ELSE
		population(selection(3)) = offspring1
	END IF

	population1 = population
	
END FUNCTION crossover


! Function that performs mutations
FUNCTION mutation (population, selection, individual_size) RESULT (population1)

	IMPLICIT NONE

	INTEGER, DIMENSION(3) :: selection
	INTEGER :: individual_size
	INTEGER :: i, num_mutations, pos_mutation

	CHARACTER (LEN=WORD_SIZE), DIMENSION(POPULATION_SIZE) :: population, population1

	num_mutations = MOD(IRAND(), MUTATIONS)

	DO i = 0, num_mutations
		pos_mutation = MOD(IRAND(), individual_size+1)
		population(selection(3)) = TRIM(population(selection(3))(1:pos_mutation-1))//&
					   ACHAR(97+MOD(IRAND(), 26))//&
					   TRIM(population(selection(3))(pos_mutation+1:individual_size))
	END DO
	
	population(selection(3)) = population(selection(3))(1:individual_size)

	population1 = population

END FUNCTION mutation

END PROGRAM genetic
