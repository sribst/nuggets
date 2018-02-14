# ANORACS : An Online, Rapid and Collaborative Stylesheet

### Build the project

#### How Build the project :

### First Solution use Docker :
```
 * Install Docker on your machine
 * Start docker demon (see on docker website for more details)
 * Set the Dockerfile with your own name user and password of gitlab(moule.informatique)
 * Start the script ./run_docker.sh from the root directories (actual or naive) this launch a virtual machine with all dependencies.
 *Finaly it will launch the script test.sh of the project.
```

### Seconde Solution build on your own machine :
```
   *install Ocaml >= 4.04
   *install opam
   *opam update && opam upgrade && opam install angstrom oasis lwt	   
   * For the first time :
   *oasis setup
   *make
```

* If for any modification in the `src` folder :

```
make
```

* If you change the `_oasis` configuration file :

```
oasis setup
make
```

### For launch the project 
```
	1 ./ws <file.csv> <final> <updates> <view0> notes :
	      -first arguments is the file .csv
	      -seconde argument : the file which is the final output after evaluation
	      -third argument : file with all updates to applies
	      -fourth arguments : the partial evaluation file view0 (give before the modification of change.txt )
	
	2 At the root of th directorie naive or actual there is a test.sh file wich be executed to generate
	  automaticly a new sheet evaluated. The number of lignes and colums it may be modified directly into the file.
	  
```

### Structure

#### We have Three distincts directories in this projet (model,control,toolkit) in actual and naive

     -naive : containe the first version of this project with an algorithme on graph.
     -actual : implement an concurent algorithme for evaluate spreedSheet

     -The main file is ws.ml
       
#### model

     Model contains all data structure needed for are manipulated in the application

##### model provided
* **CellValue.ml** : Datas types of cell and basic function of manipulation 
* **Coord.ml**     : Datas types of coordinate and basic function of manipulation
* **Log.ml**	   : Only available on naive version. Give the possibility
  		     to save the changes and theres consequences.
#### Control

     Control contains all algorithmes of evaluation of sheet and Parser

##### controler provided
* **EvalConc.ml** : Concurrent evaluation on sheet and give the possibility to
  		    focus for the modifiaction (chages...) It implement a new algorithme based on wheel calculus.
* **EvalWheel.ml** : Evaluation (non-use) wich show an exemple of use of librairie Lwt (wich use wheel calculus).
* **ParserChange.ml** : Parse file of update and return a list of modification.
* **ParserSheet.ml** :  Parse a csv sheet and record cells with a function give in arguments

#### Toolkit

     toolkit used by the application

##### toolkit provided
* **Computation.ml** : This file implements a wheel calculus this algorithme is used by EvalConc
* **FileInterface.ml** : Interface of writes in files
* **IOChannel.ml**     : internal librairie using channel (used by FileInterface)
* **Serial.mli**       : function of serialisation and deserialisation on integer
  in Bytes + useful function for serialized data.
* **Utils.ml**         : Some generics function
* **Wheel.ml**	       : This is Wheel calculus used by EevalWheel 


### Dependencies

* menhir
* ocamlbuild
* angstrom
* ocaml mpi bindings (an implementation for mpi such as openmpi)
* Lwt

### Authors

* Daniel El Ouraoui
* Sylvain Ribstein
* Elie Canonici
* Paul Laforgue

### About

This is a project made at Paris Diderot University.
