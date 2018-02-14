#define BUFSIZE 1024
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <err.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>

/* Le parseur csv */
#include "serialLib_csvparser.h"

/*** Binding Ocaml **/
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>


/** Data Types of fomula and value **/
typedef struct formula{
  int c1;
  int r1;
  int c2;
  int r2;
  int valeur;
} formula;

typedef struct value_s{
  int valeur;
} value_s;

/*** Parsing libray **/

void format_value(value_s *v,char *string_value){
  v->valeur     = atoi(string_value);
}

void format_value_marque(value_s *v,int marque){
  v->valeur     = -marque;
}

void format_formula(formula *f,char **string_formula){
  f->c1     = atoi(string_formula[0]);
  f->r1     = atoi(string_formula[1]);
  f->c2     = atoi(string_formula[2]);
  f->r2     = atoi(string_formula[3]);
  f->valeur = atoi(string_formula[4]);
}

/*C substring function: It returns a pointer to the substring */
char *substring(char *string, int position, int length) 
{
  char *pointer;
  int c;
 
  pointer = malloc(length); 
  if (pointer == NULL)
    {
      printf("Unable to allocate memory.\n");
      exit(1);
    } 
  for (c = 0 ; c < length ; c++)
    {
      *(pointer+c) = *(string+position-1);      
      string++;   
    }
  *(pointer+c) = '\0'; 
  return pointer;
}

/*C substring function: It returns a pointer to the substring */
char *substring_no_last_element(char *string, int position, int length) 
{
  char *pointer;
  int c;
 
  pointer = malloc(length); 
  if (pointer == NULL)
    {
      printf("Unable to allocate memory.\n");
      exit(1);
    } 
  for (c = 0 ; c < length-3 ; c++)
    {
      *(pointer+c) = *(string+position-1);      
      string++;   
    }
  *(pointer+c) = '\0'; 
  return pointer;
}

void parse_block(char flags [],formula fs [],value_s vs [],
		 char *buffer,int *nb_fomulas,int *nb_value,int *nb_rows){
  for(int i =0; i< BUFSIZE; i++){
    flags[i] = 'o';
  }
  int i=0,elements=0 ,currents_formula=0;
  
  // file, delimiter, first_line_is_header?
  CsvParser *csvparser = CsvParser_new_from_string(buffer, ";", 1);
  CsvRow *header;
  CsvRow *row;
  
  header = CsvParser_getHeader(csvparser);
  if (header == NULL) {
    printf("%s\n", CsvParser_getErrorMessage(csvparser));
    exit(1);
  }
  
  char **headerFields = CsvParser_getFields(header);
  char **headerFieldTmp;
  *nb_rows = CsvParser_getNumFields(header);
  /** Load the header **/
  for (i = 0 ; i < CsvParser_getNumFields(header) ; i++){
    /** extract **/    
    if (headerFields[i][0] == '=' ){
      char *sub = substring_no_last_element(headerFields[i],4,(strlen(headerFields[i])-1));      
      CsvParser *csvstr = CsvParser_new_from_string(sub, ",", 1);
      CsvRow *headerTmp = CsvParser_getHeader(csvstr);
      if (header == NULL) {
	printf("%s\n", CsvParser_getErrorMessage(csvstr));
	exit(1);
      }
      headerFieldTmp = CsvParser_getFields(headerTmp);
      
      format_formula(&fs[currents_formula],headerFieldTmp);
      format_value_marque(&vs[elements],*nb_fomulas);
      
      currents_formula++;
      *nb_fomulas = *nb_fomulas + 1;
      flags[elements] = 'f';
      /** Clean the references **/
      CsvParser_destroy(csvstr);
      free(sub);
      
    }else {
      format_value(&vs[elements],headerFields[i]);
      flags[elements] = 'v';
    }
      
    elements++;
  }
  
  /** Load the core **/
  // CsvParser_destroy_row(header); -> causes error in current version
  while ((row = CsvParser_getRow(csvparser)) ) {
    
    char **rowFields = CsvParser_getFields(row);
    
    for (i = 0 ; i < CsvParser_getNumFields(row) ; i++) {

      
      /** extract **/
      if (rowFields[i][0] == '=' && rowFields[i][strlen(rowFields[i])-1] == ')'){
	char *sub = substring_no_last_element(rowFields[i],4,(strlen(rowFields[i])-1));
	CsvParser *csvstr = CsvParser_new_from_string(sub, ",", 1);
	header = CsvParser_getHeader(csvstr);
	if (header == NULL) {
	  printf("%s\n", CsvParser_getErrorMessage(csvstr));
	  exit(1);
	}
	
	headerFields = CsvParser_getFields(header);
	format_formula(&fs[currents_formula],headerFields);
	
	/** On marque **/
	format_value_marque(&vs[elements],*nb_fomulas);
	currents_formula++;
	*nb_fomulas = *nb_fomulas + 1;
	flags[elements] = 'f';

	/** Clean the references **/
	CsvParser_destroy(csvstr);
	
	free(sub);
	
      }else if(rowFields[i][0] != '=' ) {
	format_value(&vs[elements],rowFields[i]);
	flags[elements] = 'v';
      }else break;
      elements++;
    }
    CsvParser_destroy_row(row);
  }
  CsvParser_destroy(csvparser);
  
  *nb_value    = elements;
}


/** Write and Read in File **/
void write_block_value(int desc,value_s valeur[],int nb_value){
  if (write(desc,valeur,sizeof(value_s) * nb_value) != (int)(sizeof(value_s)* nb_value)) 
    err(1,"error when we writing a value\n");	
}

void write_block_formula(int desc,formula buf[],int nb_fomulas){
  if (write(desc,(buf),
	    sizeof(formula)*nb_fomulas) != (int)(sizeof(formula)*nb_fomulas) ) 
    err(1,"error when we writing a c1 coordonate of fomula\n");     
}

int read_value_b(int desc, value_s *buf,char *name){
  int lu=0;
  if((lu = read(desc,buf,
		sizeof(value_s)))< 0){
    fprintf(stderr, "error at reading a value of file : %s\n",name);
    exit(EXIT_FAILURE);
  }
  return lu;
}
int read_formula_b(int desc,formula *buf,char *name){
  int lu=0;
  if((lu = read(desc,buf,
		sizeof(formula))) < 0){
    fprintf(stderr, "error at reading a coordonate c1 of file : %s\n",name);
    exit(EXIT_FAILURE);
  }
  return lu;
}                         


/** Get and Set function for Serializable file  **/
CAMLprim value
set_valeur (value x_args,value y_args,value length_args,value change_args){
  CAMLparam0();
  int x = Int_val(x_args);
  int y = Int_val(y_args);
  int length_row = Int_val(length_args);
  value_s valeur[1];
  int constante = sizeof(int);
  int fd = open("SerializeValues.srl",O_TRUNC|O_RDWR,0600);
  valeur[0].valeur = Int_val(change_args);
  lseek(fd,(x* constante * length_row) + (y * constante),SEEK_SET);
  write_block_value(fd,valeur,1);
  close(fd);
  CAMLreturn( Val_unit );  
}

int get_val (int x,int y,int length_row){
  value_s valeur;
  int constante = sizeof(int);
  int fd = open("SerializeValues.srl",O_RDONLY);
  lseek(fd,(x* constante * length_row) + (y * constante),SEEK_SET);
  read_value_b(fd, &valeur,"ValueSerial.srl");
  close(fd);
  return valeur.valeur;
}

CAMLprim value
set_formula_o(value x_args,value y_args,value length_args, value formula_args ){
  CAMLparam0();/** fix the bug error: ‘caml__frame’ undeclared */
  formula f[1];
  int pos;
  CAMLlocal5( c1, r1, c2,r2,valeur );

  c1 = Field(formula_args, 0);
  r1 = Field(formula_args, 1);
  c2 = Field(formula_args, 2);
  r2 = Field(formula_args, 3);
  valeur  = Field(formula_args, 4);
  
  int constante = sizeof(formula);
  int fd=open("SerializeFormula.srl",O_TRUNC|O_RDWR,0600);  
  f[0].c1 = c1;
  f[0].r1 = r1;
  f[0].c2 = c2;
  f[0].r2 = r2;
  f[0].valeur = valeur;
  pos = get_val (Int_val(x_args),Int_val(y_args),Int_val(length_args));
  lseek(fd,(pos*constante),SEEK_SET);
  write_block_formula(fd,f,1);
  close(fd);
  CAMLreturn( Val_unit );   
}


CAMLprim value
get_formula_o( value i_args ){
  CAMLparam0();/** fix the bug error: ‘caml__frame’ undeclared */
  int i = Int_val(i_args);
  formula f;
  CAMLlocal1( formula_return );
  int constante = sizeof(formula);
  int fd=open("SerializeFormula.srl",O_RDONLY);
  lseek(fd,(i*constante),SEEK_SET);
  read_formula_b(fd,&f,"SerializeFormula.srl");
  close(fd);
  
  formula_return = caml_alloc(5, 0);
  Store_field( formula_return, 0, f.c1 );
  Store_field( formula_return, 1, f.r1 );
  Store_field( formula_return, 2, f.c2 );
  Store_field( formula_return, 3, f.r2 );
  Store_field( formula_return, 4, f.valeur );
  CAMLreturn( formula_return );
}

/*
void get_formula(int i,formula *f){
  int constante = sizeof(formula);
  int fd=open("SerializeFormula.srl",O_RDONLY);
  lseek(fd,(i*constante),SEEK_SET);
  read_formula_b(fd,f,"SerializeFormula.srl");
  close(fd);
}
*/
CAMLprim value
get_valeur (value x_args,value y_args,value length_args){
  int x = Int_val(x_args);
  int y = Int_val(y_args);
  int length_row = Int_val(length_args);
  value_s valeur;
  int constante = sizeof(int);
  int fd = open("SerializeValues.srl",O_RDONLY);
  lseek(fd,(x* constante * length_row) + (y * constante),SEEK_SET);
  read_value_b(fd, &valeur,"SerializeValues.srl");
  close(fd);
  return Val_int(valeur.valeur);
}


/** Function of serialization 
    void serialize(){ **/

CAMLprim value
caml_serialize(value name){

  char *file = String_val(name);
  int original_file,serialize_file_f,serialize_file_v,lu;
  int nbvalue=0,nbformula = 1,nb_rows = 0;
  char    flags [BUFSIZE];
  formula fs    [BUFSIZE];
  value_s vs    [BUFSIZE];
  char    buf   [BUFSIZE];
  
  if((original_file =
      open(file,O_RDONLY))<0){
    warn("Error at opening file  %s \n",file);
    exit(1);
  }
  if((serialize_file_v =
      open("SerializeValues.srl",O_CREAT|O_TRUNC|O_RDWR,0600))<0){
    warn("Error at opening file  %s \n","SerializeValues.srl");
    exit(1);
  }
  if((serialize_file_f =
      open("SerializeFormula.srl",O_CREAT|O_TRUNC|O_RDWR,0600))<0){
    warn("Error at opening file  %s \n","SerializeFormula.srl");
    exit(1);
  }
  while(1){
    lu = read(original_file,buf,BUFSIZE);
    if(lu == 1024){
      int recule = BUFSIZE-1;
      while(buf[recule] != ';'){
	recule --;
	
      }
      if(recule < 1023 ){
	buf[recule] = '\0';
	int cnst = BUFSIZE-recule;
	/**On repositionne la tête de lecture */
	lseek(original_file,-(cnst+2),SEEK_CUR);
      }
    }else{
      buf[lu] = '\0';
    }
    
    parse_block(flags,fs,vs,buf,&nbformula,&nbvalue,&nb_rows);
    write_block_formula(serialize_file_f,fs,nbformula);
    write_block_value(serialize_file_v,vs,nbvalue);
    
    if (lu < BUFSIZE) break;
  }
  
  close(serialize_file_f);
  close(serialize_file_v);
  close(original_file);
  
  return Val_int(nb_rows);
}


 /*
int main(int argc, char *argv[]) {
 
  serialize("test.csv");
  formula f2;
  int fd=open("SerializeFormula.srl", O_RDONLY);

  if(atoi(argv[1]) == 0){
    get_formula(atoi(argv[2]),&f2);
    printf("c1 : %d r1 : %d c2 : %d r2 : %d valeur : %d  \n", f2.c1, f2.r1, f2.c2, f2.r2, f2.valeur);
    
  }else{
    int v = get_valeur(atoi(argv[2]),atoi(argv[3]),3);
    printf("coordoné x : %d y : %d , valeur : %d  \n",atoi(argv[2]),atoi(argv[3]), v);
  }
  close(fd);
  
}
 */
