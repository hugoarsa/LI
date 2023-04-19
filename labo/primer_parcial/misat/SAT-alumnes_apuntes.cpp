#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model; //foto de los modelos
//para marcarlos en rojo pondremos un 0 antes de la variables en las que decidamos
vector<int> modelStack; //aqui tenemos las variables que son un modelo parcial
//un indice al elemento que estamos propagando actualmente
uint indexOfNextLitToPropagate;
//la cantidad de decisiones tomadas hasta el momento
uint decisionLevel;

//por cada indice i encontramos las ocurrencias positivas por clausulas de la variable i 
vector<vector<int>> ocurr;
//en los indices i encontraremos un indicador el cual nos da la heuristica con la cual decidiremos el siguente elem
vector<double> heur;
//contador de conflictos
uint conflict;

//rellena la matriz de clausulas y pone a UNDEF el modelo
void readClauses( ){
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);  

  //primera mejora
  ocurr.resize(numVars*2+1);

  //segunda mejora
  heur.resize(numVars+1,0.0);
  
  conflict = 0;
  

  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0){
        clauses[i].push_back(lit);
        //primera y segunda mejora inicializacion
        if(lit > 0) {
          ocurr[lit].push_back(i);
          ++heur[lit];
        }
        else {
          ocurr[-lit + numVars].push_back(i);
          ++heur[-lit];
        }
    } 
  }    
}


int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}

//cambiar valor de los literales
void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;		
}

//hace todas las propagaciones hasta que no puede hacer mas o encuentra conflicto
bool propagateGivesConflict ( ) {
  /*-------------------------------------------------------------------------------------------------------------
  MEJORA: cuando propagemos un elemento en particular solo evaluar las clausulas susceptibles a que 
  este elemento a propagar haya podido producir un cuatro
  
  si un literal es positivo no hemos de mirar en los que este tambien este en positivo pues ya estan resueltas
  siempre comprobamos el valor contrario para posible propagacion o conflicto 

  lista de todas las clausulas donde esta en positivo o en negativo

  ---------------------------------------------------------------------------------------------------------------*/

  //mientras el indice que propagas no sea el ultimo vas propagando (no tomar decisiones 
  //a no ser que sea absolutamente necesario)
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    ++indexOfNextLitToPropagate;

    int valorVariable = modelStack[indexOfNextLitToPropagate];
    int valorActual = currentValueInModel(valorVariable);

    if(valorVariable > 0) valorVariable = valorVariable + numVars;
    else valorVariable = -valorVariable;

    //observamos solo las clausulas relevantes
    //i < numClauses
    for (uint i = 0; i < ocurr[valorVariable].size(); ++i) {
      int cl_relevant = ocurr[valorVariable][i];

      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;

      //observamos todos los literales de una clausula particular
      for (uint k = 0; not someLitTrue and k < clauses[cl_relevant].size(); ++k){
	      int val = currentValueInModel(clauses[i][k]);
        //si es cierto esta clausula ya esta bien y podemos dejar de mirar
	      if (val == TRUE) someLitTrue = true;
        //si hay indefinidos miramos cual es el ultimo
	      else if (val == UNDEF){ 
          ++numUndefs; 
          lastLitUndef = clauses[cl_relevant][k]; 
        }
      }

      //no hay indefinidos y la clausula esta mal, peligro
      if (not someLitTrue and numUndefs == 0) {
        conflict++;
        return true; // conflict! all lits false
      }
      //si la clausula aun no es cierta pero hay un solo indefinido lo ponemos ahi
      //caso limite importante que podemos ir haciendo sobre la marcha
      else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	
    }    
  }
  return false;
}

//tira hacia atras hasta encontrar un 0 y el siguente lo gira 
//(es decir la decision utlima no nos vale la invertimos)
void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}

/*-------------------------------------------------------------------------------------------------------------
  MEJORA: mejorar la heuristica

  encontrar variables sobre las que una decision tenga mas efecto obre el problema (las que aparecen mucho!!!)

  PRIMERA SOLUCION ESTATICA:
  Contar las apariciones (empezar por la que tenga mas apariciones), esta heuristica es estatica

  Proximamente veremos una heuristica dinamica la cual ayudara a dar un salto bastante grande en eficiencia
  

  PRIMERA SOLUCION DINAMICA:
  Este seria un contador por cada vable el cual se iria actualizando durante la ejecucion 

  Contador de todas las variables donde encuentras un conflicto aumenta (ya que estas son variables interesantes para
  evaluar en lineas generales)

  SEGUNDA SOLUCION:
  No solo nos importa que variables tienen conlfictos en general, sino las que han aparecido en conflictos recientes
  pues tienen mayor relevancia para el contexto actual

  En este sentido por lo tanto debemos de ir dividiendo entre dos la lista de los contadores para que se tengan en cuenta
  las nuevas

  No contamos la que genera el conflicto solo sino todas las variables donde encontramos el propio conflicto

  ESTA NO ES LA BUENA (pero es bastante mejor que la que tenemos)

  ---------------------------------------------------------------------------------------------------------------*/
// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
  for (uint i = 1; i <= numVars; ++i) // stupid heuristic:
    if (model[i] == UNDEF) return i;  // returns first UNDEF var, positively
  return 0; // reurns 0 when all literals are defined
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  //con clausulas de un unico literal esas ya las podemos tratar
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {

    //propaga todo lo que puedas
    while ( propagateGivesConflict() ) {
      //si no hay mucha profundidad de decision adios
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      //vuelve para atras porrque la decision que tomamos era mala
      backtrack();
      //despues del backtracking prueba a volver a propagar
    }

    //si estoy aqui no puedo propagar mas y todo tiene sentido

    //buscamos el siguente literal sobre el que propagar
    int decisionLit = getNextDecisionLiteral();

    //si no encontramos todo esta decidido entonces ya hemos decidido todo y hemos comprobado que es un modelo
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }

    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  