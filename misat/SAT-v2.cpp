#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0


/*-----------------------------------------------------------------
  MEJORA PARA V2: mejorar la heuristica

  encontrar variables sobre las que una decision tenga mas efecto 
  sobre el problema (las que aparecen mucho!!!)

  SOLUCION ESTATICA:
  Contar las apariciones (empezar por la que tenga mas apariciones), 
  esta heuristica es estatica
  

  SOLUCION DINAMICA 1:
  Este seria un contador por cada vable el cual se iria actualizando 
  durante la ejecucion 

  Contador de todas las variables donde encuentras un conflicto 
  aumenta (ya que estas son variables interesantes para evaluar 
  en lineas generales)

  SOLUCION DINAMICA 2:
  No solo nos importa que variables tienen conlfictos en general, 
  sino las que han aparecido en conflictos recientes pues tienen 
  mayor relevancia para el contexto actual

  En este sentido por lo tanto debemos de ir dividiendo entre dos 
  la lista de los contadores para que se tengan en cuenta las nuevas

  No contamos la que genera el conflicto solo sino todas las variables 
  donde encontramos el propio conflicto

  ---------------------------------------------------------------------*/

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;
//en este vector de vectores almacenare en los 
vector<vector<int>> ocurr;
//estructura que contendra la heuristica de decision y contadores para evaluarla
vector<int> heur;
uint conflicts;
uint propagations;
uint decisions;

//constantes necesarias para la heuristica (optimizadas experimentalmente)
const int apparition = 2;
const int frequency = 1000;
const double factor = 0.5;
 
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
    //el numero de ocurrencias de cada variable como p o !p
    ocurr.resize(numVars*2+1);
    heur.resize(numVars + 1, 0);
    conflicts = 0;
    propagations = 0;
    decisions = 0;
    // Read clauses
    for (uint i = 0; i < numClauses; ++i) {
        int lit;
        while (cin >> lit and lit != 0) {
            clauses[i].push_back(lit);
            if(lit > 0) { // si la vairable se encuentra en positivo
                ocurr[lit].push_back(i);
                heur[lit] += apparition;
            }
            else { // si la variable se encuentra en negativo
                ocurr[-lit+numVars].push_back(i);
                heur[-lit] += apparition;
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


void setLiteralToTrue(int lit){
    modelStack.push_back(lit);
    if (lit > 0) model[lit] = TRUE;
    else model[-lit] = FALSE;		
}

//PRINCIPALES MODIFICACIONES DE V1
bool propagateGivesConflict ( ) {
    while ( indexOfNextLitToPropagate < modelStack.size() ) {

        //extraigo el literal que ha causado la propagacion
        int lit2prop = modelStack[indexOfNextLitToPropagate];
        if (lit2prop < 0) lit2prop = -lit2prop;
        else lit2prop += numVars;

        ++indexOfNextLitToPropagate;
        //ahora aqui solo compruebo las que sean del indice particular
        for (uint i = 0; i < ocurr[lit2prop].size(); ++i) {
            int c_id = ocurr[lit2prop][i];//la clausula en la que podria haber propagacion
            bool someLitTrue = false;
            int numUndefs = 0;
            int lastLitUndef = 0;
            for (uint k = 0; not someLitTrue and k < clauses[c_id].size(); ++k){
                int val = currentValueInModel(clauses[c_id][k]);
                if (val == TRUE) someLitTrue = true;
                else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[c_id][k]; }
            }

        //decidim si hi ha conflicte o no
        if (not someLitTrue and numUndefs == 0) {
            ++conflicts;
            if(conflicts%frequency==0) //cuando ha habido una cantidad de conflictos dividimos
                for(uint i = 1; i <= numVars; ++i) heur[i] *= factor;

            //aqui ha habido conflicto asi que actualizamos la heuristica dinamica
            vector<int> literals_in_clause = clauses[c_id];
            for (uint i = 0; i < literals_in_clause.size(); ++i){
                int lit_in_clause = literals_in_clause[i];
                heur[abs(lit_in_clause)] += apparition;
            }
            
            return true; // conflict! all lits false
        }
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	
        }    
        ++propagations;
    }
    return false;
}


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


// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
    int best_heur = 0;
    int index = 0;
    for (uint i = 1; i <= numVars; ++i) // stupid heuristic:
        if (model[i] == UNDEF and heur[i] > best_heur){
            best_heur = heur[i];
            index = i;
        }

    ++decisions;
    
    return index; // reurns best index according to my heuristic
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
    for (uint i = 0; i < numClauses; ++i)
        if (clauses[i].size() == 1) {
        int lit = clauses[i][0];
        int val = currentValueInModel(lit);
        if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
        else if (val == UNDEF) setLiteralToTrue(lit);
        }
    
    // DPLL algorithm
    while (true) {
        while ( propagateGivesConflict() ) {
        if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
        backtrack();
        }
        int decisionLit = getNextDecisionLiteral();
        if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
        // start new decision level:
        modelStack.push_back(0);  // push mark indicating new DL
        ++indexOfNextLitToPropagate;
        ++decisionLevel;
        setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
    }
}  
