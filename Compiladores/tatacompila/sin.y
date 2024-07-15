%{
#include "nodes.h"

extern bool force_print_tree;
int yyerror(const char *s);
int yylex(void);
%}

%define parse.error verbose
%define parse.trace

%union { 
    char *str;
    int itg;
    double flt;
    Node *node;
}

%token TOK_IDENT TOK_FLOAT TOK_INT TOK_WORD TOK_TRUE TOK_FALSE

%token TOK_PRINT
%token TOK_WHILE
%token TOK_IF
%token TOK_ELSE
%token TOK_CASE
%token TOK_FOR

%token TOK_AND
%token TOK_OR
%token TOK_NOT
%token TOK_IGUAL
%token TOK_DIFERENTE

%type<str> TOK_IDENT
%type<itg> TOK_INT
%type<flt> TOK_FLOAT
%type<str> TOK_WORD

%type<node> globals global cmprt cmprt2 cmprt3 expr term factor unary

%start program 

%%

program : globals{      Node *program = new Program();
                        program->append($globals);
                        
                        //Analise semantica

                        CheckVarDecl cvd;
                        cvd.check(program);
                        
                                                
                        if(error_count>0){
                                cout    << "\nForam encontrados "
                                        << error_count
                                        << " erros no  No código\n"
                                        << endl;  
                        }
                        if(force_print_tree||error_count==0){
                                printf_tree(program);
                        }
                        
                }

globals: globals[gg] global {$gg->append($global);
                        $$ = $gg;       }
        |global {       Node *n = new Node();
                        n->append($global);
                        $$ = n; }


global: TOK_IDENT '=' expr ';'     { $$ = new Variavel($TOK_IDENT,$expr);     }
        |TOK_PRINT factor ';' { $$ = new Mostra($factor);      }
        |TOK_IF  '(' cmprt ')'  '{' globals '}' { $$ = new Se($cmprt,$globals); }
        |TOK_IF  '(' cmprt ')'  '{' globals[g1] '}' TOK_ELSE '{' globals[g2] '}'       { $$ = new SeSenao($cmprt,$g1,$g2);     }
        |TOK_WHILE '(' cmprt ')' '{' globals '}'    { $$ = new Enquanto($cmprt,$globals);   }
        |TOK_FOR '{' globals'}'        { $$ = new Loop($globals);      }
        |error ';'      { $$ = new Node();      }
        |error  { $$ = new Node();      }


cmprt:    cmprt[c1] TOK_OR  cmprt2[c2]     { $$ = new OpBinaria($c1,'|',$c2);      }
        | cmprt[c1] TOK_IGUAL cmprt2[c2]  { $$ = new OpBinaria($c1,'=',$c2);      }
        | cmprt[c1] '<' cmprt2[c2]         { $$ = new OpBinaria($c1,'<',$c2);      }
        | cmprt[c1] '>' cmprt2[c2]         { $$ = new OpBinaria($c1,'>',$c2);      }
        | cmprt2  {      $$ = $cmprt2;  }

cmprt2:   cmprt2[c1] TOK_AND cmprt3[c2]        { $$ = new OpBinaria($c1,'&',$c2);      }
        | cmprt2[c1] TOK_DIFERENTE cmprt3[c2]       { $$ = new OpBinaria($c1,'!',$c2);      }
        | cmprt3        { $$ = $cmprt3; }

cmprt3:  '[' cmprt ']'   { $$ = $cmprt; }
        | factor        { $$ = $factor; }
        |'!' '[' cmprt[c2] ']' { $$ = new Unario("!",$c2); }

expr:   expr[e] '+' term   {$$ = new OpBinaria($e,'+',$term);   }
        |expr[e] '-' term  {$$ = new OpBinaria($e,'-',$term);   }
        |term   { $$ = $term;   }

term:   term[t] '*' factor {  $$ = new OpBinaria($t,'*',$factor);}
        |term[t] '/' factor        { $$ = new OpBinaria($t,'/',$factor);        }
        |term[t] '^' factor        { $$ = new OpBinaria($t,'^',$factor);        }       
        |factor { $$ = $factor; }

factor: '(' expr ')'    { $$ = $expr;   }
        |TOK_WORD    { $$ = new Palavra($TOK_WORD);       }
        |TOK_INT   { $$ = new Inteiro($TOK_INT);       }
        |TOK_IDENT { $$ = new Id($TOK_IDENT);    }
        |TOK_FLOAT { $$ = new Pf($TOK_FLOAT); }
        |unary  { $$ = $unary; }
        |TOK_FALSE      { $$ = new Boleano(false);       }
        |TOK_TRUE { $$= new Boleano(true);        }

unary:  '-' factor      { $$ = new Unario("-",$factor);   }
        |'+' factor     { $$ = new Unario("+",$factor);   }

%%
