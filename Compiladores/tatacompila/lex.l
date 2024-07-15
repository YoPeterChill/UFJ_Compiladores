%{
#include <stdio.h>
#include <stdlib.h>

bool force_print_tree = false;
char * build_file_name;
class Node;
#include "sin.tab.h"
int yyerror(const char *s);
%}

%option yylineno

%%
[ \t\r\n_]  {   /*ignora*/  }
"//".*\n { /*Comentário*/ }

"oh" { return TOK_PRINT ; }

"uta" {return TOK_WHILE ; }
"queh"  {return TOK_FOR;}
"nana" {return TOK_IF;}
"donaoein" {return TOK_ELSE;}
"opa" {return TOK_NOT ;}
"evin" {return TOK_AND;}
"tata" {return TOK_OR ;}
"==" {return TOK_IGUAL ;}
"!=" {return TOK_DIFERENTE ;}
"+" {return '+' ; }
"-" {return '-' ; }
"*" {return '*' ; }
"/" {return '/' ; }
"(" {return '(' ; }
")" {return ')' ; }
";" {return ';' ; }
"=" {return '=' ; }
"<" {return '<' ; }
">" {return '>' ; }
"{" {return '{' ; }
"}" {return '}' ; }
"^" {return '^' ;}
"!" {return '!' ;}
"[" {return '[' ; }
"]" {return ']' ; }


"vo" {return TOK_TRUE;}
"vonada" {return TOK_FALSE;}

[a-zA-Z_][a-zA-Z0-9_]* {yylval.str = strndup(yytext, yyleng);
                        return TOK_IDENT ; }

[0-9]+\.[0-9]* {yylval.flt = atof(yytext);
                return TOK_FLOAT ; }

[0-9]+  {
            yylval.itg = atoi(yytext);
            return TOK_INT; }

\"([^\\\"]|\\.)*\"  {yylval.str = strndup(yytext, yyleng);
                    return TOK_WORD ; }

.   {printf("Simbolo não reconhecido! %c\n",yytext[0]);}

%%
int yywrap(){
    return 1;
}

extern int error_count;
int yyerror(const char *s){
    printf("%s:%d:0: %s\n",build_file_name, yylineno, s);
    error_count++;
    return 1;
}

int main(int argc, char *argv[]){
    if(argc <= 1){
        printf("Sintaxe: %s nome_do_programa\n",argv[0]);
        return 1;
    }
        

    for(int i=1;i<argc;i++){
        if(strcmp(argv[i],"-d")==0){
            yydebug=1;
        }
        
        if (strcmp(argv[i],"-f")==0){
            force_print_tree = true;
        }
    }

    build_file_name = argv[1];
    yyin = fopen(argv[1],"r");
    if(yyin == NULL){
        printf("Não foi possivel abrir o arquivo %s.\n",build_file_name);
        return 1;
    }
    
    yyparse();

    if(yyin)
        fclose(yyin);

    return 0;
}
