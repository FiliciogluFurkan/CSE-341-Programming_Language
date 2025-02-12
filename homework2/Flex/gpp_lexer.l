%{
    #include<stdio.h>
    #include<stdlib.h>

    typedef enum
    {
        OP_PLUS = 1, OP_MINUS, OP_MULT, OP_DIV, OP_OP, OP_CP, OP_COMMA,
        VALUEI, VALUEF, IDENTIFIER, WHITESPACE, KW_TRUE, KW_FALSE, KW_AND,
        KW_OR, KW_NOT, KW_EQUAL, KW_LESS, KW_NIL, KW_LIST, KW_APPEND, KW_CONCAT, 
        KW_SET, KW_FOR, KW_IF, KW_LOAD, KW_DISP, KW_EXIT, KW_DEFFUN,COMMENT
    }Token;

    void printToken(Token token, FILE *outputFile) {
        const char *tokenNames[] = {
            "OP_PLUS", "OP_MINUS", "OP_MULT", "OP_DIV", "OP_OP", "OP_CP", "OP_COMMA",
            "VALUEI", "VALUEF", "IDENTIFIER", "WHITESPACE", "KW_TRUE", "KW_FALSE",
            "KW_AND", "KW_OR", "KW_NOT", "KW_EQUAL", "KW_LESS", "KW_NIL", "KW_LIST", 
            "KW_APPEND", "KW_CONCAT", "KW_SET", "KW_FOR", "KW_IF", "KW_LOAD", 
            "KW_DISP", "KW_EXIT", "KW_DEFFUN","COMMENT"
    };
    
    // Write_token to the output.ttx
    if (outputFile != NULL) {
        fprintf(outputFile, "%s\n", tokenNames[token - 1]);
    }
    // this part is for users.Users can see in the terminal
    else{
        printf("%s\n", tokenNames[token - 1]);
    }
}
%}

/*regular expressions*/


VALUEI          [0-9]+
VALUEF          [0-9]+[f][0-9]+
IDENTIFIER      [a-zA-Z][a-zA-Z0-9]*
WHITESPACE      [ \t\n]+
MYSIGN          [^+-/*(), \n\t]             
COMMENT         [";;"].*
 
          
WHITESPACES     [\\t\\n]+               

/*Operators and Keywords*/

%%
[+]                                         { return OP_PLUS;    }
[-]                                         { return OP_MINUS;   }
[*]                                         { return OP_MULT;    }
[/]                                         { return OP_DIV;     }
[(]                                         { return OP_OP;      }
[)]                                         { return OP_CP;      }
[,]                                         { return OP_COMMA;   }

true                                        { return KW_TRUE;     }
false                                       { return KW_FALSE;    }
and                                         { return KW_AND;      }
or                                          { return KW_OR;       }
not                                         { return KW_NOT;      }
equal                                       { return KW_EQUAL;    }
less                                        { return KW_LESS;     }
nil                                         { return KW_NIL;      }
list                                        { return KW_LIST;     }
append                                      { return KW_APPEND;   }
concat                                      { return KW_CONCAT;   }
set                                         { return KW_SET;      }
for                                         { return KW_FOR;      }
if                                          { return KW_IF;       }
load                                        { return KW_LOAD;     }
disp                                        { return KW_DISP;     }
exit                                        { return KW_EXIT;     }
deffun                                      { return KW_DEFFUN;      }

{IDENTIFIER}                                { return IDENTIFIER; }
{VALUEF}                                    { return VALUEF; }
{VALUEI}                                    { return VALUEI; }
{COMMENT}                                   { return COMMENT; }
{WHITESPACE}                                ;
{WHITESPACES}                               ;

{MYSIGN}+[a-zA-Z0-9]+                       { fprintf(stderr, "SYNTAX_ERROR '%s' cannot be tokenized\n",yytext); exit(1);}          
{MYSIGN}+[a-zA-Z0-9]+{MYSIGN}+              { fprintf(stderr, "SYNTAX_ERROR '%s' cannot be tokenized\n",yytext); exit(1);}         
.                                           { fprintf(stderr, "SYNTAX_ERROR '%s' cannot be tokenized\n",yytext); exit(1);}                                               
%%

int main() 
{
    // Read_from file
    FILE *file = fopen("input.txt", "r");  
    if (file == NULL) {
        fprintf(stderr, "File cannot be opened!\n");
        return 1;  
    }

    // Write_to tje output.txt
    FILE *outputFile = fopen("output.txt", "w");  
    if (outputFile == NULL) {
        fprintf(stderr, "Output file cannot be opened!\n");
        fclose(file);  
        return 1;  
    }

    // Flex ile dosyayı okuma işlemi
    yyin = file;  

    Token control;
    while ((control = yylex())) {  // yylex() ile token al
        printToken(control, outputFile);  
    }

    fclose(file);  
    fclose(outputFile);  

    // Kullanıcıdan terminal girdisi alma
    char userInput[100];
    printf("\nEnter a line for lexical analysis (type 'quit' to exit): ");
    while (1) {
        fgets(userInput, sizeof(userInput), stdin);  

        // 'quit' proived stopping the program
        if (strncmp(userInput, "quit", 4) == 0) {
            break;  
        }
        

        // Redirect to Flex with input from user
        FILE *inputMemory = fmemopen(userInput, strlen(userInput), "r");
        if (inputMemory == NULL) {
            fprintf(stderr, "Error opening memory stream.\n");
            break;
        }
        
        yyin = inputMemory;  

        // Token'ları işleme ve yazdırma
        while ((control = yylex())) {
            printToken(control, NULL);  
        }

        fclose(inputMemory);  

        printf("\nEnter a line for lexical analysis (type 'quit' to exit): ");
    }

    return 0;
} 