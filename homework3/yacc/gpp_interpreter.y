%{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>

    extern FILE * yyin; // for printing to file
    int type = 0; // For evaluating VALUEF or IDENTIFIER

    typedef struct valuefSides /* Struct for VALUEF format */
    {
        int first;
        char middle;
        int second;
    }
    Numbers;

    typedef struct Function /* Struct for all functions to be defined */
    {
        char * name;
        char * procedure;
        char * payload;
    }
    Functions;
    
    typedef struct Variable /* Struct for all variables to be defined */
    {
        char * name;
        char * payload;
        //Lists * lists;
    }
    Variables;

       typedef struct {
        int numerator;
        int denominator;
    } Fraction;

    typedef enum {
        OPERATION_SUCCESS,
        DIVISION_BY_ZERO,
        OVERFLOW_ERROR
    } OperationResult;

    typedef struct {
        Fraction result;
        OperationResult status;
        char* errorMessage;
    } OperationOutput;
 
    
    Functions * functions = NULL; // We keep the defined functions in this array.
    Variables * variables = NULL; // We keep the defined variables in this array.
    //Lists * lists = NULL; // We keep the defined lists in this arrat. 
    int numberOfFunctions = 0; // Variable holding the number of functions defined
    int numberOfVariables = 0; // Variable holding the number of variables defined


    int yyerror(const char *error) /* Error for syntax error */
    {
        printf("This is an Syntax Error !\n");
        exit(EXIT_FAILURE);
    }

 Numbers parseValuef(char *str) {
    int divided, divider;
    char midChar;
    Numbers numbers = {0};  //  başlangıcı 0
    // Girdi doğrulama
    int parsed = sscanf(str, "%d%c%d", &divided, &midChar, &divider);
    
    // Hatalı girdi kontrolü
    if (parsed != 3) {
        fprintf(stderr, "Error: Invalid format for parsing '%s'. Expected: <int><char><int>\n", str);
        exit(EXIT_FAILURE);  // Hata durumunda çıkış yap
    }

    // Eğer her şey doğru şekilde çözümlendiyse, numbers yapısını doldur
    numbers.first = divided;
    numbers.second = divider;
    numbers.middle = midChar;

    return numbers;
}
  int greatest_common_divisor(int n1, int n2) {
    if (n2 == 0) {
        return n1;
    }
    return greatest_common_divisor(n2, n1 % n2);
}

    
 void defineFunction(int flag, char *name_, char *operation_) {
    int isExist = -1;

    // Check if the function already exists
    for (int i = 0; i < numberOfFunctions; ++i) {
        if (strcmp(name_, functions[i].name) == 0) {
            isExist = i;
            break;
        }
    }

    // If the function already exists, print an error and exit
    if (isExist != -1) {
        fprintf(stderr, "Error: This function has already added.\n");
        exit(EXIT_FAILURE);
    }

    // Allocate memory for the new function
    Functions *temp = realloc(functions, (numberOfFunctions + 1) * sizeof(Functions));
    if (temp == NULL) {
        fprintf(stderr, "Error: Memory could not allocated.\n");
        exit(EXIT_FAILURE);
    }
    functions = temp;

    // Initialize the new function
    functions[numberOfFunctions].name = strdup(name_);
    functions[numberOfFunctions].procedure = strdup(operation_);
    if (flag == 1) {
        functions[numberOfFunctions].payload = strdup(operation_);
    }

    printf("#function\n");
    numberOfFunctions++;
    type = 0;
}

    OperationOutput createFraction(int numerator, int denominator) {
        OperationOutput output;
        
        // control of dividing zero
        if (denominator == 0) {
            output.status = DIVISION_BY_ZERO;
            output.errorMessage = strdup("Payda sıfır olamaz");
            return output;
        }

        // Kesiri sadeleştir
        int gcd = greatest_common_divisor(abs(numerator), abs(denominator));
        
        output.result.numerator = numerator / gcd;
        output.result.denominator = denominator / gcd;
        output.status = OPERATION_SUCCESS;
        output.errorMessage = NULL;

        return output;
    }

    OperationOutput addFraction(Fraction f1, Fraction f2) {
        int newNumerator = (f1.numerator * f2.denominator) + (f1.denominator * f2.numerator);
        int newDenominator = f1.denominator * f2.denominator;
        
        return createFraction(newNumerator, newDenominator);
    }

    OperationOutput subtractFraction(Fraction f1, Fraction f2) {
        int newNumerator = (f1.numerator * f2.denominator) - (f1.denominator * f2.numerator);
        int newDenominator = f1.denominator * f2.denominator;
        
        return createFraction(newNumerator, newDenominator);
    }

    OperationOutput multiplyFraction(Fraction f1, Fraction f2) {
        int newNumerator = f1.numerator * f2.numerator;
        int newDenominator = f1.denominator * f2.denominator;
        
        return createFraction(newNumerator, newDenominator);
    }

    OperationOutput divideFraction(Fraction f1, Fraction f2) {
        // control_of_division
        if (f2.numerator == 0) {
            OperationOutput output;
            output.status = DIVISION_BY_ZERO;
            output.errorMessage = strdup("Sıfıra bölme hatası");
            return output;
        }

        int newNumerator = f1.numerator * f2.denominator;
        int newDenominator = f1.denominator * f2.numerator;
        
        return createFraction(newNumerator, newDenominator);
    }

    // helper function to convert string
    char* fractionToString(Fraction f) {
        char* result = malloc(50 * sizeof(char));
        sprintf(result, "%db%d", f.numerator, f.denominator);
        printf("%s\n",result);
        return result;

    }

    // main function to process plus,minus,division and multiply
    char* evaluateOperator(const char* op , const char* param1, const char* param2) {
        //convert parameters to fraction
        Numbers nums1 = parseValuef(param1);
        Fraction f1 = {nums1.first, nums1.second};
        
        OperationOutput result;
        
        if (strcmp(op, "OP_PLUS") == 0) {
            Numbers nums2 = parseValuef(param2);
            Fraction f2 = {nums2.first, nums2.second};
            result = addFraction(f1, f2);
        }
        else if (strcmp(op, "OP_MINUS") == 0 && param2 != NULL) {
            Numbers nums2 = parseValuef(param2);
            Fraction f2 = {nums2.first, nums2.second};
            result = subtractFraction(f1, f2);
        }
        else if (strcmp(op, "OP_MULT") == 0) {
            Numbers nums2 = parseValuef(param2);
            Fraction f2 = {nums2.first, nums2.second};
            result = multiplyFraction(f1, f2);
        }
        else if (strcmp(op, "OP_DIV") == 0) {
            Numbers nums2 = parseValuef(param2);
            Fraction f2 = {nums2.first, nums2.second};
            result = divideFraction(f1, f2);
        }
        else if (strcmp(op, "OP_MINUS") == 0 && param2 == NULL) {
            // Negasyon
            f1.numerator = -f1.numerator;
            result.result = f1;
            result.status = OPERATION_SUCCESS;
        }
        
        // Error checking
        if (result.status != OPERATION_SUCCESS) {
            fprintf(stderr, "Error: %s\n", result.errorMessage);
            free(result.errorMessage);
            exit(EXIT_FAILURE);
        }
        
        return fractionToString(result.result);
    }


char *getFunctionValue(int flag, const char *funcName, const char *param1, const char *param2) {
    int index = -1;

    // Fonksiyon adı ile eşleşen fonksiyonu bul
    for (int i = 0; i < numberOfFunctions; ++i) {
        if (strcmp(funcName, functions[i].name) == 0) {
            index = i;
            break;  // Eşleşen fonksiyon bulundu, döngüyü bitir
        }
    }

    // Fonksiyon bulunamadıysa hata mesajı
    if (index == -1) {
        fprintf(stderr, "Error: Function '%s' is not defined.\n", funcName);
        exit(EXIT_FAILURE);  // Programı sonlandır
    }

    // Fonksiyon bulunduysa, flag değerine göre işlem yap
    switch (flag) {
        case 1:
            printf("%s\n", functions[index].payload);
            return functions[index].payload;  // İçeriği döndür
        default:
            return evaluateOperator(functions[index].procedure, param1, param2);  // Operasyonu değerlendir ve döndür
    }
}


void update_variable_content(int index, const char *content_) {
    // Eski içeriği serbest bırak
    free(variables[index].payload);
    variables[index].payload = (strcmp(content_, "nil") == 0) ? NULL : strdup(content_);
    if (variables[index].payload == NULL && strcmp(content_, "nil") != 0) {
        perror("Bellek ayırma hatası");
        exit(EXIT_FAILURE);
    }
}

// Yeni değişken ekleme fonksiyonu
void add_new_variable(const char *name_, const char *content_) {
    Variables *new_variables = realloc(variables, (numberOfVariables + 1) * sizeof(Variables));
    if (new_variables == NULL) {
        perror("Bellek ayırma hatası");
        exit(EXIT_FAILURE);
    }
    variables = new_variables;

    variables[numberOfVariables].name = strdup(name_);
    if (variables[numberOfVariables].name == NULL) {
        perror("Bellek ayırma hatası");
        exit(EXIT_FAILURE);
    }

    variables[numberOfVariables].payload = (strcmp(content_, "nil") == 0) ? NULL : strdup(content_);
    if (variables[numberOfVariables].payload == NULL && strcmp(content_, "nil") != 0) {
        perror("Bellek ayırma hatası");
        exit(EXIT_FAILURE);
    }

    numberOfVariables++;
}

// setVariable fonksiyonu
void setVariable(const char *name_, const char *content_) {
    for (int i = 0; i < numberOfVariables; i++) {
        if (strcmp(variables[i].name, name_) == 0) {
            // Değişken bulunduysa içeriği güncelle
            update_variable_content(i, content_);
            return;
        }
    }

    // Değişken bulunamadıysa yeni değişken ekle
    add_new_variable(name_, content_);

}

    int is_valid_value(const char* varName) {
        return strchr(varName, '/') || 
            strcmp(varName, "true") == 0 || 
            strcmp(varName, "false") == 0 || 
            strcmp(varName, "nil") == 0;
    }

    int is_fraction(const char* varName) {
        int num1, num2;
        char op;
        return sscanf(varName, "%d%c%d", &num1, &op, &num2) == 3;
    }

    char* find_variable(const char* varName) {
        for (int i = 0; i < numberOfVariables; ++i) {
            if (strcmp(varName, variables[i].name) == 0) {
                return variables[i].payload;
            }
        }
        return NULL;
    }

    char* resolveVariable(char* varName) {
        // Check if it's a valid value (true, false, nil, fraction, etc.)
        if (is_valid_value(varName)) {
            return varName;
        }

        // Check if it's a fraction
        if (is_fraction(varName)) {
            return varName;
        }

        // Try to find the variable in the variables list
        char* content = find_variable(varName);
        if (content != NULL) {
            return content;
        }

        // If variable is not found, print error and exit
        fprintf(stderr, "Hata: '%s' değişkeni tanımlı değil.\n", varName);
        exit(EXIT_FAILURE);
    }


    char* notOperation(const char* operand) {
        return strcmp(operand, "true") ? "true" : "false";
    }

    char* andOperation( const char* operand1,const char* operand2) {
        return (strcmp(operand1, "true") == 0 && strcmp(operand2, "true") == 0) 
            ? "true" 
            : "false";
    }

    char* orOperation(const char* operand1,const char* operand2) {
        return (strcmp(operand1, "true") == 0 || strcmp(operand2, "true") == 0) 
            ? "true" 
            : "false";
    }

    char * checkEquality(const char * value1, const char * value2) /* Determine if two values are equal */
    {
        return (strcmp(value1, value2) == 0) ? "true" : "false";
    }

    char * compareLessThan(const char * value1, const char * value2) /* Compare if the first value is less than the second */
    {
        int numerator1, denominator1, numerator2, denominator2;

        Numbers fraction1 = parseValuef(value1);
        Numbers fraction2 = parseValuef(value2);

        numerator1 = fraction1.first;
        denominator1 = fraction1.second;
        numerator2 = fraction2.first;
        denominator2 = fraction2.second;

        /* Adjust fractions to a common denominator and compare numerators */
        if ((numerator1 * denominator2) < (numerator2 * denominator1))
        {
            return "true";
        }

        return "false";
    }

    void freeMemory() /* Free memory */
    {
        free(functions);
        free(variables);
    }
%}  

%union 
{
    char * string;
    char values[1000][10];
}

%token <string> OP_PLUS OP_MINUS OP_MULT OP_DIV OP_OP OP_CP COMMENT OP_COMMA 
       KW_DEF KW_DISP KW_TRUE KW_FALSE VALUEF KW_LOAD KW_AND KW_OR KW_NOT 
       KW_EQUAL KW_LESS KW_NIL KW_APPEND KW_LIST KW_CONCAT KW_SET KW_FOR KW_IF KW_EXIT 
       IDENTIFIER

%type  <string> INPUT EXP FUNCTION

%type <string> START

%% 

/* START rule */
START:  
    /* Empty Expression */
    | START EXP                        { $$ = $2; }
    | START FUNCTION                   { $$ = $2; }
    | START COMMENT                    { printf("COMMENT\n"); }
    | START OP_OP KW_EXIT OP_CP        { freeMemory(); printf("$_\n"); exit(EXIT_FAILURE); }
    ;

/* INPUT rule */
INPUT:
    EXP                            { $$ = $1; }
    | FUNCTION                     { $$ = $1; }
    | COMMENT                      { printf("COMMENT\n"); }
    | OP_OP KW_EXIT OP_CP          { freeMemory(); printf("$_\n"); exit(EXIT_FAILURE); }
    ;

/* FUNCTION rule */
FUNCTION:
    OP_OP KW_DEF IDENTIFIER EXP OP_CP                      { defineFunction(1, $3, $4); $$ = $3; }
    | OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP         { defineFunction(0, $3, $5); $$ = $3; }
    | OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP { defineFunction(0, $3, $6); $$ = $3; }
    ;

/* EXP (Expression) rule */
EXP:
    /* Arithmetic */
    OP_OP OP_PLUS    EXP EXP OP_CP   { $$ = "OP_PLUS";  if (type == 1) { evaluateOperator("OP_PLUS",  resolveVariable($3), resolveVariable($4)); } }
    | OP_OP OP_MINUS   EXP EXP OP_CP  { $$ = "OP_MINUS"; if (type == 1) { evaluateOperator("OP_MINUS", resolveVariable($3), resolveVariable($4)); } }
    | OP_OP OP_MULT    EXP EXP OP_CP  { $$ = "OP_MULT";  if (type == 1) { evaluateOperator("OP_MULT",  resolveVariable($3), resolveVariable($4)); } }
    | OP_OP OP_DIV     EXP EXP OP_CP  { $$ = "OP_DIV";   if (type == 1) { evaluateOperator("OP_DIV",   resolveVariable($3), resolveVariable($4)); } }
    | OP_OP OP_MINUS   EXP OP_CP      { $$ = "OP_MINUS"; if (type == 1) { evaluateOperator("OP_MINUS", resolveVariable($3), NULL); } }

    /* Boolean */
    | OP_OP KW_NOT     EXP OP_CP      { $$ = notOperation(resolveVariable($3)); }
    | OP_OP KW_AND     EXP EXP OP_CP  { $$ = andOperation(resolveVariable($3), resolveVariable($4)); }
    | OP_OP KW_OR      EXP EXP OP_CP  { $$ = orOperation(resolveVariable($3), resolveVariable($4)); }
    | OP_OP KW_EQUAL   EXP EXP OP_CP  { $$ = checkEquality(resolveVariable($3), resolveVariable($4)); }
    | OP_OP KW_LESS    EXP EXP OP_CP  { $$ = compareLessThan(resolveVariable($3), resolveVariable($4)); }
    | OP_OP KW_IF EXP EXP EXP OP_CP   { $$ = (strcmp($3, "true") == 0) ? $4 : $5; }

    /* Assignment */
    | OP_OP KW_SET IDENTIFIER EXP OP_CP   { setVariable($3, $4); $$ = $4; }

    /* Calling Functions */
    | OP_OP KW_DISP EXP OP_CP             { $$ = resolveVariable($3); 
                                              if ($$ != NULL && strcmp($$, "nil") != 0) { 
                                                  printf("%s\n", $$); 
                                              } else { 
                                                  fprintf(stderr, "Error: This is (%s) a nil variable (it is not printable)\n", $3); 
                                                  exit(EXIT_FAILURE); 
                                              }
                                            }
    | OP_OP IDENTIFIER OP_CP              { $$ = getFunctionValue(1, $2, NULL, NULL); }
    | OP_OP IDENTIFIER EXP OP_CP          { $$ = getFunctionValue(0, $2, resolveVariable($3), NULL); }
    | OP_OP IDENTIFIER EXP EXP OP_CP      { $$ = getFunctionValue(0, $2, resolveVariable($3), resolveVariable($4)); }

    /* Primitive */
    | IDENTIFIER                          { $$ = $1; type = 0; }
    | VALUEF                              { $$ = $1; type = 1; }
    | KW_FALSE                            { $$ = "false"; }
    | KW_TRUE                             { $$ = "true"; }
    | KW_NIL                              { $$ = "nil"; }
    | OP_PLUS                             { $$ = "OP_PLUS"; }
    | OP_MINUS                            { $$ = "OP_MINUS"; }
    | OP_MULT                             { $$ = "OP_MULT"; }
    | OP_DIV                              { $$ = "OP_DIV"; }
    ;

%%

void print_usage() {
    printf("Usage: ./gpp_interpreter or ./gpp_interpreter <filename>\n");
}

void handle_file_input(const char *filename) {
    yyin = fopen(filename, "r");
    if (yyin == NULL) {
        printf("File not found\n");
        exit(0);
    }
}

int main(int argc, char *argv[]) {
    if (argc == 1) {
        printf("Type (exit) for exit\n");
        printf(">\n");
    } 
    else if (argc == 2) {
        handle_file_input(argv[1]);
    } 
    else {
        printf("Too many arguments you entered.please enter an valid command \n");
        print_usage();
        return 0;
    }

    yyparse();
    return 0;
}
