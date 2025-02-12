/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "gpp_interpreter.y"

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

#line 482 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    OP_PLUS = 258,                 /* OP_PLUS  */
    OP_MINUS = 259,                /* OP_MINUS  */
    OP_MULT = 260,                 /* OP_MULT  */
    OP_DIV = 261,                  /* OP_DIV  */
    OP_OP = 262,                   /* OP_OP  */
    OP_CP = 263,                   /* OP_CP  */
    COMMENT = 264,                 /* COMMENT  */
    OP_COMMA = 265,                /* OP_COMMA  */
    KW_DEF = 266,                  /* KW_DEF  */
    KW_DISP = 267,                 /* KW_DISP  */
    KW_TRUE = 268,                 /* KW_TRUE  */
    KW_FALSE = 269,                /* KW_FALSE  */
    VALUEF = 270,                  /* VALUEF  */
    KW_LOAD = 271,                 /* KW_LOAD  */
    KW_AND = 272,                  /* KW_AND  */
    KW_OR = 273,                   /* KW_OR  */
    KW_NOT = 274,                  /* KW_NOT  */
    KW_EQUAL = 275,                /* KW_EQUAL  */
    KW_LESS = 276,                 /* KW_LESS  */
    KW_NIL = 277,                  /* KW_NIL  */
    KW_APPEND = 278,               /* KW_APPEND  */
    KW_LIST = 279,                 /* KW_LIST  */
    KW_CONCAT = 280,               /* KW_CONCAT  */
    KW_SET = 281,                  /* KW_SET  */
    KW_FOR = 282,                  /* KW_FOR  */
    KW_IF = 283,                   /* KW_IF  */
    KW_EXIT = 284,                 /* KW_EXIT  */
    IDENTIFIER = 285               /* IDENTIFIER  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define OP_PLUS 258
#define OP_MINUS 259
#define OP_MULT 260
#define OP_DIV 261
#define OP_OP 262
#define OP_CP 263
#define COMMENT 264
#define OP_COMMA 265
#define KW_DEF 266
#define KW_DISP 267
#define KW_TRUE 268
#define KW_FALSE 269
#define VALUEF 270
#define KW_LOAD 271
#define KW_AND 272
#define KW_OR 273
#define KW_NOT 274
#define KW_EQUAL 275
#define KW_LESS 276
#define KW_NIL 277
#define KW_APPEND 278
#define KW_LIST 279
#define KW_CONCAT 280
#define KW_SET 281
#define KW_FOR 282
#define KW_IF 283
#define KW_EXIT 284
#define IDENTIFIER 285

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 413 "gpp_interpreter.y"

    char * string;
    char values[1000][10];

#line 600 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_OP_PLUS = 3,                    /* OP_PLUS  */
  YYSYMBOL_OP_MINUS = 4,                   /* OP_MINUS  */
  YYSYMBOL_OP_MULT = 5,                    /* OP_MULT  */
  YYSYMBOL_OP_DIV = 6,                     /* OP_DIV  */
  YYSYMBOL_OP_OP = 7,                      /* OP_OP  */
  YYSYMBOL_OP_CP = 8,                      /* OP_CP  */
  YYSYMBOL_COMMENT = 9,                    /* COMMENT  */
  YYSYMBOL_OP_COMMA = 10,                  /* OP_COMMA  */
  YYSYMBOL_KW_DEF = 11,                    /* KW_DEF  */
  YYSYMBOL_KW_DISP = 12,                   /* KW_DISP  */
  YYSYMBOL_KW_TRUE = 13,                   /* KW_TRUE  */
  YYSYMBOL_KW_FALSE = 14,                  /* KW_FALSE  */
  YYSYMBOL_VALUEF = 15,                    /* VALUEF  */
  YYSYMBOL_KW_LOAD = 16,                   /* KW_LOAD  */
  YYSYMBOL_KW_AND = 17,                    /* KW_AND  */
  YYSYMBOL_KW_OR = 18,                     /* KW_OR  */
  YYSYMBOL_KW_NOT = 19,                    /* KW_NOT  */
  YYSYMBOL_KW_EQUAL = 20,                  /* KW_EQUAL  */
  YYSYMBOL_KW_LESS = 21,                   /* KW_LESS  */
  YYSYMBOL_KW_NIL = 22,                    /* KW_NIL  */
  YYSYMBOL_KW_APPEND = 23,                 /* KW_APPEND  */
  YYSYMBOL_KW_LIST = 24,                   /* KW_LIST  */
  YYSYMBOL_KW_CONCAT = 25,                 /* KW_CONCAT  */
  YYSYMBOL_KW_SET = 26,                    /* KW_SET  */
  YYSYMBOL_KW_FOR = 27,                    /* KW_FOR  */
  YYSYMBOL_KW_IF = 28,                     /* KW_IF  */
  YYSYMBOL_KW_EXIT = 29,                   /* KW_EXIT  */
  YYSYMBOL_IDENTIFIER = 30,                /* IDENTIFIER  */
  YYSYMBOL_YYACCEPT = 31,                  /* $accept  */
  YYSYMBOL_START = 32,                     /* START  */
  YYSYMBOL_FUNCTION = 33,                  /* FUNCTION  */
  YYSYMBOL_EXP = 34                        /* EXP  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   200

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  31
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  4
/* YYNRULES -- Number of rules.  */
#define YYNRULES  34
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  83

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   285


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   430,   430,   432,   433,   434,   435,   448,   449,   450,
     456,   457,   458,   459,   460,   463,   464,   465,   466,   467,
     468,   471,   474,   482,   483,   484,   487,   488,   489,   490,
     491,   492,   493,   494,   495
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "OP_PLUS", "OP_MINUS",
  "OP_MULT", "OP_DIV", "OP_OP", "OP_CP", "COMMENT", "OP_COMMA", "KW_DEF",
  "KW_DISP", "KW_TRUE", "KW_FALSE", "VALUEF", "KW_LOAD", "KW_AND", "KW_OR",
  "KW_NOT", "KW_EQUAL", "KW_LESS", "KW_NIL", "KW_APPEND", "KW_LIST",
  "KW_CONCAT", "KW_SET", "KW_FOR", "KW_IF", "KW_EXIT", "IDENTIFIER",
  "$accept", "START", "FUNCTION", "EXP", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-27)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -27,    29,   -27,   -27,   -27,   -27,   -27,    44,   -27,   -27,
     -27,   -27,   -27,   -27,   -27,   -27,   137,   137,   137,   137,
     -26,   137,   137,   137,   137,   137,   137,   -19,   137,     5,
      91,    63,   137,   104,   137,   137,   157,     7,   137,   137,
      13,   137,   137,   137,   137,   -27,   -27,   124,    16,   -27,
      22,    31,    32,   170,    33,   -27,    37,    46,   -27,    49,
      50,    52,   137,   -27,    68,   -27,   -27,   -27,   -27,   137,
      69,   -27,   -27,   -27,   -27,   -27,   -27,    70,   -27,    71,
     -27,   -27,   -27
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       2,     0,     1,    31,    32,    33,    34,     0,     5,    29,
      28,    27,    30,    26,     4,     3,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     6,    23,     0,     0,    14,
       0,     0,     0,    26,     0,    22,     0,     0,    15,     0,
       0,     0,     0,    24,     0,    10,    11,    12,    13,    26,
       0,     7,    16,    17,    18,    19,    21,     0,    25,     0,
       8,    20,     9
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -27,   -27,   -27,   -16
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,     1,    14,    15
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    47,    55,    48,    50,    51,    52,
      54,    58,    56,    57,    65,    59,    60,    61,    62,     2,
      66,    64,     3,     4,     5,     6,     7,    70,     8,    67,
      68,    71,     9,    10,    11,    72,    77,    16,    17,    18,
      19,    12,     0,    79,    73,    20,    21,    74,    75,    13,
      76,    22,    23,    24,    25,    26,    16,    17,    18,    19,
      27,     0,    28,    29,    30,    21,    78,    80,    81,    82,
      22,    23,    24,    25,    26,     0,     0,     0,     0,    27,
       0,    28,     0,    30,     3,     4,     5,     6,    31,    46,
       0,     0,     0,     0,     9,    10,    11,     3,     4,     5,
       6,    31,    49,    12,     0,     0,     0,     9,    10,    11,
       0,    13,     0,     0,     0,     0,    12,     3,     4,     5,
       6,    31,    63,     0,    13,     0,     0,     9,    10,    11,
       3,     4,     5,     6,    31,     0,    12,     0,     0,     0,
       9,    10,    11,     0,    13,     0,     0,     0,     0,    12,
       3,     4,     5,     6,    31,     0,     0,    13,     0,     0,
       9,    10,    11,     3,     4,     5,     6,    31,     0,    12,
       0,     0,     0,     9,    10,    11,     0,    53,     0,     0,
       0,     0,    12,     0,     0,     0,     0,     0,     0,     0,
      69
};

static const yytype_int8 yycheck[] =
{
      16,    17,    18,    19,    30,    21,    22,    23,    24,    25,
      26,    30,    28,     8,    30,     8,    32,    33,    34,    35,
      36,     8,    38,    39,     8,    41,    42,    43,    44,     0,
       8,    47,     3,     4,     5,     6,     7,    53,     9,     8,
       8,     8,    13,    14,    15,     8,    62,     3,     4,     5,
       6,    22,    -1,    69,     8,    11,    12,     8,     8,    30,
       8,    17,    18,    19,    20,    21,     3,     4,     5,     6,
      26,    -1,    28,    29,    30,    12,     8,     8,     8,     8,
      17,    18,    19,    20,    21,    -1,    -1,    -1,    -1,    26,
      -1,    28,    -1,    30,     3,     4,     5,     6,     7,     8,
      -1,    -1,    -1,    -1,    13,    14,    15,     3,     4,     5,
       6,     7,     8,    22,    -1,    -1,    -1,    13,    14,    15,
      -1,    30,    -1,    -1,    -1,    -1,    22,     3,     4,     5,
       6,     7,     8,    -1,    30,    -1,    -1,    13,    14,    15,
       3,     4,     5,     6,     7,    -1,    22,    -1,    -1,    -1,
      13,    14,    15,    -1,    30,    -1,    -1,    -1,    -1,    22,
       3,     4,     5,     6,     7,    -1,    -1,    30,    -1,    -1,
      13,    14,    15,     3,     4,     5,     6,     7,    -1,    22,
      -1,    -1,    -1,    13,    14,    15,    -1,    30,    -1,    -1,
      -1,    -1,    22,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      30
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    32,     0,     3,     4,     5,     6,     7,     9,    13,
      14,    15,    22,    30,    33,    34,     3,     4,     5,     6,
      11,    12,    17,    18,    19,    20,    21,    26,    28,    29,
      30,     7,    34,    34,    34,    34,    30,    34,    34,    34,
      34,    34,    34,    30,    34,     8,     8,    34,    34,     8,
      34,    34,    34,    30,    34,     8,    34,    34,     8,    34,
      34,    34,    34,     8,    34,     8,     8,     8,     8,    30,
      34,     8,     8,     8,     8,     8,     8,    34,     8,    34,
       8,     8,     8
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    31,    32,    32,    32,    32,    32,    33,    33,    33,
      34,    34,    34,    34,    34,    34,    34,    34,    34,    34,
      34,    34,    34,    34,    34,    34,    34,    34,    34,    34,
      34,    34,    34,    34,    34
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     2,     4,     5,     6,     7,
       5,     5,     5,     5,     4,     4,     5,     5,     5,     5,
       6,     5,     4,     3,     4,     5,     1,     1,     1,     1,
       1,     1,     1,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 3: /* START: START EXP  */
#line 432 "gpp_interpreter.y"
                                       { (yyval.string) = (yyvsp[0].string); }
#line 1679 "y.tab.c"
    break;

  case 4: /* START: START FUNCTION  */
#line 433 "gpp_interpreter.y"
                                       { (yyval.string) = (yyvsp[0].string); }
#line 1685 "y.tab.c"
    break;

  case 5: /* START: START COMMENT  */
#line 434 "gpp_interpreter.y"
                                       { printf("COMMENT\n"); }
#line 1691 "y.tab.c"
    break;

  case 6: /* START: START OP_OP KW_EXIT OP_CP  */
#line 435 "gpp_interpreter.y"
                                       { freeMemory(); printf("$_\n"); exit(EXIT_FAILURE); }
#line 1697 "y.tab.c"
    break;

  case 7: /* FUNCTION: OP_OP KW_DEF IDENTIFIER EXP OP_CP  */
#line 448 "gpp_interpreter.y"
                                                           { defineFunction(1, (yyvsp[-2].string), (yyvsp[-1].string)); (yyval.string) = (yyvsp[-2].string); }
#line 1703 "y.tab.c"
    break;

  case 8: /* FUNCTION: OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP  */
#line 449 "gpp_interpreter.y"
                                                           { defineFunction(0, (yyvsp[-3].string), (yyvsp[-1].string)); (yyval.string) = (yyvsp[-3].string); }
#line 1709 "y.tab.c"
    break;

  case 9: /* FUNCTION: OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP  */
#line 450 "gpp_interpreter.y"
                                                              { defineFunction(0, (yyvsp[-4].string), (yyvsp[-1].string)); (yyval.string) = (yyvsp[-4].string); }
#line 1715 "y.tab.c"
    break;

  case 10: /* EXP: OP_OP OP_PLUS EXP EXP OP_CP  */
#line 456 "gpp_interpreter.y"
                                     { (yyval.string) = "OP_PLUS";  if (type == 1) { evaluateOperator("OP_PLUS",  resolveVariable((yyvsp[-2].string)), resolveVariable((yyvsp[-1].string))); } }
#line 1721 "y.tab.c"
    break;

  case 11: /* EXP: OP_OP OP_MINUS EXP EXP OP_CP  */
#line 457 "gpp_interpreter.y"
                                      { (yyval.string) = "OP_MINUS"; if (type == 1) { evaluateOperator("OP_MINUS", resolveVariable((yyvsp[-2].string)), resolveVariable((yyvsp[-1].string))); } }
#line 1727 "y.tab.c"
    break;

  case 12: /* EXP: OP_OP OP_MULT EXP EXP OP_CP  */
#line 458 "gpp_interpreter.y"
                                      { (yyval.string) = "OP_MULT";  if (type == 1) { evaluateOperator("OP_MULT",  resolveVariable((yyvsp[-2].string)), resolveVariable((yyvsp[-1].string))); } }
#line 1733 "y.tab.c"
    break;

  case 13: /* EXP: OP_OP OP_DIV EXP EXP OP_CP  */
#line 459 "gpp_interpreter.y"
                                      { (yyval.string) = "OP_DIV";   if (type == 1) { evaluateOperator("OP_DIV",   resolveVariable((yyvsp[-2].string)), resolveVariable((yyvsp[-1].string))); } }
#line 1739 "y.tab.c"
    break;

  case 14: /* EXP: OP_OP OP_MINUS EXP OP_CP  */
#line 460 "gpp_interpreter.y"
                                      { (yyval.string) = "OP_MINUS"; if (type == 1) { evaluateOperator("OP_MINUS", resolveVariable((yyvsp[-1].string)), NULL); } }
#line 1745 "y.tab.c"
    break;

  case 15: /* EXP: OP_OP KW_NOT EXP OP_CP  */
#line 463 "gpp_interpreter.y"
                                      { (yyval.string) = notOperation(resolveVariable((yyvsp[-1].string))); }
#line 1751 "y.tab.c"
    break;

  case 16: /* EXP: OP_OP KW_AND EXP EXP OP_CP  */
#line 464 "gpp_interpreter.y"
                                      { (yyval.string) = andOperation(resolveVariable((yyvsp[-2].string)), resolveVariable((yyvsp[-1].string))); }
#line 1757 "y.tab.c"
    break;

  case 17: /* EXP: OP_OP KW_OR EXP EXP OP_CP  */
#line 465 "gpp_interpreter.y"
                                      { (yyval.string) = orOperation(resolveVariable((yyvsp[-2].string)), resolveVariable((yyvsp[-1].string))); }
#line 1763 "y.tab.c"
    break;

  case 18: /* EXP: OP_OP KW_EQUAL EXP EXP OP_CP  */
#line 466 "gpp_interpreter.y"
                                      { (yyval.string) = checkEquality(resolveVariable((yyvsp[-2].string)), resolveVariable((yyvsp[-1].string))); }
#line 1769 "y.tab.c"
    break;

  case 19: /* EXP: OP_OP KW_LESS EXP EXP OP_CP  */
#line 467 "gpp_interpreter.y"
                                      { (yyval.string) = compareLessThan(resolveVariable((yyvsp[-2].string)), resolveVariable((yyvsp[-1].string))); }
#line 1775 "y.tab.c"
    break;

  case 20: /* EXP: OP_OP KW_IF EXP EXP EXP OP_CP  */
#line 468 "gpp_interpreter.y"
                                      { (yyval.string) = (strcmp((yyvsp[-3].string), "true") == 0) ? (yyvsp[-2].string) : (yyvsp[-1].string); }
#line 1781 "y.tab.c"
    break;

  case 21: /* EXP: OP_OP KW_SET IDENTIFIER EXP OP_CP  */
#line 471 "gpp_interpreter.y"
                                          { setVariable((yyvsp[-2].string), (yyvsp[-1].string)); (yyval.string) = (yyvsp[-1].string); }
#line 1787 "y.tab.c"
    break;

  case 22: /* EXP: OP_OP KW_DISP EXP OP_CP  */
#line 474 "gpp_interpreter.y"
                                          { (yyval.string) = resolveVariable((yyvsp[-1].string)); 
                                              if ((yyval.string) != NULL && strcmp((yyval.string), "nil") != 0) { 
                                                  printf("%s\n", (yyval.string)); 
                                              } else { 
                                                  fprintf(stderr, "Error: This is (%s) a nil variable (it is not printable)\n", (yyvsp[-1].string)); 
                                                  exit(EXIT_FAILURE); 
                                              }
                                            }
#line 1800 "y.tab.c"
    break;

  case 23: /* EXP: OP_OP IDENTIFIER OP_CP  */
#line 482 "gpp_interpreter.y"
                                          { (yyval.string) = getFunctionValue(1, (yyvsp[-1].string), NULL, NULL); }
#line 1806 "y.tab.c"
    break;

  case 24: /* EXP: OP_OP IDENTIFIER EXP OP_CP  */
#line 483 "gpp_interpreter.y"
                                          { (yyval.string) = getFunctionValue(0, (yyvsp[-2].string), resolveVariable((yyvsp[-1].string)), NULL); }
#line 1812 "y.tab.c"
    break;

  case 25: /* EXP: OP_OP IDENTIFIER EXP EXP OP_CP  */
#line 484 "gpp_interpreter.y"
                                          { (yyval.string) = getFunctionValue(0, (yyvsp[-3].string), resolveVariable((yyvsp[-2].string)), resolveVariable((yyvsp[-1].string))); }
#line 1818 "y.tab.c"
    break;

  case 26: /* EXP: IDENTIFIER  */
#line 487 "gpp_interpreter.y"
                                          { (yyval.string) = (yyvsp[0].string); type = 0; }
#line 1824 "y.tab.c"
    break;

  case 27: /* EXP: VALUEF  */
#line 488 "gpp_interpreter.y"
                                          { (yyval.string) = (yyvsp[0].string); type = 1; }
#line 1830 "y.tab.c"
    break;

  case 28: /* EXP: KW_FALSE  */
#line 489 "gpp_interpreter.y"
                                          { (yyval.string) = "false"; }
#line 1836 "y.tab.c"
    break;

  case 29: /* EXP: KW_TRUE  */
#line 490 "gpp_interpreter.y"
                                          { (yyval.string) = "true"; }
#line 1842 "y.tab.c"
    break;

  case 30: /* EXP: KW_NIL  */
#line 491 "gpp_interpreter.y"
                                          { (yyval.string) = "nil"; }
#line 1848 "y.tab.c"
    break;

  case 31: /* EXP: OP_PLUS  */
#line 492 "gpp_interpreter.y"
                                          { (yyval.string) = "OP_PLUS"; }
#line 1854 "y.tab.c"
    break;

  case 32: /* EXP: OP_MINUS  */
#line 493 "gpp_interpreter.y"
                                          { (yyval.string) = "OP_MINUS"; }
#line 1860 "y.tab.c"
    break;

  case 33: /* EXP: OP_MULT  */
#line 494 "gpp_interpreter.y"
                                          { (yyval.string) = "OP_MULT"; }
#line 1866 "y.tab.c"
    break;

  case 34: /* EXP: OP_DIV  */
#line 495 "gpp_interpreter.y"
                                          { (yyval.string) = "OP_DIV"; }
#line 1872 "y.tab.c"
    break;


#line 1876 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 498 "gpp_interpreter.y"


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
