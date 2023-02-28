%option noyywrap
%{
    #define YY_NO_UNPUT
    #define YY_NO_INPUT

    #include "parser.h"
    #include <ostream>
    #include <fstream>
    using namespace std;

    extern FILE *yyin; 
    extern FILE *yyout;
    extern bool dump_tokens;

    void DEBUG_FOR_LAB4(std::string s){
        std::string DEBUG_INFO = "[DEBUG LAB4]: \t" + s + "\n";
        fputs(DEBUG_INFO.c_str(), yyout);
    }
%}

DECIMIAL ([1-9][0-9]*|0)
OCTADECIMAL ([0][1-7][0-7]*)
HEXADECIMAL (0[xX][1-9a-fA-F][0-9a-fA-F]*)
ID [[:alpha:]_][[:alpha:][:digit:]_]*
EOL (\r\n|\n|\r)
WHITE [\t ]

LINECOMMENT \/\/([^\n])*

BLOCKCOMMENTBEGIN \/\*
BLOCKCOMMENT1 \n
BLOCKCOMMENT2 .
BLOCKCOMMENTEND \*\/
%x COMMENT

%%

"int" {
    /*
    * Questions: 
    *   Q1: Why we need to return INT in further labs?
    *   Q2: What is "INT" actually?
    */
    if(dump_tokens)
        DEBUG_FOR_LAB4("INT\tint");
    return INT;
}
"void" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("VOID\tvoid");
    return VOID;
}
"float" {
   if(dump_tokens)
        DEBUG_FOR_LAB4("FLOAT\tvoid");
    return FLOAT;
}
"if" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("IF\tif");
    return IF;
};
"else" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("ELSE\telse");
    return ELSE;
};
"return" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("RETURN\treturn");
    return RETURN;
};
"while" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("WHILE\twhile");
    return WHILE;
}
"const" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("CONST\tconst");
    return CONST;
}
"break" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("BREAK\tbreak");
    return BREAK;
}
"continue" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("CONTINUE\tcontinue");
    return CONTINUE;
}
"=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("ASSIGN\t=");
    return ASSIGN;
}
"==" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("EQ\t==");
    return EQ;
}
"!=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("NOTEQ\t!=");
    return NOTEQ;
}
"<=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LESSQ\t<=");
    return LESSQ;
}
">=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("MOREQ\t>=");
    return MOREQ;
}

"<" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LESS\t<");
    return LESS;
}
">" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("MORE\t<");
    return MORE;
}
"+" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("ADD\t+");
    return ADD;
}
"-" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("SUB\t-");
    return SUB;
}
"*" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("MUL\t*");
    return MUL;
}
"/" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("DIV\t/");
    return DIV;
}
"%" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("DIV\t%");
    return MOD;
}
"&&" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("AND\t<");
    return AND;
}
"||" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("OR\t<");
    return OR;
}
"!" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("NOT\t<");
    return NOT;
}
";" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("SEMICOLON\t;");
    return SEMICOLON;
}
"(" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LPAREN\t(");
    return LPAREN;
}
")" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("RPAREN\t)");
    return RPAREN;
}
"{" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LBRACE\t{");
    return LBRACE;
}
"}" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("RBRACE\t}");
    return RBRACE;
}

"[" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LBRACKET\t[");
    
    return LBRACKET;
}
"]" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("RBRACKET\t}");
    
    return RBRACKET;
}






"," {
    if(dump_tokens)
        DEBUG_FOR_LAB4("COMMA\t,");
    return COMMA;
}
"getint" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("GETINT\t,");
    return GETINT;
}
"putint" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("PUTINT\t,");
    return PUTINT;
}
"putch" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("PUTCH\t");
    return PUTCH;
}
"getch" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("GETCH\t,");
    return GETCH;
}
"getfloat" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("GETFLOAT\t,");
    return GETFLOAT;
}
"putfloat" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("PUTFLOAT\t,");
    return PUTFLOAT;
}




{DECIMIAL} {
    if(dump_tokens)
        DEBUG_FOR_LAB4(yytext);
    yylval.itype = atoi(yytext);
    return INTEGER;
}
{OCTADECIMAL} {
    if(dump_tokens)
        DEBUG_FOR_LAB4(yytext);
    int num = 0; 
    string s(yytext);
    int siz=s.size();
    int a=0;
    for (int i=0;i<siz;i++) {
            a= yytext[i]-'0';
            num = num*8 +a ;
        }
    yylval.itype=num;
    return INTEGER;
    
}
{HEXADECIMAL} {
    int num = 0; 
    // string s(yytext);
    // int siz=s.size();
    // int a=0;
    // for (int i=0;i<siz;i++) {
    //         if(i<2){

    //         }
    //         else{
    //             if('A'<=yytext[i]&&yytext[i]<='F'){
    //                 a= (yytext[i]-'A')+10;
    //             }
    //             if('a'<=yytext[i]&&yytext[i]<='f'){
    //                 a= (yytext[i]-'a')+10;
    //             }
    //             else{
    //                 a= yytext[i]-'0';
    //             }
    //             num = num*16 +a ; 
    //         }
    // }
    sscanf(yytext, "%x", &num);
    yylval.itype=num;
    return INTEGER;

}
{ID} {
    if(dump_tokens)
        DEBUG_FOR_LAB4(yytext);
    char *lexeme;
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    return ID;
}

{EOL} yylineno++;
{WHITE}

{LINECOMMENT} 
{BLOCKCOMMENTBEGIN} {BEGIN COMMENT;}
<COMMENT>{BLOCKCOMMENT1} {
    yylineno++;
}       
<COMMENT>{BLOCKCOMMENT2} {}
<COMMENT>{BLOCKCOMMENTEND} {BEGIN INITIAL;}
%%
