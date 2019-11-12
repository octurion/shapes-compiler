%{
#include "ast.h"
#include "parser.tab.h"

#define YY_USER_ACTION update_loc(yylloc, yytext, yyleng);

static void init_loc(YYLTYPE* loc);
static void update_loc(YYLTYPE* loc, const char* text, yy_size_t len);
%}

%option reentrant
%option warn
%option bison-bridge bison-locations
%option never-interactive
%option nodefault noyywrap nounput noinput

DIGIT [0-9]
ALPHA [a-zA-Z_]
ALNUM [0-9a-zA-Z_]

%%

"class"  { return T_CLASS;  }
"layout" { return T_LAYOUT; }
"pool"   { return T_POOL;   }
"rec"    { return T_REC;    }
"let"    { return T_LET;    }
"fn"     { return T_FN;     }

{ALPHA}{ALNUM}* {
	// It's Bison that's going to set up the location properly
	yylval->ident = new Identifier(std::string(yytext, yyleng), Location());
	return T_IDENT;
}

"=" { return T_EQ; }

"+" { return T_PLUS; }

"," { return T_COMMA;     }
":" { return T_COLON;     }
";" { return T_SEMICOLON; }

"<" { return T_LANGLE; }
">" { return T_RANGLE; }

"[" { return T_LSQUARE; }
"]" { return T_RSQUARE; }

"(" { return T_LPAREN; }
")" { return T_RPAREN; }

"{" { return T_LBRACE; }
"}" { return T_RBRACE; }

[\t\r\n ] ;

<<EOF>> { return T_END; }

. { return T_UNRECOGNIZED; }

%%

static void update_loc(YYLTYPE* loc, const char* text, yy_size_t len)
{
	if (len == 1 && text[0] == '\n') {
		loc->first_line += 1;
		loc->last_line += 1;

		loc->first_column = loc->last_column = 1;
		return;
	}

	if (loc->last_column == 1) {
		loc->last_column = 0;
	}

	loc->first_column = loc->last_column + 1;
	loc->last_column = loc->first_column + len - 1;
}
