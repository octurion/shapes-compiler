%{
#include "parser.tab.h"

#define YY_USER_INIT init_loc(yylloc);
#define YY_USER_ACTION update_loc(yylloc, yytext, yyleng, YY_START);

// Fix the tab width to 4 for now (when calculating line numbers)
static const unsigned TAB_WIDTH = 4;

static void init_loc(YYLTYPE* loc);
static void update_loc(YYLTYPE* loc, const char* text, yy_size_t len, int state);
%}

%option reentrant
%option warn
%option bison-bridge bison-locations
%option never-interactive
%option nodefault noyywrap nounput noinput

%x MULTILINE_COMMENT

DIGIT [0-9]
ALPHA [a-zA-Z_]
ALNUM [0-9a-zA-Z_]

NON_PRINT_ASCII [\x00-\x1F\x7F]

%%

"class"  { return T_CLASS;  }
"export" { return T_EXPORT; }
"where"  { return T_WHERE;  }
"layout" { return T_LAYOUT; }
"pool"   { return T_POOL;   }
"pools"  { return T_POOLS;  }
"rec"    { return T_REC;    }
"let"    { return T_LET;    }
"fn"     { return T_FN;     }
"none"   { return T_NONE;   }

"if"       { return T_IF;       }
"else"     { return T_ELSE;     }
"for"      { return T_FOR;      }
"foreach"  { return T_FOREACH;  }
"simd"     { return T_SIMD;     }
"while"    { return T_WHILE;    }
"break"    { return T_BREAK;    }
"continue" { return T_CONTINUE; }
"return"   { return T_RETURN;   }

"true"  { return T_TRUE;  }
"false" { return T_FALSE; }

"new"  { return T_NEW;  }
"null" { return T_NULL; }
"this" { return T_THIS; }

"as" { return T_AS; }

"uniform" { return T_UNIFORM; }
"varying" { return T_VARYING; }

"bool" { return T_BOOL; }
"i8"   { return T_I8;   }
"u8"   { return T_U8;   }
"i16"  { return T_I16;  }
"u16"  { return T_U16;  }
"i32"  { return T_I32;  }
"u32"  { return T_U32;  }
"i64"  { return T_I64;  }
"u64"  { return T_U64;  }
"f32"  { return T_F32;  }
"f64"  { return T_F64;  }

{ALPHA}{ALNUM}* {
	yylval->str = new std::string(yytext, yyleng);
	return T_IDENT;
}

{DIGIT}+("i8"|"u8"|"i16"|"u16"|"i32"|"u32"|"i64"|"u64")? {
	yylval->str = new std::string(yytext, yyleng);
	return T_INT_CONST;
}

{DIGIT}+("f32"|"f64") {
	yylval->str = new std::string(yytext, yyleng);
	return T_DOUBLE_CONST;
}

(({DIGIT}+"."{DIGIT}*)|("."{DIGIT}+))[Ee][+-]?{DIGIT}+("f32"|"f64")? {
	yylval->str = new std::string(yytext, yyleng);
	return T_DOUBLE_CONST;
}

"==" { return T_EQ; }
"!=" { return T_NE; }

"<=" { return T_GE; }
">=" { return T_LE; }

"+=" { return T_PLUS_ASSIGN;  }
"-=" { return T_MINUS_ASSIGN; }
"*=" { return T_TIMES_ASSIGN; }
"/=" { return T_DIV_ASSIGN;   }

"&=" { return T_AND_ASSIGN; }
"|=" { return T_OR_ASSIGN;  }
"^=" { return T_XOR_ASSIGN; }

"<<=" { return T_SHL_ASSIGN; }
">>=" { return T_SHR_ASSIGN; }

"+" { return T_PLUS;  }
"-" { return T_MINUS; }
"*" { return T_TIMES; }
"/" { return T_DIV;   }

"&&" { return T_LAND; }
"||" { return T_LOR;  }
"!"  { return T_NOT; }

"&" { return T_AND; }
"|" { return T_OR;  }
"^" { return T_XOR; }

"<<" { return T_SHL; }
">>" { return T_SHR; }

".." { return T_DOTDOT; }

"," { return T_COMMA;     }
":" { return T_COLON;     }
";" { return T_SEMICOLON; }
"." { return T_DOT; }

"<" { return T_LANGLE; }
">" { return T_RANGLE; }

"[" { return T_LSQUARE; }
"]" { return T_RSQUARE; }

"(" { return T_LPAREN; }
")" { return T_RPAREN; }

"{" { return T_LBRACE; }
"}" { return T_RBRACE; }

"="  { return T_ASSIGN; }

"/*" { BEGIN(MULTILINE_COMMENT); }
<MULTILINE_COMMENT>{
	"*/"    { BEGIN(INITIAL); }
	<<EOF>> { BEGIN(INITIAL); return T_UNTERMINATED_COMMENT; }
	(.|\n)  ;
}

"//".* ;

[\t\r\n ]* ;

<<EOF>> { return T_END; }

{NON_PRINT_ASCII} { return T_UNPRINTABLE_CHARACTER; }

. { return T_UNRECOGNIZED_TOKEN; }

%%

static void init_loc(YYLTYPE* loc)
{
	loc->first_line = 1;
	loc->first_column = 0;

	loc->last_line = 1;
	loc->last_column = 0;
}

static void update_loc(YYLTYPE* loc, const char* text, yy_size_t len, int state)
{
	if (state != MULTILINE_COMMENT) {
		loc->first_line = loc->last_line;
		loc->first_column = loc->last_column + 1;
	}

	for (yy_size_t i = 0; i < len; i++) {
		if (text[i] == '\n') {
			loc->last_line++;
			loc->last_column = 0;
			continue;
		}

		loc->last_column += (text[i] == '\t')
			? TAB_WIDTH - (loc->last_column % TAB_WIDTH)
			: 1;
	}
}
