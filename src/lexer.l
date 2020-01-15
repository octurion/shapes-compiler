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

"new"  { return T_NEW;  }
"null" { return T_NULL; }
"this" { return T_THIS; }

"uniform" { return T_UNIFORM; }
"varying" { return T_VARYING; }

"void" { return T_VOID; }
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
	// It's Bison that's going to set up the location properly
	yylval->ident = new Identifier(std::string(yytext, yyleng), Location());
	return T_IDENT;
}

{DIGIT}+ {
	// It's Bison that's going to set up the location properly
	yylval->num = new Number(std::string(yytext, yyleng), Location());
	return T_NUM;
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

"="  { return T_ASSIGN;   }

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
