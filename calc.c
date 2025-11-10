// Eren Unal 241adb152
// gcc -std=c11 -O2 -Wall -Wextra -o calc calc.c -lm
// ./calc test8.txt
//
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef _WIN32
#define SEP "\\"
#else
#define SEP "/"
#endif

// === TOKEN DEFINITIONS ===
typedef enum { 
    T_NUMBER, T_PLUS, T_MINUS, T_STAR, T_SLASH, 
    T_POW, T_LPAREN, T_RPAREN, T_EOF 
} TokenType;

typedef struct {
    TokenType type;
    const char *start;
    int pos1;
    double num;
} Token;

// === SCANNER ===
// Converts input string into a stream of tokens.
typedef struct {
    const char *src;
    size_t len;
    size_t i;
    int pos1;
    int at_line_start;
} Scanner;

static int g_err = 0;
static void set_err(int p){ if(!g_err) g_err = p; }

static void scan_init(Scanner *S, const char *src, size_t len){
    S->src = src; S->len = len; S->i = 0; S->pos1 = 1; S->at_line_start = 1;
}
static int peek(Scanner *S){ return (S->i>=S->len)? EOF : (unsigned char)S->src[S->i]; }
static int getc2(Scanner *S){
    if (S->i>=S->len) return EOF;
    int c = (unsigned char)S->src[S->i++];
    S->pos1++;
    S->at_line_start = (c=='\n');
    return c;
}

// Skip spaces and line comments starting with '#'
static void skip_ws_and_comment(Scanner *S){
    for(;;){
        while(isspace(peek(S))) getc2(S);
        if (S->at_line_start){
            size_t j=S->i;
            while(j<S->len && isspace((unsigned char)S->src[j]) && S->src[j]!='\n') j++;
            if (j<S->len && S->src[j]=='#'){
                while(1){ int c=getc2(S); if(c=='\n'||c==EOF) break; }
                continue;
            }
        }
        break;
    }
}

// === TOKENIZER ===
static Token next_tok(Scanner *S){
    skip_ws_and_comment(S);
    Token t; memset(&t,0,sizeof(t));
    t.start = S->src + S->i;
    t.pos1 = S->pos1;

    int c = peek(S);
    if (c==EOF){ t.type = T_EOF; return t; }

    // number literal
    if (isdigit(c) || c=='.'){
        char *endp;
        errno = 0;
        double v = strtod(S->src + S->i, &endp);
        if (endp == S->src + S->i){
            getc2(S);
            set_err(t.pos1);
            t.type = T_EOF;
            return t;
        }
        size_t used = (size_t)(endp - (S->src + S->i));
        for(size_t k=0;k<used;k++) getc2(S);
        t.type = T_NUMBER; t.num = v;
        return t;
    }

    // symbols and operators
    if (c=='+'){ getc2(S); t.type=T_PLUS; return t; }
    if (c=='-'){ getc2(S); t.type=T_MINUS; return t; }
    if (c=='*'){
        getc2(S);
        if (peek(S)=='*'){ getc2(S); t.type=T_POW; return t; }
        t.type=T_STAR; return t;
    }
    if (c=='/'){ getc2(S); t.type=T_SLASH; return t; }
    if (c=='('){ getc2(S); t.type=T_LPAREN; return t; }
    if (c==')'){ getc2(S); t.type=T_RPAREN; return t; }

    // unknown character
    getc2(S); set_err(t.pos1); t.type=T_EOF; return t;
}

// === PARSER ===
typedef struct { Token *a; size_t n, cap; } TV;
static void tv_push(TV *v, Token t){
    if (v->n==v->cap){ v->cap = v->cap? v->cap*2 : 64; v->a = (Token*)realloc(v->a, v->cap*sizeof(Token)); }
    v->a[v->n++] = t;
}

typedef struct { TV toks; size_t i; } Parser;
static Token* p_peek(Parser *P){ return (P->i<P->toks.n)? &P->toks.a[P->i] : NULL; }
static Token* p_adv(Parser *P){ if(P->i<P->toks.n) P->i++; return &P->toks.a[P->i-1]; }
static int p_match(Parser *P, TokenType tp){ Token *t=p_peek(P); if(t && t->type==tp){ p_adv(P); return 1;} return 0; }

static double parse_expr(Parser *P);

// primary: numbers or parenthesis
static double parse_primary(Parser *P, int *outpos){
    Token *t = p_peek(P);
    if (!t){ set_err(1); return 0; }
    if (t->type==T_NUMBER){ *outpos = t->pos1; p_adv(P); return t->num; }
    if (p_match(P,T_LPAREN)){
        int lpos = P->toks.a[P->i-1].pos1;
        double v = parse_expr(P);
        if (!p_match(P,T_RPAREN)){
            Token *u = p_peek(P);
            if (u) set_err(u->pos1); else set_err(lpos);
        }
        *outpos = lpos; return v;
    }
    set_err(t->pos1); return 0;
}

// handle unary +/- (like -3 or +4)
static double parse_unary(Parser *P, int *outpos){
    int sign = 1, pos = 0;
    while (p_match(P,T_PLUS) || p_match(P,T_MINUS)){
        Token *prev = &P->toks.a[P->i-1];
        if (prev->type==T_MINUS) sign = -sign;
        pos = prev->pos1;
    }
    double v = parse_primary(P, &pos);
    *outpos = pos; return sign*v;
}

// exponentiation right-associative (**)
static double parse_power(Parser *P, int *outpos){
    double left = parse_unary(P, outpos);
    if (p_match(P,T_POW)){
        int rpos=0; double right = parse_power(P, &rpos);
        *outpos = P->toks.a[P->i-1].pos1; return pow(left, right);
    }
    return left;
}

// * and /
static double parse_term(Parser *P, int *outpos){
    int apos=0; double val = parse_power(P,&apos);
    for(;;){
        if (p_match(P,T_STAR)){
            int bpos=0; double b = parse_power(P,&bpos);
            val *= b; *outpos = bpos;
        } else if (p_match(P,T_SLASH)){
            int bpos=0; double b = parse_power(P,&bpos);
            if (fabs(b) < 1e-12){ set_err(bpos); return 0; }
            val /= b; *outpos = bpos;
        } else break;
    }
    return val;
}

// + and -
static double parse_expr(Parser *P){
    int pos=0; double v = parse_term(P,&pos);
    for(;;){
        if (p_match(P,T_PLUS)){ int p2=0; v += parse_term(P,&p2); }
        else if (p_match(P,T_MINUS)){ int p2=0; v -= parse_term(P,&p2); }
        else break;
    }
    return v;
}

// tokenize whole input
static void tokenize_all(Scanner *S, TV *out){
    memset(out,0,sizeof(*out));
    for(;;){
        Token t = next_tok(S);
        tv_push(out,t);
        if (t.type==T_EOF) break;
        if (g_err) break;
    }
}

// === EVALUATION AND OUTPUT ===
static int eval_and_write(const char *buf, size_t len, FILE *fo){
    g_err = 0;
    Scanner Sc; scan_init(&Sc, buf, len);
    TV v; tokenize_all(&Sc, &v);

    if (!g_err){
        Parser P; P.toks = v; P.i=0;
        (void)parse_expr(&P);
        Token *t = p_peek(&P);
        if (t && t->type!=T_EOF) set_err(t->pos1);
    }

    if (g_err){
        fprintf(fo, "ERROR:%d\n", g_err);
        free(v.a);
        return 1;
    } else {
        Parser P2; P2.toks = v; P2.i=0;
        double val = parse_expr(&P2);
        double near = llround(val);
        if (fabs(val - near) < 1e-12) fprintf(fo, "%lld\n", (long long)near);
        else fprintf(fo, "%.15g\n", val);
        free(v.a);
        return 0;
    }
}

// === FILE & DIRECTORY HELPERS ===
static const char* getenv_or(const char *k, const char *d){
    const char *v = getenv(k); return (v && *v)? v : d;
}

static int ensure_dir(const char *path){
    struct stat st;
    if (stat(path,&st)==0){
#ifdef S_ISDIR
        if (S_ISDIR(st.st_mode)) return 0;
#else
        if ((st.st_mode & S_IFMT) == S_IFDIR) return 0;
#endif
        errno = ENOTDIR; return -1;
    }
#ifdef _WIN32
    return mkdir(path);
#else
    return mkdir(path, 0775);
#endif
}

// build default output dir name: <basename>_eren_<id>
static void build_default_outdir(const char *input_path, char *out, size_t n){
    const char *slash = strrchr(input_path, SEP[0]);
    const char *base = slash? slash+1 : input_path;
    char base_noext[256]; memset(base_noext,0,sizeof(base_noext));
    strncpy(base_noext, base, sizeof(base_noext)-1);
    char *dot = strrchr(base_noext, '.'); if (dot) *dot = '\0';
    const char *user = getenv_or("USER","eren");
    const char *sid  = getenv_or("STUDENT_ID","241adb152");
    snprintf(out, n, "%s_%s_%s", base_noext, user, sid);
}

// build file name for output
static void build_output_filename(const char *input_path, char *out, size_t n){
    const char *slash = strrchr(input_path, SEP[0]);
    const char *base = slash? slash+1 : input_path;
    const char *nm  = getenv_or("NAME","Eren");
    const char *ln  = getenv_or("LASTNAME","Unal");
    const char *sid = getenv_or("STUDENT_ID","241adb152");
    snprintf(out, n, "%s_%s_%s_%s.txt", base, nm, ln, sid);
}

// read full file into memory
static char* read_all(const char *path, size_t *olen){
    FILE *f = fopen(path,"rb"); if(!f) return NULL;
    fseek(f,0,SEEK_END); long sz = ftell(f); if(sz<0){ fclose(f); return NULL; }
    fseek(f,0,SEEK_SET);
    char *buf = (char*)malloc((size_t)sz+1); if(!buf){ fclose(f); return NULL; }
    size_t n = fread(buf,1,(size_t)sz,f); fclose(f); buf[n]='\0'; if(olen) *olen=n; return buf;
}

static int ends_with(const char *s, const char *suf){
    size_t a=strlen(s), b=strlen(suf); if(b>a) return 0; return strncmp(s+(a-b),suf,b)==0;
}

// process one .txt file
static int process_one(const char *in_path, const char *out_dir){
    size_t len=0; char *buf = read_all(in_path,&len);
    if(!buf){ fprintf(stderr,"read fail: %s\n", in_path); return 1; }
    if (ensure_dir(out_dir)!=0){ fprintf(stderr,"outdir fail: %s\n", out_dir); free(buf); return 1; }
    char out_name[512]; build_output_filename(in_path,out_name,sizeof(out_name));
    char out_path[1024]; snprintf(out_path,sizeof(out_path), "%s%s%s", out_dir, SEP, out_name);
    FILE *fo = fopen(out_path,"wb"); if(!fo){ fprintf(stderr,"write fail: %s\n", out_path); free(buf); return 1; }
    (void)eval_and_write(buf,len,fo);
    fclose(fo); free(buf); return 0;
}

// === MAIN ===
static void usage(void){
    fprintf(stderr,"Usage:\n");
    fprintf(stderr,"  calc [-d DIR|--dir DIR] [-o OUTDIR|--output-dir OUTDIR] input.txt\n");
}

int main(int argc, char **argv){
    const char *dir = NULL, *outd=NULL, *input=NULL;
    for(int i=1;i<argc;i++){
        if(!strcmp(argv[i],"-d")||!strcmp(argv[i],"--dir")){ if(i+1>=argc){usage(); return 1;} dir=argv[++i]; }
        else if(!strcmp(argv[i],"-o")||!strcmp(argv[i],"--output-dir")){ if(i+1>=argc){usage(); return 1;} outd=argv[++i]; }
        else if(argv[i][0]=='-'){ usage(); return 1; }
        else input=argv[i];
    }
    if(!input){ usage(); return 1; }

    char def_out[512]={0};
    if(!outd){ build_default_outdir(input,def_out,sizeof(def_out)); outd=def_out; }

    int rc=0;
    if (dir){
        DIR *d = opendir(dir);
        if(!d){ fprintf(stderr,"dir open fail: %s\n", dir); return 1; }
        struct dirent *e;
        while((e=readdir(d))){
            if(e->d_name[0]=='.') continue;
            if(!ends_with(e->d_name,".txt")) continue;
            char in_path[1024]; snprintf(in_path,sizeof(in_path), "%s%s%s", dir, SEP, e->d_name);
            rc |= process_one(in_path, outd);
        }
        closedir(d);
    } else {
        rc = process_one(input, outd);
    }
    return rc?1:0;
}
