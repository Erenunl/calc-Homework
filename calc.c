// Eren Unal 241adb152
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

// ---------- Configuration for naming ----------
static const char *STUDENT_FIRST = "Eren";
static const char *STUDENT_LAST  = "Unal";    
static const char *STUDENT_ID    = "241adb152";

// ---------- Tokenizer ----------
typedef enum {
    T_EOF = 0,
    T_NUM,
    T_PLUS, T_MINUS,
    T_STAR, T_SLASH,
    T_POW,          // **
    T_LPAREN, T_RPAREN
} TokenKind;

typedef struct {
    TokenKind kind;
    double    num;        
    size_t    start_pos;  
} Token;

typedef struct {
    Token *data;
    size_t len;
    size_t cap;
} TokenVec;

typedef struct {
    const char *src;      
    size_t      n;        
    size_t      i;       
    size_t      abspos;   
    int         at_line_start; 
} Scanner;

static void tv_init(TokenVec *v) { v->data=NULL; v->len=0; v->cap=0; }
static void tv_push(TokenVec *v, Token t) {
    if (v->len == v->cap) {
        size_t nc = v->cap ? v->cap*2 : 64;
        Token *nd = (Token*)realloc(v->data, nc * sizeof(Token));
        if (!nd) { perror("realloc"); exit(1); }
        v->data = nd; v->cap = nc;
    }
    v->data[v->len++] = t;
}
static void tv_free(TokenVec *v) { free(v->data); v->data=NULL; v->len=v->cap=0; }

static void scan_init(Scanner *s, const char *src, size_t n) {
    s->src = src; s->n = n; s->i = 0; s->abspos = 1; s->at_line_start = 1;
}

static int is_line_ws(int c) { return c==' ' || c=='\t' || c=='\r'; }

static void skip_ws_and_comments(Scanner *s) {
    for (;;) {
        // At line start, skip spaces/tabs, then if '#' -> skip to end of line
        size_t saved_i = s->i;
        size_t saved_pos = s->abspos;
        if (s->at_line_start) {
            while (s->i < s->n && is_line_ws((unsigned char)s->src[s->i])) {
                if (s->src[s->i] == '\t' || s->src[s->i] == ' ' || s->src[s->i] == '\r') {
                    s->i++; s->abspos++;
                } else {
                    break;
                }
            }
            if (s->i < s->n && s->src[s->i] == '#') {
                // skip until '\n' or EOF
                while (s->i < s->n) {
                    char c = s->src[s->i++];
                    s->abspos++;
                    if (c == '\n') { s->at_line_start = 1; break; }
                }
                continue; // loop again in case next line is also comment/blank
            }
        }
        // General whitespace anywhere (including newlines). Newline ends "line start" and begins next.
        int progressed = 0;
        while (s->i < s->n) {
            char c = s->src[s->i];
            if (c==' ' || c=='\t' || c=='\r') {
                s->i++; s->abspos++; progressed=1;
            } else if (c=='\n') {
                s->i++; s->abspos++; progressed=1;
                s->at_line_start = 1;
            } else {
                break;
            }
        }
        // If we didn't consume anything in this iteration and no comment matched, we are done
        if (!progressed && (saved_i == s->i && saved_pos == s->abspos)) break;
        // Otherwise, loop again to catch a possible comment after new line start
    }
}

static Token make_token(TokenKind k, size_t pos) {
    Token t; t.kind = k; t.num = 0.0; t.start_pos = pos; return t;
}

static void scan_tokens(Scanner *s, TokenVec *out) {
    tv_init(out);
    while (s->i < s->n) {
        skip_ws_and_comments(s);
        if (s->i >= s->n) break;

        size_t tok_pos = s->abspos;
        char c = s->src[s->i];

        // Numbers (float allowed). We do NOT treat leading +/- as unary here.
        if (isdigit((unsigned char)c) || (c=='.' && s->i+1<s->n && isdigit((unsigned char)s->src[s->i+1]))) {
            // strtod from current location
            char *endptr = NULL;
            errno = 0;
            double val = strtod(s->src + s->i, &endptr);
            // compute how many chars consumed
            size_t consumed = (size_t)(endptr - (s->src + s->i));
            if (consumed == 0) {
                // Should not happen due to initial check, treat as single '.' token error later
                s->i++; s->abspos++;
                tv_push(out, make_token(T_EOF, tok_pos)); // force stop
                break;
            } else {
                // Advance
                s->i += consumed;
                s->abspos += consumed;
                Token t = make_token(T_NUM, tok_pos);
                t.num = val;
                tv_push(out, t);
                s->at_line_start = 0;
                continue;
            }
        }

        // Operators / Parens
        if (c == '+') { s->i++; s->abspos++; tv_push(out, make_token(T_PLUS, tok_pos)); s->at_line_start=0; continue; }
        if (c == '-') { s->i++; s->abspos++; tv_push(out, make_token(T_MINUS,tok_pos)); s->at_line_start=0; continue; }
        if (c == '*') {
            // check for '**'
            if (s->i+1 < s->n && s->src[s->i+1] == '*') {
                s->i += 2; s->abspos += 2; tv_push(out, make_token(T_POW, tok_pos)); s->at_line_start=0; continue;
            } else {
                s->i++; s->abspos++; tv_push(out, make_token(T_STAR, tok_pos)); s->at_line_start=0; continue;
            }
        }
        if (c == '/') { s->i++; s->abspos++; tv_push(out, make_token(T_SLASH, tok_pos)); s->at_line_start=0; continue; }
        if (c == '(') { s->i++; s->abspos++; tv_push(out, make_token(T_LPAREN, tok_pos)); s->at_line_start=0; continue; }
        if (c == ')') { s->i++; s->abspos++; tv_push(out, make_token(T_RPAREN, tok_pos)); s->at_line_start=0; continue; }

        // Unknown character: push a fake token to allow parser to flag error at this pos, then try to resync
        // But per spec, we report first error position; pushing EOF-like stop helps.
        // We'll represent this by pushing a T_EOF but at current position, which parser will likely treat as unexpected.
        // Before that, advance one char to avoid infinite loop.
        s->i++; s->abspos++; s->at_line_start = (s->src[s->i-1] == '\n');
        // Create a special token sequence: we'll encode as PLUS of a 0-length? Simpler: store as EOF and stop.
        tv_push(out, make_token(T_EOF, tok_pos));
        break;
    }
    // EOF token with start at position n+1
    tv_push(out, make_token(T_EOF, s->n + 1));
}

// ---------- Parser / Evaluator ----------
typedef struct {
    TokenVec toks;
    size_t   idx;        
    size_t   error_pos;  
} Parser;

static Token *peek(Parser *p) {
    if (p->idx < p->toks.len) return &p->toks.data[p->idx];
    return &p->toks.data[p->toks.len-1];
}
static Token *advance(Parser *p) {
    if (p->idx < p->toks.len) return &p->toks.data[p->idx++];
    return &p->toks.data[p->toks.len-1];
}
static int match(Parser *p, TokenKind k) {
    if (peek(p)->kind == k) { advance(p); return 1; }
    return 0;
}

typedef struct {
    double value;
    size_t start_pos; 
} Eval;

// Forward declarations
static Eval parse_expr(Parser *p);
static Eval parse_term(Parser *p);
static Eval parse_power(Parser *p);
static Eval parse_primary(Parser *p);

static void set_error(Parser *p, size_t pos) {
    if (p->error_pos == 0) p->error_pos = pos ? pos : 1;
}

static Eval make_eval(double v, size_t pos) { Eval e; e.value=v; e.start_pos=pos; return e; }

static Eval parse_primary(Parser *p) {
    Token *t = peek(p);
    if (t->kind == T_NUM) {
        advance(p);
        return make_eval(t->num, t->start_pos);
    }
    if (t->kind == T_LPAREN) {
        size_t lpos = t->start_pos;
        advance(p);
        Eval e = parse_expr(p);
        if (p->error_pos) return e;
        if (!match(p, T_RPAREN)) {
            // Expected ')': report at current token start (which could be EOF at n+1)
            set_error(p, peek(p)->start_pos);
            return e;
        }
        // For position, keep '(' position as the start of this primary
        return make_eval(e.value, lpos);
    }
    // Unexpected token
    set_error(p, t->start_pos);
    return make_eval(0.0, t->start_pos);
}

// Right-associative power: primary ('**' power)?
static Eval parse_power(Parser *p) {
    Eval left = parse_primary(p);
    if (p->error_pos) return left;
    if (match(p, T_POW)) {
        // Exponent part
        Token *powTok = &p->toks.data[p->idx-1];
        Eval right = parse_power(p); // recurse for right-assoc
        if (p->error_pos) return left;
        // pow with doubles; mimic Python: 0**0 == 1
        errno = 0;
        double res = pow(left.value, right.value);
        (void)powTok; 
        return make_eval(res, left.start_pos);
    }
    return left;
}

static Eval parse_term(Parser *p) {
    Eval acc = parse_power(p);
    if (p->error_pos) return acc;
    for (;;) {
        TokenKind k = peek(p)->kind;
        if (k == T_STAR || k == T_SLASH) {
            Token *op = advance(p);
            Eval rhs = parse_power(p);
            if (p->error_pos) return acc;
            if (k == T_STAR) {
                acc.value = acc.value * rhs.value;
            } else {
                
                if (fabs(rhs.value) <= 0.0) {
                    // report at start of divisor (rhs)
                    set_error(p, rhs.start_pos);
                    return acc;
                }
                acc.value = acc.value / rhs.value;
            }
            
        } else {
            break;
        }
    }
    return acc;
}

static Eval parse_expr(Parser *p) {
    Eval acc = parse_term(p);
    if (p->error_pos) return acc;
    for (;;) {
        TokenKind k = peek(p)->kind;
        if (k == T_PLUS || k == T_MINUS) {
            advance(p);
            Eval rhs = parse_term(p);
            if (p->error_pos) return acc;
            if (k == T_PLUS) acc.value = acc.value + rhs.value;
            else             acc.value = acc.value - rhs.value;
        } else {
            break;
        }
    }
    return acc;
}

// ---------- Evaluation entry ----------
static int is_integral_double(double x) {
    // Check closeness to nearest long long
    double r = nearbyint(x);
    return fabs(x - r) < 1e-12;
}

static int evaluate_buffer(const char *buf, size_t n, double *out_val, size_t *out_errpos) {
    Scanner s; scan_init(&s, buf, n);
    TokenVec v; scan_tokens(&s, &v);
    Parser p; p.toks = v; p.idx = 0; p.error_pos = 0;

    Eval e = parse_expr(&p);
    if (p.error_pos == 0) {
        // after parse, expect EOF
        if (peek(&p)->kind != T_EOF) {
            set_error(&p, peek(&p)->start_pos);
        }
    }
    int ok = 0;
    if (p.error_pos) {
        *out_errpos = p.error_pos;
        ok = 0;
    } else {
        *out_val = e.value;
        ok = 1;
    }
    tv_free(&v);
    return ok;
}

// ---------- Filesystem utilities ----------
static int ensure_dir_exists(const char *path) {
    struct stat st;
    if (stat(path, &st) == 0) {
        if (S_ISDIR(st.st_mode)) return 0;
        fprintf(stderr, "Path exists but is not a directory: %s\n", path);
        return -1;
    }
    if (mkdir(path, 0775) != 0) {
        perror("mkdir");
        return -1;
    }
    return 0;
}

static const char *basename_noext(const char *path, char *out, size_t outsz) {
    const char *base = strrchr(path, '/');
#ifdef _WIN32
    const char *base2 = strrchr(path, '\\');
    if (!base || (base2 && base2 > base)) base = base2;
#endif
    base = base ? base+1 : path;
    // copy base
    snprintf(out, outsz, "%s", base);
    // strip extension if ".txt"
    size_t len = strlen(out);
    if (len >= 4 && strcmp(out+len-4, ".txt") == 0) {
        out[len-4] = '\0';
    }
    return out;
}

static void choose_default_outdir(const char *input_label, char *outdir, size_t outdir_sz) {
    // <input_base>_<username>_<studentid>
    const char *user = getenv("USER");
#ifdef _WIN32
    if (!user) user = getenv("USERNAME");
#endif
    if (!user) user = "user";
    snprintf(outdir, outdir_sz, "%s_%s_%s", input_label, user, STUDENT_ID);
}

// ---------- I/O processing ----------
static int write_result_file(const char *outdir, const char *infile_path, int is_dir_mode, double val, int ok, size_t errpos) {
    char base[PATH_MAX]; basename_noext(infile_path, base, sizeof(base));

    // Output filename: <base>_Eren_Unal_241adb152.txt
    char outname[PATH_MAX];
    snprintf(outname, sizeof(outname), "%s_%s_%s_%s.txt", base, STUDENT_FIRST, STUDENT_LAST, STUDENT_ID);

    char outpath[PATH_MAX];
    snprintf(outpath, sizeof(outpath), "%s/%s", outdir, outname);

    FILE *f = fopen(outpath, "w");
    if (!f) { perror("fopen output"); return -1; }
    if (!ok) {
        fprintf(f, "ERROR:%zu\n", errpos);
    } else {
        if (is_integral_double(val)) {
            // print as integer without decimal
            // Use llround to avoid "-0"
            long long ival = (long long) llround(val);
            fprintf(f, "%lld\n", ival);
        } else {
            fprintf(f, "%.15g\n", val);
        }
    }
    fclose(f);
    return 0;
}

static int process_single_file(const char *infile, const char *outdir) {
    // Read entire file
    FILE *f = fopen(infile, "rb");
    if (!f) { perror("fopen input"); return -1; }
    if (fseek(f, 0, SEEK_END) != 0) { perror("fseek"); fclose(f); return -1; }
    long sz = ftell(f);
    if (sz < 0) { perror("ftell"); fclose(f); return -1; }
    if (fseek(f, 0, SEEK_SET) != 0) { perror("fseek"); fclose(f); return -1; }

    char *buf = (char*)malloc((size_t)sz + 1);
    if (!buf) { perror("malloc"); fclose(f); return -1; }
    size_t rd = fread(buf, 1, (size_t)sz, f);
    fclose(f);
    if (rd != (size_t)sz) { fprintf(stderr, "Short read\n"); free(buf); return -1; }
    buf[sz] = '\0';

    double val = 0.0; size_t errpos = 0;
    int ok = evaluate_buffer(buf, (size_t)sz, &val, &errpos);
    free(buf);
    return write_result_file(outdir, infile, 0, val, ok, errpos);
}

static int ends_with_txt(const char *name) {
    size_t len = strlen(name);
    return (len >= 4 && strcmp(name+len-4, ".txt")==0);
}

static int process_directory(const char *dirpath, const char *outdir) {
    DIR *d = opendir(dirpath);
    if (!d) { perror("opendir"); return -1; }
    struct dirent *ent;
    int rc = 0;
    while ((ent = readdir(d)) != NULL) {
        if (strcmp(ent->d_name, ".")==0 || strcmp(ent->d_name, "..")==0) continue;
        char path[PATH_MAX];
        snprintf(path, sizeof(path), "%s/%s", dirpath, ent->d_name);

        // Stat to ensure it's a regular file and ends with .txt
        struct stat st;
        if (stat(path, &st) != 0) continue;
        if (!S_ISREG(st.st_mode)) continue;
        if (!ends_with_txt(ent->d_name)) continue;

        if (process_single_file(path, outdir) != 0) rc = -1;
    }
    closedir(d);
    return rc;
}

// ---------- CLI ----------
static void usage(const char *prog) {
    fprintf(stderr, "Usage:\n");
    fprintf(stderr, "  %s [-d DIR|--dir DIR] [-o OUTDIR|--output-dir OUTDIR] input.txt\n", prog);
    fprintf(stderr, "Notes:\n");
    fprintf(stderr, "  - If -d/--dir is provided, processes all *.txt in DIR (ignores subfolders). 'input.txt' is ignored in this mode.\n");
    fprintf(stderr, "  - If -o is omitted, creates default output folder <input_base>_<username>_%s.\n", STUDENT_ID);
}

int main(int argc, char **argv) {
    const char *diropt = NULL;
    const char *outdir_opt = NULL;
    const char *input_file = NULL;

    // Simple argv parse
    for (int i=1; i<argc; i++) {
        if ((strcmp(argv[i], "-d")==0 || strcmp(argv[i], "--dir")==0) && i+1<argc) {
            diropt = argv[++i];
        } else if ((strcmp(argv[i], "-o")==0 || strcmp(argv[i], "--output-dir")==0) && i+1<argc) {
            outdir_opt = argv[++i];
        } else if (argv[i][0] == '-' ) {
            usage(argv[0]); return 2;
        } else {
            input_file = argv[i];
        }
    }

    if (!diropt && !input_file) { usage(argv[0]); return 2; }

    char outdir[PATH_MAX];
    if (outdir_opt) {
        snprintf(outdir, sizeof(outdir), "%s", outdir_opt);
    } else {
        // Choose default based on single input or dir
        char label[PATH_MAX];
        if (diropt) {
            // Use last path component of diropt as base
            const char *base = strrchr(diropt, '/');
#ifdef _WIN32
            const char *base2 = strrchr(diropt, '\\');
            if (!base || (base2 && base2 > base)) base = base2;
#endif
            base = base ? base+1 : diropt;
            snprintf(label, sizeof(label), "%s", base);
        } else {
            basename_noext(input_file, label, sizeof(label));
        }
        choose_default_outdir(label, outdir, sizeof(outdir));
    }

    if (ensure_dir_exists(outdir) != 0) return 1;

    int rc = 0;
    if (diropt) {
        rc = process_directory(diropt, outdir);
    } else {
        rc = process_single_file(input_file, outdir);
    }
    return (rc==0) ? 0 : 1;
}
