#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <map>
#include <functional>

#include <cinttypes>
#include <cstring>
#include <cfloat>
#include <cassert>
#include <cstdarg>

#define car(x) (x->v_cons.head)
#define cdr(x) (x->v_cons.tail)
#define cons(x, y) (makeCons(x, y))
#define acons(x, y, z) (cons(makeCons(x, y), z))

#define INTPOOL_MIN -5
#define INTPOOL_MAX 256

enum ObjType {
    T_NULL,
    T_INT,
    T_FLOAT,
    T_STRING,
    T_BOOL,
    T_SYMBOL,
    T_CONS,
    T_ENV,
    T_BULITIN,
    T_FUNCTION,
    T_LAMBDA,
    T_MACRO
};

struct Obj;

typedef Obj*(*Bulitin)(Obj*, Obj*);

struct Obj {
    ObjType type;
    union {
        int64_t v_int;
        double v_float;
        char* v_str;
        bool v_bool;
        char* v_symbol;
        struct {
            Obj* fn_name;
            int64_t fn_param_count;
            union {
                struct {
                    Bulitin ptr;
                } v_bulitin;
                struct {
                    Obj* params;
                    Obj* body;
                    Obj* env;
                } v_lambda;
                struct {
                    Obj* params;
                    Obj* body;
                } v_function;
                struct {
                    Obj* params;
                    Obj* body;
                } v_macro;
            };
        };
        
        struct {
            Obj* head;
            Obj* tail;
        } v_cons;
        struct {
            Obj* up; // up env
            Obj* vars; // cons(cons(name1 obj1) cons(cons(name2 obj2) cons(cons(name3 obj3) cons(cons(name4 obj4) null))))
        } v_env;
    };
    Obj(ObjType type)
    : type(type) {
    }
};

static Obj* nullObj;
static Obj* trueObj;
static Obj* falseObj;
static Obj* globalEnv;
static Obj* symbols;
static Obj* modules;

static std::map<int64_t, Obj*> integerCacheMap;

Obj* parse_list();
Obj* parse_int_float();
Obj* parse_string();
Obj* parse_symbol();
Obj* parse_quote();
Obj* eval(Obj* env, Obj* x);
Obj* eval_list(Obj* env, Obj* x);
Obj* macroexpand(Obj* env, Obj* macro, Obj* args);
int64_t list_length(Obj* x);
bool is_list(Obj* x);
std::string readTextFile(const std::string& filename);

void printStackTrace(Obj* env) {
    // TODO unimplements
}

void throw_error_v(Obj* env, const char* format, va_list ap) {
    printStackTrace(env);
    vprintf(format, ap);
    ::exit(-1);
}

void throw_error(Obj* env, const char* format, ...) {
    va_list ap;
    va_start(ap, format);
    throw_error_v(env, format, ap);
    va_end(ap);
}

void throw_error_assert(bool cond, Obj* env, const char* format, ...) {
    if(cond) return;
    va_list ap;
    va_start(ap, format);
    throw_error_v(env, format, ap);
    va_end(ap);
}

Obj* toBoolObj(bool b) {
    return b ? trueObj : falseObj;
}

std::string typeToString(ObjType type);

Obj* makeObj(ObjType type) {
    return new Obj(type);
}

Obj* makeCons(Obj* head, Obj* tail) {
    Obj* obj = makeObj(T_CONS);
    obj->v_cons.head = head;
    obj->v_cons.tail = tail;
    return obj;
}

// integer cache pool


Obj* makeInt(int64_t x) {
    Obj* obj = nullptr;
    if(x >= INTPOOL_MIN && x < INTPOOL_MAX) {
        if(integerCacheMap.find(x) == integerCacheMap.end()) {
            obj = makeObj(T_INT);
            obj->v_int = x;
            integerCacheMap[x] = obj;
        }
        return integerCacheMap[x];
    }
    obj = makeObj(T_INT);
    obj->v_int = x;
    return obj;
}

Obj* makeFloat(double x) {
    auto obj = makeObj(T_FLOAT);
    obj->v_float = x;
    return obj;
}

Obj* makeSymbol(const char* name) {
    Obj* obj = makeObj(T_SYMBOL);
    obj->v_symbol = ::strdup(name);
    symbols = cons(obj, symbols);
    return obj;
}

Obj* makeString(const char* str) {
    Obj* obj = makeObj(T_STRING);
    obj->v_str = ::strdup(str);
    return obj;
}

Obj* makeEnv(Obj* up, Obj* vars) {
    Obj* obj = makeObj(T_ENV);
    obj->v_env.up = up;
    obj->v_env.vars = vars;
    return obj;
}

Obj* pushEnv(Obj* env, Obj* vars, Obj* values) {
    throw_error_assert(list_length(vars) == list_length(values), env, "can't apply function: number of argument does not match");
    Obj *map = nullObj;
    for (Obj *p = vars, *q = values; p != nullObj; p = cdr(p), q = cdr(q)) {
        Obj *sym = car(p);
        Obj *val = car(q);
        map = acons(sym, val, map);
    }
    return makeEnv(env, map);
}

Obj* intern(const char* name) {
    for(Obj* p = symbols; p != nullObj; p = cdr(p)) {
        if(::strcmp(car(p)->v_symbol, name) == 0) {
            return car(p);
        }
    }
    return makeSymbol(name);
}

void addVar(Obj* env, Obj* symbol, Obj* obj) {
    env->v_env.vars = acons(symbol, obj, env->v_env.vars);
}

void visitObj(Obj* obj, const std::function<void(Obj*)>& cb, bool recursion) {
    cb(obj);
    if(obj->type == T_CONS) {
        for(Obj* p = obj; p != nullObj; p = cdr(p)) {
            if(recursion) {
                visitObj(car(p), cb, recursion);
            } else {
                cb(car(p));
            }
        }
    }
}

Obj* findVar(Obj* env, Obj* symbol) {
    Obj* vars = env->v_env.vars;
    if(vars != nullObj) {
        for(Obj* var = car(vars); vars != nullObj; vars = cdr(vars), var = car(vars)) {
            if(car(var) == symbol || ::strcmp(car(var)->v_symbol, symbol->v_symbol) == 0) {
                return var;
            }
        }
    }
    if(env->v_env.up != nullObj) {
        return findVar(env->v_env.up, symbol);
    }
    return nullptr;
}

static std::string source;
static size_t sourcePosition = 0;
static size_t sourceLength = 0;

int peekChar() {
    if(sourcePosition < sourceLength) {
        return source[sourcePosition];
    }
    return EOF;
}

int nextChar() {
    if(sourcePosition < sourceLength) {
        return source[sourcePosition++];
    }
    return EOF;
}

int skipChar(int ch) {
    int c = nextChar();
    assert(c == ch);
    return c;
}


Obj* parse() {
    for(;;) {
        int c = peekChar();
        if(::isspace(c)) {
            nextChar();
            continue;
        }
        if(c == EOF) {
            return nullptr;
        }
        if(c == ';') {
            while(peekChar() != '\n') {
                nextChar();
            }
            skipChar('\n');
            continue;
        }
        if(c == '(') {
            return parse_list();
        }
        if(::isdigit(c)) {
            return parse_int_float();
        }
        if(c == '\"') {
            return parse_string();
        }
        if(c == '\'') {
            return parse_quote();
        }
        if(::isalpha(c) || ::strchr("+-*/=!@#$%^&", c)) {
            return parse_symbol();
        }
        throw_error(globalEnv, "ParserError: unprocessed character: %c", c);
    }
    return nullObj;
}

Obj* parse_all() {
    Obj *head, *tail;
    head = tail = cons(parse(), nullObj);
    while(peekChar() != EOF) {
        tail->v_cons.tail = cons(parse(), nullObj);
        tail = tail->v_cons.tail;
    }
    return head;
}

Obj* parse_int_float() {
    bool isFloat = false;
    char s[64];
    char* p = s;
    if(peekChar() == '-') {
        *p++ = nextChar();
    }
    while(::isdigit(peekChar())) {
        *p++ = nextChar();
    }
    if(peekChar() == '.') {
        isFloat = true;
        *p++ = nextChar();
        while(::isdigit(peekChar())) {
            *p++ = nextChar();
        }
    }
    *p = '\0';
    Obj* obj;
    if(isFloat) {
        obj = makeFloat(::atof(s));
    } else {
        obj = makeInt(::atof(s));
    }
    return obj;
}

Obj* parse_string() {
    skipChar('\"');
    char str[1024] = { 0 };
    char *p = str;
    while(peekChar() != '\"') {
        *p++ = nextChar();
    }
    skipChar('\"');
    return makeString(str);
}

Obj* parse_symbol() {
    char sym[128] = { 0 };
    int p = 0;
    while(::isalpha(peekChar()) || ::strchr("+-*/=!@#$%^&", peekChar())) {
        sym[p++] = nextChar();
    }
    sym[p] = '\0';
    return intern(sym);
}

Obj* parse_list() {
    skipChar('(');
    if(peekChar() == ')') {
        nextChar();
        return nullObj;
    }

    Obj *head, *tail;
    head = tail = cons(parse(), nullObj);

    while(peekChar() != ')') {
        if(::isspace(peekChar())) {
            nextChar();
            continue;
        }
        tail->v_cons.tail = cons(parse(), nullObj);
        tail = tail->v_cons.tail;
    }

    skipChar(')');

    return head;
}


void run_before(const std::string& text) {
    source.assign(text);
    sourcePosition = 0;
    sourceLength = source.length();
}

Obj* run(Obj* env) {
    Obj* retObj = nullObj;
    for(Obj* p = parse_all(); p != nullObj; p = cdr(p)) {
        retObj = eval(env, car(p));
    }
    return retObj;
}

bool moduleExists(const char* moduleName) {
    assert(moduleName);
    for(Obj* p = modules; p != nullObj; p = cdr(p)) {
        if(!strcmp(car(p)->v_str, moduleName)) {
            return true;
        }
    }
    return false;
}

void loadModule(Obj* env, const std::string& moduleName) {
    if(moduleExists(moduleName.c_str())) return;
    modules = cons(makeString(moduleName.c_str()), modules);
    std::cout << "load module: " << moduleName << std::endl;
    run_before(readTextFile(moduleName));
    run(env);
}

std::string typeToString(ObjType type) {
    switch(type) {
        case T_NULL: return "NULL";
        case T_INT: return "INT";
        case T_FLOAT: return "FLOAT";
        case T_STRING: return "STRING";
        case T_BOOL: return "BOOL";
        case T_SYMBOL: return "SYMBOL";
        case T_CONS: return "CONS";
        case T_BULITIN: return "BULITIN";
        case T_FUNCTION: return "FUNCTION";
        case T_LAMBDA: return "LAMBDA";
        case T_MACRO: return "MACRO";
        case T_ENV: return "ENV";
    }
    return "UNDEFINED";
}

Obj* bulitin_typeof(Obj* env, Obj* x) {
    return intern(typeToString(car(x)->type).c_str());
}

int objToStr(Obj* x, char* str);

Obj* print(Obj* x) {
    char str[1024] = { 0 };
    if(objToStr(x, str) > 0) {
        ::printf("%s", str);
    }
    return nullObj;
}

Obj* bulitin_print(Obj* env, Obj* x) {
    print(car(x));
    return nullObj;
}

Obj* bulitin_println(Obj* env, Obj* x) {
    bulitin_print(env, x);
    putchar('\n');
    return nullObj;
}

#define binary_op(f, op) \
Obj* bulitin_##f(Obj* env, Obj* x) {    \
    Obj* a = eval(env, car(x)); \
    Obj* b = eval(env, car(cdr(x)));    \
    if(::strcmp(#op, "+") == 0 && (a->type == T_STRING && b->type == T_STRING)) return makeString(strcat(a->v_str, b->v_str)); \
    if(a->type == T_INT) {  \
        if(b->type == T_INT) return makeInt(a->v_int op b->v_int);    \
        else if(b->type == T_FLOAT) return makeFloat(static_cast<double>(a->v_int) op b->v_float);   \
    } else if(a->type == T_FLOAT) { \
        if(b->type == T_INT) return makeFloat(a->v_float op static_cast<double>(b->v_int));   \
        else if(b->type == T_FLOAT) return makeFloat(a->v_float op b->v_float);  \
    } \
    throw_error(env, "TypeError: unsupported operand type(s) for %s: '%s' and '%s'", #op, typeToString(a->type).c_str(), typeToString(b->type).c_str()); \
    return nullObj; \
}

binary_op(add, +);
binary_op(sub, -);
binary_op(mul, *);
binary_op(div, /);

Obj* bulitin_list(Obj* env, Obj* x) {
    return x;
}

Obj* bulitin_car(Obj* env, Obj* x) {
    return car(car(x));
}

Obj* bulitin_cdr(Obj* env, Obj* x) {
    return cdr(car(x));
}

Obj* bulitin_cons(Obj* env, Obj* x) {
    return cons(car(x), car(cdr(x)));
}

Obj* bulitin_progn(Obj* env, Obj* x) {
    Obj* currentEnv = makeEnv(env, nullObj);
    Obj* retObj = nullObj;
    for(Obj* p = x; p != nullObj; p = cdr(p)) {
        retObj = eval(currentEnv, car(p));
    }
    return retObj;
}

Obj* bulitin_setq(Obj* env, Obj* x) {
    assert(car(x)->type == T_SYMBOL);
    Obj* var = findVar(env, car(x));
    Obj* obj = eval(env, car(cdr(x)));
    if(var) {
        var->v_cons.tail = obj;
    } else {
        addVar(env, car(x), obj);
    }
    return obj;
}

Obj* check_paramters(Obj* env, Obj* params) {
    if(params == intern("null")) params = nullObj;
    for(Obj* p = params; p != nullObj; p = cdr(p)) {
        throw_error_assert(p->type == T_CONS, env, "parameter list is not a flat list");
        throw_error_assert(car(p)->type == T_SYMBOL, env, "parameter must be a symbol");
    }
    return params;
}

Obj* bulitin_defun(Obj* env, Obj* x) {
    Obj* funcObj = makeObj(T_FUNCTION);
    funcObj->fn_name = car(x);
    funcObj->v_function.params = check_paramters(env, car(cdr(x)));
    funcObj->fn_param_count = list_length(car(cdr(x)));
    funcObj->v_function.body = cdr(cdr(x));
    addVar(env, funcObj->fn_name, funcObj);
    return funcObj;
}

Obj* bulitin_lambda(Obj* env, Obj* x) {
    Obj* params = car(cdr(x));
    if(params == intern("null")) {
        params = nullObj;
    }
    Obj* lambdaObj = makeObj(T_LAMBDA);
    lambdaObj->fn_name = intern("LAMBDA1");
    lambdaObj->v_lambda.params = check_paramters(env, car(x));
    lambdaObj->fn_param_count = list_length(car(x));
    lambdaObj->v_lambda.body = cdr(x);
    lambdaObj->v_lambda.env = env;
    return lambdaObj;
}

Obj* bulitin_defmacro(Obj* env, Obj* x) {
    Obj* macroObj = makeObj(T_MACRO);
    macroObj->fn_name = car(x);
    macroObj->v_macro.params = check_paramters(env, car(cdr(x)));
    macroObj->fn_param_count = list_length(car(cdr(x)));
    macroObj->v_macro.body = cdr(cdr(x));
    addVar(env, macroObj->fn_name, macroObj);
    return macroObj;
}

// (macroexpand '(F 1 2 3))
Obj* bulitin_macroexpand(Obj* env, Obj* x) {
    return macroexpand(env, cdr(findVar(env, car(car(x)))), cdr(car(x)));
}

Obj* cloneObj(Obj* x) {
    Obj* destObj = ::makeObj(x->type); 
    ::memcpy(destObj, x, sizeof(Obj));
    if(destObj->type == T_CONS) {
        for(Obj* p = destObj; p != nullObj; p = cdr(p)) {
            p->v_cons.head = cloneObj(car(p));
        }
    }
    return destObj;
}

Obj* parse_quote() {
    skipChar('\'');
    Obj* quote = intern("quote");
    return cons(quote, cons(parse(), nullObj));
}

Obj* bulitin_quote(Obj* env, Obj* x)  {
    return car(x);
}

Obj* bulitin_cond(Obj* env, Obj* x) {
    for(Obj* p = x; p != nullObj; p = cdr(p)) {
        Obj* item = car(p);
        Obj* cond = eval(env, car(item));
        if(cond != falseObj && cond != nullObj) {
            return eval(env, car(cdr(item)));
        }
    }
    return nullObj;
}

Obj* bulitin_if(Obj* env, Obj* x) {
    Obj* test = eval(env, car(x));
    Obj* thenBody = car(cdr(x));
    Obj* elseBody = car(cdr(cdr(x)));
    if(test == falseObj || test == nullObj) {
        return eval(env, elseBody);
    } else {
        return eval(env, thenBody);
    }
}

#define binary_logic_op(f, op) \
Obj* bulitin_##f(Obj* env, Obj* x) { \
    Obj* a = car(x); \
    Obj* b = car(cdr(x)); \
    if((a == nullObj || b == nullObj) && (::strcmp(#op , "==") == 0 || ::strcmp(#op , "!=") == 0)) return toBoolObj(a op b); \
    if(a->type == T_INT && b->type == T_INT) return toBoolObj(a->v_int op b->v_int); \
    if(a->type == T_INT && b->type == T_FLOAT) return toBoolObj(static_cast<double>(a->v_int) op b->v_float); \
    if(a->type == T_FLOAT && b->type == T_FLOAT) return toBoolObj(a->v_float op b->v_float); \
    if(a->type == T_FLOAT && b->type == T_INT) return toBoolObj(a->v_float op static_cast<double>(b->v_int)); \
    if(a->type == T_STRING && ::strcmp(#op , "==") == 0) return toBoolObj(::strcmp(a->v_str, b->v_str) == 0); \
    if(a->type == T_STRING && ::strcmp(#op , "!=") == 0) return toBoolObj(::strcmp(a->v_str, b->v_str) != 0); \
    if(a->type == T_SYMBOL && ::strcmp(#op , "==") == 0) return toBoolObj(::strcmp(a->v_symbol, b->v_symbol) == 0); \
    if(a->type == T_SYMBOL && ::strcmp(#op , "!=") == 0) return toBoolObj(::strcmp(a->v_symbol, b->v_symbol) != 0); \
    throw_error(env, "TypeError: '%s' not supported between instances of '%s' and '%s'", #f, typeToString(a->type).c_str(), typeToString(b->type).c_str()); \
    return falseObj; \
}

binary_logic_op(eq, ==);
binary_logic_op(neq, !=);
binary_logic_op(gt, >);
binary_logic_op(gte, >=);
binary_logic_op(lt, <);
binary_logic_op(lte, <=);

int64_t list_length(Obj* x) {
    int64_t i = 0;
    for(Obj* p = x; p != nullObj; p = cdr(p)) {
        if(p->type != T_CONS) {
            // is not a list
            return -1;
        }
        i++;
    }
    return i;
}

bool is_list(Obj* x) {
    return list_length(x) != -1;
}

Obj* bulitin_length(Obj* env, Obj* x)  {
    return makeInt(list_length(car(x)));
}

Obj* bulitin_eval(Obj* env, Obj* x) {
    x = car(x);
    if(x->type == T_STRING) {
        run_before(std::string(x->v_str));
        return run(env);
    }
    return eval(env, x);
}

Obj* bulitin_import(Obj* env, Obj* x) {
    throw_error_assert(car(x)->type == T_STRING, env, "import() parameter type must be string");
    loadModule(env, std::string(car(x)->v_str));
    return nullObj;
}

int objToStr(Obj* x, char* str) {
    switch(x->type) {
        case T_NULL: strcpy(str, "null"); return 4;
        case T_INT: return sprintf(str, "%" PRId64, x->v_int);
        case T_FLOAT: return sprintf(str, "%f", x->v_float);
        case T_STRING: strcpy(str, x->v_str); break;
        case T_BOOL: x->v_bool ? strcpy(str, "true") : strcpy(str, "false"); break;
        case T_SYMBOL: strcpy(str, x->v_symbol); break;
        case T_CONS: {
            char* ptr = str;
            *ptr++ = '(';
            for(Obj* p = x; p != nullObj; p = cdr(p)) {
                if(p->type == T_CONS) {
                    ptr += objToStr(car(p), ptr);
                    if(cdr(p) != nullObj) *ptr++ = ' ';
                } else {
                    *ptr++ = '.';
                    *ptr++ = ' ';
                    ptr += objToStr(p, ptr);
                    break;
                }
            }
            *ptr++ = ')';
            break;
        }
        default: sprintf(str, "<%s>", typeToString(x->type).c_str()); break;
    }
    return strlen(str);
}

void addBulitin(Obj* env, const char* name, Bulitin bulitin, int64_t param_count) {
    Obj* bulitinObj = makeObj(T_BULITIN);
    bulitinObj->fn_name = intern(name);
    bulitinObj->fn_param_count = param_count;
    bulitinObj->v_bulitin.ptr = bulitin;
    addVar(env, bulitinObj->fn_name, bulitinObj);
}

void defineBulitins(Obj* env) {
    addBulitin(env, "print", bulitin_print, 1);
    addBulitin(env, "println", bulitin_println, 1);
    addBulitin(env, "+", bulitin_add, 2);
    addBulitin(env, "-", bulitin_sub, 2);
    addBulitin(env, "*", bulitin_mul, 2);
    addBulitin(env, "/", bulitin_div, 2);
    addBulitin(env, "list", bulitin_list, -1); /* unlimited number of parameters */
    addBulitin(env, "car", bulitin_car, 1);
    addBulitin(env, "cdr", bulitin_cdr, 1);
    addBulitin(env, "progn", bulitin_progn, -1);
    addBulitin(env, "setq", bulitin_setq, 2);
    addBulitin(env, "defun", bulitin_defun, 3);
    addBulitin(env, "quote", bulitin_quote, 1);
    addBulitin(env, "length", bulitin_length, 1);
    addBulitin(env, "cond", bulitin_cond, -1);
    addBulitin(env, "if", bulitin_if, 3);
    addBulitin(env, "eq", bulitin_eq, 2);
    addBulitin(env, "neq", bulitin_neq, 2);
    addBulitin(env, "gt", bulitin_gt, 2);
    addBulitin(env, "gte", bulitin_gte, 2);
    addBulitin(env, "lt", bulitin_lt, 2);
    addBulitin(env, "lte", bulitin_lte, 2);
    addBulitin(env, "typeof", bulitin_typeof, 1);
    addBulitin(env, "lambda", bulitin_lambda, 2);
    addBulitin(env, "eval", bulitin_eval, 1);
    addBulitin(env, "defmacro", bulitin_defmacro, 3);
    addBulitin(env, "macroexpand", bulitin_macroexpand, 1);
    addBulitin(env, "cons", bulitin_cons, 2);
    addBulitin(env, "import", bulitin_import, 1);

    addVar(env, intern("null"), nullObj);
    addVar(env, intern("true"), trueObj);
    addVar(env, intern("false"), falseObj);

    addVar(env, intern("NULL"), intern("NULL"));
    addVar(env, intern("INT"), intern("INT"));
    addVar(env, intern("FLOAT"), intern("FLOAT"));
    addVar(env, intern("STRING"), intern("STRING"));
    addVar(env, intern("SYMBOL"), intern("SYMBOL"));
    addVar(env, intern("BOOL"), intern("BOOL"));
    addVar(env, intern("CONS"), intern("CONS"));
    addVar(env, intern("BULITIN"), intern("BULITIN"));
    addVar(env, intern("FUNCTION"), intern("FUNCTION"));
    addVar(env, intern("ENV"), intern("ENV"));
    addVar(env, intern("UNDEFINED"), intern("UNDEFINED"));
}

bool isNotEvalListBulitin(Bulitin bulitin) {
    return bulitin == bulitin_quote
        || bulitin == bulitin_setq
        || bulitin == bulitin_defun
        || bulitin == bulitin_lambda
        || bulitin == bulitin_defmacro
        || bulitin == bulitin_cond
        || bulitin == bulitin_progn
        || bulitin == bulitin_if;
}

Obj* macroexpand(Obj* env, Obj* macro, Obj* args) {
    Obj* newEnv = pushEnv(env, macro->v_macro.params, args);
    // return bulitin_progn(newEnv, cons(cons(macro->v_macro.body, nullObj), nullObj));
    return bulitin_progn(newEnv, macro->v_macro.body);
}

Obj* apply_macro(Obj* env, Obj* macro, Obj* args) {
    Obj* expanded = macroexpand(env, macro, args);
    return eval(env, expanded);
}

Obj* apply_function(Obj* env, Obj* fn, Obj* args) {
    Obj* newEnv = nullptr;
    Obj* body = nullptr;
    int64_t argc = list_length(args);
    throw_error_assert(fn->fn_param_count == -1 || argc == fn->fn_param_count, env, 
        "%s() takes %" PRId64 " positional arguments but %" PRId64 " were given", 
        fn->fn_name->v_symbol, fn->fn_param_count, argc);
    if(fn->type != T_BULITIN || !isNotEvalListBulitin(fn->v_bulitin.ptr)) {
        args = eval_list(env, args);
    }
    if(fn->type == T_BULITIN) {
        return fn->v_bulitin.ptr(env, args);
    } else if(fn->type == T_FUNCTION) {
        newEnv = pushEnv(env, fn->v_function.params, args);
        body = fn->v_function.body;
    } else if(fn->type == T_LAMBDA) {
        newEnv = pushEnv(fn->v_lambda.env, fn->v_lambda.params, args);
        body = fn->v_lambda.body;
    }
    return bulitin_progn(newEnv, body);
}

Obj* eval_list(Obj* env, Obj* x) {
    Obj *head, *tail;
    head = tail = nullptr;
    for (Obj *lp = x; lp != nullObj; lp = cdr(lp)) {
        Obj *tmp = eval(env, car(lp));
        if(head == NULL) {
            head = tail = cons(tmp, nullObj);
        } else {
            tail->v_cons.tail = cons(tmp, nullObj);
            tail = tail->v_cons.tail;
        }
    }
    return head == nullptr ? nullObj : head;
}

Obj* eval(Obj* env, Obj* x) {
    if(!x) return nullObj;
    switch(x->type) {
    case T_NULL:
        return nullObj;
    case T_INT:
    case T_FLOAT:
    case T_STRING:
        return x;
    case T_SYMBOL: {
            Obj* obj = findVar(env, x);
            if(obj == nullptr || obj == nullObj) {
                throw_error(env, "can't find symbol: %s", x->v_symbol);
            }
            return cdr(obj);
        }
    case T_CONS: {
        Obj* obj = eval(env, car(x));
        Obj* args = cdr(x);
        if(obj->type == T_BULITIN || obj->type == T_FUNCTION || obj->type == T_LAMBDA) {
            return apply_function(env, obj, args);
        } else if(obj->type == T_MACRO) {
            return apply_macro(env, obj, args);
        } else {
            char objstr[512] = { 0 };
            objToStr(obj, objstr);
            throw_error(env, "can't call type: %s(%s)", typeToString(obj->type).c_str(), objstr);
        }
    }
    default: break;
    }
    return x;
}

std::string readTextFile(const std::string& filename) {
    std::ifstream ifs(filename);
    std::stringstream ss;
    ss << ifs.rdbuf();
    ifs.close();
    return ss.str();
}

void init() {
    nullObj = makeObj(T_NULL);
    trueObj = makeObj(T_BOOL); trueObj->v_bool = true;
    falseObj = makeObj(T_BOOL); falseObj->v_bool = false;
    globalEnv = makeEnv(nullObj, nullObj);
    symbols = nullObj;
    modules = nullObj;
    defineBulitins(globalEnv);
    loadModule(globalEnv, "./lib.lisp");
}

void repl() {
    std::string input;
    for(;;) {
        std::cout << ">>> ";
        std::getline(std::cin, input, '\n');
        if(std::cin.eof()) break;
        run_before(input);
        print(run(globalEnv));
        std::cout << std::endl;
    }
}

int main(int argc, char** argv) {

    init();

    if(argc > 1) {
        run_before(readTextFile(std::string(argv[1])));
        run(globalEnv);
    } else {
        repl();
    }

    return 0;
}
