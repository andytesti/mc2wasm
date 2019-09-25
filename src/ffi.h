typedef unsigned int McSymbolId;

typedef struct McSymbol {
    McSymbolId id;
    unsigned char name;
    unsigned int name_len;
} McSymbol;

typedef struct McObject McObject;
typedef struct McStruct McStruct;
typedef unsigned int McStatus;

typedef McStatus (*McMethod)(const McObject**, const McStruct*, const McObject**);

typedef struct McMethodDef {
    McSymbol* name;
    McMethod method;
    unsigned int args_len;
} McMethodDef;

typedef struct McMethodDefIndex {
    McMethodDef method_def;
    unsigned int method_def_len;
} McMethodDefIndex;

typedef struct McClassDef {
    McSymbol* name;
    McMethodDefIndex* methods[10];
} McClassDef;

typedef struct McClassDefIndex {
    McClassDef* classes;
    unsigned int classes_len;
} McClassDefIndex;

McClassDefIndex* __mc_class_def_index();
McStatus __mc_invoke_0(const McObject** ret, const McObject* target, const McSymbol* symbol);