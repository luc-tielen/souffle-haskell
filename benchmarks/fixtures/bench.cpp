
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_uif__0_1_2__111 {
using t_tuple = Tuple<RamDomain, 3>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamUnsigned>(a[0]) < ramBitCast<RamUnsigned>(b[0])) ? -1 : (ramBitCast<RamUnsigned>(a[0]) > ramBitCast<RamUnsigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :((ramBitCast<RamFloat>(a[2]) < ramBitCast<RamFloat>(b[2])) ? -1 : (ramBitCast<RamFloat>(a[2]) > ramBitCast<RamFloat>(b[2])) ? 1 :(0)));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamUnsigned>(a[0]) < ramBitCast<RamUnsigned>(b[0]))|| (ramBitCast<RamUnsigned>(a[0]) == ramBitCast<RamUnsigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))|| (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1])) && ((ramBitCast<RamFloat>(a[2]) < ramBitCast<RamFloat>(b[2]))));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamUnsigned>(a[0]) == ramBitCast<RamUnsigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]))&&(ramBitCast<RamFloat>(a[2]) == ramBitCast<RamFloat>(b[2]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[3];
std::copy(ramDomain, ramDomain + 3, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2) {
RamDomain data[3] = {a0,a1,a2};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_111(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 3 direct b-tree index 0 lex-order [0,1,2]\n";
ind_0.printStats(o);
}
};
struct t_btree_uiif__0_1_2_3__1111 {
using t_tuple = Tuple<RamDomain, 4>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamUnsigned>(a[0]) < ramBitCast<RamUnsigned>(b[0])) ? -1 : (ramBitCast<RamUnsigned>(a[0]) > ramBitCast<RamUnsigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2])) ? -1 : (ramBitCast<RamSigned>(a[2]) > ramBitCast<RamSigned>(b[2])) ? 1 :((ramBitCast<RamFloat>(a[3]) < ramBitCast<RamFloat>(b[3])) ? -1 : (ramBitCast<RamFloat>(a[3]) > ramBitCast<RamFloat>(b[3])) ? 1 :(0))));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamUnsigned>(a[0]) < ramBitCast<RamUnsigned>(b[0]))|| (ramBitCast<RamUnsigned>(a[0]) == ramBitCast<RamUnsigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))|| (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1])) && ((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2]))|| (ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2])) && ((ramBitCast<RamFloat>(a[3]) < ramBitCast<RamFloat>(b[3])))));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamUnsigned>(a[0]) == ramBitCast<RamUnsigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]))&&(ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2]))&&(ramBitCast<RamFloat>(a[3]) == ramBitCast<RamFloat>(b[3]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[4];
std::copy(ramDomain, ramDomain + 4, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2,RamDomain a3) {
RamDomain data[4] = {a0,a1,a2,a3};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_0000(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_0000(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_1111(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_1111(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_1111(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 4 direct b-tree index 0 lex-order [0,1,2,3]\n";
ind_0.printStats(o);
}
};

class Sf_bench : public SouffleProgram {
private:
static inline bool regex_wrapper(const std::string& pattern, const std::string& text) {
   bool result = false; 
   try { result = std::regex_match(text, std::regex(pattern)); } catch(...) { 
     std::cerr << "warning: wrong pattern provided for match(\"" << pattern << "\",\"" << text << "\").\n";
}
   return result;
}
private:
static inline std::string substr_wrapper(const std::string& str, size_t idx, size_t len) {
   std::string result; 
   try { result = str.substr(idx,len); } catch(...) { 
     std::cerr << "warning: wrong index position provided by substr(\"";
     std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
   } return result;
}
public:
// -- initialize symbol table --
SymbolTable symTable;// -- initialize record table --
RecordTable recordTable;
// -- Table: numbers_fact
Own<t_btree_uif__0_1_2__111> rel_1_numbers_fact = mk<t_btree_uif__0_1_2__111>();
souffle::RelationWrapper<0,t_btree_uif__0_1_2__111,Tuple<RamDomain,3>,3,0> wrapper_rel_1_numbers_fact;
// -- Table: strings_fact
Own<t_btree_uiif__0_1_2_3__1111> rel_2_strings_fact = mk<t_btree_uiif__0_1_2_3__1111>();
souffle::RelationWrapper<1,t_btree_uiif__0_1_2_3__1111,Tuple<RamDomain,4>,4,0> wrapper_rel_2_strings_fact;
public:
Sf_bench() : 
wrapper_rel_1_numbers_fact(*rel_1_numbers_fact,symTable,"numbers_fact",std::array<const char *,3>{{"u:unsigned","i:number","f:float"}},std::array<const char *,3>{{"u","n","f"}}),

wrapper_rel_2_strings_fact(*rel_2_strings_fact,symTable,"strings_fact",std::array<const char *,4>{{"u:unsigned","s:symbol","i:number","f:float"}},std::array<const char *,4>{{"u","s","n","f"}}){
addRelation("numbers_fact",&wrapper_rel_1_numbers_fact,true,true);
addRelation("strings_fact",&wrapper_rel_2_strings_fact,true,true);
}
~Sf_bench() {
}
private:
std::string inputDirectory;
std::string outputDirectory;
bool performIO;
std::atomic<RamDomain> ctr{};

std::atomic<size_t> iter{};
void runFunction(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "", bool performIOArg = false) {
this->inputDirectory = inputDirectoryArg;
this->outputDirectory = outputDirectoryArg;
this->performIO = performIOArg;
SignalHandler::instance()->set();
#if defined(_OPENMP)
if (getNumThreads() > 0) {omp_set_num_threads(getNumThreads());}
#endif

// -- query evaluation --
{
 std::vector<RamDomain> args, ret;
subroutine_0(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_1(args, ret);
}

// -- relation hint statistics --
SignalHandler::instance()->reset();
}
public:
void run() override { runFunction("", "", false); }
public:
void runAll(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "") override { runFunction(inputDirectoryArg, outputDirectoryArg, true);
}
public:
void printAll(std::string outputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\ts\tn\tf"},{"name","strings_fact"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 4, \"auxArity\": 0, \"params\": [\"u\", \"s\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 4, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"s:symbol\", \"i:number\", \"f:float\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_2_strings_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\tn\tf"},{"name","numbers_fact"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"u\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_numbers_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\ts\tn\tf"},{"fact-dir","."},{"name","strings_fact"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 4, \"auxArity\": 0, \"params\": [\"u\", \"s\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 4, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"s:symbol\", \"i:number\", \"f:float\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_strings_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\tn\tf"},{"fact-dir","."},{"name","numbers_fact"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"u\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_numbers_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
public:
void dumpInputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "strings_fact";
rwOperation["types"] = "{\"relation\": {\"arity\": 4, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"s:symbol\", \"i:number\", \"f:float\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_strings_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "numbers_fact";
rwOperation["types"] = "{\"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_numbers_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "strings_fact";
rwOperation["types"] = "{\"relation\": {\"arity\": 4, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"s:symbol\", \"i:number\", \"f:float\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_strings_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "numbers_fact";
rwOperation["types"] = "{\"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_numbers_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
void executeSubroutine(std::string name, const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) override {
if (name == "stratum_0") {
subroutine_0(args, ret);
return;}
if (name == "stratum_1") {
subroutine_1(args, ret);
return;}
fatal("unknown subroutine");
}
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_0(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\tn\tf"},{"fact-dir","."},{"name","numbers_fact"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"u\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_numbers_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\tn\tf"},{"name","numbers_fact"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"u\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_numbers_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_1(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\ts\tn\tf"},{"fact-dir","."},{"name","strings_fact"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 4, \"auxArity\": 0, \"params\": [\"u\", \"s\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 4, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"s:symbol\", \"i:number\", \"f:float\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_strings_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\ts\tn\tf"},{"name","strings_fact"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 4, \"auxArity\": 0, \"params\": [\"u\", \"s\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 4, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"s:symbol\", \"i:number\", \"f:float\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_2_strings_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
};
SouffleProgram *newInstance_bench(){return new Sf_bench;}
SymbolTable *getST_bench(SouffleProgram *p){return &reinterpret_cast<Sf_bench*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_bench: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_bench();
};
public:
factory_Sf_bench() : ProgramFactory("bench"){}
};
extern "C" {
factory_Sf_bench __factory_Sf_bench_instance;
}
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(bench.dl)",
R"()",
R"()",
false,
R"()",
1);
if (!opt.parse(argc,argv)) return 1;
souffle::Sf_bench obj;
#if defined(_OPENMP) 
obj.setNumThreads(opt.getNumJobs());

#endif
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
