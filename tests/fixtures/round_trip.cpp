
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_1__0__1 {
using t_tuple = Tuple<RamDomain, 1>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (a[0] < b[0]) ? -1 : ((a[0] > b[0]) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return  a[0] < b[0];
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return a[0] == b[0];
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[1];
std::copy(ramDomain, ramDomain + 1, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0) {
RamDomain data[1] = {a0};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_0(const t_tuple& lower, const t_tuple& upper, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_0(const t_tuple& lower, const t_tuple& upper) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper, context& h) const {
auto pos = ind_0.find(lower, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_1(lower,upper,h);
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
o << " arity 1 direct b-tree index 0 lex-order [0]\n";
ind_0.printStats(o);
}
};

class Sf_round_trip : public SouffleProgram {
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
// -- Table: float_fact
std::unique_ptr<t_btree_1__0__1> rel_1_float_fact = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<0,t_btree_1__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_1_float_fact;
// -- Table: number_fact
std::unique_ptr<t_btree_1__0__1> rel_2_number_fact = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<1,t_btree_1__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_2_number_fact;
// -- Table: string_fact
std::unique_ptr<t_btree_1__0__1> rel_3_string_fact = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<2,t_btree_1__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_3_string_fact;
// -- Table: unsigned_fact
std::unique_ptr<t_btree_1__0__1> rel_4_unsigned_fact = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<3,t_btree_1__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_4_unsigned_fact;
public:
Sf_round_trip() : 
wrapper_rel_1_float_fact(*rel_1_float_fact,symTable,"float_fact",std::array<const char *,1>{{"f:float"}},std::array<const char *,1>{{"x"}}),

wrapper_rel_2_number_fact(*rel_2_number_fact,symTable,"number_fact",std::array<const char *,1>{{"i:number"}},std::array<const char *,1>{{"x"}}),

wrapper_rel_3_string_fact(*rel_3_string_fact,symTable,"string_fact",std::array<const char *,1>{{"s:symbol"}},std::array<const char *,1>{{"x"}}),

wrapper_rel_4_unsigned_fact(*rel_4_unsigned_fact,symTable,"unsigned_fact",std::array<const char *,1>{{"u:unsigned"}},std::array<const char *,1>{{"x"}}){
addRelation("float_fact",&wrapper_rel_1_float_fact,true,true);
addRelation("number_fact",&wrapper_rel_2_number_fact,true,true);
addRelation("string_fact",&wrapper_rel_3_string_fact,true,true);
addRelation("unsigned_fact",&wrapper_rel_4_unsigned_fact,true,true);
}
~Sf_round_trip() {
}
private:
std::string inputDirectory;
std::string outputDirectory;
bool performIO;
std::atomic<RamDomain> ctr{};

std::atomic<size_t> iter{};
void runFunction(std::string inputDirectory = ".", std::string outputDirectory = ".", bool performIO = false) {
this->inputDirectory = inputDirectory;
this->outputDirectory = outputDirectory;
this->performIO = performIO;
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
{
 std::vector<RamDomain> args, ret;
subroutine_2(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_3(args, ret);
}

// -- relation hint statistics --
SignalHandler::instance()->reset();
}
public:
void run() override { runFunction(".", ".", false); }
public:
void runAll(std::string inputDirectory = ".", std::string outputDirectory = ".") override { runFunction(inputDirectory, outputDirectory, true);
}
public:
void printAll(std::string outputDirectory = ".") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","x"},{"filename","./string_fact.csv"},{"name","string_fact"},{"operation","output"},{"types","{\"records\": {}, \"string_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_3_string_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","x"},{"filename","./unsigned_fact.csv"},{"name","unsigned_fact"},{"operation","output"},{"types","{\"records\": {}, \"unsigned_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"u:unsigned\"]}}"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_unsigned_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","x"},{"filename","./number_fact.csv"},{"name","number_fact"},{"operation","output"},{"types","{\"number_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}, \"records\": {}}"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_2_number_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","x"},{"filename","./float_fact.csv"},{"name","float_fact"},{"operation","output"},{"types","{\"float_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"f:float\"]}, \"records\": {}}"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_float_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectory = ".") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./string_fact.facts"},{"name","string_fact"},{"operation","input"},{"types","{\"records\": {}, \"string_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_string_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./number_fact.facts"},{"name","number_fact"},{"operation","input"},{"types","{\"number_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}, \"records\": {}}"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_number_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./unsigned_fact.facts"},{"name","unsigned_fact"},{"operation","input"},{"types","{\"records\": {}, \"unsigned_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"u:unsigned\"]}}"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_4_unsigned_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./float_fact.facts"},{"name","float_fact"},{"operation","input"},{"types","{\"float_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"f:float\"]}, \"records\": {}}"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_float_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
public:
void dumpInputs(std::ostream& out = std::cout) override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "string_fact";
rwOperation["types"] = "{\"string_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_3_string_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "number_fact";
rwOperation["types"] = "{\"number_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_number_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "unsigned_fact";
rwOperation["types"] = "{\"unsigned_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"u:unsigned\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_unsigned_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "float_fact";
rwOperation["types"] = "{\"float_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"f:float\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_float_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs(std::ostream& out = std::cout) override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "string_fact";
rwOperation["types"] = "{\"string_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_3_string_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "unsigned_fact";
rwOperation["types"] = "{\"unsigned_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"u:unsigned\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_unsigned_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "number_fact";
rwOperation["types"] = "{\"number_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_number_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "float_fact";
rwOperation["types"] = "{\"float_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"f:float\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_float_fact);
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
if (name == "stratum_2") {
subroutine_2(args, ret);
return;}
if (name == "stratum_3") {
subroutine_3(args, ret);
return;}
fatal("unknown subroutine");
}
void subroutine_0(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./string_fact.facts"},{"name","string_fact"},{"operation","input"},{"types","{\"records\": {}, \"string_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectory.empty() && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_string_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","x"},{"filename","./string_fact.csv"},{"name","string_fact"},{"operation","output"},{"types","{\"records\": {}, \"string_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectory.empty() && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_3_string_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
void subroutine_1(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./number_fact.facts"},{"name","number_fact"},{"operation","input"},{"types","{\"number_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}, \"records\": {}}"}});
if (!inputDirectory.empty() && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_number_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","x"},{"filename","./number_fact.csv"},{"name","number_fact"},{"operation","output"},{"types","{\"number_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}, \"records\": {}}"}});
if (!outputDirectory.empty() && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_2_number_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
void subroutine_2(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./unsigned_fact.facts"},{"name","unsigned_fact"},{"operation","input"},{"types","{\"records\": {}, \"unsigned_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"u:unsigned\"]}}"}});
if (!inputDirectory.empty() && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_4_unsigned_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","x"},{"filename","./unsigned_fact.csv"},{"name","unsigned_fact"},{"operation","output"},{"types","{\"records\": {}, \"unsigned_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"u:unsigned\"]}}"}});
if (!outputDirectory.empty() && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_unsigned_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
void subroutine_3(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./float_fact.facts"},{"name","float_fact"},{"operation","input"},{"types","{\"float_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"f:float\"]}, \"records\": {}}"}});
if (!inputDirectory.empty() && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_float_fact);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","x"},{"filename","./float_fact.csv"},{"name","float_fact"},{"operation","output"},{"types","{\"float_fact\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"f:float\"]}, \"records\": {}}"}});
if (!outputDirectory.empty() && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_float_fact);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
};
SouffleProgram *newInstance_round_trip(){return new Sf_round_trip;}
SymbolTable *getST_round_trip(SouffleProgram *p){return &reinterpret_cast<Sf_round_trip*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_round_trip: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_round_trip();
};
public:
factory_Sf_round_trip() : ProgramFactory("round_trip"){}
};
extern "C" {
factory_Sf_round_trip __factory_Sf_round_trip_instance;
}
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(round_trip.dl)",
R"(.)",
R"(.)",
false,
R"()",
1);
if (!opt.parse(argc,argv)) return 1;
souffle::Sf_round_trip obj;
#if defined(_OPENMP) 
obj.setNumThreads(opt.getNumJobs());

#endif
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
