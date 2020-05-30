
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_2__0_1__01__11 {
using t_tuple = Tuple<RamDomain, 2>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0,1>>;
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
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
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
range<iterator> lowerUpperRange_00(const t_tuple& lower, const t_tuple& upper, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& lower, const t_tuple& upper) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_01(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_tuple low(lower); t_tuple high(lower);
low[1] = MIN_RAM_SIGNED;
high[1] = MAX_RAM_SIGNED;
return make_range(ind_0.lower_bound(low, h.hints_0), ind_0.upper_bound(high, h.hints_0));
}
range<t_ind_0::iterator> lowerUpperRange_01(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_01(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
auto pos = ind_0.find(lower, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
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
o << " arity 2 direct b-tree index 0 lex-order [0,1]\n";
ind_0.printStats(o);
}
};
struct t_btree_2__0_1__11 {
using t_tuple = Tuple<RamDomain, 2>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0,1>>;
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
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
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
range<iterator> lowerUpperRange_00(const t_tuple& lower, const t_tuple& upper, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& lower, const t_tuple& upper) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
auto pos = ind_0.find(lower, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
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
o << " arity 2 direct b-tree index 0 lex-order [0,1]\n";
ind_0.printStats(o);
}
};

class Sf_path : public SouffleProgram {
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
SymbolTable symTable{
	R"_(a)_",
	R"_(b)_",
	R"_(c)_",
};// -- initialize record table --
RecordTable recordTable;
// -- Table: @delta_reachable
std::unique_ptr<t_btree_2__0_1__01__11> rel_1_delta_reachable = std::make_unique<t_btree_2__0_1__01__11>();
// -- Table: @new_reachable
std::unique_ptr<t_btree_2__0_1__01__11> rel_2_new_reachable = std::make_unique<t_btree_2__0_1__01__11>();
// -- Table: edge
std::unique_ptr<t_btree_2__0_1__11> rel_3_edge = std::make_unique<t_btree_2__0_1__11>();
souffle::RelationWrapper<0,t_btree_2__0_1__11,Tuple<RamDomain,2>,2,0> wrapper_rel_3_edge;
// -- Table: reachable
std::unique_ptr<t_btree_2__0_1__11> rel_4_reachable = std::make_unique<t_btree_2__0_1__11>();
souffle::RelationWrapper<1,t_btree_2__0_1__11,Tuple<RamDomain,2>,2,0> wrapper_rel_4_reachable;
public:
Sf_path() : 
wrapper_rel_3_edge(*rel_3_edge,symTable,"edge",std::array<const char *,2>{{"s:symbol","s:symbol"}},std::array<const char *,2>{{"n","m"}}),

wrapper_rel_4_reachable(*rel_4_reachable,symTable,"reachable",std::array<const char *,2>{{"s:symbol","s:symbol"}},std::array<const char *,2>{{"n","m"}}){
addRelation("edge",&wrapper_rel_3_edge,false,false);
addRelation("reachable",&wrapper_rel_4_reachable,false,true);
}
~Sf_path() {
}
private:
void runFunction(std::string inputDirectory = ".", std::string outputDirectory = ".", bool performIO = false) {
SignalHandler::instance()->set();
std::atomic<size_t> iter(0);

#if defined(_OPENMP)
if (getNumThreads() > 0) {omp_set_num_threads(getNumThreads());}
#endif

// -- query evaluation --
SignalHandler::instance()->setMsg(R"_(edge("a","b").
in file /private/tmp/path.dl [6:1-6:16])_");
[&](){
CREATE_OP_CONTEXT(rel_3_edge_op_ctxt,rel_3_edge->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(0)),ramBitCast(RamSigned(1))}};
rel_3_edge->insert(tuple,READ_OP_CONTEXT(rel_3_edge_op_ctxt));
}
();SignalHandler::instance()->setMsg(R"_(edge("b","c").
in file /private/tmp/path.dl [7:1-7:16])_");
[&](){
CREATE_OP_CONTEXT(rel_3_edge_op_ctxt,rel_3_edge->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(1)),ramBitCast(RamSigned(2))}};
rel_3_edge->insert(tuple,READ_OP_CONTEXT(rel_3_edge_op_ctxt));
}
();SignalHandler::instance()->setMsg(R"_(reachable(x,y) :- 
   edge(x,y).
in file /private/tmp/path.dl [8:1-8:31])_");
if(!(rel_3_edge->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_3_edge_op_ctxt,rel_3_edge->createContext());
CREATE_OP_CONTEXT(rel_4_reachable_op_ctxt,rel_4_reachable->createContext());
for(const auto& env0 : *rel_3_edge) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_4_reachable->insert(tuple,READ_OP_CONTEXT(rel_4_reachable_op_ctxt));
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_4_reachable_op_ctxt,rel_4_reachable->createContext());
CREATE_OP_CONTEXT(rel_1_delta_reachable_op_ctxt,rel_1_delta_reachable->createContext());
for(const auto& env0 : *rel_4_reachable) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_1_delta_reachable->insert(tuple,READ_OP_CONTEXT(rel_1_delta_reachable_op_ctxt));
}
}
();iter = 0;
for(;;) {
SignalHandler::instance()->setMsg(R"_(reachable(x,z) :- 
   edge(x,y),
   reachable(y,z).
in file /private/tmp/path.dl [9:1-9:48])_");
if(!(rel_3_edge->empty()) && !(rel_1_delta_reachable->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_3_edge_op_ctxt,rel_3_edge->createContext());
CREATE_OP_CONTEXT(rel_4_reachable_op_ctxt,rel_4_reachable->createContext());
CREATE_OP_CONTEXT(rel_1_delta_reachable_op_ctxt,rel_1_delta_reachable->createContext());
CREATE_OP_CONTEXT(rel_2_new_reachable_op_ctxt,rel_2_new_reachable->createContext());
for(const auto& env0 : *rel_3_edge) {
const Tuple<RamDomain,2> lower{{ramBitCast(env0[1]),0}};
const Tuple<RamDomain,2> upper{{ramBitCast(env0[1]),0}};
auto range = rel_1_delta_reachable->lowerUpperRange_01(lower, upper,READ_OP_CONTEXT(rel_1_delta_reachable_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_4_reachable->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_4_reachable_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env1[1])}};
rel_2_new_reachable->insert(tuple,READ_OP_CONTEXT(rel_2_new_reachable_op_ctxt));
}
}
}
}
();}
if(rel_2_new_reachable->empty()) break;
[&](){
CREATE_OP_CONTEXT(rel_4_reachable_op_ctxt,rel_4_reachable->createContext());
CREATE_OP_CONTEXT(rel_2_new_reachable_op_ctxt,rel_2_new_reachable->createContext());
for(const auto& env0 : *rel_2_new_reachable) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_4_reachable->insert(tuple,READ_OP_CONTEXT(rel_4_reachable_op_ctxt));
}
}
();std::swap(rel_1_delta_reachable, rel_2_new_reachable);
rel_2_new_reachable->purge();
iter++;
}
iter = 0;
rel_1_delta_reachable->purge();
rel_2_new_reachable->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"filename","./reachable.csv"},{"name","reachable"},{"operation","output"},{"types","{\"reachable\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}, \"records\": {}}"}});
if (!outputDirectory.empty() && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_reachable);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) rel_3_edge->purge();
if (performIO) rel_4_reachable->purge();

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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"filename","./reachable.csv"},{"name","reachable"},{"operation","output"},{"types","{\"reachable\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}, \"records\": {}}"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_reachable);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectory = ".") override {
}
public:
void dumpInputs(std::ostream& out = std::cout) override {
}
public:
void dumpOutputs(std::ostream& out = std::cout) override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "reachable";
rwOperation["types"] = "{\"reachable\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_reachable);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
};
SouffleProgram *newInstance_path(){return new Sf_path;}
SymbolTable *getST_path(SouffleProgram *p){return &reinterpret_cast<Sf_path*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_path: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_path();
};
public:
factory_Sf_path() : ProgramFactory("path"){}
};
extern "C" {
factory_Sf_path __factory_Sf_path_instance;
}
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(/tmp/path.dl)",
R"(.)",
R"(.)",
false,
R"()",
1);
if (!opt.parse(argc,argv)) return 1;
souffle::Sf_path obj;
#if defined(_OPENMP) 
obj.setNumThreads(opt.getNumJobs());

#endif
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
