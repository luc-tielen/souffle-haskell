
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_ii__0_1__11 {
static constexpr Relation::arity_type Arity = 2;
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
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
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
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
struct t_btree_ii__0_1__11__10 {
static constexpr Relation::arity_type Arity = 2;
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
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
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
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
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_10(lower,upper,h);
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
static inline std::string substr_wrapper(const std::string& str, std::size_t idx, std::size_t len) {
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
SpecializedRecordTable<0> recordTable{};
// -- Table: edge
Own<t_btree_ii__0_1__11> rel_1_edge = mk<t_btree_ii__0_1__11>();
souffle::RelationWrapper<t_btree_ii__0_1__11> wrapper_rel_1_edge;
// -- Table: reachable
Own<t_btree_ii__0_1__11> rel_2_reachable = mk<t_btree_ii__0_1__11>();
souffle::RelationWrapper<t_btree_ii__0_1__11> wrapper_rel_2_reachable;
// -- Table: @delta_reachable
Own<t_btree_ii__0_1__11__10> rel_3_delta_reachable = mk<t_btree_ii__0_1__11__10>();
// -- Table: @new_reachable
Own<t_btree_ii__0_1__11__10> rel_4_new_reachable = mk<t_btree_ii__0_1__11__10>();
public:
Sf_path()
: wrapper_rel_1_edge(0, *rel_1_edge, *this, "edge", std::array<const char *,2>{{"s:symbol","s:symbol"}}, std::array<const char *,2>{{"n","m"}}, 0)
, wrapper_rel_2_reachable(1, *rel_2_reachable, *this, "reachable", std::array<const char *,2>{{"s:symbol","s:symbol"}}, std::array<const char *,2>{{"n","m"}}, 0)
{
addRelation("edge", wrapper_rel_1_edge, true, true);
addRelation("reachable", wrapper_rel_2_reachable, false, true);
}
~Sf_path() {
}

private:
std::string             inputDirectory;
std::string             outputDirectory;
SignalHandler*          signalHandler {SignalHandler::instance()};
std::atomic<RamDomain>  ctr {};
std::atomic<std::size_t>     iter {};

void runFunction(std::string  inputDirectoryArg,
                 std::string  outputDirectoryArg,
                 bool         performIOArg,
                 bool         pruneImdtRelsArg) {
    this->inputDirectory  = std::move(inputDirectoryArg);
    this->outputDirectory = std::move(outputDirectoryArg);
    this->performIO       = performIOArg;
    this->pruneImdtRels   = pruneImdtRelsArg; 

    // set default threads (in embedded mode)
    // if this is not set, and omp is used, the default omp setting of number of cores is used.
#if defined(_OPENMP)
    if (0 < getNumThreads()) { omp_set_num_threads(getNumThreads()); }
#endif

    signalHandler->set();
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
signalHandler->reset();
}
public:
void run() override { runFunction("", "", false, false); }
public:
void runAll(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "", bool performIOArg=true, bool pruneImdtRelsArg=true) override { runFunction(inputDirectoryArg, outputDirectoryArg, performIOArg, pruneImdtRelsArg);
}
public:
void printAll(std::string outputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"auxArity","0"},{"name","reachable"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"n\", \"m\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_2_reachable);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"auxArity","0"},{"name","edge"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"n\", \"m\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_edge);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"auxArity","0"},{"fact-dir","."},{"name","edge"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"n\", \"m\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_edge);
} catch (std::exception& e) {std::cerr << "Error loading edge data: " << e.what() << '\n';}
}
public:
void dumpInputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "edge";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_edge);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "reachable";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_reachable);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "edge";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_edge);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
RecordTable& getRecordTable() override {
return recordTable;
}
void setNumThreads(std::size_t numThreadsValue) override {
SouffleProgram::setNumThreads(numThreadsValue);
symTable.setNumLanes(getNumThreads());
recordTable.setNumLanes(getNumThreads());
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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"auxArity","0"},{"fact-dir","."},{"name","edge"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"n\", \"m\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_edge);
} catch (std::exception& e) {std::cerr << "Error loading edge data: " << e.what() << '\n';}
}
signalHandler->setMsg(R"_(edge("a","b").
in file /tmp/souffle-haskell/tests/fixtures/path.dl [11:1-11:16])_");
[&](){
CREATE_OP_CONTEXT(rel_1_edge_op_ctxt,rel_1_edge->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(0)),ramBitCast(RamSigned(1))}};
rel_1_edge->insert(tuple,READ_OP_CONTEXT(rel_1_edge_op_ctxt));
}
();signalHandler->setMsg(R"_(edge("b","c").
in file /tmp/souffle-haskell/tests/fixtures/path.dl [12:1-12:16])_");
[&](){
CREATE_OP_CONTEXT(rel_1_edge_op_ctxt,rel_1_edge->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(1)),ramBitCast(RamSigned(2))}};
rel_1_edge->insert(tuple,READ_OP_CONTEXT(rel_1_edge_op_ctxt));
}
();if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"auxArity","0"},{"name","edge"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"n\", \"m\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_edge);
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
signalHandler->setMsg(R"_(reachable(x,y) :- 
   edge(x,y).
in file /tmp/souffle-haskell/tests/fixtures/path.dl [14:1-14:31])_");
if(!(rel_1_edge->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_2_reachable_op_ctxt,rel_2_reachable->createContext());
CREATE_OP_CONTEXT(rel_1_edge_op_ctxt,rel_1_edge->createContext());
for(const auto& env0 : *rel_1_edge) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_2_reachable->insert(tuple,READ_OP_CONTEXT(rel_2_reachable_op_ctxt));
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_2_reachable_op_ctxt,rel_2_reachable->createContext());
CREATE_OP_CONTEXT(rel_3_delta_reachable_op_ctxt,rel_3_delta_reachable->createContext());
for(const auto& env0 : *rel_2_reachable) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_3_delta_reachable->insert(tuple,READ_OP_CONTEXT(rel_3_delta_reachable_op_ctxt));
}
}
();iter = 0;
for(;;) {
signalHandler->setMsg(R"_(reachable(x,z) :- 
   edge(x,y),
   reachable(y,z).
in file /tmp/souffle-haskell/tests/fixtures/path.dl [15:1-15:48])_");
if(!(rel_1_edge->empty()) && !(rel_3_delta_reachable->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_2_reachable_op_ctxt,rel_2_reachable->createContext());
CREATE_OP_CONTEXT(rel_4_new_reachable_op_ctxt,rel_4_new_reachable->createContext());
CREATE_OP_CONTEXT(rel_1_edge_op_ctxt,rel_1_edge->createContext());
CREATE_OP_CONTEXT(rel_3_delta_reachable_op_ctxt,rel_3_delta_reachable->createContext());
for(const auto& env0 : *rel_1_edge) {
auto range = rel_3_delta_reachable->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_3_delta_reachable_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_2_reachable->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_2_reachable_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env1[1])}};
rel_4_new_reachable->insert(tuple,READ_OP_CONTEXT(rel_4_new_reachable_op_ctxt));
}
}
}
}
();}
if(rel_4_new_reachable->empty()) break;
[&](){
CREATE_OP_CONTEXT(rel_2_reachable_op_ctxt,rel_2_reachable->createContext());
CREATE_OP_CONTEXT(rel_4_new_reachable_op_ctxt,rel_4_new_reachable->createContext());
for(const auto& env0 : *rel_4_new_reachable) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_2_reachable->insert(tuple,READ_OP_CONTEXT(rel_2_reachable_op_ctxt));
}
}
();std::swap(rel_3_delta_reachable, rel_4_new_reachable);
rel_4_new_reachable->purge();
iter++;
}
iter = 0;
rel_3_delta_reachable->purge();
rel_4_new_reachable->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"auxArity","0"},{"name","reachable"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"n\", \"m\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_2_reachable);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (pruneImdtRels) rel_1_edge->purge();
if (pruneImdtRels) rel_2_reachable->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
};
SouffleProgram *newInstance_path(){return new Sf_path;}
SymbolTable *getST_path(SouffleProgram *p){return &reinterpret_cast<Sf_path*>(p)->getSymbolTable();}

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
souffle::CmdOptions opt(R"(path.dl)",
R"()",
R"()",
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
