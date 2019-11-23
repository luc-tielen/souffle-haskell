
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
using namespace ram;
struct t_btree_2__0_1__3 {
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
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_2__0_1__3& other) {
ind_0.insertAll(other.ind_0);
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
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t) const {
context h;
return equalRange_3(t, h);
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
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 2 direct b-tree index [0,1]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};
struct t_btree_2__0_1__1__3 {
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
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_2__0_1__1__3& other) {
ind_0.insertAll(other.ind_0);
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
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t, context& h) const {
t_tuple low(t); t_tuple high(t);
low[1] = MIN_RAM_DOMAIN;
high[1] = MAX_RAM_DOMAIN;
return make_range(ind_0.lower_bound(low, h.hints_0), ind_0.upper_bound(high, h.hints_0));
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t) const {
context h;
return equalRange_1(t, h);
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t) const {
context h;
return equalRange_3(t, h);
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
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 2 direct b-tree index [0,1]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
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
private:
static inline RamDomain wrapper_tonumber(const std::string& str) {
   RamDomain result=0; 
   try { result = stord(str); } catch(...) { 
     std::cerr << "error: wrong string provided by to_number(\"";
     std::cerr << str << "\") functor.\n";
     raise(SIGFPE);
   } return result;
}
public:
// -- initialize symbol table --
SymbolTable symTable
{
	R"_(a)_",
	R"_(b)_",
	R"_(c)_",
};// -- Table: edge
std::unique_ptr<t_btree_2__0_1__3> rel_1_edge = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<0,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_1_edge;
// -- Table: reachable
std::unique_ptr<t_btree_2__0_1__3> rel_2_reachable = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<1,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_2_reachable;
// -- Table: @delta_reachable
std::unique_ptr<t_btree_2__0_1__1__3> rel_3_delta_reachable = std::make_unique<t_btree_2__0_1__1__3>();
// -- Table: @new_reachable
std::unique_ptr<t_btree_2__0_1__1__3> rel_4_new_reachable = std::make_unique<t_btree_2__0_1__1__3>();
public:
Sf_path() : 
wrapper_rel_1_edge(*rel_1_edge,symTable,"edge",std::array<const char *,2>{{"s:symbol","s:symbol"}},std::array<const char *,2>{{"n","m"}}),

wrapper_rel_2_reachable(*rel_2_reachable,symTable,"reachable",std::array<const char *,2>{{"s:symbol","s:symbol"}},std::array<const char *,2>{{"n","m"}}){
addRelation("edge",&wrapper_rel_1_edge,false,false);
addRelation("reachable",&wrapper_rel_2_reachable,false,true);
}
~Sf_path() {
}
private:
void runFunction(std::string inputDirectory = ".", std::string outputDirectory = ".", size_t stratumIndex = (size_t) -1, bool performIO = false) {
SignalHandler::instance()->set();
std::atomic<size_t> iter(0);

// -- query evaluation --
/* BEGIN STRATUM 0 */
[&]() {
SignalHandler::instance()->setMsg(R"_(edge("a","b").
in file /Users/luc/personal/souffle-c/path.dl [5:1-5:16])_");
rel_1_edge->insert(RamDomain(0),RamDomain(1));
SignalHandler::instance()->setMsg(R"_(edge("b","c").
in file /Users/luc/personal/souffle-c/path.dl [6:1-6:16])_");
rel_1_edge->insert(RamDomain(1),RamDomain(2));
}();
/* END STRATUM 0 */
/* BEGIN STRATUM 1 */
[&]() {
SignalHandler::instance()->setMsg(R"_(reachable(x,y) :- 
   edge(x,y).
in file /Users/luc/personal/souffle-c/path.dl [8:1-8:31])_");
if(!(rel_1_edge->empty())) {
{
CREATE_OP_CONTEXT(rel_1_edge_op_ctxt,rel_1_edge->createContext());
CREATE_OP_CONTEXT(rel_2_reachable_op_ctxt,rel_2_reachable->createContext());
for(const auto& env0 : *rel_1_edge) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[1])}});
rel_2_reachable->insert(tuple,READ_OP_CONTEXT(rel_2_reachable_op_ctxt));
}
}
}
rel_3_delta_reachable->insertAll(*rel_2_reachable);
iter = 0;
for(;;) {
SignalHandler::instance()->setMsg(R"_(reachable(x,z) :- 
   edge(x,y),
   reachable(y,z).
in file /Users/luc/personal/souffle-c/path.dl [9:1-9:48])_");
if(((!(rel_1_edge->empty())) && (!(rel_3_delta_reachable->empty())))) {
{
CREATE_OP_CONTEXT(rel_1_edge_op_ctxt,rel_1_edge->createContext());
CREATE_OP_CONTEXT(rel_2_reachable_op_ctxt,rel_2_reachable->createContext());
CREATE_OP_CONTEXT(rel_3_delta_reachable_op_ctxt,rel_3_delta_reachable->createContext());
CREATE_OP_CONTEXT(rel_4_new_reachable_op_ctxt,rel_4_new_reachable->createContext());
for(const auto& env0 : *rel_1_edge) {
const Tuple<RamDomain,2> key({{env0[1],0}});
auto range = rel_3_delta_reachable->equalRange_1(key,READ_OP_CONTEXT(rel_3_delta_reachable_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_2_reachable->contains(Tuple<RamDomain,2>({{env0[0],env1[1]}}),READ_OP_CONTEXT(rel_2_reachable_op_ctxt)))) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1])}});
rel_4_new_reachable->insert(tuple,READ_OP_CONTEXT(rel_4_new_reachable_op_ctxt));
}
}
}
}
}
if(rel_4_new_reachable->empty()) break;
rel_2_reachable->insertAll(*rel_4_new_reachable);
std::swap(rel_3_delta_reachable, rel_4_new_reachable);
rel_4_new_reachable->purge();
iter++;
}
iter = 0;
if (!isHintsProfilingEnabled()) rel_3_delta_reachable->purge();
if (!isHintsProfilingEnabled()) rel_4_new_reachable->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"filename","./reachable.csv"},{"name","reachable"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_2_reachable);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_1_edge->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_2_reachable->purge();
}();
/* END STRATUM 1 */

// -- relation hint statistics --
if(isHintsProfilingEnabled()) {
std::cout << " -- Operation Hint Statistics --\n";
std::cout << "Relation rel_1_edge:\n";
rel_1_edge->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_2_reachable:\n";
rel_2_reachable->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_3_delta_reachable:\n";
rel_3_delta_reachable->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_4_new_reachable:\n";
rel_4_new_reachable->printHintStatistics(std::cout,"  ");
std::cout << "\n";
}
SignalHandler::instance()->reset();
}
public:
void run(size_t stratumIndex = (size_t) -1) override { runFunction(".", ".", stratumIndex, false); }
public:
void runAll(std::string inputDirectory = ".", std::string outputDirectory = ".", size_t stratumIndex = (size_t) -1) override { runFunction(inputDirectory, outputDirectory, stratumIndex, true);
}
public:
void printAll(std::string outputDirectory = ".") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","n\tm"},{"filename","./reachable.csv"},{"name","reachable"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_2_reachable);
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
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_2_reachable");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_2_reachable);
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
static factory_Sf_path __factory_Sf_path_instance;
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(path.dl)",
R"(.)",
R"(.)",
false,
R"()",
1,
-1);
if (!opt.parse(argc,argv)) return 1;
#if defined(_OPENMP) 
omp_set_nested(true);

#endif
souffle::Sf_path obj;
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir(), opt.getStratumIndex());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
