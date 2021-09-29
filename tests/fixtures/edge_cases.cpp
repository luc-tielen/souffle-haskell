
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_iii__0_1_2__111 {
using t_tuple = Tuple<RamDomain, 3>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2])) ? -1 : (ramBitCast<RamSigned>(a[2]) > ramBitCast<RamSigned>(b[2])) ? 1 :(0)));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))|| (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1])) && ((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2]))));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]))&&(ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2]));
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
struct t_btree_i__0__1 {
using t_tuple = Tuple<RamDomain, 1>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :(0);
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]));
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
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper, context& h) const {
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

class Sf_edge_cases : public SouffleProgram {
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
	R"_()_",
	R"_(abc)_",
	R"_(long_string_from_DL:...............................................................................................................................................................................................................................................................................................end)_",
	R"_(∀)_",
	R"_(∀∀)_",
};// -- initialize record table --
RecordTable recordTable;
// -- Table: empty_strings
Own<t_btree_iii__0_1_2__111> rel_1_empty_strings = mk<t_btree_iii__0_1_2__111>();
souffle::RelationWrapper<0,t_btree_iii__0_1_2__111,Tuple<RamDomain,3>,3,0> wrapper_rel_1_empty_strings;
// -- Table: long_strings
Own<t_btree_i__0__1> rel_2_long_strings = mk<t_btree_i__0__1>();
souffle::RelationWrapper<1,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_2_long_strings;
// -- Table: no_strings
Own<t_btree_uif__0_1_2__111> rel_3_no_strings = mk<t_btree_uif__0_1_2__111>();
souffle::RelationWrapper<2,t_btree_uif__0_1_2__111,Tuple<RamDomain,3>,3,0> wrapper_rel_3_no_strings;
// -- Table: unicode
Own<t_btree_i__0__1> rel_4_unicode = mk<t_btree_i__0__1>();
souffle::RelationWrapper<3,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_4_unicode;
public:
Sf_edge_cases() : 
wrapper_rel_1_empty_strings(*rel_1_empty_strings,symTable,"empty_strings",std::array<const char *,3>{{"s:symbol","s:symbol","i:number"}},std::array<const char *,3>{{"s","s2","n"}}),

wrapper_rel_2_long_strings(*rel_2_long_strings,symTable,"long_strings",std::array<const char *,1>{{"s:symbol"}},std::array<const char *,1>{{"s"}}),

wrapper_rel_3_no_strings(*rel_3_no_strings,symTable,"no_strings",std::array<const char *,3>{{"u:unsigned","i:number","f:float"}},std::array<const char *,3>{{"u","n","f"}}),

wrapper_rel_4_unicode(*rel_4_unicode,symTable,"unicode",std::array<const char *,1>{{"s:symbol"}},std::array<const char *,1>{{"s"}}){
addRelation("empty_strings",&wrapper_rel_1_empty_strings,true,true);
addRelation("long_strings",&wrapper_rel_2_long_strings,true,true);
addRelation("no_strings",&wrapper_rel_3_no_strings,true,true);
addRelation("unicode",&wrapper_rel_4_unicode,true,true);
}
~Sf_edge_cases() {
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
void run() override { runFunction("", "", false); }
public:
void runAll(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "") override { runFunction(inputDirectoryArg, outputDirectoryArg, true);
}
public:
void printAll(std::string outputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s\ts2\tn"},{"name","empty_strings"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"s\", \"s2\", \"n\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\", \"i:number\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_empty_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s"},{"name","long_strings"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"s\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_2_long_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\tn\tf"},{"name","no_strings"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"u\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_3_no_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s"},{"name","unicode"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"s\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_unicode);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s"},{"fact-dir","."},{"name","long_strings"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"s\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_long_strings);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s"},{"fact-dir","."},{"name","unicode"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"s\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_4_unicode);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\tn\tf"},{"fact-dir","."},{"name","no_strings"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"u\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_no_strings);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s\ts2\tn"},{"fact-dir","."},{"name","empty_strings"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"s\", \"s2\", \"n\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\", \"i:number\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_empty_strings);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
public:
void dumpInputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "long_strings";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_long_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "unicode";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_unicode);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "no_strings";
rwOperation["types"] = "{\"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_3_no_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "empty_strings";
rwOperation["types"] = "{\"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\", \"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_empty_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "empty_strings";
rwOperation["types"] = "{\"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\", \"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_empty_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "long_strings";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_long_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "no_strings";
rwOperation["types"] = "{\"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_3_no_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "unicode";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_unicode);
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
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_0(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s\ts2\tn"},{"fact-dir","."},{"name","empty_strings"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"s\", \"s2\", \"n\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\", \"i:number\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_empty_strings);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
SignalHandler::instance()->setMsg(R"_(empty_strings("","",42).
in file /home/luc/personal/souffle-haskell/tests/fixtures/edge_cases.dl [20:1-20:27])_");
[&](){
CREATE_OP_CONTEXT(rel_1_empty_strings_op_ctxt,rel_1_empty_strings->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(0)),ramBitCast(RamSigned(0)),ramBitCast(RamSigned(42))}};
rel_1_empty_strings->insert(tuple,READ_OP_CONTEXT(rel_1_empty_strings_op_ctxt));
}
();SignalHandler::instance()->setMsg(R"_(empty_strings("","abc",42).
in file /home/luc/personal/souffle-haskell/tests/fixtures/edge_cases.dl [21:1-21:30])_");
[&](){
CREATE_OP_CONTEXT(rel_1_empty_strings_op_ctxt,rel_1_empty_strings->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(0)),ramBitCast(RamSigned(1)),ramBitCast(RamSigned(42))}};
rel_1_empty_strings->insert(tuple,READ_OP_CONTEXT(rel_1_empty_strings_op_ctxt));
}
();SignalHandler::instance()->setMsg(R"_(empty_strings("abc","",42).
in file /home/luc/personal/souffle-haskell/tests/fixtures/edge_cases.dl [22:1-22:30])_");
[&](){
CREATE_OP_CONTEXT(rel_1_empty_strings_op_ctxt,rel_1_empty_strings->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(1)),ramBitCast(RamSigned(0)),ramBitCast(RamSigned(42))}};
rel_1_empty_strings->insert(tuple,READ_OP_CONTEXT(rel_1_empty_strings_op_ctxt));
}
();if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s\ts2\tn"},{"name","empty_strings"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"s\", \"s2\", \"n\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\", \"i:number\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_empty_strings);
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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s"},{"fact-dir","."},{"name","long_strings"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"s\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_long_strings);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
SignalHandler::instance()->setMsg(R"_(long_strings("long_string_from_DL:...............................................................................................................................................................................................................................................................................................end").
in file /home/luc/personal/souffle-haskell/tests/fixtures/edge_cases.dl [25:1-25:328])_");
[&](){
CREATE_OP_CONTEXT(rel_2_long_strings_op_ctxt,rel_2_long_strings->createContext());
Tuple<RamDomain,1> tuple{{ramBitCast(RamSigned(2))}};
rel_2_long_strings->insert(tuple,READ_OP_CONTEXT(rel_2_long_strings_op_ctxt));
}
();if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s"},{"name","long_strings"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"s\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_2_long_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_2(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s"},{"fact-dir","."},{"name","unicode"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"s\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_4_unicode);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
SignalHandler::instance()->setMsg(R"_(unicode("∀").
in file /home/luc/personal/souffle-haskell/tests/fixtures/edge_cases.dl [30:1-30:16])_");
[&](){
CREATE_OP_CONTEXT(rel_4_unicode_op_ctxt,rel_4_unicode->createContext());
Tuple<RamDomain,1> tuple{{ramBitCast(RamSigned(3))}};
rel_4_unicode->insert(tuple,READ_OP_CONTEXT(rel_4_unicode_op_ctxt));
}
();SignalHandler::instance()->setMsg(R"_(unicode("∀∀").
in file /home/luc/personal/souffle-haskell/tests/fixtures/edge_cases.dl [31:1-31:19])_");
[&](){
CREATE_OP_CONTEXT(rel_4_unicode_op_ctxt,rel_4_unicode->createContext());
Tuple<RamDomain,1> tuple{{ramBitCast(RamSigned(4))}};
rel_4_unicode->insert(tuple,READ_OP_CONTEXT(rel_4_unicode_op_ctxt));
}
();if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","s"},{"name","unicode"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"s\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_unicode);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_3(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\tn\tf"},{"fact-dir","."},{"name","no_strings"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"u\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_no_strings);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
SignalHandler::instance()->setMsg(R"_(no_strings(42,-100,1.5).
in file /home/luc/personal/souffle-haskell/tests/fixtures/edge_cases.dl [33:1-33:27])_");
[&](){
CREATE_OP_CONTEXT(rel_3_no_strings_op_ctxt,rel_3_no_strings->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamUnsigned(42)),ramBitCast(RamSigned(-100)),ramBitCast(RamFloat(1.5))}};
rel_3_no_strings->insert(tuple,READ_OP_CONTEXT(rel_3_no_strings_op_ctxt));
}
();SignalHandler::instance()->setMsg(R"_(no_strings(123,-456,3.14).
in file /home/luc/personal/souffle-haskell/tests/fixtures/edge_cases.dl [34:1-34:29])_");
[&](){
CREATE_OP_CONTEXT(rel_3_no_strings_op_ctxt,rel_3_no_strings->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamUnsigned(123)),ramBitCast(RamSigned(-456)),ramBitCast(RamFloat(3.1400001))}};
rel_3_no_strings->insert(tuple,READ_OP_CONTEXT(rel_3_no_strings_op_ctxt));
}
();if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","u\tn\tf"},{"name","no_strings"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"u\", \"n\", \"f\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"u:unsigned\", \"i:number\", \"f:float\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_3_no_strings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
};
SouffleProgram *newInstance_edge_cases(){return new Sf_edge_cases;}
SymbolTable *getST_edge_cases(SouffleProgram *p){return &reinterpret_cast<Sf_edge_cases*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_edge_cases: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_edge_cases();
};
public:
factory_Sf_edge_cases() : ProgramFactory("edge_cases"){}
};
extern "C" {
factory_Sf_edge_cases __factory_Sf_edge_cases_instance;
}
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(edge_cases.dl)",
R"()",
R"()",
false,
R"()",
1);
if (!opt.parse(argc,argv)) return 1;
souffle::Sf_edge_cases obj;
#if defined(_OPENMP) 
obj.setNumThreads(opt.getNumJobs());

#endif
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
