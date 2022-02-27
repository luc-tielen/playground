
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_ii__1_0__11__01 {
static constexpr Relation::arity_type Arity = 2;
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :((ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))|| (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1])) && ((ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]))&&(ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]));
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
range<t_ind_0::iterator> lowerUpperRange_01(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_01(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_01(lower,upper,h);
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
o << " arity 2 direct b-tree index 0 lex-order [1,0]\n";
ind_0.printStats(o);
}
};
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

class Sf_categorize : public SouffleProgram {
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
	R"_(web)_",
	R"_(internet)_",
};// -- initialize record table --
SpecializedRecordTable<0> recordTable{};
// -- Table: normalize_category
Own<t_btree_ii__1_0__11__01> rel_1_normalize_category = mk<t_btree_ii__1_0__11__01>();
souffle::RelationWrapper<t_btree_ii__1_0__11__01> wrapper_rel_1_normalize_category;
// -- Table: user_package_category
Own<t_btree_ii__0_1__11> rel_2_user_package_category = mk<t_btree_ii__0_1__11>();
souffle::RelationWrapper<t_btree_ii__0_1__11> wrapper_rel_2_user_package_category;
// -- Table: normalize_issue
Own<t_btree_ii__0_1__11> rel_3_normalize_issue = mk<t_btree_ii__0_1__11>();
souffle::RelationWrapper<t_btree_ii__0_1__11> wrapper_rel_3_normalize_issue;
// -- Table: normalized_package_category
Own<t_btree_ii__0_1__11> rel_4_normalized_package_category = mk<t_btree_ii__0_1__11>();
souffle::RelationWrapper<t_btree_ii__0_1__11> wrapper_rel_4_normalized_package_category;
public:
Sf_categorize()
: wrapper_rel_1_normalize_category(0, *rel_1_normalize_category, *this, "normalize_category", std::array<const char *,2>{{"s:symbol","s:symbol"}}, std::array<const char *,2>{{"normalized","user_input"}}, 0)
, wrapper_rel_2_user_package_category(1, *rel_2_user_package_category, *this, "user_package_category", std::array<const char *,2>{{"s:symbol","s:symbol"}}, std::array<const char *,2>{{"pkg_name","category"}}, 0)
, wrapper_rel_3_normalize_issue(2, *rel_3_normalize_issue, *this, "normalize_issue", std::array<const char *,2>{{"s:symbol","s:symbol"}}, std::array<const char *,2>{{"pkg_name","category"}}, 0)
, wrapper_rel_4_normalized_package_category(3, *rel_4_normalized_package_category, *this, "normalized_package_category", std::array<const char *,2>{{"s:symbol","s:symbol"}}, std::array<const char *,2>{{"pkg_name","category"}}, 0)
{
addRelation("normalize_category", wrapper_rel_1_normalize_category, false, false);
addRelation("user_package_category", wrapper_rel_2_user_package_category, true, false);
addRelation("normalize_issue", wrapper_rel_3_normalize_issue, false, true);
addRelation("normalized_package_category", wrapper_rel_4_normalized_package_category, false, true);
}
~Sf_categorize() {
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
{
 std::vector<RamDomain> args, ret;
subroutine_2(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_3(args, ret);
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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","pkg_name\tcategory"},{"auxArity","0"},{"name","normalize_issue"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"pkg_name\", \"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_3_normalize_issue);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","pkg_name\tcategory"},{"auxArity","0"},{"name","normalized_package_category"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"pkg_name\", \"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_normalized_package_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","pkg_name\tcategory"},{"auxArity","0"},{"fact-dir","."},{"name","user_package_category"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"pkg_name\", \"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_user_package_category);
} catch (std::exception& e) {std::cerr << "Error loading user_package_category data: " << e.what() << '\n';}
}
public:
void dumpInputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "user_package_category";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_user_package_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "normalize_issue";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_3_normalize_issue);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "normalized_package_category";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_normalized_package_category);
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
signalHandler->setMsg(R"_(normalize_category("web","web").
in file /home/luc/personal/souffle-haskell-example/cbits/categorize.dl [25:1-25:34])_");
[&](){
CREATE_OP_CONTEXT(rel_1_normalize_category_op_ctxt,rel_1_normalize_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(0)),ramBitCast(RamSigned(0))}};
rel_1_normalize_category->insert(tuple,READ_OP_CONTEXT(rel_1_normalize_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalize_category("web","internet").
in file /home/luc/personal/souffle-haskell-example/cbits/categorize.dl [26:1-26:39])_");
[&](){
CREATE_OP_CONTEXT(rel_1_normalize_category_op_ctxt,rel_1_normalize_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(0)),ramBitCast(RamSigned(1))}};
rel_1_normalize_category->insert(tuple,READ_OP_CONTEXT(rel_1_normalize_category_op_ctxt));
}
();}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_1(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","pkg_name\tcategory"},{"auxArity","0"},{"fact-dir","."},{"name","user_package_category"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"pkg_name\", \"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_user_package_category);
} catch (std::exception& e) {std::cerr << "Error loading user_package_category data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_2(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(normalize_issue(pkg,category) :- 
   user_package_category(pkg,category),
   !normalize_category(_,category).
in file /home/luc/personal/souffle-haskell-example/cbits/categorize.dl [19:1-21:36])_");
if(!(rel_2_user_package_category->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_3_normalize_issue_op_ctxt,rel_3_normalize_issue->createContext());
CREATE_OP_CONTEXT(rel_2_user_package_category_op_ctxt,rel_2_user_package_category->createContext());
CREATE_OP_CONTEXT(rel_1_normalize_category_op_ctxt,rel_1_normalize_category->createContext());
for(const auto& env0 : *rel_2_user_package_category) {
if( !(!rel_1_normalize_category->lowerUpperRange_01(Tuple<RamDomain,2>{{ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[1])}},Tuple<RamDomain,2>{{ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_1_normalize_category_op_ctxt)).empty())) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_3_normalize_issue->insert(tuple,READ_OP_CONTEXT(rel_3_normalize_issue_op_ctxt));
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","pkg_name\tcategory"},{"auxArity","0"},{"name","normalize_issue"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"pkg_name\", \"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_3_normalize_issue);
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
signalHandler->setMsg(R"_(normalized_package_category(pkg,normalized) :- 
   user_package_category(pkg,user_category),
   normalize_category(normalized,user_category).
in file /home/luc/personal/souffle-haskell-example/cbits/categorize.dl [15:1-17:49])_");
if(!(rel_2_user_package_category->empty()) && !(rel_1_normalize_category->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_2_user_package_category_op_ctxt,rel_2_user_package_category->createContext());
CREATE_OP_CONTEXT(rel_1_normalize_category_op_ctxt,rel_1_normalize_category->createContext());
CREATE_OP_CONTEXT(rel_4_normalized_package_category_op_ctxt,rel_4_normalized_package_category->createContext());
for(const auto& env0 : *rel_2_user_package_category) {
auto range = rel_1_normalize_category->lowerUpperRange_01(Tuple<RamDomain,2>{{ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[1])}},Tuple<RamDomain,2>{{ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_1_normalize_category_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env1[0])}};
rel_4_normalized_package_category->insert(tuple,READ_OP_CONTEXT(rel_4_normalized_package_category_op_ctxt));
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","pkg_name\tcategory"},{"auxArity","0"},{"name","normalized_package_category"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"pkg_name\", \"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_normalized_package_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (pruneImdtRels) rel_2_user_package_category->purge();
if (pruneImdtRels) rel_1_normalize_category->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
};
SouffleProgram *newInstance_categorize(){return new Sf_categorize;}
SymbolTable *getST_categorize(SouffleProgram *p){return &reinterpret_cast<Sf_categorize*>(p)->getSymbolTable();}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_categorize: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_categorize();
};
public:
factory_Sf_categorize() : ProgramFactory("categorize"){}
};
extern "C" {
factory_Sf_categorize __factory_Sf_categorize_instance;
}
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(/home/luc/personal/souffle-haskell-example/cbits/categorize.dl)",
R"()",
R"()",
false,
R"()",
1);
if (!opt.parse(argc,argv)) return 1;
souffle::Sf_categorize obj;
#if defined(_OPENMP) 
obj.setNumThreads(opt.getNumJobs());

#endif
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
