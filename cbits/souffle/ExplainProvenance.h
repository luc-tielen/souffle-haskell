/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ExplainProvenance.h
 *
 * Abstract class for implementing an instance of the explain interface for provenance
 *
 ***********************************************************************/

#pragma once

#include "ExplainTree.h"
#include "RamTypes.h"
#include "SouffleInterface.h"
#include "WriteStreamCSV.h"

#include <sstream>
#include <string>
#include <vector>

namespace souffle {

/* Equivalence class for variables in query command */
class Equivalence {
public:
    ~Equivalence() = default;
    Equivalence(char t, std::string s, std::pair<size_t, size_t> idx) : type(t), symbol(s) {
        indices.push_back(idx);
    }

    Equivalence(const Equivalence& o) = default;

    Equivalence& operator=(const Equivalence& o) = default;

    void push_back(std::pair<size_t, size_t> idx) {
        indices.push_back(idx);
    }

    // verify if elements at the indices are equivalent in the given product
    bool verify(const std::vector<tuple>& product) const {
        for (size_t i = 1; i < indices.size(); ++i) {
            if (product[indices[i].first][indices[i].second] !=
                    product[indices[i - 1].first][indices[i - 1].second]) {
                return false;
            }
        }
        return true;
    }

    // extract index of the first occurrence of the varible
    std::pair<size_t, size_t> getFirstIdx() {
        return indices[0];
    }

    // get hte indices of the varible
    std::vector<std::pair<size_t, size_t>> getIndices() {
        return indices;
    }

    // return type of the variable of the equivalence class, 'i' for RamDomain, 's' for symbol
    char getType() {
        return type;
    }

    // get the symbol of variable
    std::string getSymbol() {
        return symbol;
    }

private:
    char type;
    std::string symbol;
    std::vector<std::pair<size_t, size_t>> indices;
};

/* const constraints for values in query command */
class ConstConstraint {
public:
    ConstConstraint() = default;
    ~ConstConstraint() = default;
    void push_back(std::pair<std::pair<size_t, size_t>, RamDomain> constr) {
        constConstrs.push_back(constr);
    }

    // verify if the query product satisifies constant constraint
    bool verify(const std::vector<tuple>& product) const {
        for (auto constr : constConstrs) {
            if (product[constr.first.first][constr.first.second] != constr.second) {
                return false;
            }
        }
        return true;
    }

    // get the const constraint vector
    std::vector<std::pair<std::pair<size_t, size_t>, RamDomain>>& getConstraints() {
        return constConstrs;
    }

private:
    std::vector<std::pair<std::pair<size_t, size_t>, RamDomain>> constConstrs;
};

/** utility function to split a string */
inline std::vector<std::string> split(const std::string& s, char delim, int times = -1) {
    std::vector<std::string> v;
    std::stringstream ss(s);
    std::string item;

    while ((times > 0 || times <= -1) && std::getline(ss, item, delim)) {
        v.push_back(item);
        times--;
    }

    if (ss.peek() != EOF) {
        std::string remainder;
        std::getline(ss, remainder);
        v.push_back(remainder);
    }

    return v;
}

class ExplainProvenance {
public:
    ExplainProvenance(SouffleProgram& prog, bool useSublevels)
            : prog(prog), symTable(prog.getSymbolTable()), useSublevels(useSublevels) {}
    virtual ~ExplainProvenance() = default;

    virtual void setup() = 0;

    virtual std::unique_ptr<TreeNode> explain(
            std::string relName, std::vector<std::string> tuple, size_t depthLimit) = 0;

    virtual std::unique_ptr<TreeNode> explainSubproof(
            std::string relName, RamDomain label, size_t depthLimit) = 0;

    virtual std::vector<std::string> explainNegationGetVariables(
            std::string relName, std::vector<std::string> args, size_t ruleNum) = 0;

    virtual std::unique_ptr<TreeNode> explainNegation(std::string relName, size_t ruleNum,
            const std::vector<std::string>& tuple, std::map<std::string, std::string>& bodyVariables) = 0;

    virtual std::string getRule(std::string relName, size_t ruleNum) = 0;

    virtual std::vector<std::string> getRules(std::string relName) = 0;

    virtual std::string measureRelation(std::string relName) = 0;

    virtual void printRulesJSON(std::ostream& os) = 0;

    virtual std::string queryProcess(
            const std::vector<std::pair<std::string, std::vector<std::string>>>& rels) = 0;

    virtual std::string getRelationOutput(const std::string& relName) {
        auto rel = prog.getRelation(relName);
        if (rel == nullptr) {
            return "Relation " + relName + " not found\n";
        }

        // create symbol mask
        std::vector<bool> symMask(rel->getArity());

        for (size_t i = 0; i < rel->getArity(); i++) {
            if (*(rel->getAttrType(i)) == 's') {
                symMask.at(i) = true;
            }
        }

        // create IODirectives
        IODirectives dir;
        dir.setRelationName(relName);

        // redirect cout to stringstream
        std::stringstream out;
        auto originalCoutBuf = std::cout.rdbuf(out.rdbuf());

        // print relation
        printRelationOutput(symMask, dir, *rel);

        // restore original cout buffer
        std::cout.rdbuf(originalCoutBuf);

        return out.str();
    }

protected:
    SouffleProgram& prog;
    SymbolTable& symTable;
    bool useSublevels;

    std::vector<RamDomain> argsToNums(
            const std::string& relName, const std::vector<std::string>& args) const {
        std::vector<RamDomain> nums;

        auto rel = prog.getRelation(relName);
        if (rel == nullptr) {
            return nums;
        }

        for (size_t i = 0; i < args.size(); i++) {
            if (*rel->getAttrType(i) == 's') {
                // remove quotation marks
                if (args[i].size() >= 2 && args[i][0] == '"' && args[i][args[i].size() - 1] == '"') {
                    auto originalStr = args[i].substr(1, args[i].size() - 2);
                    nums.push_back(symTable.lookup(originalStr));
                }
            } else {
                nums.push_back(std::stoi(args[i]));
            }
        }

        return nums;
    }

    std::vector<std::string> numsToArgs(const std::string& relName, const std::vector<RamDomain>& nums,
            std::vector<bool>* err = nullptr) const {
        std::vector<std::string> args;

        auto rel = prog.getRelation(relName);
        if (rel == nullptr) {
            return args;
        }

        for (size_t i = 0; i < nums.size(); i++) {
            if (err && (*err)[i]) {
                args.push_back("_");
            } else {
                if (*rel->getAttrType(i) == 's') {
                    args.push_back("\"" + std::string(symTable.resolve(nums[i])) + "\"");
                } else {
                    args.push_back(std::to_string(nums[i]));
                }
            }
        }

        return args;
    }

    virtual void printRelationOutput(
            const std::vector<bool>& symMask, const IODirectives& ioDir, const Relation& rel) = 0;
};

}  // end of namespace souffle
