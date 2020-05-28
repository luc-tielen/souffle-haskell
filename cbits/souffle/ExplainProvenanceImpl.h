/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ExplainProvenanceImpl.h
 *
 * Implementation of abstract class in ExplainProvenance.h for guided Impl provenance
 *
 ***********************************************************************/

#pragma once

#include "BinaryConstraintOps.h"
#include "ExplainProvenance.h"
#include "Util.h"

#include <algorithm>
#include <chrono>
#include <map>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

namespace souffle {

class ExplainProvenanceImpl : public ExplainProvenance {
public:
    ExplainProvenanceImpl(SouffleProgram& prog, bool useSublevels) : ExplainProvenance(prog, useSublevels) {
        setup();
    }

    void setup() override {
        // for each clause, store a mapping from the head relation name to body relation names
        for (auto& rel : prog.getAllRelations()) {
            std::string name = rel->getName();

            // only process info relations
            if (name.find("@info") == std::string::npos) {
                continue;
            }

            // find all the info tuples
            for (auto& tuple : *rel) {
                std::vector<std::string> bodyLiterals;

                RamDomain ruleNum;
                tuple >> ruleNum;

                for (size_t i = 1; i < rel->getArity() - 1; i++) {
                    std::string bodyLit;
                    tuple >> bodyLit;
                    bodyLiterals.push_back(bodyLit);
                }

                std::string rule;
                tuple >> rule;

                info.insert({std::make_pair(name.substr(0, name.find(".@info")), ruleNum), bodyLiterals});
                rules.insert({std::make_pair(name.substr(0, name.find(".@info")), ruleNum), rule});
            }
        }
    }

    std::unique_ptr<TreeNode> explain(std::string relName, std::vector<RamDomain> tuple, int ruleNum,
            int levelNum, std::vector<RamDomain> subtreeLevels, size_t depthLimit) {
        std::stringstream joinedArgs;
        joinedArgs << join(numsToArgs(relName, tuple), ", ");
        auto joinedArgsStr = joinedArgs.str();

        // if fact
        if (levelNum == 0) {
            return std::make_unique<LeafNode>(relName + "(" + joinedArgsStr + ")");
        }

        assert(info.find(std::make_pair(relName, ruleNum)) != info.end() && "invalid rule for tuple");

        // if depth limit exceeded
        if (depthLimit <= 1) {
            tuple.push_back(ruleNum);
            tuple.push_back(levelNum);

            for (auto subtreeLevel : subtreeLevels) {
                tuple.push_back(subtreeLevel);
            }

            // find if subproof exists already
            size_t idx = 0;
            auto it = std::find(subproofs.begin(), subproofs.end(), tuple);
            if (it != subproofs.end()) {
                idx = it - subproofs.begin();
            } else {
                subproofs.push_back(tuple);
                idx = subproofs.size() - 1;
            }

            return std::make_unique<LeafNode>("subproof " + relName + "(" + std::to_string(idx) + ")");
        }

        auto internalNode = std::make_unique<InnerNode>(
                relName + "(" + joinedArgsStr + ")", "(R" + std::to_string(ruleNum) + ")");

        // make return vector pointer
        std::vector<RamDomain> ret;
        std::vector<bool> err;

        if (useSublevels)
            // add subtree level numbers to tuple
            for (auto subtreeLevel : subtreeLevels) {
                tuple.push_back(subtreeLevel);
            }
        else {
            tuple.push_back(levelNum);
        }

        // execute subroutine to get subproofs
        prog.executeSubroutine(relName + "_" + std::to_string(ruleNum) + "_subproof", tuple, ret, err);

        // recursively get nodes for subproofs
        size_t tupleCurInd = 0;
        auto bodyRelations = info[std::make_pair(relName, ruleNum)];

        // start from begin + 1 because the first element represents the head atom
        for (auto it = bodyRelations.begin() + 1; it < bodyRelations.end(); it++) {
            std::string bodyLiteral = *it;
            // split bodyLiteral since it contains relation name plus arguments
            std::string bodyRel = splitString(bodyLiteral, ',')[0];

            // check whether the current atom is a constraint
            assert(bodyRel.size() > 0 && "body of a relation should have positive length");
            bool isConstraint = std::find(constraintList.begin(), constraintList.end(),
                                        splitString(bodyRel, ',')[0]) != constraintList.end();

            // handle negated atom names
            auto bodyRelAtomName = bodyRel;
            if (bodyRel[0] == '!') {
                bodyRelAtomName = bodyRel.substr(1);
            }

            // traverse subroutine return
            size_t arity;
            size_t numberOfHeights;
            if (isConstraint) {
                // we only handle binary constraints, and assume arity is 4 to account for hidden provenance
                // annotations
                arity = 4;
                numberOfHeights = 1;
            } else {
                arity = prog.getRelation(bodyRelAtomName)->getArity();
                numberOfHeights = prog.getRelation(bodyRelAtomName)->getNumberOfHeights();
            }
            auto tupleEnd = tupleCurInd + arity;

            // store current tuple and error
            std::vector<RamDomain> subproofTuple;
            std::vector<bool> subproofTupleError;

            for (; tupleCurInd < tupleEnd - 1 - numberOfHeights; tupleCurInd++) {
                subproofTuple.push_back(ret[tupleCurInd]);
                subproofTupleError.push_back(err[tupleCurInd]);
            }

            int subproofRuleNum = ret[tupleCurInd];
            int subproofLevelNum = ret[tupleCurInd + 1];

            tupleCurInd += 2;

            std::vector<RamDomain> subsubtreeLevels;
            for (; tupleCurInd < tupleEnd; tupleCurInd++) {
                subsubtreeLevels.push_back(ret[tupleCurInd]);
            }

            // for a negation, display the corresponding tuple and do not recurse
            if (bodyRel[0] == '!') {
                std::stringstream joinedTuple;
                joinedTuple << join(numsToArgs(bodyRelAtomName, subproofTuple, &subproofTupleError), ", ");
                auto joinedTupleStr = joinedTuple.str();
                internalNode->add_child(std::make_unique<LeafNode>(bodyRel + "(" + joinedTupleStr + ")"));
                internalNode->setSize(internalNode->getSize() + 1);
                // for a binary constraint, display the corresponding values and do not recurse
            } else if (isConstraint) {
                std::stringstream joinedConstraint;

                if (isNumericBinaryConstraintOp(toBinaryConstraintOp(bodyRel))) {
                    joinedConstraint << subproofTuple[0] << " " << bodyRel << " " << subproofTuple[1];
                } else {
                    joinedConstraint << bodyRel << "(\"" << symTable.resolve(subproofTuple[0]) << "\", \""
                                     << symTable.resolve(subproofTuple[1]) << "\")";
                }

                internalNode->add_child(std::make_unique<LeafNode>(joinedConstraint.str()));
                internalNode->setSize(internalNode->getSize() + 1);
                // otherwise, for a normal tuple, recurse
            } else {
                auto child = explain(bodyRel, subproofTuple, subproofRuleNum, subproofLevelNum,
                        subsubtreeLevels, depthLimit - 1);
                internalNode->setSize(internalNode->getSize() + child->getSize());
                internalNode->add_child(std::move(child));
            }

            tupleCurInd = tupleEnd;
        }

        return std::move(internalNode);
    }

    std::unique_ptr<TreeNode> explain(
            std::string relName, std::vector<std::string> args, size_t depthLimit) override {
        auto tuple = argsToNums(relName, args);
        if (tuple.empty()) {
            return std::make_unique<LeafNode>("Relation not found");
        }

        std::tuple<int, int, std::vector<RamDomain>> tupleInfo = findTuple(relName, tuple);

        int ruleNum = std::get<0>(tupleInfo);
        int levelNum = std::get<1>(tupleInfo);
        std::vector<RamDomain> subtreeLevels = std::get<2>(tupleInfo);

        if (ruleNum < 0 || levelNum == -1) {
            return std::make_unique<LeafNode>("Tuple not found");
        }

        return explain(relName, tuple, ruleNum, levelNum, subtreeLevels, depthLimit);
    }

    std::unique_ptr<TreeNode> explainSubproof(
            std::string relName, RamDomain subproofNum, size_t depthLimit) override {
        if (subproofNum >= (int)subproofs.size()) {
            return std::make_unique<LeafNode>("Subproof not found");
        }

        auto tup = subproofs[subproofNum];

        auto rel = prog.getRelation(relName);

        RamDomain ruleNum;
        ruleNum = tup[rel->getArity() - rel->getNumberOfHeights() - 1];

        RamDomain levelNum;
        levelNum = tup[rel->getArity() - rel->getNumberOfHeights()];

        std::vector<RamDomain> subtreeLevels;

        for (size_t i = rel->getArity() - rel->getNumberOfHeights() + 1; i < rel->getArity(); i++) {
            subtreeLevels.push_back(tup[i]);
        }

        tup.erase(tup.begin() + rel->getArity() - rel->getNumberOfHeights() - 1, tup.end());

        return explain(relName, tup, ruleNum, levelNum, subtreeLevels, depthLimit);
    }

    std::vector<std::string> explainNegationGetVariables(
            std::string relName, std::vector<std::string> args, size_t ruleNum) override {
        std::vector<std::string> variables;

        // check that the tuple actually doesn't exist
        std::tuple<int, int, std::vector<RamDomain>> foundTuple =
                findTuple(relName, argsToNums(relName, args));
        if (std::get<0>(foundTuple) != -1 || std::get<1>(foundTuple) != -1) {
            // return a sentinel value
            return std::vector<std::string>({"@"});
        }

        // atom meta information stored for the current rule
        auto atoms = info[std::make_pair(relName, ruleNum)];

        // the info stores the set of atoms, if there is only 1 atom, then it must be the head, so it must be
        // a fact
        if (atoms.size() <= 1) {
            return std::vector<std::string>({"@fact"});
        }

        // atoms[0] represents variables in the head atom
        auto headVariables = splitString(atoms[0], ',');

        // check that head variable bindings make sense, i.e. for a head like a(x, x), make sure both x are
        // the same value
        std::map<std::string, std::string> headVariableMapping;
        for (size_t i = 0; i < headVariables.size(); i++) {
            if (headVariableMapping.find(headVariables[i]) == headVariableMapping.end()) {
                headVariableMapping[headVariables[i]] = args[i];
            } else {
                if (headVariableMapping[headVariables[i]] != args[i]) {
                    return std::vector<std::string>({"@non_matching"});
                }
            }
        }

        // get body variables
        std::vector<std::string> uniqueBodyVariables;
        for (auto it = atoms.begin() + 1; it < atoms.end(); it++) {
            auto atomRepresentation = splitString(*it, ',');

            // atomRepresentation.begin() + 1 because the first element is the relation name of the atom
            // which is not relevant for finding variables
            for (auto atomIt = atomRepresentation.begin() + 1; atomIt < atomRepresentation.end(); atomIt++) {
                if (!contains(uniqueBodyVariables, *atomIt) && !contains(headVariables, *atomIt)) {
                    uniqueBodyVariables.push_back(*atomIt);
                }
            }
        }

        return uniqueBodyVariables;
    }

    std::unique_ptr<TreeNode> explainNegation(std::string relName, size_t ruleNum,
            const std::vector<std::string>& tuple,
            std::map<std::string, std::string>& bodyVariables) override {
        // construct a vector of unique variables that occur in the rule
        std::vector<std::string> uniqueVariables;

        // we also need to know the type of each variable
        std::map<std::string, char> variableTypes;

        // atom meta information stored for the current rule
        auto atoms = info[std::make_pair(relName, ruleNum)];

        // atoms[0] represents variables in the head atom
        auto headVariables = splitString(atoms[0], ',');

        uniqueVariables.insert(uniqueVariables.end(), headVariables.begin(), headVariables.end());

        // get body variables
        for (auto it = atoms.begin() + 1; it < atoms.end(); it++) {
            auto atomRepresentation = splitString(*it, ',');

            // atomRepresentation.begin() + 1 because the first element is the relation name of the atom
            // which is not relevant for finding variables
            for (auto atomIt = atomRepresentation.begin() + 1; atomIt < atomRepresentation.end(); atomIt++) {
                if (!contains(uniqueVariables, *atomIt) && !contains(headVariables, *atomIt)) {
                    uniqueVariables.push_back(*atomIt);

                    if (!contains(constraintList, atomRepresentation[0])) {
                        // store type of variable
                        auto currentRel = prog.getRelation(atomRepresentation[0]);
                        assert(currentRel != nullptr &&
                                ("relation " + atomRepresentation[0] + " doesn't exist").c_str());
                        variableTypes[*atomIt] =
                                *currentRel->getAttrType(atomIt - atomRepresentation.begin() - 1);
                    } else if (atomIt->find("agg_") != std::string::npos) {
                        variableTypes[*atomIt] = 'i';
                    }
                }
            }
        }

        std::vector<RamDomain> args;

        size_t varCounter = 0;

        // construct arguments to pass in to the subroutine
        // - this contains the variable bindings selected by the user

        // add number representation of tuple
        auto tupleNums = argsToNums(relName, tuple);
        args.insert(args.end(), tupleNums.begin(), tupleNums.end());
        varCounter += tuple.size();

        while (varCounter < uniqueVariables.size()) {
            auto var = uniqueVariables[varCounter];
            auto varValue = bodyVariables[var];
            if (variableTypes[var] == 's') {
                if (varValue.size() >= 2 && varValue[0] == '"' && varValue[varValue.size() - 1] == '"') {
                    auto originalStr = varValue.substr(1, varValue.size() - 2);
                    args.push_back(symTable.lookup(originalStr));
                } else {
                    // assume no quotation marks
                    args.push_back(symTable.lookup(varValue));
                }
            } else {
                args.push_back(std::stoi(varValue));
            }

            varCounter++;
        }

        // set up return and error vectors for subroutine calling
        std::vector<RamDomain> ret;
        std::vector<bool> err;

        // execute subroutine to get subproofs
        prog.executeSubroutine(
                relName + "_" + std::to_string(ruleNum) + "_negation_subproof", args, ret, err);

        // construct tree nodes
        std::stringstream joinedArgsStr;
        joinedArgsStr << join(tuple, ",");
        auto internalNode = std::make_unique<InnerNode>(
                relName + "(" + joinedArgsStr.str() + ")", "(R" + std::to_string(ruleNum) + ")");

        // traverse return vector and construct child nodes
        // making sure we display existent and non-existent tuples correctly
        int literalCounter = 1;
        for (size_t returnCounter = 0; returnCounter < ret.size(); returnCounter++) {
            // check what the next contained atom is
            bool atomExists;
            if (err[returnCounter]) {
                atomExists = false;
                returnCounter++;
            } else {
                atomExists = true;
                assert(err[returnCounter + 1] && "there should be a separator for literals in return");
                returnCounter += 2;
            }

            // get the relation of the current atom
            auto atomRepresentation = splitString(atoms[literalCounter], ',');
            std::string bodyRel = atomRepresentation[0];

            // check whether the current atom is a constraint
            bool isConstraint =
                    std::find(constraintList.begin(), constraintList.end(), bodyRel) != constraintList.end();

            // handle negated atom names
            auto bodyRelAtomName = bodyRel;
            if (bodyRel[0] == '!') {
                bodyRelAtomName = bodyRel.substr(1);
            }

            // traverse subroutine return
            size_t arity;
            size_t numberOfHeights;
            if (isConstraint) {
                // we only handle binary constraints, and assume arity is 4 to account for hidden provenance
                // annotations
                arity = 4;
                numberOfHeights = 1;
            } else {
                arity = prog.getRelation(bodyRelAtomName)->getArity();
                numberOfHeights = prog.getRelation(bodyRelAtomName)->getNumberOfHeights();
            }

            // process current literal
            std::vector<RamDomain> atomValues;
            std::vector<bool> atomErrs;
            size_t j = returnCounter;

            for (; j < returnCounter + arity - 1 - numberOfHeights; j++) {
                atomValues.push_back(ret[j]);
                atomErrs.push_back(err[j]);
            }

            // add child nodes to the proof tree
            std::stringstream childLabel;
            if (isConstraint) {
                assert(atomValues.size() == 2 && "not a binary constraint");

                if (isNumericBinaryConstraintOp(toBinaryConstraintOp(bodyRel))) {
                    childLabel << atomValues[0] << " " << bodyRel << " " << atomValues[1];
                } else {
                    childLabel << bodyRel << "(\"" << symTable.resolve(atomValues[0]) << "\", \""
                               << symTable.resolve(atomValues[1]) << "\")";
                }
            } else {
                childLabel << bodyRel << "(";
                childLabel << join(numsToArgs(bodyRelAtomName, atomValues, &atomErrs), ", ");
                childLabel << ")";
            }

            if (atomExists) {
                childLabel << " ✓";
            } else {
                childLabel << " x";
            }

            internalNode->add_child(std::make_unique<LeafNode>(childLabel.str()));
            internalNode->setSize(internalNode->getSize() + 1);

            returnCounter = j - 1;
            literalCounter++;
        }

        return std::move(internalNode);
    }

    std::string getRule(std::string relName, size_t ruleNum) override {
        auto key = make_pair(relName, ruleNum);

        auto rule = rules.find(key);
        if (rule == rules.end()) {
            return "Rule not found";
        } else {
            return rule->second;
        }
    }

    std::vector<std::string> getRules(std::string relName) override {
        std::vector<std::string> relRules;
        // go through all rules
        for (auto& rule : rules) {
            if (rule.first.first == relName) {
                relRules.push_back(rule.second);
            }
        }

        return relRules;
    }

    std::string measureRelation(std::string relName) override {
        auto rel = prog.getRelation(relName);

        if (rel == nullptr) {
            return "No relation found\n";
        }

        auto size = rel->size();
        int skip = size / 10;

        if (skip == 0) skip = 1;

        std::stringstream ss;

        auto before_time = std::chrono::high_resolution_clock::now();

        int numTuples = 0;
        int proc = 0;
        for (auto& tuple : *rel) {
            auto tupleStart = std::chrono::high_resolution_clock::now();

            if (numTuples % skip != 0) {
                numTuples++;
                continue;
            }

            std::vector<RamDomain> currentTuple;
            for (size_t i = 0; i < rel->getArity() - 1 - rel->getNumberOfHeights(); i++) {
                RamDomain n;
                if (*rel->getAttrType(i) == 's') {
                    std::string s;
                    tuple >> s;
                    n = symTable.lookupExisting(s.c_str());
                } else {
                    tuple >> n;
                }

                currentTuple.push_back(n);
            }

            RamDomain ruleNum;
            tuple >> ruleNum;

            RamDomain levelNum;
            tuple >> levelNum;

            std::vector<RamDomain> subtreeLevels;

            for (size_t i = rel->getArity() - rel->getNumberOfHeights() + 1; i < rel->getArity(); i++) {
                RamDomain subLevel;
                tuple >> subLevel;
                subtreeLevels.push_back(subLevel);
            }

            std::cout << "Tuples expanded: "
                      << explain(relName, currentTuple, ruleNum, levelNum, subtreeLevels, 10000)->getSize();

            numTuples++;
            proc++;

            auto tupleEnd = std::chrono::high_resolution_clock::now();
            auto tupleDuration =
                    std::chrono::duration_cast<std::chrono::duration<double>>(tupleEnd - tupleStart);

            std::cout << ", Time: " << tupleDuration.count() << "\n";
        }

        auto after_time = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::duration<double>>(after_time - before_time);

        ss << "total: " << proc << " ";
        ss << duration.count() << std::endl;

        return ss.str();
    }

    void printRulesJSON(std::ostream& os) override {
        os << "\"rules\": [\n";
        bool first = true;
        for (auto const& cur : rules) {
            if (first)
                first = false;
            else
                os << ",\n";
            os << "\t{ \"rule-number\": \"(R" << cur.first.second << ")\", \"rule\": \""
               << stringify(cur.second) << "\"}";
        }
        os << "\n]\n";
    }

    std::string queryProcess(
            const std::vector<std::pair<std::string, std::vector<std::string>>>& rels) override {
        std::string queryResult = "";
        std::regex varRegex("[a-zA-Z_][a-zA-Z_0-9]*", std::regex_constants::extended);
        std::regex symbolRegex("\"([^\"]*)\"", std::regex_constants::extended);
        std::regex numberRegex("[0-9]+", std::regex_constants::extended);

        std::smatch argsMatcher;

        // map for variable name and corresponding equivalence class
        std::map<std::string, Equivalence> nameToEquivalence;

        // const constraints that solution must satisfy
        ConstConstraint constConstraints;

        // varRels stores relation of tuples that need to resolve which contains at least one variable
        std::vector<Relation*> varRels;

        // counter for adding element to varRels
        size_t idx = 0;

        // parse arguments in each relation Tuple
        for (size_t i = 0; i < rels.size(); ++i) {
            Relation* relation = prog.getRelation(rels[i].first);
            // constTulpe stores value of constant argument in the relation tuple
            std::vector<RamDomain> constTuple;
            // query relation does not exist
            if (relation == nullptr) {
                queryResult += "Relation <" + rels[i].first + "> does not exist\n";
                return queryResult;
            }
            // airty error
            if (relation->getArity() - 2 != rels[i].second.size()) {
                queryResult += "<" + rels[i].first + "> has arity of " +
                               std::to_string(relation->getArity() - 2) + "\n";
                return queryResult;
            }

            // check if args contain variable
            bool containVar = false;
            for (size_t j = 0; j < rels[i].second.size(); ++j) {
                // arg is a varaible
                if (std::regex_match(rels[i].second[j], argsMatcher, varRegex)) {
                    containVar = true;
                    auto nameToEquivalenceIter = nameToEquivalence.find(argsMatcher[0]);
                    // if varaible has not shown up before, create an equivalence class for it and add it to
                    // map, otherwise add its indices to corresponding equivalence class
                    if (nameToEquivalenceIter == nameToEquivalence.end()) {
                        nameToEquivalence.insert(
                                {argsMatcher[0], Equivalence(*(relation->getAttrType(j)), argsMatcher[0],
                                                         std::make_pair(idx, j))});
                    } else {
                        nameToEquivalenceIter->second.push_back(std::make_pair(idx, j));
                    }
                    // arg is a symbol
                } else if (std::regex_match(rels[i].second[j], argsMatcher, symbolRegex)) {
                    if (*(relation->getAttrType(j)) != 's') {
                        queryResult += argsMatcher.str(0) + " does not match type defined in relation\n";
                        return queryResult;
                    }
                    // find index of symbol and add indices pair to constConstraints
                    RamDomain rd = prog.getSymbolTable().lookup(argsMatcher[1]);
                    constConstraints.push_back(std::make_pair(std::make_pair(idx, j), rd));
                    if (!containVar) {
                        constTuple.push_back(rd);
                    }
                    // arg is number
                } else if (std::regex_match(rels[i].second[j], argsMatcher, numberRegex)) {
                    if (*(relation->getAttrType(j)) != 'i') {
                        queryResult += argsMatcher.str(0) + " does not match type defined in relation\n";
                        return queryResult;
                    }
                    // convert number string to number and add index, number pair to constConstraints
                    RamDomain rd = std::stoi(argsMatcher[0]);
                    constConstraints.push_back(std::make_pair(std::make_pair(idx, j), rd));
                    if (!containVar) {
                        constTuple.push_back(rd);
                    }
                }
            }

            // if tuple does not contain any variable, check if existence of the tuple
            if (!containVar) {
                bool tupleExist = containsTuple(relation, constTuple);

                // if relation contains this tuple, remove all related constraints and output tuple exits
                if (tupleExist) {
                    queryResult += "Tuple " + rels[i].first + "(";
                    for (size_t l = 0; l < rels[i].second.size() - 1; ++l) {
                        queryResult += rels[i].second[l] + ", ";
                    }
                    queryResult += rels[i].second.back() + ") exists\n";
                } else {  // otherwise, there is no solution for given query
                    queryResult += "Tuple " + rels[i].first + "(";
                    for (size_t l = 0; l < rels[i].second.size() - 1; ++l) {
                        queryResult += rels[i].second[l] + ", ";
                    }
                    queryResult += rels[i].second.back() + ") does not exist\n";
                }
                // erase the related constant constraints from constConstraints vector
                constConstraints.getConstraints().erase(
                        constConstraints.getConstraints().end() - relation->getArity() + 2,
                        constConstraints.getConstraints().end());
            } else {
                varRels.push_back(relation);
                ++idx;
            }
        }

        // if varRels size is 0, all given tuples only contain constant and exist, no variable to resolve
        if (varRels.size() == 0) {
            return queryResult;
        }

        // the width of the slot to print solution properly, default width is 8
        size_t slotWidth = 8;
        std::vector<std::vector<RamDomain>> solutions;

        // find solution for parametrised query
        findQuerySolution(varRels, nameToEquivalence, constConstraints, solutions, slotWidth);

        // change slotWidth if any varaible name length is greater than slotWidth
        for (auto var : nameToEquivalence) {
            if (var.first.length() > slotWidth) {
                slotWidth = var.first.length();
            }
        }

        // form string of solution
        std::string solutionString = getSolutionString(solutions, nameToEquivalence, slotWidth);

        queryResult += solutionString;
        return queryResult;
    }

private:
    std::map<std::pair<std::string, size_t>, std::vector<std::string>> info;
    std::map<std::pair<std::string, size_t>, std::string> rules;
    std::vector<std::vector<RamDomain>> subproofs;
    std::vector<std::string> constraintList = {
            "=", "!=", "<", "<=", ">=", ">", "match", "contains", "not_match", "not_contains"};

    std::tuple<int, int, std::vector<RamDomain>> findTuple(
            const std::string& relName, std::vector<RamDomain> tup) {
        auto rel = prog.getRelation(relName);

        if (rel == nullptr) {
            return std::make_tuple(-1, -1, std::vector<RamDomain>());
        }

        // find correct tuple
        for (auto& tuple : *rel) {
            bool match = true;
            std::vector<RamDomain> currentTuple;

            for (size_t i = 0; i < rel->getArity() - 1 - rel->getNumberOfHeights(); i++) {
                RamDomain n;
                if (*rel->getAttrType(i) == 's') {
                    std::string s;
                    tuple >> s;
                    n = symTable.lookupExisting(s);
                } else {
                    tuple >> n;
                }

                currentTuple.push_back(n);

                if (n != tup[i]) {
                    match = false;
                    break;
                }
            }

            if (match) {
                RamDomain ruleNum;
                tuple >> ruleNum;

                RamDomain levelNum;
                tuple >> levelNum;

                std::vector<RamDomain> subtreeLevels;

                for (size_t i = rel->getArity() - rel->getNumberOfHeights() + 1; i < rel->getArity(); i++) {
                    RamDomain subLevel;
                    tuple >> subLevel;
                    subtreeLevels.push_back(subLevel);
                }

                return std::make_tuple(ruleNum, levelNum, subtreeLevels);
            }
        }

        // if no tuple exists
        return std::make_tuple(-1, -1, std::vector<RamDomain>());
    }

    void printRelationOutput(
            const std::vector<bool>& symMask, const IODirectives& ioDir, const Relation& rel) override {
        WriteCoutCSVFactory()
                .getWriter(symMask, prog.getSymbolTable(), ioDir, true, rel.getNumberOfHeights())
                ->writeAll(rel);
    }

    /*
     * find solution for parametrised query satisify constant constraints and equivalence constraints, also
     * modify the width of slot to print solutions properly.
     * @param varRels, reference to vector of relation of tuple contains at least one variable in its
     * arguments
     * @param nameToEquivalence, reference to variable name and corresponding equivalence class
     * @param constConstraints, reference to const constraints must be satisfied
     * @solutions, reference to vector stores solutions
     * @slotWidth, reference to width to print solution properly
     * */
    void findQuerySolution(const std::vector<Relation*>& varRels,
            const std::map<std::string, Equivalence>& nameToEquivalence,
            const ConstConstraint& constConstraints, std::vector<std::vector<RamDomain>>& solutions,
            size_t& slotWidth) {
        // vector of iterators for relations in varRels
        std::vector<Relation::iterator> varRelationIterators;
        for (auto relation : varRels) {
            varRelationIterators.push_back(relation->begin());
        }

        // iterate through the vector of iterators to find solution
        while (true) {
            bool isSolution = true;
            // the vector that contains the tuples the iterators currently points to
            std::vector<tuple> element;
            for (auto it : varRelationIterators) {
                element.push_back(*it);
            }
            // check if tuple satisifies variable equivalence
            for (auto var : nameToEquivalence) {
                if (!var.second.verify(element)) {
                    isSolution = false;
                    break;
                }
            }
            if (isSolution) {
                // check if tuple satisifies constant constraints
                isSolution = constConstraints.verify(element);
            }

            // if current tuple safisfies both equivalence and constant constraint, store variable values in
            // solutions
            if (isSolution) {
                std::vector<RamDomain> solution;
                for (auto var : nameToEquivalence) {
                    auto idx = var.second.getFirstIdx();
                    solution.push_back(element[idx.first][idx.second]);
                    // check the number of the solution
                    size_t solutionWidth;
                    if (var.second.getType() == 's') {
                        solutionWidth =
                                prog.getSymbolTable().resolve(element[idx.first][idx.second]).length();
                    } else {
                        solutionWidth = std::to_string(element[idx.first][idx.second]).length();
                    }
                    if (solutionWidth > slotWidth) {
                        slotWidth = solutionWidth;
                    }
                }
                solutions.push_back(solution);
            }
            // increment the iterators
            size_t i = varRels.size() - 1;
            bool terminate = true;
            for (auto it = varRelationIterators.rbegin(); it != varRelationIterators.rend(); ++it) {
                if ((++(*it)) != varRels[i]->end()) {
                    terminate = false;
                    break;
                } else {
                    (*it) = varRels[i]->begin();
                    --i;
                }
            }
            if (terminate) {
                break;
            }
        }
    }

    // get the solution string with given solutions, map of varaible name and their equivalence class and
    // slotWidth
    std::string getSolutionString(std::vector<std::vector<RamDomain>>& solutions,
            std::map<std::string, Equivalence> nameToEquivalence, size_t slotWidth) {
        std::string solutionString = "";
        // use '|' to delimit each variable
        solutionString += "|";
        for (auto var : nameToEquivalence) {
            std::string s(slotWidth, ' ');
            s.replace(0, var.first.length(), var.first);
            solutionString += s + "|";
        }
        solutionString += "\n";
        for (size_t i = 0; i < solutions.size(); ++i) {
            size_t j = 0;
            solutionString += "|";
            for (auto var : nameToEquivalence) {
                std::string s(slotWidth, ' ');
                if (var.second.getType() == 'i') {
                    s.replace(0, std::to_string(solutions[i][j]).length(), std::to_string(solutions[i][j]));
                } else {
                    s.replace(0, prog.getSymbolTable().resolve(solutions[i][j]).length(),
                            prog.getSymbolTable().resolve(solutions[i][j]));
                }
                solutionString += s + "|";
                ++j;
            }
            solutionString += "\n";
        }

        return solutionString;
    }

    // check if constTuple exists in relation
    bool containsTuple(Relation* relation, const std::vector<RamDomain>& constTuple) {
        bool tupleExist = false;
        for (auto it = relation->begin(); it != relation->end(); ++it) {
            bool eq = true;
            for (size_t j = 0; j < constTuple.size(); ++j) {
                if (constTuple[j] != (*it)[j]) {
                    eq = false;
                    break;
                }
            }
            if (eq) {
                tupleExist = true;
                break;
            }
        }
        return tupleExist;
    }
};

}  // end of namespace souffle
