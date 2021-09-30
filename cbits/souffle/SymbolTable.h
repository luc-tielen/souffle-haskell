/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, 2015 Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SymbolTable.h
 *
 * Encodes/decodes symbols to numbers (and vice versa).
 *
 ***********************************************************************/

#pragma once

#include "souffle/RamTypes.h"
#include "souffle/datastructure/ConcurrentFlyweight.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/ParallelUtil.h"
#include "souffle/utility/StreamUtil.h"
#include <algorithm>
#include <cstdlib>
#include <deque>
#include <initializer_list>
#include <iostream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace souffle {

/**
 * @class SymbolTable
 *
 * SymbolTable encodes symbols to numbers and decodes numbers to symbols.
 */
class SymbolTable : protected FlyweightImpl<std::string> {
private:
    using Base = FlyweightImpl<std::string>;

public:
    using iterator = typename Base::iterator;

    /** @brief Construct a symbol table with the given number of concurrent access lanes. */
    SymbolTable(const std::size_t LaneCount = 1) : Base(LaneCount) {}

    /** @brief Construct a symbol table with the given initial symbols. */
    SymbolTable(std::initializer_list<std::string> symbols) : Base(1, symbols.size()) {
        for (const auto& symbol : symbols) {
            findOrInsert(symbol);
        }
    }

    /** @brief Construct a symbol table with the given number of concurrent access lanes and initial symbols.
     */
    SymbolTable(const std::size_t LaneCount, std::initializer_list<std::string> symbols)
            : Base(LaneCount, symbols.size()) {
        for (const auto& symbol : symbols) {
            findOrInsert(symbol);
        }
    }

    /**
     * @brief Set the number of concurrent access lanes.
     * This function is not thread-safe, do not call when other threads are using the datastructure.
     */
    void setNumLanes(const std::size_t NumLanes) {
        Base::setNumLanes(NumLanes);
    }

    /** @brief Return an iterator on the first symbol. */
    iterator begin() const {
        return Base::begin();
    }

    /** @brief Return an iterator past the last symbol. */
    iterator end() const {
        return Base::end();
    }

    /** @brief Check if the given symbol exist. */
    bool weakContains(const std::string& symbol) const {
        return Base::weakContains(symbol);
    }

    /** @brief Encode a symbol to a symbol index. */
    RamDomain encode(const std::string& symbol) {
        return Base::findOrInsert(symbol).first;
    }

    /** @brief Decode a symbol index to a symbol. */
    const std::string& decode(const RamDomain index) const {
        return Base::fetch(index);
    }

    /** @brief Encode a symbol to a symbol index; aliases encode. */
    RamDomain unsafeEncode(const std::string& symbol) {
        return encode(symbol);
    }

    /** @brief Decode a symbol index to a symbol; aliases decode. */
    const std::string& unsafeDecode(const RamDomain index) const {
        return decode(index);
    }

    /**
     * @brief Encode the symbol, it is inserted if it does not exist.
     *
     * @return the symbol index and a boolean indicating if an insertion
     * happened.
     */
    std::pair<RamDomain, bool> findOrInsert(const std::string& symbol) {
        auto Res = Base::findOrInsert(symbol);
        return std::make_pair(Res.first, Res.second);
    }
};

}  // namespace souffle
