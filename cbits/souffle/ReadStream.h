/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ReadStream.h
 *
 ***********************************************************************/

#pragma once

#include "IODirectives.h"
#include "RamTypes.h"
#include "SymbolTable.h"

#include <memory>
#include <string>
#include <vector>

namespace souffle {

class ReadStream {
public:
    ReadStream(const std::vector<bool>& symbolMask, SymbolTable& symbolTable, const bool prov,
            const size_t numberOfHeights)
            : symbolMask(symbolMask), symbolTable(symbolTable), isProvenance(prov),
              arity(static_cast<uint8_t>(symbolMask.size() - (prov ? (numberOfHeights + 1) : 0))),
              numberOfHeights(numberOfHeights) {}
    template <typename T>
    void readAll(T& relation) {
        auto lease = symbolTable.acquireLock();
        (void)lease;
        while (const auto next = readNextTuple()) {
            const RamDomain* ramDomain = next.get();
            relation.insert(ramDomain);
        }
    }

    virtual ~ReadStream() = default;

protected:
    virtual std::unique_ptr<RamDomain[]> readNextTuple() = 0;
    const std::vector<bool>& symbolMask;
    SymbolTable& symbolTable;
    const bool isProvenance;
    const uint8_t arity;
    const size_t numberOfHeights;
};

class ReadStreamFactory {
public:
    virtual std::unique_ptr<ReadStream> getReader(const std::vector<bool>& symbolMask,
            SymbolTable& symbolTable, const IODirectives& ioDirectives, const bool provenance,
            const size_t number) = 0;
    virtual const std::string& getName() const = 0;
    virtual ~ReadStreamFactory() = default;
};

} /* namespace souffle */
