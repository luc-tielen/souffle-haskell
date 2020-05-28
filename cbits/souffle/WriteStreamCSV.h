/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file WriteStreamCSV.h
 *
 ***********************************************************************/

#pragma once

#include "IODirectives.h"
#include "ParallelUtils.h"
#include "SymbolTable.h"
#include "WriteStream.h"
#ifdef USE_LIBZ
#include "gzfstream.h"
#endif

#include <cassert>
#include <fstream>
#include <memory>
#include <ostream>
#include <string>

namespace souffle {

class WriteStreamCSV {
protected:
    virtual std::string getDelimiter(const IODirectives& ioDirectives) const {
        if (ioDirectives.has("delimiter")) {
            return ioDirectives.get("delimiter");
        }
        return "\t";
    }
};

class WriteFileCSV : public WriteStreamCSV, public WriteStream {
public:
    WriteFileCSV(const std::vector<bool>& symbolMask, const SymbolTable& symbolTable,
            const IODirectives& ioDirectives, const size_t numberOfHeights = 0, const bool provenance = false)
            : WriteStream(symbolMask, symbolTable, provenance, numberOfHeights),
              delimiter(getDelimiter(ioDirectives)),
              file(ioDirectives.getFileName(), std::ios::out | std::ios::binary) {
        if (ioDirectives.has("headers") && ioDirectives.get("headers") == "true") {
            file << ioDirectives.get("attributeNames") << std::endl;
        }
    }

    ~WriteFileCSV() override = default;

protected:
    const std::string delimiter;
    std::ofstream file;

    void writeNullary() override {
        file << "()\n";
    }

    void writeNextTuple(const RamDomain* tuple) override {
        if (symbolMask.at(0)) {
            file << symbolTable.unsafeResolve(tuple[0]);
        } else {
            file << tuple[0];
        }
        for (size_t col = 1; col < arity; ++col) {
            file << delimiter;
            if (symbolMask.at(col)) {
                file << symbolTable.unsafeResolve(tuple[col]);
            } else {
                file << tuple[col];
            }
        }
        file << "\n";
    }
};

#ifdef USE_LIBZ
class WriteGZipFileCSV : public WriteStreamCSV, public WriteStream {
public:
    WriteGZipFileCSV(const std::vector<bool>& symbolMask, const SymbolTable& symbolTable,
            const IODirectives& ioDirectives, const size_t numberOfHeights = 0, const bool provenance = false)
            : WriteStream(symbolMask, symbolTable, provenance, numberOfHeights),
              delimiter(getDelimiter(ioDirectives)),
              file(ioDirectives.getFileName(), std::ios::out | std::ios::binary) {
        if (ioDirectives.has("headers") && ioDirectives.get("headers") == "true") {
            file << ioDirectives.get("attributeNames") << std::endl;
        }
    }

    ~WriteGZipFileCSV() override = default;

protected:
    void writeNullary() override {
        file << "()\n";
    }

    void writeNextTuple(const RamDomain* tuple) override {
        if (symbolMask.at(0)) {
            file << symbolTable.unsafeResolve(tuple[0]);
        } else {
            file << tuple[0];
        }
        for (size_t col = 1; col < arity; ++col) {
            file << delimiter;
            if (symbolMask.at(col)) {
                file << symbolTable.unsafeResolve(tuple[col]);
            } else {
                file << tuple[col];
            }
        }
        file << "\n";
    }

    const std::string delimiter;
    gzfstream::ogzfstream file;
};
#endif

class WriteCoutCSV : public WriteStreamCSV, public WriteStream {
public:
    WriteCoutCSV(const std::vector<bool>& symbolMask, const SymbolTable& symbolTable,
            const IODirectives& ioDirectives, const size_t numberOfHeights = 0, const bool provenance = false)
            : WriteStream(symbolMask, symbolTable, provenance, numberOfHeights),
              delimiter(getDelimiter(ioDirectives)) {
        std::cout << "---------------\n" << ioDirectives.getRelationName();
        if (ioDirectives.has("headers") && ioDirectives.get("headers") == "true") {
            std::cout << "\n" << ioDirectives.get("attributeNames");
        }
        std::cout << "\n===============\n";
    }

    ~WriteCoutCSV() override {
        std::cout << "===============\n";
    }

protected:
    void writeNullary() override {
        std::cout << "()\n";
    }

    void writeNextTuple(const RamDomain* tuple) override {
        if (symbolMask.at(0)) {
            std::cout << symbolTable.unsafeResolve(tuple[0]);
        } else {
            std::cout << tuple[0];
        }
        for (size_t col = 1; col < arity; ++col) {
            std::cout << delimiter;
            if (symbolMask.at(col)) {
                std::cout << symbolTable.unsafeResolve(tuple[col]);
            } else {
                std::cout << tuple[col];
            }
        }
        std::cout << "\n";
    }

    const std::string delimiter;
};

class WriteCoutPrintSize : public WriteStream {
public:
    WriteCoutPrintSize(const IODirectives& ioDirectives)
            : WriteStream({}, {}, false, 1, true), lease(souffle::getOutputLock().acquire()) {
        std::cout << ioDirectives.getRelationName() << "\t";
    }

    ~WriteCoutPrintSize() override = default;

protected:
    void writeNullary() override {
        assert(false && "attempting to iterate over a print size operation");
    }

    void writeNextTuple(const RamDomain* /* tuple */) override {
        assert(false && "attempting to iterate over a print size operation");
    }

    void writeSize(std::size_t size) override {
        std::cout << size << "\n";
    }

    Lock::Lease lease;
};

class WriteFileCSVFactory : public WriteStreamFactory {
public:
    std::unique_ptr<WriteStream> getWriter(const std::vector<bool>& symbolMask,
            const SymbolTable& symbolTable, const IODirectives& ioDirectives, const bool provenance,
            const size_t numberOfHeights) override {
#ifdef USE_LIBZ
        if (ioDirectives.has("compress")) {
            return std::make_unique<WriteGZipFileCSV>(
                    symbolMask, symbolTable, ioDirectives, numberOfHeights, provenance);
        }
#endif
        return std::make_unique<WriteFileCSV>(
                symbolMask, symbolTable, ioDirectives, numberOfHeights, provenance);
    }
    const std::string& getName() const override {
        static const std::string name = "file";
        return name;
    }
    ~WriteFileCSVFactory() override = default;
};

class WriteCoutCSVFactory : public WriteStreamFactory {
public:
    std::unique_ptr<WriteStream> getWriter(const std::vector<bool>& symbolMask,
            const SymbolTable& symbolTable, const IODirectives& ioDirectives, const bool provenance,
            const size_t numberOfHeights) override {
        return std::make_unique<WriteCoutCSV>(
                symbolMask, symbolTable, ioDirectives, numberOfHeights, provenance);
    }
    const std::string& getName() const override {
        static const std::string name = "stdout";
        return name;
    }
    ~WriteCoutCSVFactory() override = default;
};

class WriteCoutPrintSizeFactory : public WriteStreamFactory {
public:
    std::unique_ptr<WriteStream> getWriter(const std::vector<bool>& /* symbolMask */,
            const SymbolTable& /* symbolTable */, const IODirectives& ioDirectives,
            const bool /* provenance */, const size_t /* numberOfHeights */) override {
        return std::make_unique<WriteCoutPrintSize>(ioDirectives);
    }
    const std::string& getName() const override {
        static const std::string name = "stdoutprintsize";
        return name;
    }
    ~WriteCoutPrintSizeFactory() override = default;
};

} /* namespace souffle */
