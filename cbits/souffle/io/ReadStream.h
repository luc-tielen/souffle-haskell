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

#include "souffle/RamTypes.h"
#include "souffle/RecordTable.h"
#include "souffle/SymbolTable.h"
#include "souffle/io/SerialisationStream.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StringUtil.h"
#include "souffle/utility/json11.h"
#include <cctype>
#include <cstddef>
#include <map>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace souffle {

class ReadStream : public SerialisationStream<false> {
protected:
    ReadStream(
            const std::map<std::string, std::string>& rwOperation, SymbolTable& symTab, RecordTable& recTab)
            : SerialisationStream(symTab, recTab, rwOperation) {}

public:
    template <typename T>
    void readAll(T& relation) {
        auto lease = symbolTable.acquireLock();
        (void)lease;
        while (const auto next = readNextTuple()) {
            const RamDomain* ramDomain = next.get();
            relation.insert(ramDomain);
        }
    }

protected:
    /**
     * Read a record from a string.
     *
     * @param source - string containing a record
     * @param recordTypeName - record type.
     * @parem pos - start parsing from this position.
     * @param consumed - if not nullptr: number of characters read.
     *
     */
    RamDomain readRecord(const std::string& source, const std::string& recordTypeName, size_t pos = 0,
            size_t* charactersRead = nullptr) {
        const size_t initial_position = pos;

        // Check if record type information are present
        auto&& recordInfo = types["records"][recordTypeName];
        if (recordInfo.is_null()) {
            throw std::invalid_argument("Missing record type information: " + recordTypeName);
        }

        // Handle nil case
        consumeWhiteSpace(source, pos);
        if (source.substr(pos, 3) == "nil") {
            if (charactersRead != nullptr) {
                *charactersRead = 3;
            }
            return 0;
        }

        auto&& recordTypes = recordInfo["types"];
        const size_t recordArity = recordInfo["arity"].long_value();

        std::vector<RamDomain> recordValues(recordArity);

        consumeChar(source, '[', pos);

        for (size_t i = 0; i < recordArity; ++i) {
            const std::string& recordType = recordTypes[i].string_value();
            size_t consumed = 0;

            if (i > 0) {
                consumeChar(source, ',', pos);
            }
            consumeWhiteSpace(source, pos);
            switch (recordType[0]) {
                case 's': {
                    recordValues[i] = symbolTable.unsafeLookup(readUntil(source, ",]", pos, &consumed));
                    break;
                }
                case 'i': {
                    recordValues[i] = RamSignedFromString(source.substr(pos), &consumed);
                    break;
                }
                case 'u': {
                    recordValues[i] = ramBitCast(RamUnsignedFromString(source.substr(pos), &consumed));
                    break;
                }
                case 'f': {
                    recordValues[i] = ramBitCast(RamFloatFromString(source.substr(pos), &consumed));
                    break;
                }
                case 'r': {
                    recordValues[i] = readRecord(source, recordType, pos, &consumed);
                    break;
                }
                case '+': {
                    recordValues[i] = readADT(source, recordType, pos, &consumed);
                    break;
                }
                default: fatal("Invalid type attribute");
            }
            pos += consumed;
        }
        consumeChar(source, ']', pos);

        if (charactersRead != nullptr) {
            *charactersRead = pos - initial_position;
        }

        return recordTable.pack(recordValues.data(), recordValues.size());
    }

    RamDomain readADT(const std::string& source, const std::string& adtName, size_t pos = 0,
            size_t* charactersRead = nullptr) {
        const size_t initial_position = pos;

        // Branch will are encoded as [branchIdx, [branchValues...]].
        RamDomain branchIdx = -1;

        auto&& adtInfo = types["ADTs"][adtName];
        const auto& branches = adtInfo["branches"];

        if (adtInfo.is_null() || !branches.is_array()) {
            throw std::invalid_argument("Missing ADT information: " + adtName);
        }

        // Consume initial character
        consumeChar(source, '$', pos);
        std::string constructor = readAlphanumeric(source, pos);

        json11::Json branchInfo = [&]() -> json11::Json {
            for (auto branch : branches.array_items()) {
                ++branchIdx;
                if (branch["name"].string_value() == constructor) {
                    return branch;
                }
            }

            throw std::invalid_argument("Missing branch information: " + constructor);
        }();

        assert(branchInfo["types"].is_array());
        auto branchTypes = branchInfo["types"].array_items();

        // Handle a branch without arguments.
        if (branchTypes.empty()) {
            if (charactersRead != nullptr) {
                *charactersRead = pos - initial_position;
            }
            RamDomain emptyArgs = recordTable.pack(toVector<RamDomain>().data(), 0);
            return recordTable.pack(toVector<RamDomain>(branchIdx, emptyArgs).data(), 2);
        }

        consumeChar(source, '(', pos);

        std::vector<RamDomain> branchArgs(branchTypes.size());

        for (size_t i = 0; i < branchTypes.size(); ++i) {
            auto argType = branchTypes[i].string_value();
            assert(!argType.empty());

            size_t consumed = 0;

            if (i > 0) {
                consumeChar(source, ',', pos);
            }
            consumeWhiteSpace(source, pos);

            switch (argType[0]) {
                case 's': {
                    branchArgs[i] = symbolTable.unsafeLookup(readUntil(source, ",)", pos, &consumed));
                    break;
                }
                case 'i': {
                    branchArgs[i] = RamSignedFromString(source.substr(pos), &consumed);
                    break;
                }
                case 'u': {
                    branchArgs[i] = ramBitCast(RamUnsignedFromString(source.substr(pos), &consumed));
                    break;
                }
                case 'f': {
                    branchArgs[i] = ramBitCast(RamFloatFromString(source.substr(pos), &consumed));
                    break;
                }
                case 'r': {
                    branchArgs[i] = readRecord(source, argType, pos, &consumed);
                    break;
                }
                case '+': {
                    branchArgs[i] = readADT(source, argType, pos, &consumed);
                    break;
                }
                default: fatal("Invalid type attribute");
            }
            pos += consumed;
        }

        consumeChar(source, ')', pos);

        if (charactersRead != nullptr) {
            *charactersRead = pos - initial_position;
        }

        // Store branch either as [branch_id, [arguments]] or [branch_id, argument].
        RamDomain branchValue = [&]() -> RamDomain {
            if (branchArgs.size() != 1) {
                return recordTable.pack(branchArgs.data(), branchArgs.size());
            } else {
                return branchArgs[0];
            }
        }();

        return recordTable.pack(toVector<RamDomain>(branchIdx, branchValue).data(), 2);
    }

    /**
     * Read the next alphanumeric sequence (corresponding to IDENT).
     * Consume preceding whitespace.
     * TODO (darth_tytus): use std::string_view?
     */
    std::string readAlphanumeric(const std::string& source, size_t& pos) {
        consumeWhiteSpace(source, pos);
        if (pos >= source.length()) {
            throw std::invalid_argument("Unexpected end of input");
        }

        const size_t bgn = pos;
        while (pos < source.length() && std::isalnum(static_cast<unsigned char>(source[pos]))) {
            ++pos;
        }

        return source.substr(bgn, pos - bgn);
    }

    std::string readUntil(const std::string& source, const std::string stopChars, const size_t pos,
            size_t* charactersRead) {
        size_t endOfSymbol = source.find_first_of(stopChars, pos);

        if (endOfSymbol == std::string::npos) {
            throw std::invalid_argument("Unexpected end of input");
        }

        *charactersRead = endOfSymbol - pos;

        return source.substr(pos, *charactersRead);
    }

    /**
     * Read past given character, consuming any preceding whitespace.
     */
    void consumeChar(const std::string& str, char c, size_t& pos) {
        consumeWhiteSpace(str, pos);
        if (pos >= str.length()) {
            throw std::invalid_argument("Unexpected end of input");
        }
        if (str[pos] != c) {
            std::stringstream error;
            error << "Expected: \'" << c << "\', got: " << str[pos];
            throw std::invalid_argument(error.str());
        }
        ++pos;
    }

    /**
     * Advance position in the string until first non-whitespace character.
     */
    void consumeWhiteSpace(const std::string& str, size_t& pos) {
        while (pos < str.length() && std::isspace(static_cast<unsigned char>(str[pos]))) {
            ++pos;
        }
    }

    virtual Own<RamDomain[]> readNextTuple() = 0;
};

class ReadStreamFactory {
public:
    virtual Own<ReadStream> getReader(
            const std::map<std::string, std::string>&, SymbolTable&, RecordTable&) = 0;
    virtual const std::string& getName() const = 0;
    virtual ~ReadStreamFactory() = default;
};

} /* namespace souffle */
