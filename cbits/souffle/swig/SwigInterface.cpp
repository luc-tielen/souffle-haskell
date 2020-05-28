/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SwigInterface.cpp
 *
 * Defines the function to create an instance of a SouffleProgram through SWIG
 *
 ***********************************************************************/

#include "SwigInterface.h"

/**
 * Enables an instance of name to be created
 */
SWIGSouffleProgram* newInstance(const std::string& name) {
    souffle::SouffleProgram* prog = souffle::ProgramFactory::newInstance(name);
    SWIGSouffleProgram* p = new SWIGSouffleProgram(prog);
    return p;
}
