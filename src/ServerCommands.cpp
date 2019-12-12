/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include "ServerCommands.hpp"

#include <set>
#include <memory> // for std::unique_ptr

namespace MyCAD
{
namespace Server
{
namespace Commands
{
namespace
{
    // Will be used to store a set of registered commands.
    std::set<std::unique_ptr<Command>> KNOWN_COMMANDS;
} // namespace

//=============================================================================
//                     Commands Class Definition
//=============================================================================
/** A Command is an action that the Server will know how to execute. It consists of a
 *  `token`, which is a verb that describes the given Command. There are no restrictions
 *  placed upon the lexography of the `token` at this level, however a later `Register`
 *  function will likely ensure that it does not contain a space.
 */
Command::Command(std::string token, std::string help="")
    : myToken(std::move(token)), myHelp(std::move(help))
{}

// Virtual descructor still needs a definition!!!
virtual ~Command()=0;

std::string const& Command::token() const
{
    return myToken;
}

void Command::getHelp() const
{
    if(myHelp.empty())
    {
        std::stringstream out;
        out << "The command \"" << myToken << "\"";
        out << " does not have any help documentation.";
    }
}
} // namespace Commands
} // namespace Server
} // namespace MyCAD
