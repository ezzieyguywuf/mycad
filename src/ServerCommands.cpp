/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Exception.hpp>
#include "ServerCommands.hpp"

#include <set>
#include <memory> // for std::unique_ptr

namespace MyCAD
{
namespace Server
{
/** Please note that this namespace is an implementation detail. Typical users and
 *  developers do not need to know or care about it. Indeed, based on how it is
 *  implemented (note that the header is not distributed), the only place that this
 *  namespace can be accessed from is the Server.cpp file - nobody else can access these
 *  methods/functions (I think).
 *
 *  Nonetheless, in keeping with the mission of this project to provide clear and
 *  meaningful documentation, we will document these classes and allow them to be
 *  published in the doxygen documentation. This way, it will become easier to maintain
 *  this portion of the code base.
 */
namespace Commands
{
namespace
{
    // Will be used to store a set of registered commands.
    std::set<std::unique_ptr<Command>> KNOWN_COMMANDS;
} // namespace

void RegisterCommand(std::unique_ptr<Command> command)
{
    if(command->token().find(' ') != std::npos)
    {
        throw MyCAD::Exception("A Command token can NOT contain a space.");
    }
}

/** Note to developers: you can and should adjust the lines in this method whenever you
 *  are defining a new command. Further, you can comment out or delete lines that contain
 *  commands that you wish to temporarily (or permanentl!) make Server forget about.
 */
void RegisterAllCommands()
{}

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
