/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include "Commands.hpp"
#include <MyCAD/Communication.hpp>
#include <MyCAD/Exceptions.hpp>

#include <utility> // for std::move
#include <string>  // for std::getline
#include <sstream>

namespace MyCAD
{
/** This namespace describes the fundamental userspace of MyCAD. In a nutshell, MyCAD is
 *  designed from the beginning to work in a server-client model. The classes in this
 *  namespace describe that server-client relationship, as well as how to communicate
 *  between the two.
 */
namespace Communication
{

//=============================================================================
//                   Command Abstract Base Class Definition
//=============================================================================
/** A Command is an action that the Server will know how to execute. It consists of a
 *  `token`, which is a verb that describes the given Command. There are no restrictions
 *  placed upon the lexography of the `token` at this level, however a later `Register`
 *  function will likely ensure that it does not contain a space.
 */
Command::Command(std::string token, std::string help)
    : myToken(std::move(token)), myHelp(std::move(help))
{}

// Virtual descructor still needs a definition!!!
Command::~Command(){}

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

/** This is an implementation detail, but it is worth noting. We split up the
 *  externally-facing `operator()` call, which the User will utilize, from the internal
 *  `protected` `execute` in order to allow derived classes to do whatever they want while
 *  still allowing us here in the base class to check how things went and return an
 *  appropriate response to the user.
 */
std::string Command::operator()(std::string const& data, Shapes::Space& space)
{
    bool ret = this->execute(data, space);
    if(not ret)
    {
        return "There was some sort of error executing your request. You gave us \"" + data + "\"\n";
    }
    return this->getResult();
}

//=============================================================================
//                       Server Class Definition
//=============================================================================
/** A Server knows how to process various Request and do something with them. In general,
 *  this will probably mean creating/manipulating/querying topological or geometric
 *  information.
 */
Server::Server()
{}

void Server::RegisterCommand(std::unique_ptr<Command> command)
{
    if(command->token().find(' ') != std::string::npos)
    {
        throw MyCAD::Exception("A Command token can NOT contain a space.");
    }
    known_commands.emplace_back(std::move(command));
}

    known_commands.emplace(std::move(command));
}

std::string Server::processRequest(std::string const& request)
{
    // Parse out the token and the "remainder"
    std::string token, remainder;
    std::stringstream ss;
    ss << request;
    ss >> token;
    std::getline(ss, remainder);

    // Now, figure out if we have a registered Command that matches this token
    for(const auto& command : known_commands)
    {
        if(command->token() == token)
        {
            // If so, execute the command
            return command->operator()(remainder, space);
        }
    }
    return "I don't understand the command \"" + token + "\"";
}

//=============================================================================
//                      Free (global) Function Definitions
//=============================================================================

void RegisterAllCommands(Server& server)
{
    server.RegisterCommand(std::move(std::unique_ptr<Commands::Version>(new Commands::Version)));
    server.RegisterCommand(std::move(std::unique_ptr<Commands::Add>(new Commands::Add)));
}

} // Communication
} // MyCAD
