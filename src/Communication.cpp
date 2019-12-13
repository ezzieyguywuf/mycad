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

namespace
{
    // Will be used to store a set of known commands.
    std::set<std::unique_ptr<Command>> KNOWN_COMMANDS;
    void initializeCommands()
    {
            KNOWN_COMMANDS.emplace(std::move(std::unique_ptr<Commands::Version>(new Commands::Version)));
            KNOWN_COMMANDS.emplace(std::move(std::unique_ptr<Commands::Add>(new Commands::Add)));
            KNOWN_COMMANDS.emplace(std::move(std::unique_ptr<Commands::ListVertices>(new Commands::ListVertices)));
    }

} // namespace

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
{
    if(this->token().find(' ') != std::string::npos)
    {
        throw MyCAD::Exception("A Command token can NOT contain a space.");
    }
}

// Virtual descructor still needs a definition!!!
Command::~Command(){}

std::string const& Command::token() const
{
    return myToken;
}

std::string Command::getHelp() const
{
    if(myHelp.empty())
    {
        std::stringstream out;
        out << "The command \"" << myToken << "\"";
        out << " does not have any help documentation.";
        return out.str();
    }
    return myHelp;
}

/** This is an implementation detail, but it is worth noting. We split up the
 *  externally-facing `operator()` call, which the User will utilize, from the internal
 *  `protected` `execute` in order to allow derived classes to do whatever they want while
 *  still allowing us here in the base class to check how things went and return an
 *  appropriate response to the user.
 */
std::string Command::operator()(std::string const& data, Shapes::Space& space)
{
    return this->execute(data, space);
}

//=============================================================================
//                       Server Class Definition
//=============================================================================
Server::Server()
{
    if(KNOWN_COMMANDS.empty())
    {
        initializeCommands();
    }
}
std::string Server::processRequest(std::string const& request)
{
    // Parse out the token and the "remainder"
    std::string token, remainder;
    std::stringstream ss;
    ss << request;
    ss >> token;
    std::getline(ss, remainder);

    // First, check if the user wants to shutdown
    if (token == "quit")
    {
        readyToGoToSleep = true;
        return "Shutting down...";
    }

    // Clear out the stringstream so we can use it to build up the help info if needed.
    ss.clear();
    ss.str("");
    // Now, figure out if we have a registered Command that matches this token
    for(const auto& command : KNOWN_COMMANDS)
    {
        // If they want help, we just need to gather the appropriate data.
        if(token == "help")
        {
            ss << "[" << command->token() << "]: ";
            ss << command->getHelp() << std::endl;
        }
        else
        {
            // If they didn't ask for help, does the token requested match one of the ones
            // that are registered?
            if(command->token() == token)
            {
                // If so, execute the command
                return command->operator()(remainder, space);
            }
        }
    }

    if(not ss.str().empty())
    {
        return ss.str();
    }

    return "I don't understand the command \"" + token + "\"";
}

bool Server::shutdown() const
{
    return readyToGoToSleep;
}
} // Communication
} // MyCAD
