/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#ifndef MYCAD_COMMUNICATION_HEADER
#define MYCAD_COMMUNICATION_HEADER

#include <MyCAD/Shapes.hpp>

#include <string>
#include <vector>

namespace MyCAD
{
/** @brief The fundamental user-space utilities of MyCAD */
namespace Communication
{

/** @brief A Command is an action that Server knows how to perform */
class Command
{
    public:
        /** @brief token CANNOT contain a space
         *  @param token a string which represents this Command
         *  @param help  documentation for the Command.
         */
        Command(std::string token, std::string help="");
        virtual ~Command()=0;

        /** @brief returns the identifying token for this Command */
        std::string const& token() const;

        /** @brief returns the help documentation */
        void getHelp() const;

        /** @brief execute the Command
         *  @param data All the information the user passed after the token.
         *  @param server The instance of Space against which to execute the command. The
         *                Command can and probably will modify Space in some way.
         *  @returns A message from the Command letting you know how things went
         */
        std::string operator()(std::string const& data, Shapes::Space& space);

    protected:
        /** @brief This will be called in order to execute the given Command
         *  @param data All the information the user passed after the token.
         *  @param server The instance of Space against which to execute the command. The
         *                Command can and probably will modify Space in some way.
         *  @returns A message from the execute call. Derived classes should use this to
         *           communicate with the User, for example, whethe or not the command
         *           succeeded
         */
        virtual std::string execute(std::string const& data, Shapes::Space& space) = 0;

    private:
        std::string myToken;
        std::string myHelp;
};

/** A Server knows how to process a Request and do something with it */
class Server
{
    public:
        Server();

        /** @brief Process a Request
         *  @returns Some sort of message saying how things went
         */
        std::string processRequest(std::string const& request);

        /** @brief Used to register commands that Server understands */
        void RegisterCommand(std::unique_ptr<Command> command);

    private:
        // Will be used to store a set of registered commands.
        std::set<std::unique_ptr<Command>> known_commands;

        // Will store our....world
        MyCAD::Shapes::Space space;
};

/** @brief This can be called to register all the commands at runtime */
void RegisterAllCommands(Server& server);

} // namespace Communication
} // namespace MyCAD
#endif //MYCAD_COMMUNICATION_HEADER
