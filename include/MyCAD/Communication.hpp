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

/** A Client knows how to generate Request, send them to Server, and query for a response */
class Client
{

};

/** A Request describes an action or a query that we have for the MyCAD Server */
class Request
{
    public:
        Request(std::string request);
        std::string const& get() const;

    private:
        std::string myRequest;
};

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

        /** @brief This will be called in order to execute the given Command
         *  @param data All the information the user passed after the token.
         *  @param server The instance of Space against which to execute the command. The
         *                Command can and probably will modify Space in some way.
         */
        virtual std::string execute(std::string const& data, Shapes::Space& space) const;

    private:
        std::string myToken;
        std::string myHelp;
};

/** A Server knows how to process a Request and do something with it */
class Server
{
    public:
        Server();

        /** @brief Process command-line arguments */
        bool processArgs(int argc, char ** argv);

        /** @brief Process a Request
         *  @returns true on success
         *  @returns false on error
         */
        bool processRequest(Request const& request);

        /** @brief Returns the result of processing the last Request
         *  @returns an empty std::string if there was an error
         *  @returns the result of the last Request
         */
        std::string getResponse() const;

        /** @brief Used to register commands that Server understands */
        void RegisterCommand(std::unique_ptr<Command> command);

        constexpr static const char* EXIT = "EXIT_MAIN_LOOP";
    private:
        // Will be used to store a set of registered commands.
        std::set<std::unique_ptr<Command>> known_commands;

        // Will store our....world
        MyCAD::Shapes::Space space;
};
} // namespace Communication
} // namespace MyCAD
#endif //MYCAD_COMMUNICATION_HEADER
