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

        constexpr static const char* EXIT = "EXIT_MAIN_LOOP";
    private:
        std::string myResponse;
        std::vector<MyCAD::Shapes::Vertex> vertices;
};
} // namespace Communication
} // namespace MyCAD
#endif //MYCAD_COMMUNICATION_HEADER
