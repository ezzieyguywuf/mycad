/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#ifndef MYCAD_COMMUNICATION_HEADER
#define MYCAD_COMMUNICATION_HEADER

#include <string>

namespace MyCAD
{
namespace Communication
{
class Client
{

};

class Request
{
    public:
        Request(std::string request);
        std::string const& get() const;

    private:
        std::string myRequest;
};

class Server
{
    public:
        Server();

        bool processArgs(int argc, char ** argv) const;
        int processRequest(Request const& request) const;

        /** Returns the result of processing the last Request
         *  @returns an empty std::string if there was an error
         *  @returns the result of the last Request
         */
        std::string getResponse() const;
};
} // namespace Communication
} // namespace MyCAD
#endif //MYCAD_COMMUNICATION_HEADER
