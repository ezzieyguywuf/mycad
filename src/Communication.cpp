/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Communication.hpp>

#include "cxxopts.hpp"

#include <utility> // for std::move

namespace MyCAD
{
namespace Communication
{

namespace
{
    cxxopts::Options OPTIONS("MyCAD", "A Computer Aided Design program.");
} // namespace
//=============================================================================
//                      Request Class Definition
//=============================================================================
Request::Request(std::string aRequest)
    : myRequest(std::move(aRequest))
{}

std::string const& Request::get() const
{
    return myRequest;
}

//=============================================================================
//                       Server Class Definition
//=============================================================================
Server::Server()
{
    static bool first = true;
    if (first)
    {
        first = false;
        OPTIONS.add_options()
            ("v,version", "Return the version of the MyCAD server")
            ;
    }
}
bool Server::processArgs(int argc, char ** argv) const
{
    try{
        cxxopts::ParseResult result = OPTIONS.parse(argc, argv);
        if(result.count("version") > 0)
        {
            std::cout << "MyCAD©, v" MYCAD_VERSION << std::endl;
        }
    }
    catch (cxxopts::OptionParseException const& e)
    {
        std::cout << e.what() << std::endl;
        return false;
    }
    return true;
}

std::string Server::processRequest(Request const& request) const
{
    if(request.get() == "version")
    {
        return MYCAD_VERSION;
    }
    return "";
}
} // Communication
} // MyCAD
