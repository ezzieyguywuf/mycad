/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Communication.hpp>

#include "cxxopts.hpp"

namespace MyCAD
{
namespace Communication
{

namespace
{
    cxxopts::Options OPTIONS("MyCAD", "A Computer Aided Design program.");
} // namespace

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
} // Communication
} // MyCAD
