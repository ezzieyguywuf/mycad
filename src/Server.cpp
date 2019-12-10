/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>

 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <MyCAD/Server.hpp>

#include "cxxopts.hpp"

namespace MyCAD
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
            ("d,debug", "Enable debugging.")
            ;
    }
}
bool Server::processArgs(int argc, char ** argv) const
{
    try{
        auto result = OPTIONS.parse(argc, argv);
    }
    catch (cxxopts::OptionParseException const& e)
    {
        std::cout << e.what() << std::endl;
        return false;
    }
    return true;
}
} // MyCAD
