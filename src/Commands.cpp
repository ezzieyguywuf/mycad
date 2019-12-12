/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Exceptions.hpp>
#include <MyCAD/Geometry.hpp>
#include <MyCAD/Shapes.hpp>

#include "ServerCommands.hpp"

#include <set>
#include <memory>  // for std::unique_ptr
#include <utility> // for std::move
#include <iostream>

namespace MyCAD
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

void RegisterAllCommands()
{
    RegisterCommand(std::move(std::unique_ptr<Command>(new Version)));
    RegisterCommand(std::move(std::unique_ptr<Add>(new Add)));
}

//=============================================================================
//                          The Version Command
//=============================================================================
Version::Version()
    : Command("version", "Returns the version of the running MyCAD_Server")
{}

void Version::execute(std::string const& data) const
{
    std::cout << "MyCAD©, v" MYCAD_VERSION << std::endl;

    if (not data.empty())
    {
        std::clog << "ignored the following input from the user: \"";
        std::clog << data << "\"" << std::endl;
    }
}

//=============================================================================
//                               The Add Command
//=============================================================================
Add::Add()
    : Command("add", "Allows users to add various topological entities to....space")
{}

void Add::execute(std::string const& data) const
{
    // Extract the user's targets
    std::stringstream ss;
    ss << data;
    MyCAD::Geometry::Number x,y;
    ss >> x >> y;

    // Check if there's any trailing data
    std::string remainder;
    std::getline(ss, remainder);
    if(not remainder.empty())
    {
        std::clog << "Ignoring trailing data \"" << remainder << "\"" << std::endl;
    }

    // Create the vertex
    VERTICES.emplace_back(MyCAD::Geometry::Point(x, y));

    // Let the user know everything went well.
    std::stringstream oss;
    oss << "Added vertex at " << VERTICES.back();
}

} // namespace Commands
} // namespace MyCAD
