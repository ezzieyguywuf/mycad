/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Exceptions.hpp>
#include <MyCAD/Geometry.hpp>
#include <MyCAD/Shapes.hpp>

#include "Commands.hpp"

#include <set>
#include <memory>  // for std::unique_ptr
#include <utility> // for std::move
#include <iostream>

namespace MyCAD
{
/** Please note that this namespace is an implementation detail. Typical users and
 *  developers do not need to know or care about it. Indeed, based on how it is
 *  implemented (note that the header is not distributed), the only place that this
 *  namespace can be accessed from is the main.cpp file - nobody else can access these
 *  methods/functions (I think).
 *
 *  Nonetheless, in keeping with the mission of this project to provide clear and
 *  meaningful documentation, we will document these classes and allow them to be
 *  published in the doxygen documentation. This way, it will become easier to maintain
 *  this portion of the code base.
 */
namespace Commands
{

//=============================================================================
//                          The Version Command
//=============================================================================
Version::Version()
    : Communication::Command("version", "Returns the version of the running MyCAD_Server")
{}

std::string Version::execute(std::string const& data, Shapes::Space& /*space*/)
{
    if (not data.empty())
    {
        std::clog << "ignored the following input from the user: \"";
        std::clog << data << "\"" << std::endl;
    }

    return "MyCAD©, v" MYCAD_VERSION;
}

//=============================================================================
//                               The Add Command
//=============================================================================
Add::Add()
    : Communication::Command("add", "Allows users to add various topological entities to....space")
{
    known_commands.emplace(std::move(std::unique_ptr<Commands::AddVertex>(new AddVertex)));
}

std::string Add::execute(std::string const& data, Shapes::Space& space)
{
    // Parse out the token and the "remainder"
    std::string token, remainder;
    std::stringstream ss;
    ss << data;
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

    return "[add] Unknown sub-command \"" + token + "\"";
}
//=============================================================================
//                               The AddVertex Command
//=============================================================================
AddVertex::AddVertex()
    : Communication::Command("vertex", "Add a vertex at the given x-y location")
{}


std::string AddVertex::execute(std::string const& data, Shapes::Space& space)
{
    // Extract the user's targets
    std::stringstream ss;
    ss << data;
    MyCAD::Geometry::Number x,y;
    ss >> x >> y;

    // Check if there's any trailing data
    std::string remainder;
    std::getline(ss, remainder);

    // Create the vertex
    MyCAD::Geometry::Point point(x, y);
    space.addVertex(MyCAD::Shapes::Vertex(point));

    // Let the user know everything went well.
    std::stringstream oss;
    oss << "Added vertex at " << space.getVertices().back();
    if(not remainder.empty())
    {
        oss << ". Ignored trailing data \"" << remainder << "\"" << std::endl;
    }
    return oss.str();
}
} // namespace Commands
} // namespace MyCAD
