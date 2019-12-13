/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include "catch.hpp"

#include <MyCAD/Communication.hpp>

SCENARIO("A Remote User sends a TCP/IP request to the Server and expects a response")
{
    GIVEN("an instance of MyCAD::Server")
    {
        MyCAD::Communication::Server server;

        WHEN("A request for the version is received")
        {
            std::string request("version");
            THEN("We should get the expected result")
            {
                REQUIRE(server.processRequest(request) == "MyCAD©, v" MYCAD_VERSION);
            }
        }

        WHEN("A request to create a Vertex in 2D-space is recieved")
        {
            server.processRequest("add vertex 10 20");
            THEN("We should end up with a vertex in that location")
            {
                REQUIRE_FALSE(server.processRequest("list") == "(10,20)");
            }
        }
    }
}
