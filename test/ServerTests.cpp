/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include "catch.hpp"

#include <MyCAD/Server.hpp>

SCENARIO("A Remote User sends a TCP/IP request to the Server and expects a response")
{
    GIVEN("an instance of MyCAD::Server")
    {
        MyCAD::Server server;

        WHEN("I instantiate MyCAD::Client")
        {
            MyCAD::Client client;
            THEN("Client should be able to ask Server a quest.")
            {

            }
        }
    }
}

SCENARIO("The server should accept and understanding various command-line arguments", "[server]")
{
    GIVEN("an instance of MyCAD::Server")
    {
        MyCAD::Server server;

        WHEN("a recognized list of command-line arguments is provided")
        {
            char* argv[] = {(char*) "MyCAD", 
                            (char*) "--version"};
            THEN("Server should let us know there were no parsing issues")
            {
                REQUIRE(server.processArgs(2, argv) == true);
            }
        }

        WHEN("an un-recognized flag is provided")
        {
            char* argv[] = {(char*) "MyCAD", 
                            (char*) "--debug"};
            THEN("Server should inform us that there was a parsing issue")
            {
                REQUIRE(server.processArgs(2, argv) == false);
            }
        }
    }
}
