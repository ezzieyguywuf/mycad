/* 
 * Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>

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
#include "catch.hpp"

SCENARIO("The server should accept and understanding various command-line arguments", "[server]")
{
    GIVEN("an instance of MyCAD::Server")
    {
        MyCAD::Server server;
        WHEN("a recognized list of command-line arguments is provided")
        {
            char * argv[] = {"MyCAD_Tests", "--version"};
            THEN("Server should let us know there were no parsing issues")
            {
                REQUIRE(server.processArgs(2, argv) == true);
            }
        }
        WHEN("an un-recognized flag is provided")

        {
            char * argv[] = {"MyCAD_Tests", "--debug"};
            THEN("Server should inform us that there was a parsing issue")
            {
                REQUIRE(server.processArgs(2, argv) == false);
            }
        }
    }
}
