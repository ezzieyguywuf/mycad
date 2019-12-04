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

#include <MyCAD/Geometry.hpp>

SCENARIO("CAD Programs require cartesian geometry")
{
    GIVEN("A point")
    {
        float x = 1;
        float y = 2;
        float z = 3;
        MyCAD::Geometry::Point pnt(x, y, z);

        WHEN("the X value is retrieved")
        {
            THEN("it should equal the constructed value")
            {
                REQUIRE(pnt.x() == x);
            }
        }

        WHEN("the Y value is retrieved")
        {
            THEN("it should equal the constructed value")
            {
                REQUIRE(pnt.y() == y);
            }
        }

        WHEN("the Z value is retrieved")
        {
            THEN("it should equal the constructed value")
            {
                REQUIRE(pnt.z() == z);
            }
        }
    }
}
