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

SCENARIO("CAD Programs require cartesian geometry", "[Geometry]")
{
    GIVEN("a Point")
    {
        float x = 1;
        float y = 2;
        float z = 3;
        MyCAD::Geometry::Point pnt(x, y, z);

        WHEN("the any cartesian value is requested")
        {
            THEN("it should equal the value used to construct it")
            {
                REQUIRE(pnt.x() == x);
                REQUIRE(pnt.y() == y);
                REQUIRE(pnt.z() == z);
            }
        }
    }
    GIVEN("two Points")
    {
        MyCAD::Geometry::Point p1(0, 0, 0);
        MyCAD::Geometry::Point p2(10, 10, 10);
        WHEN("we make a Line between them")
        {
            MyCAD::Geometry::Line line(p1, p2);
            THEN("it should get parametrized")
            {
                REQUIRE(line.getLowerParameter() == 0);
                REQUIRE(line.getUpperParameter() == 10);
            }
            THEN("two Lines created with them should be equal")
            {
                MyCAD::Geometry::Line line2(p1, p2);
                REQUIRE(line == line2);
            }
        }
    }
}

