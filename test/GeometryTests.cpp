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
        double x = 1;
        double y = 2;
        MyCAD::Geometry::Point pnt(x, y);

        WHEN("the any cartesian value is requested")
        {
            THEN("it should equal the value used to construct it")
            {
                Point_2 check(x, y);
                REQUIRE(pnt.x() == check.x());
                REQUIRE(pnt.y() == check.y());
            }
        }
    }
    //GIVEN("two Points")
    //{
        //MyCAD::Geometry::Point p1(0, 0);
        //MyCAD::Geometry::Point p2(10, 10);
        //WHEN("we make a LineSegment between them")
        //{
            //MyCAD::Geometry::LineSegment line(p1, p2);
            //THEN("we should be able to retrieve the begining and ending points")
            //{
                //REQUIRE(line.start() == p1);
                //REQUIRE(line.end() == p2);
            //}
            //THEN("two Lines created with them should be equal")
            //{
                //MyCAD::Geometry::Line line2(p1, p2);
                //REQUIRE(line == line2);
            //}
        //}
    //}
}

