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

#include <MyCAD/Shapes.hpp>

SCENARIO("Basic topological entities wrap geometric constructs")
{
    GIVEN("A Point")
    {
        MyCAD::Geometry::Point pnt(1,2,3);
        WHEN("a Vertex is created with it")
        {
            MyCAD::Shapes::Vertex vert(pnt);
            THEN("we should be able to get that same point back.")
            {
                REQUIRE(pnt == vert.point());
            }
        }
    }
    //Given("Two points")
    //{
        //MyCAD::Geometry::Point p1(0,0,0);
        //MyCAD::Geometry::Point p2(10,10,10);
        //WHEN("an Edge is created with them")
        //{
            //MyCAD::Shapes::Edge edge(p1, p2);
            //THEN("a straight Line should be inferred and created between the two points")
            //{
                //MyCAD::Geometry::Line line(p1, p2);
                //REQUIRE(edge.Line(), line);
            //}
        //}
    //}
}

SCENARIO("CAD Programs can create Primitive Solids")
{
    GIVEN("A Box")
    {
        MyCAD::Shapes::Box box(10, 10, 10);
        WHEN("the Faces are retrieved")
        {
            const std::vector<MyCAD::Shapes::Face>& faces = box.getFaces();
            THEN("there should be only 6 faces")
            {
                REQUIRE(faces.size() == 6);
            }
        }
    }
}
