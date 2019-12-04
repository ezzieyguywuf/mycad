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

using Catch::Matchers::UnorderedEquals;

SCENARIO("Basic topological entities wrap geometric constructs", "[Shapes]")
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

    GIVEN("a line")
    {
        MyCAD::Geometry::Point p1(0,0,0);
        MyCAD::Geometry::Point p2(10,10,10);
        MyCAD::Geometry::Line line(p1, p2);
        WHEN("an Edge is created with it")
        {
            MyCAD::Shapes::Edge edge(line);
            THEN("we should be able to retrive the Line")
            {
                REQUIRE(edge.getLine() == line);
            }
        }
    }

    GIVEN("two lines with one common Point")
    {
        MyCAD::Geometry::Point p1(0,0,0);
        MyCAD::Geometry::Point p2(10,10,0);
        MyCAD::Geometry::Point p3(0,10,10);

        MyCAD::Geometry::Line l1(p1, p2);
        MyCAD::Geometry::Line l2(p2, p3);
        WHEN("a Wire is made with them")
        {
            MyCAD::Shapes::Wire wire({l1, l2});
            THEN("we should be able to retrieve the two intermediate Edge.")
            {
                MyCAD::Shapes::Edge e1(l1);
                MyCAD::Shapes::Edge e2(l2);
                std::vector<MyCAD::Shapes::Edge> check({e1, e2});
                REQUIRE_THAT(wire.getEdges(), UnorderedEquals(check));
            }
        }
    }
}

SCENARIO("CAD Programs can create Primitive Solids", "[Shapes]")
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
