/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include "catch.hpp"

#include <MyCAD/Shapes.hpp>

using Catch::Matchers::UnorderedEquals;

SCENARIO("Basic topological entities wrap geometric constructs", "[Shapes]")
{
    GIVEN("A Point")
    {
        MyCAD::Geometry::Point pnt(1,2);
        WHEN("a Vertex is created with it")
        {
            MyCAD::Shapes::Vertex vert(pnt);
            THEN("we should be able to get that same point back.")
            {
                REQUIRE(pnt == vert.point());
            }
        }
    }

    GIVEN("a LineSegment")
    {
        MyCAD::Geometry::Point p1(0,0);
        MyCAD::Geometry::Point p2(10,10);
        MyCAD::Geometry::LineSegment line(p1, p2);
        WHEN("an Edge is created with it")
        {
            MyCAD::Shapes::Edge edge(line);
            THEN("we should be able to retrive the LineSegment")
            {
                REQUIRE(edge.getLineSegment() == line);
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
