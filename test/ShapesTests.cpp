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
    GIVEN("A point")
    {
        MyCAD::Geometry::Point pnt(1,2,3);
        WHEN("a vertex is created with it")
        {
            MyCAD::Shapes::Vertex vert(pnt);
            THEN("we should be able to get that same point back.")
            {
                REQUIRE(pnt == vert.point());
            }
        }
    }
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
