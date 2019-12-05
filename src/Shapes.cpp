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
#include <MyCAD/Shapes.hpp>

namespace MyCAD{
namespace Shapes{
//=============================================================================
//                      Vertex Class Definition
//=============================================================================
Vertex::Vertex(MyCAD::Geometry::Point const& pnt)
    : myPoint(pnt)
{
}

MyCAD::Geometry::Point const& Vertex::point() const
{
    return myPoint;
}

//=============================================================================
//                      Edge Class Definition
//=============================================================================
Edge::Edge(MyCAD::Geometry::LineSegment const& aLineSegment)
    : myLineSegment(aLineSegment)
{
}

MyCAD::Geometry::LineSegment const& Edge::getLineSegment() const
{
    return myLineSegment;
}

bool Edge::operator==(Edge const& anEdge) const
{
    return this->getLineSegment() == anEdge.getLineSegment();
}

bool Edge::operator!=(Edge const& anEdge) const
{
    return not (*this == anEdge);
}

//=============================================================================
//                      Wire Class Definition
//=============================================================================
Wire::Wire(std::vector<MyCAD::Geometry::LineSegment> const& lineSegments)
{
    for(MyCAD::Geometry::LineSegment const& aLineSegment: lineSegments)
    {
        myEdges.emplace_back(aLineSegment);
    }
}

std::vector<Edge> const& Wire::getEdges() const
{
    return myEdges;
}

//=============================================================================
//                         Box Class Definition
//=============================================================================
/** Notice that it is possible to pass negative numbers into this constructor. What this
 *  means is that you wish for some portion of the Box to be in negative cartesian space.
 *
 *  To clarify, Box(10, 10, 10), constructs a cube of length 10 in the (+x, +y, +z)
 *  quadrant.
 *
 *  Box(10, 10, -10), however, constructs the same box in the (+x, +y, -z) quadrant.
 */
Box::Box(unsigned int /*x*/, unsigned int /*y*/, unsigned int /*z*/)
{
}

std::vector<Face> Box::getFaces() const
{
    return {Face(), Face(), Face(), Face(), Face(), Face()};
}

} // namespace Shapes
} // namespace MyCAD
