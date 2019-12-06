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
#include <MyCAD/Exceptions.hpp>
#include <MyCAD/Shapes.hpp>

namespace MyCAD{
namespace Shapes{
//=============================================================================
//                      Vertex Class Definition
//=============================================================================
Vertex::Vertex(Geometry::Point const& pnt)
    : myPoint(pnt)
{
}

Geometry::Point const& Vertex::point() const
{
    return myPoint;
}

//=============================================================================
//                      Edge Class Definition
//=============================================================================
Edge::Edge(Geometry::LineSegment const& aLineSegment)
    : myLineSegment(aLineSegment)
{
}

Geometry::LineSegment const& Edge::getLineSegment() const
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
Wire::Wire(std::vector<Geometry::LineSegment> const& lineSegments)
{

    Geometry::LineSegment const& last = lineSegments.at(0);
    for(Geometry::LineSegment const& aLineSegment: lineSegments)
    {
        if (aLineSegment == last)
        {
            continue;
        }
        if(aLineSegment.min() != last.max())
        {
            std::string message = R"err(
Each consecutive LineSegment must line up to the last one!

In other words, given two LineSegment "first" and "next", first.max() === next.min().
Review CGAL::Segment_2 documentation, as well as
CGAL::Arragement_2::insert_from_left_vertex documentation if you want a lot more
information about this (the CGAL::Arragement_2 User Manual in general is a great reference
here.))err"
            throw Exception(message);
        }
        last = aLineSegment;
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
