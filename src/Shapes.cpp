/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Exceptions.hpp>
#include <MyCAD/Shapes.hpp>

namespace MyCAD{
/** A Shape is distinct from geometry. Consider a Point (a geometric entity) defined in
 *  2-dimensional euclidian space at \f$x = 5\f$, \f$y = 5\f$, or more conventionally
 *  \f$(5,5)\f$. By definition, this point is located in a single spot. It cannot be
 *  moved.
 *
 *  Similarly, the line \f$y=x+5\f$ specifically describes an infinite line that crosses
 *  the y-axis at \f$y=5\f$ with a slope of \f$1\f$. Again - this line cannot be
 *  translated or rotated, because then we would have a different line entirely.
 *
 *  This is what we refer to as "Geometry", and we have classes for some common geometric
 *  constructs in the Geometry namespace.
 *
 *  Now, let's consider a Vertex, a "Topological" entity. What I mean by "Topological" is
 *  that it exists as a single-point in space (exactly like a Point), but is not affected
 *  by changes in size/shape/location.
 *
 *  In other words, a Vertex can be moved. Similarly, an Edge (which is like a Line, but
 *  bounded, i.e. not infinite) can be translated, rotated etc. Wheneven these
 *  transformations happen, by definition, the Edge will need a new, distinct Line to
 *  describe it geometrically. But it's still the Same Edge - only the Line has changed.
 *
 *  Just like moving a Vertex means we're associating it with a different Point - but it's
 *  still the same Vertex.
 */
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
/** A Wire is comprised of several "interlocking" LineSegment. Each succesive LineSegment
 *  provided to the constructor _must_ be connected to either the first LineSegment or the
 *  previous LineSegment at one end.
 *
 *  In other words:
 *
 *  1. None of the LineSegment can cross or intersect in any way
 *  2. Each LineSegment must be connected to at least one other LineSegment by way of one
 *     of its ends
 *  3. The order in which the LineSegment are provided is important.
 */
Wire::Wire(std::vector<Edge> const& /*edges*/)
{
}

std::vector<Edge> Wire::getEdges() const
{
    std::vector<Edge> out;
    for(auto it = arr.edges_begin() ; it != arr.edges_end() ; it++ )
    {
        Geometry::Segment_2 seg(it->curve());
        Geometry::Point source(seg.source().x(), seg.source().y());
        Geometry::Point target(seg.target().x(), seg.target().y());
        Geometry::LineSegment line(source, target);
        out.emplace_back(line);
    }
    return out;
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

//=============================================================================
//                      Free (global) Function Definitions
//=============================================================================

std::ostream& operator<< (std::ostream& ost, Vertex const& aVertex)
{
    ost << aVertex.point();
    return ost;
}

std::ostream& operator<< (std::ostream& ost, Edge const& anEdge)
{
    ost << anEdge.getLineSegment();
    return ost;
}

std::ostream& operator<< (std::ostream& ost, Wire const& aWire)
{
    bool first = true;
    for(auto const& anEdge : aWire.getEdges())
    {
        if (not first)
        {
            ost << " => ";
        }
        if (first)
        {
            first = false;
        }
        ost << anEdge;
    }

    return ost;
}
} // namespace Shapes
} // namespace MyCAD
