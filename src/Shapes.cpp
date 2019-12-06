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

    Geometry::Halfedge_handle lastEdge(nullptr);
    for(Geometry::LineSegment const& aLineSegment: lineSegments)
    {
        Geometry::Segment_2 seg = aLineSegment.getGeometry();
        if (lastEdge == Geometry::Halfedge_handle(nullptr))
        {
            lastEdge = arr.insert_in_face_interior(seg, arr.unbounded_face());
            continue;
        }

        // Find out if they intersect
        auto v = CGAL::intersection(Geometry::Segment_2(lastEdge->curve()), seg);
        if(v)
        {
            const Geometry::Point_2 *p = boost::get<Geometry::Point_2>(&*v);
            if(p)
            {
                // They intersect at a point. But is it one of the end-points?
                Geometry::Vertex_handle lastLeft(nullptr);
                Geometry::Vertex_handle lastRight(nullptr);

                switch(lastEdge->direction())
                {
                    case CGAL::ARR_LEFT_TO_RIGHT:
                    {
                        lastLeft  = lastEdge->source();
                        lastRight = lastEdge->target();
                        break;
                    }
                    case CGAL::ARR_RIGHT_TO_LEFT:
                    {
                        lastLeft  = lastEdge->target();
                        lastRight = lastEdge->source();
                        break;
                    }
                    default:
                    {
                        throw Exception("I don't know how to handle that Arr_halfedge_direction.");
                    }
                }
                if (seg.min() == lastLeft->point()  or
                    seg.max() == lastLeft->point()  or
                    seg.min() == lastRight->point() or
                    seg.max() == lastRight->point())
                {
                    lastEdge = CGAL::insert_non_intersecting_curve(arr, seg);
                    continue;
                }
            }
        }
        throw Exception("Each subsquent LineSegment must share only an end-point with the previous.");
    }
}

std::vector<Edge> Wire::getEdges() const
{
    std::vector<Edge> out;
    for(auto it = arr.edges_begin() ; it != arr.edges_end() ; it++ )
    {
        Geometry::Segment_2 seg(it->curve());
        Geometry::LineSegment line(seg);
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
