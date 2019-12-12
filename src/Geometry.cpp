/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Exceptions.hpp>
#include <MyCAD/Geometry.hpp>

#include <CGAL/intersections.h>

namespace MyCAD{
/** This namespace describes MyCAD's entry-point into the CGAL world. Anything
 *  CGAL-related is fully abstracted away within this namespace. What this means is that
 *  any users of MyCAD can forget about CGAL altogether, and simply access the information
 *  they need using the classes and functions found in this namespace.
 *
 *  As a developer, please keep in mind that this abstraction is intended to be strict on
 *  purpose - in the future, this makes it more feasible to move away from CGAL into a
 *  different library.
 */
namespace Geometry{
namespace
{
    MyCAD::Exception CROSSOVER_EXCEPTION(
            "The segment being added cannot cross over an existing segment.");
    MyCAD::Exception COINCIDENT_EXCEPTION(
            "The segment being added coincides with the existing arrangement, and it's not at an end-vertex");
    MyCAD::Exception NO_END_COINCIDENCE_EXCEPTION(
            "The segment being added is not coincident with one of the open ends of the arrangement.");
} // namespace
//=============================================================================
//                        Point Class Definition
//=============================================================================
/** A point is the simplest geometric construct that is supported in MyCAD. It is
 *  described simply as a set of n-coordinates. The value of "n" is dictated by the type
 *  of CGAL kernel that is being used. At the time of this documentation, 2019-12-07,
 *  there appears to be strong support for 2D geometry in CGAL, very good support for 3D
 *  geometry, and burgeoning support for nD geometry.
 *
 *  Our geometric classes are all currently strictly 2D. This might change in the future,
 *  which will affect the API.
 */
Point::Point(Number const& x, Number const& y)
    :myPoint(x, y)
{}

/** @brief Get the precise x-coordinate.
 */
Number Point::x() const
{
    return myPoint.x();
}

/** @brief Get the precise y-coordinate.
 */
Number Point::y() const
{
    return myPoint.y();
}

bool Point::operator==(Point const& aPoint) const
{
    return myPoint == aPoint.myPoint;
}

bool Point::operator!=(Point const& aPoint) const
{
    return not (*this == aPoint);
}

//=============================================================================
//                        LineSegment Class Definition
//=============================================================================

LineSegment::LineSegment(Point const& p1, Point const& p2)
    : myMonotoneCurve({p1.x(), p1.y()}, {p2.x(), p2.y()})
{}

LineSegment::LineSegment(LineSegment const& seg)
    : myMonotoneCurve(seg.myMonotoneCurve)
{}

Point LineSegment::start() const
{
    return Point(myMonotoneCurve.source().x(), myMonotoneCurve.source().y());
}

Point LineSegment::end() const
{
    return Point(myMonotoneCurve.target().x(), myMonotoneCurve.target().y());
}

Point LineSegment::min() const
{
    Segment_2 seg(myMonotoneCurve);
    return Point(seg.min().x(), seg.min().y());
}

Point LineSegment::max() const
{
    Segment_2 seg(myMonotoneCurve);
    return Point(seg.max().x(), seg.max().y());
}

bool LineSegment::intersects(LineSegment const& aLineSegment) const
{
    auto ans = intersection(getSegment(), aLineSegment.getSegment());
    if(ans)
        return true;
    return false;
}

bool LineSegment::operator==(LineSegment const& aLineSegment) const
{
    return getSegment() == aLineSegment.getSegment();
}

bool LineSegment::operator!=(LineSegment const& aLineSegment) const
{
    return not (*this == aLineSegment);
}

/** The reason this is needed is because Arrangement_2::Traits_2::X_monotone_curve_2 is
 * some sort of wrapper around Segment_2 but does not implement the entire Segment_2
 * interface. It's in the documentation. I don't understand exactly __why__ they did it
 * this way, I just know that they did....
 */
Segment_2 LineSegment::getSegment() const
{
    return Segment_2(myMonotoneCurve);
}

//=============================================================================
//                        Arrangement Class Definition
//=============================================================================
/** @note This calls Arrangement::addSegment on each LineSegment in the vector. Please
 *        note that Arrangement::addSegment may throw an exception
 */
Arrangement::Arrangement(std::vector<LineSegment> const& segments)
{
    Geometry::Halfedge_handle lastEdge(nullptr);
    for(Geometry::LineSegment const& aLineSegment: segments)
    {
        Geometry::Point start = aLineSegment.start();
        Geometry::Point end = aLineSegment.end();

        Geometry::Point_2 source(start.x(), start.y());
        Geometry::Point_2 target(end.x(), end.y());

        Geometry::Segment_2 seg(source, target);

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

std::vector<LineSegment> Arrangement::getLineSegments() const
{
    std::vector<LineSegment> out;
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

bool Arrangement::checkEnd(Halfedge_handle edge, Segment_2 const& segment)
{
    Segment_2 endSeg(edge->curve());

    // We need to know which vertex on the edge is lexicographically the left and the
    // right
    Vertex_handle leftVertex(nullptr);
    Vertex_handle rightVertex(nullptr);
    switch(edge->direction())
    {
        case CGAL::Arr_halfedge_direction::ARR_LEFT_TO_RIGHT:
        {
            leftVertex = edge->source();
            rightVertex = edge->target();
            break;
        }
        case CGAL::Arr_halfedge_direction::ARR_RIGHT_TO_LEFT:
        {
            leftVertex = edge->target();
            rightVertex = edge->source();
            break;
        }
    }

    IntersectionType intersection = intersects(endSeg, segment);

    switch(intersection)
    {
        case IntersectionType::Cross:
            throw CROSSOVER_EXCEPTION;
        case IntersectionType::None:
        {
            return false;
        }
        case  IntersectionType::LeftEnd:
        {
            if(endSeg.min() == segment.max())
            {
                arr.insert_from_right_vertex(segment, leftVertex);
            }
            else
            {
                arr.insert_from_left_vertex(segment, leftVertex);
            }
            return true;
        }
        case IntersectionType::RightEnd:
        {
            if(endSeg.max() == segment.max())
            {
                arr.insert_from_right_vertex(segment, rightVertex);
            }
            else
            {
                arr.insert_from_left_vertex(segment, rightVertex);
            }
            return true;
        }
    }

    return false;
}

bool Arrangement::checkEnds(Segment_2 const& segment)
{
    if(arr.number_of_edges() == 0)
    {
        throw MyCAD::Exception("Must have at least one edge in order to checkEnds");
    }
    Arrangement_2::Edge_iterator first = arr.edges_begin();
    bool ret = checkEnd(first, segment);
    if(not ret)
    {
        Arrangement_2::Edge_iterator end = arr.edges_end();
        end--;
        ret = checkEnd(end, segment);
    }
    return ret;
}

Segment_2 Arrangement::makeSegment(LineSegment const& segment) const
{
    // To start, we'll build a CGAL Segment_2 from our MyCAD::LineSegment
    Point start = segment.start();
    Point end = segment.end();

    Point_2 source(start.x(), start.y());
    Point_2 target(end.x(), end.y());

    return Segment_2(source, target);
}

Arrangement::IntersectionType
Arrangement::intersects(Segment_2 const& seg1, Segment_2 const& seg2)
{
    auto v = CGAL::intersection(seg1, seg2);
    if(v)
    {
        if(const Point_2 *p = boost::get<Point_2>(&*v))
        {
            if(*p == seg1.min())
            {
                return IntersectionType::LeftEnd;
            }
            else if(*p == seg1.max())
            {
                return IntersectionType::RightEnd;
            }
        }
        return IntersectionType::Cross;
    }
    return IntersectionType::None;
}

//=============================================================================
//                      Free (global) Function Definitions
//=============================================================================

std::ostream& operator<< (std::ostream& ost, Point const& aPoint)
{
    ost << "(" << aPoint.x() << ", " << aPoint.y() << ")";
    return ost;
}

std::ostream& operator<< (std::ostream& ost, LineSegment const& aLineSegment)
{
    ost << aLineSegment.start() << " -> " << aLineSegment.end();
    return ost;
}

} // namespace Geometry
} // namespace MyCAD
