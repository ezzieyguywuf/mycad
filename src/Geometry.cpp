/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

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
Arrangement::Arrangement(std::vector<LineSegment> const& segments)
{
    Geometry::Halfedge_handle lastEdge(nullptr);
    for(Geometry::LineSegment const& aLineSegment: lineSegments)
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
