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

Point_2 Point::getGeometry() const
{
    return myPoint;
}

//=============================================================================
//                        LineSegment Class Definition
//=============================================================================

LineSegment::LineSegment(Point const& p1, Point const& p2)
    : myMonotoneCurve({p1.x(), p1.y()}, {p2.x(), p2.y()})
{}

LineSegment::LineSegment(Segment_2 const& seg)
    : myMonotoneCurve(seg)
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
    auto ans = intersection(getGeometry(), aLineSegment.getGeometry());
    if(ans)
        return true;
    return false;
}

Segment_2 LineSegment::getGeometry() const
{
    return myMonotoneCurve;
}

bool LineSegment::operator==(LineSegment const& aLineSegment) const
{
    return getGeometry() == aLineSegment.getGeometry();
}

bool LineSegment::operator!=(LineSegment const& aLineSegment) const
{
    return not (*this == aLineSegment);
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
