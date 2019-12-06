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
#include <MyCAD/Geometry.hpp>

#include <CGAL/intersections.h>

namespace MyCAD{
namespace Geometry{
//=============================================================================
//                        Point Class Definition
//=============================================================================
Point::Point(Kernel::FT const& x, Kernel::FT const& y)
    :myPoint(x, y)
{}

Kernel::FT Point::x() const
{
    return CGAL::to_double(myPoint.x());
}

Kernel::FT Point::y() const
{
    return CGAL::to_double(myPoint.y());
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
