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

//=============================================================================
//                        LineSegment Class Definition
//=============================================================================

LineSegment::LineSegment(Point const& p1, Point const& p2)
    : mySegment({p1.x(), p1.y()}, {p2.x(), p2.y()})
{}

Point LineSegment::start() const
{
    return Point(mySegment.source().x(), mySegment.source().y());
}

Point LineSegment::end() const
{
    return Point(mySegment.target().x(), mySegment.target().y());
}

bool LineSegment::intersects(LineSegment const& aLineSegment) const
{
    auto ans = intersection(mySegment, aLineSegment.mySegment);
    if(ans)
        return true;
    return false;
}

bool LineSegment::operator==(LineSegment const& aLineSegment) const
{
    return mySegment == aLineSegment.mySegment;
}

bool LineSegment::operator!=(LineSegment const& aLineSegment) const
{
    return not (*this == aLineSegment);
}

} // namespace Geometry
} // namespace MyCAD
