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

namespace MyCAD{
namespace Geometry{
//=============================================================================
//                        Point Class Definition
//=============================================================================
Point::Point(float x, float y)
    : myPoint(x, y)
{}

float Point::x() const
{
    return CGAL::to_double(myPoint.x());
}

float Point::y() const
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
//                        Line Class Definition
//=============================================================================

Line::Line(Point const& /*p1*/, Point const& /*p2*/)
{
}

float Line::getLowerParameter() const
{
    return 0;
}

float Line::getUpperParameter() const
{
    return 10;
}

bool Line::operator==(Line const& aLine) const
{
    return (this->getLowerParameter() == aLine.getLowerParameter() and 
            this->getUpperParameter() == aLine.getUpperParameter());
}

bool Line::operator!=(Line const& aLine) const
{
    return not (*this == aLine);
}

} // namespace Geometry
} // namespace MyCAD
