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

#ifndef MYCAD_GEOMETRY_HEADER
#define MYCAD_GEOMETRY_HEADER

#include <CGAL/Simple_cartesian.h>
typedef CGAL::Simple_cartesian<double> Kernel;
typedef Kernel::Point_2 Point_2;
typedef Kernel::Segment_2 Segment_2;

/** @brief Everything in the MyCAD library is contained in the MyCAD namespace*/
namespace MyCAD
{
/** @brief Anything related to geomtry*/
namespace Geometry
{
    /** @brief A point in cartesian space.*/
    class Point
    {
        public:
            /** @brief Construct a Point in 2D space
             *  @param x,y The cartesian coordinates of the point
             */
            Point(float x, float y);

            /// @name Access Methods
            ///@{
            /// @brief Retrieve the given coordinates of the Point.
            float x() const;
            float y() const;
            ///@}


            /// @name Operators
            ///@{
            bool operator==(Point const& aPoint) const;
            bool operator!=(Point const& aPoint) const;
            ///@}

        private:
            Point_2 myPoint;
    };

    /** @brief A parametrized line*/
    class Line
    {
        public:
            Line(Point const& p1, Point const& p2);

            /** @name Parameter Information*/
            ///@{
            /** Get information about the parametrization of the line*/
            float getLowerParameter() const;
            float getUpperParameter() const;
            ///@}

            /** @name operators*/
            ///@{
            bool operator==(Line const& aLine) const;
            bool operator!=(Line const& aLine) const;
            ///@}
    };
} // namespace Geometry
} // namespace MyCAD
#endif // MYCAD_GEOMETRY_HEADER
