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
                /** @brief Construct a Point in 3D space
                 *  @param x,y,z The cartesian coordinates of the point
                 */
                Point(float x, float y, float z);

                /// @name Access Methods
                ///@{
                /// @brief Retrieve the given coordinates of the Point.
                float x() const;
                float y() const;
                float z() const;
                ///@}


                /// @name Operators
                ///@{
                bool operator==(Point const& aPoint) const;
                bool operator!=(Point const& aPoint) const;
                ///@}

            private:
                float myX;
                float myY;
                float myZ;
        };

        /** @brief A parametrized line*/
        class Line
        {
            public:
                Line(Point const& p1, Point const& p2);
                float getLowerParameter() const;
                float getUpperParameter() const;
        };
    }
}
#endif // MYCAD_GEOMETRY_HEADER
