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

#include <CGAL/Cartesian.h>
#include <CGAL/Arr_segment_traits_2.h>
#include <CGAL/Arrangement_2.h>

/** @brief Everything in the MyCAD library is contained in the MyCAD namespace*/
namespace MyCAD
{
/** @brief Anything related to geomtry*/
namespace Geometry
{
    // These typedefs define the portions of the CGAL infrastructure that we'll be using.
    typedef int                                Number_type;
    typedef CGAL::Cartesian<Number_type>       Kernel;
    typedef CGAL::Arr_segment_traits_2<Kernel> Traits_2;
    typedef Traits_2::Point_2                  Point_2;
    typedef Traits_2::X_monotone_curve_2       Segment_2;
    typedef CGAL::Arrangement_2<Traits_2>      Arrangement_2;
    typedef Arrangement_2::Halfedge_handle     Halfedge_handle;
    typedef Arrangement_2::Vertex_handle       Vertex_handle;


    /** @brief A point in cartesian space.*/
    class Point
    {
        public:
            /** @brief Construct a Point in 2D space
             *  @param x,y The cartesian coordinates of the point
             */
            Point(Kernel::FT const& x, Kernel::FT const& y);

            /// @name Access methods
            /// @{
            /// Retrieve the precise coordinates
            Kernel::FT x() const;
            Kernel::FT y() const;
            ///@}

            /** Return a copy of the underlying CGAL Point_2*/
            Point_2 getGeometry() const;

            /// @name Operators
            ///@{
            bool operator==(Point const& aPoint) const;
            bool operator!=(Point const& aPoint) const;
            ///@}

        private:
            Point_2 myPoint;
    };

    /** @brief A parametrized line*/
    class LineSegment
    {
        public:
            LineSegment(Point const& p1, Point const& p2);

            /** Returns the start point of the Segment*/
            Point start() const;
            /** Returns the end point of the Segment*/
            Point end() const;

            /** Returns the point with the lowest x-value */
            Point min() const;
            /** Returns the point with the highest x-value */
            Point max() const;

            /** Check if this LineSegment intersects with another*/
            bool intersects(LineSegment const& aLineSegment) const;

            /** Return a copy of the underlying CGAL Segment_2*/
            Segment_2 getGeometry() const;

            /** @name operators*/
            ///@{
            bool operator==(LineSegment const& aLineSegment) const;
            bool operator!=(LineSegment const& aLineSegment) const;
            ///@}

        private:
            Segment_2 mySegment;
    };
} // namespace Geometry
} // namespace MyCAD
#endif // MYCAD_GEOMETRY_HEADER
