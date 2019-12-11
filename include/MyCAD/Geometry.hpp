/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#ifndef MYCAD_GEOMETRY_HEADER
#define MYCAD_GEOMETRY_HEADER

#include <CGAL/Cartesian.h>
#include <CGAL/Arr_segment_traits_2.h>
#include <CGAL/Arrangement_2.h>

#include <ostream>

/** @brief Everything in the MyCAD library is contained in the MyCAD namespace*/
namespace MyCAD
{
/** @brief Anything related to geomtry*/
namespace Geometry
{
/** @{
 *  @name CGAL-Related Typedefs
 *
 *  These typedefs are our entry-point into CGAL. They fully describe the type of geometry
 *  that we can use, as well as the various geometric objects that we understand.
 */
/** @brief The precision for numbers - change as desired/needed.
 */
typedef int                                Number;
typedef CGAL::Cartesian<Number>            Kernel;
typedef CGAL::Arr_segment_traits_2<Kernel> Traits_2;
typedef Traits_2::Point_2                  Point_2;
typedef Traits_2::X_monotone_curve_2       MonoCurve_2;
typedef Kernel::Segment_2                  Segment_2;
typedef CGAL::Arrangement_2<Traits_2>      Arrangement_2;
typedef Arrangement_2::Halfedge_handle     Halfedge_handle;
typedef Arrangement_2::Vertex_handle       Vertex_handle;
///@}


/** @brief A point in space.*/
class Point
{
    public:
        /** @brief Construct a Point in 2D space
         *
         *  @param x,y The cartesian coordinates of the point
         */
        Point(Number const& x, Number const& y);

        /** @name Geometric Coordinate Access
         *
         *  The precision is entirely dictated by the precision of the Number
         *  typedef.
         *
         *  @{
         */
        Number x() const;
        Number y() const;
        /** @} */

        /** @brief Return a copy of the underlying CGAL Point_2*/
        Point_2 getGeometry() const;

        /** @name Operators
         *  @{
         */
        bool operator==(Point const& aPoint) const;
        bool operator!=(Point const& aPoint) const;
        /** @} */

    private:
        Point_2 myPoint;
};

/** @brief A parametrized line*/
class LineSegment
{
    public:
        LineSegment(Point const& p1, Point const& p2);
        LineSegment(Segment_2 const& seg);

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
        MonoCurve_2 myMonotoneCurve;
};

std::ostream& operator<< (std::ostream& ost, Point const& aPoint);
std::ostream& operator<< (std::ostream& ost, LineSegment const& aLineSegment);

} // namespace Geometry
} // namespace MyCAD
#endif // MYCAD_GEOMETRY_HEADER
