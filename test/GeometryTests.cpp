/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include "catch.hpp"

#include <MyCAD/Geometry.hpp>
#include <MyCAD/Exceptions.hpp>

using Point_2 = MyCAD::Geometry::Point_2;
using Catch::Matchers::UnorderedEquals;

SCENARIO("CAD Programs require cartesian geometry", "[Geometry]")
{
    GIVEN("a Point")
    {
        double x = 1;
        double y = 2;
        MyCAD::Geometry::Point pnt(x, y);

        WHEN("any cartesian value is requested")
        {
            THEN("it should equal the value used to construct it")
            {
                Point_2 check(x, y);
                REQUIRE(pnt.x() == check.x());
                REQUIRE(pnt.y() == check.y());
            }
        }
    }

    GIVEN("two Points")
    {
        MyCAD::Geometry::Point p1(0, 0);
        MyCAD::Geometry::Point p2(10, 10);
        WHEN("we make a LineSegment between them")
        {
            MyCAD::Geometry::LineSegment line(p1, p2);
            THEN("we should be able to retrieve the begining and ending points")
            {
                REQUIRE(line.start() == p1);
                REQUIRE(line.end() == p2);
            }
            THEN("a second LineSegment created with them should be equal to the first.")
            {
                MyCAD::Geometry::LineSegment line2(p1, p2);
                REQUIRE(line == line2);
            }
        }
    }

    GIVEN("a LineSegment")
    {
        MyCAD::Geometry::LineSegment s1({0, 0}, {10, 10});

        WHEN("a second, non-overlapping LineSegment is constructed")
        {
            MyCAD::Geometry::LineSegment s2({15, 15}, {20, 20});

            THEN("the two should not intersect")
            {

                REQUIRE_FALSE(s1.intersects(s2));
            }
        }

        WHEN("a second, overlapping LineSegment is constructed")
        {
            MyCAD::Geometry::LineSegment s2({10,0}, {0, 10});
            THEN("the two should intersect")
            {
                REQUIRE(s1.intersects(s2));
            }
        }
    }

    GIVEN("a LineSegment")
    {
        MyCAD::Geometry::LineSegment s1({0,0}, {10, 10});

        WHEN("a second LineSegment shares a Source end-point and is co-linear and non-overlapping")
        {
            MyCAD::Geometry::LineSegment s2({10,10}, {15, 15});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Source end-point and is co-linear and overlaps")
        {
            MyCAD::Geometry::LineSegment s2({10,10}, {5, 5});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_THROWS_AS(MyCAD::Geometry::Arrangement({s1, s2}), MyCAD::Exception);
            }
        }

        WHEN("a second LineSegment shares a Source end-point and is vertical up")
        {
            MyCAD::Geometry::LineSegment s2({10,10}, {10, 15});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Source end-point and is vertical down")
        {
            MyCAD::Geometry::LineSegment s2({10,10}, {10, 5});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Source end-point and goes to the right and up")
        {
            MyCAD::Geometry::LineSegment s2({10,10}, {15, 20});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Source end-point and goes to the left and up")
        {
            MyCAD::Geometry::LineSegment s2({10,10}, {5, 20});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Source end-point and goes to the right and down")
        {
            MyCAD::Geometry::LineSegment s2({10,10}, {15, 0});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Source end-point and goes to the left and down")
        {
            MyCAD::Geometry::LineSegment s2({10,10}, {5, 0});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Target end-point and is co-linear and does not overlap")
        {
            MyCAD::Geometry::LineSegment s2({15,15}, {10, 10});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Target end-point and is co-linear and overlaps")
        {
            MyCAD::Geometry::LineSegment s2({5,5}, {10, 10});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_THROWS_AS(MyCAD::Geometry::Arrangement({s1, s2}), MyCAD::Exception);
            }
        }

        WHEN("a second LineSegment shares a Target end-point and is vertical up")
        {
            MyCAD::Geometry::LineSegment s2({10,0}, {10, 10});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Target end-point and is vertical down")
        {
            MyCAD::Geometry::LineSegment s2({10,15}, {10, 10});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Target end-point and goes up and right")
        {
            MyCAD::Geometry::LineSegment s2({5,0}, {10, 10});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Target end-point and goes up and left")
        {
            MyCAD::Geometry::LineSegment s2({20,0}, {10, 10});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Target end-point and goes down and right")
        {
            MyCAD::Geometry::LineSegment s2({5,20}, {10, 10});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment shares a Target end-point and goes down and left")
        {
            MyCAD::Geometry::LineSegment s2({15,20}, {10, 10});

            THEN("we should be able to construct an Arrangement")
            {
                REQUIRE_NOTHROW(MyCAD::Geometry::Arrangement({s1, s2}));
            }
        }

        WHEN("a second LineSegment does not share an end-point")
        {
            MyCAD::Geometry::LineSegment s2({10,5}, {15, 5});

            THEN("we should NOT be able to construct an Arrangement")
            {
                REQUIRE_THROWS_AS(MyCAD::Geometry::Arrangement({s1, s2}), MyCAD::Exception);
            }
        }
    }
}

