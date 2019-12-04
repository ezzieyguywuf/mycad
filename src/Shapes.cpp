#include <MyCAD/Shapes.hpp>

using namespace MyCAD::Shapes;
//=============================================================================
//                      Vertex Class Definition
//=============================================================================
Vertex::Vertex(MyCAD::Geometry::Point const& pnt)
    : myPoint(pnt)
{
}

MyCAD::Geometry::Point const& Vertex::point() const
{
    return myPoint;
}

//=============================================================================
//                         Box Class Definition
//=============================================================================
/** Notice that it is possible to pass negative numbers into this constructor. What this
 *  means is that you wish for some portion of the Box to be in negative cartesian space.
 *
 *  To clarify, Box(10, 10, 10), constructs a cube of length 10 in the (+x, +y, +z)
 *  quadrant.
 *
 *  Box(10, 10, -10), however, constructs the same box in the (+x, +y, -z) quadrant.
 */
Box::Box(unsigned int /*x*/, unsigned int /*y*/, unsigned int /*z*/)
{
}

std::vector<Face> Box::getFaces() const
{
    return {Face(), Face(), Face(), Face(), Face(), Face()};
}
