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
Box::Box(unsigned int /*x*/, unsigned int /*y*/, unsigned int /*z*/)
{
}

std::vector<Face> Box::getFaces() const
{
    return {Face(), Face(), Face(), Face(), Face(), Face()};
}
